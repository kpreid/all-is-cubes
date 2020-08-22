// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use cgmath::{Vector2, Vector3};
use lazy_static::lazy_static;
use rayon::iter::{ParallelIterator, IntoParallelIterator};
use std::convert::TryInto;
use std::io;
use termion::{color};
use termion::event::{Event, Key};

use all_is_cubes::camera::{Camera, ProjectionHelper};
use all_is_cubes::math::{RGB, RGBA};
use all_is_cubes::raycast::{Face, Raycaster};
use all_is_cubes::space::{GridArray, PackedLight, Space};

/// Processes events for moving a camera. Returns all those events it does not process.
pub fn controller(camera: &mut Camera, event: Event) -> Option<Event> {
    camera.auto_rotate = false;  // stop on any keypress
    match event {
        Event::Key(key) => match key {
            Key::Char('w') | Key::Char('W') => { camera.walk(0.0, -1.0); },
            Key::Char('a') | Key::Char('A') => { camera.walk(-1.0, 0.0); },
            Key::Char('s') | Key::Char('S') => { camera.walk(0.0, 1.0); },
            Key::Char('d') | Key::Char('D') => { camera.walk(1.0, 0.0); },
            Key::Up => { camera.body.pitch += 5.0; },
            Key::Left => { camera.body.yaw += 5.0; },
            Key::Down => { camera.body.pitch -= 5.0; },
            Key::Right => { camera.body.yaw -= 5.0; },
            _ => { return Some(event); },
        },
        _ => { return Some(event); },
    }
    None  // match branches default to consuming event
}

pub fn viewport_from_terminal_size() -> io::Result<Vector2<usize>> {
    let (w, h) = termion::terminal_size()?;
    Ok(Vector2::new(w.max(1) as usize, (h - 5).max(1) as usize))
}

// Draw the space to an ANSI terminal using raytracing.
pub fn draw_space<O: io::Write>(
    space: &Space,
    projection: &mut ProjectionHelper,
    camera: &Camera,
    out: &mut O,
) -> io::Result<()> {
    // TODO: consider refactoring so that we don't need &mut Camera
    projection.set_view_matrix(camera.view());
    let projection_copy_for_parallel: ProjectionHelper = *projection;

    // Diagnostic info accumulators
    let mut number_of_cubes_examined :usize = 0;

    // Copy data out of Space (whose access is not thread safe due to contained URefs).
    let grid = *space.grid();
    let space_data: GridArray<TracingData> = space.extract(grid, |_index, block, lighting| {
        TracingData {
            color: block.color(),
            lighting,
            // TODO precompute this on indices
            character: block.attributes().display_name.chars().next().unwrap_or(' '),
        }
    });

    // Construct iterator over pixel positions.
    let viewport = projection.viewport();
    let pixel_iterator = (0..viewport.y).into_par_iter().map(move |ych| {
        let y = -projection.normalize_pixel_y(ych);
        (0..viewport.x).into_par_iter().map(move |xch| {
            let x = projection_copy_for_parallel.normalize_pixel_x(xch);
            (xch, ych, x, y)
        })
    }).flatten();

    let output_iterator = pixel_iterator.map(move |(xch, ych, x, y)| {
        let (origin, direction) = projection_copy_for_parallel.project_ndc_into_world(x, y);
        let ray = Raycaster::new(origin, direction).within_grid(grid);
        let (text, count) = character_from_ray(ray, &space_data);
        (xch, ych, text, count)
    });

    // The actual output writing must be done serially, so collect() the results.
    write!(out, "{}", termion::cursor::Goto(1, 1))?;
    for (xch, ych, text, count) in output_iterator.collect::<Vec<_>>() {
        if xch == 0 && ych != 0 {
            write!(out, "{}", *END_OF_LINE)?;
        }
        number_of_cubes_examined += count;
        write!(out, "{}", text)?;
    }
    write!(out,
        "{}{}Cubes traced through: {}\r\n",
        *END_OF_LINE,
        termion::clear::AfterCursor,
        number_of_cubes_examined)?;
    out.flush()?;

    Ok(())
}

#[derive(Clone, Debug)]
struct TracingData {
    color: RGBA,
    lighting: PackedLight,
    character: char,
}

/// Compute a colored character corresponding to what one ray hits.
fn character_from_ray(ray :Raycaster, space_data: &GridArray<TracingData>) -> (String, usize) {
    fn scale(x :f32) -> u8 {
        let scale = 5.0;
        (x * scale).max(0.0).min(scale) as u8
    }

    let mut number_passed: usize = 0;
    let mut hit_text: Option<String> = None;
    let mut color_accumulator = RGB::ZERO;
    let mut ray_alpha = 1.0;
    for hit in ray {
        number_passed += 1;
        if number_passed > 1000 {
            // Abort excessively long traces.
            return (
                format!("{}{}X", color::Bg(color::Red), color::Fg(color::Black)),
                number_passed);
        }

        let block = &space_data[hit.cube];
        let color = block.color;
        if color.alpha() <= 0.0 {
            continue;
        }

        // Use text of the first non-completely-transparent block.
        if hit_text.is_none() {
            // TODO: For more Unicode correctness, index by grapheme cluster...
            // ...and do something clever about double-width characters.
            hit_text = Some(block.character.to_string());
        }
        
        // Find lighting.
        let lighting: RGB = space_data.get(hit.previous_cube())
            .map(|b| b.lighting.into())
            .unwrap_or(PackedLight::SKY.into());

        // Blend in color of this block.
        // Note this is not true volumetric ray tracing: we're considering each
        // block to be discrete.
        let alpha_for_add = color.alpha() * ray_alpha;
        ray_alpha *= 1.0 - color.alpha();
        color_accumulator += fake_lighting_adjustment(color.to_rgb() * lighting, hit.face)
             * alpha_for_add;
    }

    if number_passed == 0 {
        // Didn't intersect the world at all. Draw these as plain background.
        return (
            format!("{}{} ", color::Bg(color::Reset), color::Fg(color::Reset)),
            0,
        );
    }

    // Fill up color buffer with "sky" color.
    let sky_vary = (number_passed.min(4) as f32) / 5.0;
    let sky_color = RGB::new(sky_vary, sky_vary, 1.0);
    color_accumulator += sky_color * ray_alpha;
    
    // TODO: Pick 8/256/truecolor based on what the terminal supports.
    let converted_color = color::AnsiValue::rgb(
        scale(color_accumulator.red()),
        scale(color_accumulator.green()),
        scale(color_accumulator.blue()));
    let colored_text = if let Some(text) = hit_text {
        format!("{}{}{}",
            color::Bg(converted_color),
            color::Fg(color::Black),
            text)
    } else {
        format!("{}{}.",
            color::Bg(converted_color),
            color::Fg(color::AnsiValue::rgb(5, 5, 5)))
    };
    (colored_text, number_passed)
}

fn fake_lighting_adjustment(rgb: RGB, face: Face) -> RGB {
    let one_step = 1.0/5.0;
    let v = Vector3::new(1.0, 1.0, 1.0);
    let modifier = match face {
        Face::PY => v * one_step * 2.0,
        Face::NY => v * one_step * -1.0,
        Face::NX | Face::PX => v * one_step * 1.0,
        _ => v * 0.0,
    };
    rgb + modifier.try_into().unwrap()
}

lazy_static! {
    static ref END_OF_LINE: String = format!("{}{}\r\n",
        color::Bg(color::Reset),
        color::Fg(color::Reset));
}

// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use cgmath::{EuclideanSpace as _, Point3, Vector2, Vector3};
use lazy_static::lazy_static;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::convert::TryInto;
use std::io;
use termion::color;
use termion::event::{Event, Key};
use typed_arena::Arena;

use all_is_cubes::camera::{Camera, ProjectionHelper};
use all_is_cubes::math::{FreeCoordinate, RGB, RGBA};
use all_is_cubes::raycast::{Face, Raycaster};
use all_is_cubes::space::{Grid, GridArray, PackedLight, Space, SKY};

/// Processes events for moving a camera. Returns all those events it does not process.
#[rustfmt::skip]
pub fn controller(camera: &mut Camera, event: Event) -> Option<Event> {
    camera.auto_rotate = false;  // stop on any keypress
    match event {
        Event::Key(key) => match key {
            Key::Char('w') | Key::Char('W') => { camera.set_velocity_input(( 0.0,  0.0, -1.0)); },
            Key::Char('a') | Key::Char('A') => { camera.set_velocity_input((-1.0,  0.0,  0.0)); },
            Key::Char('s') | Key::Char('S') => { camera.set_velocity_input(( 0.0,  0.0,  1.0)); },
            Key::Char('d') | Key::Char('D') => { camera.set_velocity_input(( 1.0,  0.0,  0.0)); },
            Key::Char('e') | Key::Char('E') => { camera.set_velocity_input(( 0.0,  1.0,  0.0)); },
            Key::Char('c') | Key::Char('C') => { camera.set_velocity_input(( 0.0, -1.0,  0.0)); },
            Key::Up => { camera.body.pitch += 5.0; },
            Key::Down => { camera.body.pitch -= 5.0; },
            Key::Left => { camera.body.yaw -= 5.0; },
            Key::Right => { camera.body.yaw += 5.0; },
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
    let mut number_of_cubes_examined: usize = 0;

    // Copy data out of Space (whose access is not thread safe due to contained URefs).
    let grid = *space.grid();
    let recur_arena = Arena::with_capacity(space.distinct_blocks_unfiltered().len());
    let block_data: Vec<TracingBlock> = space
        .distinct_blocks_unfiltered()
        .iter()
        .map(|block| {
            let character = block
                .attributes()
                .display_name
                .chars()
                .next()
                .unwrap_or(' ');
            if let Some(space_ref) = block.space() {
                let block_space = space_ref.borrow();
                TracingBlock::Recur(recur_arena.alloc(block_space.extract(
                    *block_space.grid(),
                    |_index, sub_block, _lighting| {
                        // Do not recurse indefinitely, just one level, because that's the
                        // standard visualization. TODO: But maybe it'd be more fun if...?
                        (character, sub_block.color())
                    },
                )))
            } else {
                TracingBlock::Atom(character, block.color())
            }
        })
        .collect();
    let space_data: GridArray<TracingCubeData> =
        space.extract(grid, |index, _block, lighting| TracingCubeData {
            block: block_data[index as usize],
            lighting,
        });

    // Construct iterator over pixel positions.
    let viewport = projection.viewport();
    let pixel_iterator = (0..viewport.y)
        .into_par_iter()
        .map(move |ych| {
            let y = -projection.normalize_pixel_y(ych);
            (0..viewport.x).into_par_iter().map(move |xch| {
                let x = projection_copy_for_parallel.normalize_pixel_x(xch);
                (xch, ych, x, y)
            })
        })
        .flatten();

    let output_iterator = pixel_iterator.map(move |(xch, ych, x, y)| {
        let (origin, direction) = projection_copy_for_parallel.project_ndc_into_world(x, y);
        let (text, count) = character_from_ray(origin, direction, grid, &space_data);
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
    write!(
        out,
        "{}{}Cubes traced through: {}\r\n",
        *END_OF_LINE,
        termion::clear::AfterCursor,
        number_of_cubes_examined
    )?;
    out.flush()?;

    Ok(())
}

/// Compute a colored character corresponding to what one ray hits.
fn character_from_ray(
    origin: Point3<FreeCoordinate>,
    direction: Vector3<FreeCoordinate>,
    grid: Grid,
    space_data: &GridArray<TracingCubeData>,
) -> TraceResult {
    let mut s: TracingState = TracingState::default();
    for hit in Raycaster::new(origin, direction).within_grid(grid) {
        if s.count_step_should_stop() {
            break;
        }

        let cube_data = &space_data[hit.cube];
        match &cube_data.block {
            TracingBlock::Atom(character, color) => {
                if color.alpha() <= 0.0 {
                    // Skip lighting lookup
                    continue;
                }

                // Find lighting.
                let lighting: RGB = space_data
                    .get(hit.previous_cube())
                    .map(|b| b.lighting.into())
                    .unwrap_or(SKY);

                s.trace_through_surface(*character, *color, lighting, hit.face);
            }
            TracingBlock::Recur(array) => {
                // Find lighting.
                // TODO: duplicated code
                let lighting: RGB = space_data
                    .get(hit.previous_cube())
                    .map(|b| b.lighting.into())
                    .unwrap_or(SKY);

                // Find where the origin in the space's coordinate system is.
                // TODO: Raycaster does not efficiently implement advancing from outside a
                // grid. Fix that to get way more performance.
                let adjusted_origin = Point3::from_vec(
                    (origin - hit.cube.cast::<FreeCoordinate>().unwrap())
                        * (array.grid().size().x as FreeCoordinate),
                );

                for subcube_hit in
                    Raycaster::new(adjusted_origin, direction).within_grid(*array.grid())
                {
                    if s.count_step_should_stop() {
                        break;
                    }
                    let (character, color) = array[subcube_hit.cube];
                    s.trace_through_surface(character, color, lighting, subcube_hit.face);
                }
            }
        }
    }
    s.finish()
}

#[derive(Clone, Debug)]
struct TracingCubeData<'a> {
    block: TracingBlock<'a>,
    lighting: PackedLight,
}
#[derive(Clone, Copy, Debug)]
enum TracingBlock<'a> {
    Atom(char, RGBA),
    Recur(&'a GridArray<(char, RGBA)>),
}

type TraceResult = (String, usize);

#[derive(Clone, Debug)]
struct TracingState {
    /// Number of cubes traced through -- controlled by the caller, so not necessarily
    /// equal to the number of calls to trace_through_surface().
    number_passed: usize,
    /// Character to draw, if determined yet.
    hit_text: Option<String>,
    /// Color buffer, in “premultiplied alpha” format (each contribution is scaled by
    /// the current `ray_alpha`).
    color_accumulator: RGB,
    /// Fraction of the color value that is to be determined by future, rather than past,
    /// tracing; starts at 1.0 and decreases as opaque surfaces are encountered.
    ray_alpha: f32,
}
impl TracingState {
    fn count_step_should_stop(&mut self) -> bool {
        self.number_passed += 1;
        if self.number_passed > 1000 {
            // Abort excessively long traces.
            self.hit_text = Some("X".to_string());
            self.color_accumulator = RGB::new(1.0, 0.0, 0.0);
            self.ray_alpha = 0.0;
            true
        } else {
            // Check if we've passed through essentially full opacity.
            self.ray_alpha <= 1e-3
        }
    }

    #[rustfmt::skip]
    fn finish(mut self) -> TraceResult {
        if self.number_passed == 0 {
            // Didn't intersect the world at all. Draw these as plain background.
            return (format!("{}{} ", color::Bg(color::Reset), color::Fg(color::Reset)), 0);
        }

        // Fill up color buffer with "sky" color. Also use it to visualize the world boundary.
        let sky_vary = 1.0 - (self.number_passed.min(3) as f32 - 1.0) / 5.0;
        let sky_color = RGB::new(sky_vary, sky_vary, 1.0);
        self.color_accumulator += sky_color * self.ray_alpha;
        self.ray_alpha = 0.0;

        // TODO: Pick 8/256/truecolor based on what the terminal supports.
        fn scale(x: f32) -> u8 {
            let scale = 5.0;
            (x * scale).max(0.0).min(scale) as u8
        }
        let converted_color = color::AnsiValue::rgb(
            scale(self.color_accumulator.red()),
            scale(self.color_accumulator.green()),
            scale(self.color_accumulator.blue()),
        );
        let colored_text = if let Some(text) = self.hit_text {
            format!("{}{}{}",
                color::Bg(converted_color),
                color::Fg(color::Black),
                text)
        } else {
            format!("{}{}.",
                color::Bg(converted_color),
                color::Fg(color::AnsiValue::rgb(5, 5, 5)))
        };
        (colored_text, self.number_passed)
    }

    /// Apply the effect of a given surface color.
    ///
    /// Note this is not true volumetric ray tracing: we're considering each
    /// voxel s to be discrete.
    fn trace_through_surface(&mut self, character: char, surface: RGBA, lighting: RGB, face: Face) {
        let surface_alpha = surface.alpha();
        if surface_alpha <= 0.0 {
            return;
        }
        let alpha_for_add = surface_alpha * self.ray_alpha;
        self.ray_alpha *= 1.0 - surface_alpha;
        self.color_accumulator +=
            fake_lighting_adjustment(surface.to_rgb() * lighting, face) * alpha_for_add;

        // Use text of the first non-completely-transparent block.
        if self.hit_text.is_none() {
            // TODO: For more Unicode correctness, index by grapheme cluster...
            // ...and do something clever about double-width characters.
            self.hit_text = Some(character.to_string());
        }
    }
}
impl Default for TracingState {
    fn default() -> Self {
        Self {
            number_passed: 0,
            hit_text: None,
            color_accumulator: RGB::ZERO,
            ray_alpha: 1.0,
        }
    }
}

fn fake_lighting_adjustment(rgb: RGB, face: Face) -> RGB {
    let one_step = 1.0 / 5.0;
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
    static ref END_OF_LINE: String =
        format!("{}{}\r\n", color::Bg(color::Reset), color::Fg(color::Reset));
}

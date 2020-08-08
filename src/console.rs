// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use cgmath::{Basis2, Deg, EuclideanSpace, Matrix4, Ortho, Point3, Rotation, Rotation2, Transform, Vector2, Vector3, Vector4};
use rayon::iter::{ParallelIterator, IntoParallelIterator};
use std::io;
use termion;
use termion::{color};
use termion::event::{Event, Key};

use crate::camera::Camera;
use crate::math::{FreeCoordinate};
use crate::raycast::{Face, Raycaster};
use crate::space::{Grid, Space};

type M = Matrix4<FreeCoordinate>;

/// Describes a camera for an ascii-art view of 3D space.
pub struct View {
    viewport: Vector2<u16>,
    pub camera: Camera,
}

impl View {
    /// Fits the given grid into the viewport.
    pub fn for_grid(viewport: Vector2<u16>, grid: &Grid) -> Self {
        let character_aspect_ratio = 0.5;
        let viewport_aspect_ratio :FreeCoordinate =
            viewport.x as FreeCoordinate
            / viewport.y as FreeCoordinate
             * character_aspect_ratio;

        Self {
            viewport: viewport,
            camera: Camera::for_grid(viewport_aspect_ratio, grid),
        }
    }

    fn matrix(&self) -> M {
        // Inverse of regular camera matrix because we are transforming screen space to
        // world space to raycast, instead of transforming vertices in world space to 
        // screen space.
        self.camera.matrix().inverse_transform().unwrap()
    }

    /// Processes events for moving a View. Returns all those events it does not process.
    pub fn controller(&mut self, event: Event) -> Option<Event> {
        match event {
            Event::Key(key) => match key {
                Key::Char('w') | Key::Char('W') => { self.camera.walk(0.0, -1.0); },
                Key::Char('a') | Key::Char('A') => { self.camera.walk(1.0, 0.0); },
                Key::Char('s') | Key::Char('S') => { self.camera.walk(0.0, 1.0); },
                Key::Char('d') | Key::Char('D') => { self.camera.walk(-1.0, 0.0); },
                Key::Up => { self.camera.pitch += 5.0; },
                Key::Left => { self.camera.yaw += 5.0; },
                Key::Down => { self.camera.pitch -= 5.0; },
                Key::Right => { self.camera.yaw -= 5.0; },
                _ => { return Some(event); },
            },
            _ => { return Some(event); },
        }
        None  // match branches default to consuming event
    }
}

pub fn viewport_from_terminal_size() -> io::Result<Vector2<u16>> {
    let (w, h) = termion::terminal_size()?;
    Ok(Vector2::new(w.max(1), (h - 5).max(1)))
}

// Draw the space to an ANSI terminal using raytracing.
pub fn draw_space<O: io::Write>(space: &Space, view: &View, out: &mut O) -> io::Result<()> {
    let view_matrix = view.matrix();

    // Diagnostic info accumulators
    let mut number_of_cubes_examined :usize = 0;

    let pixel_iterator = (0..view.viewport.y).into_par_iter().map(move |ych| {
        let y = normalize_pixel_coordinate(ych, view.viewport.y);
        (0..view.viewport.x).into_par_iter().map(move |xch| {
            let x = normalize_pixel_coordinate(xch, view.viewport.x);
            (xch, ych, x, y)
        })
    }).flatten();

    let output_iterator = pixel_iterator.map(move |(xch, ych, x, y)| {
        let (text, count) = character_from_cell(x, y, &view_matrix, &space);
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

/// As per OpenGL normalized device coordinates, output ranges from -1 to 1.
fn normalize_pixel_coordinate(position: u16, size: u16) -> FreeCoordinate {
    (position as FreeCoordinate + 0.5) / (size as FreeCoordinate) * -2.0 + 1.0
}

fn character_from_cell(x: FreeCoordinate, y: FreeCoordinate, view_matrix: &Matrix4<FreeCoordinate>, space: &Space) -> (String, usize) {
    // Homogeneous-coordinate endpoints of the camera ray.
    let ndc_near = Vector4::new(x, y, 1.0, 1.0);
    let ndc_far = Vector4::new(x, y, -1.0, 1.0);
    // World-space endpoints of the camera ray.
    let world_near = Point3::from_homogeneous(view_matrix * ndc_near);
    let world_far = Point3::from_homogeneous(view_matrix * ndc_far);
    let direction = world_far - world_near;

    let ray = Raycaster::new(world_near, direction).within_grid(*space.grid());
    character_from_ray(ray, space)
}

/// Compute a colored character corresponding to what one ray hits.
fn character_from_ray(ray :Raycaster, space: &Space) -> (String, usize) {
    fn scale(x :f32) -> u8 {
        let scale = 5.0;
        (x * scale).max(0.0).min(scale) as u8
    }

    let mut number_passed :usize = 0;
    for hit in ray {
        number_passed += 1;
        if number_passed > 1000 {
            // Abort excessively long traces.
            return (
                format!("{}{}X", color::Bg(color::Red), color::Fg(color::Black)),
                number_passed);
        }

        let block = &space[hit.cube];
        let attributes = &block.attributes();
        let color = block.color();
        // TODO: Implement general transparency. We might as well.
        if color.binary_opaque() {
            let rgba = block.color().to_rgba();
            // TODO: Pick 8/256/truecolor based on what the terminal supports.
            let rgba = fake_lighting_adjustment(rgba, hit.face);
            let converted_color = color::AnsiValue::rgb(
                scale(rgba.x), scale(rgba.y), scale(rgba.z));
            return (
                format!("{}{}{}",
                    color::Bg(converted_color),
                    color::Fg(color::Black),
                    &attributes.display_name[0..1]),
                number_passed);
        }
    }

    // Didn't hit any blocks; write a "sky".
    let s :String;
    if number_passed > 0 {
        // Intersected the world, but no opaque blocks.
        let c = number_passed.min(4) as u8;
        s = format!("{}{}.",
            color::Bg(color::AnsiValue::rgb(c, c, 5)),
            color::Fg(color::AnsiValue::rgb(5, 5, 5)));
    } else {
        // Didn't intersect the world at all. Draw these as plain background
        s = format!("{}{} ",
            color::Bg(color::Reset),
            color::Fg(color::Reset));
    }
    return (s, number_passed);
}

fn fake_lighting_adjustment(rgba :Vector4<f32>, face :Face) -> Vector4<f32> {
    let one_step = 1.0/5.0;
    let v = Vector4::new(1.0, 1.0, 1.0, 0.0);
    let modifier = match face {
        Face::PY => v * one_step * 2.0,
        Face::NY => v * one_step * -1.0,
        Face::NX | Face::PX => v * one_step * 1.0,
        _ => v * 0.0,
    };
    rgba + modifier
}

lazy_static! {
    static ref END_OF_LINE: String = format!("{}{}\r\n",
        color::Bg(color::Reset),
        color::Fg(color::Reset));
}

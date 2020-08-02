// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use cgmath::{Deg, EuclideanSpace, Matrix4, Ortho, Point3, Transform, Vector2, Vector4};
use std::io;
use termion;
use termion::{color};

use crate::math::{FreeCoordinate};
use crate::raycast::Raycaster;
use crate::space::{Grid, Space};

type M = Matrix4<FreeCoordinate>;

/// Describes a camera for an ascii-art view of 3D space.
pub struct View {
    viewport: Vector2<u16>,
    projection: M,
    center: Point3<FreeCoordinate>,
    pub yaw: FreeCoordinate,
}

impl View {
    /// Fits the given grid into the viewport.
    pub fn for_grid(viewport: Vector2<u16>, grid: &Grid) -> Self {
        // TODO: ew, complicated and also wrongish (need euclidean distance to corner), but this is all prototype stuff anyway.
        let grid_view_radius = *(grid.size()[..].into_iter().max().unwrap()) as FreeCoordinate * 0.8;

        let character_aspect_ratio = 0.5;  // approximately half as wide as high

        let viewport_aspect_ratio :FreeCoordinate =
            viewport.x as FreeCoordinate
            / viewport.y as FreeCoordinate
             * character_aspect_ratio;

        View {
            viewport: viewport,
            projection: Ortho {
                left: -grid_view_radius * viewport_aspect_ratio,
                right: grid_view_radius * viewport_aspect_ratio,
                bottom: -grid_view_radius,
                top: grid_view_radius,
                // TODO: far & near should be longer...
                far: grid_view_radius * 1.0,
                near: -grid_view_radius * 1.0,
            }.into(),
            center: ((grid.lower_bounds() + grid.upper_bounds().to_vec()) / 2)
                .map(|x| x as FreeCoordinate),
            yaw: 0.0,
        }
    }

    fn matrix(&self) -> M {
        // TODO: For performance, we ought to do the inverse to the projection once
        (
            self.projection
            * Matrix4::from_angle_x(Deg(self.yaw * 0.5))
            * Matrix4::from_angle_y(Deg(self.yaw))
            * Matrix4::from_translation(-(self.center.to_vec()))
        ).inverse_transform().unwrap()
    }
}

// Draw the space to an ANSI terminal using raytracing.
pub fn draw_space<O: io::Write>(space: &Space, view: &View, out: &mut O) -> io::Result<()> {
    let &grid = space.grid();
    let view_matrix = view.matrix();

    write!(out, "{}", termion::cursor::Goto(1, 1))?;

    for ych in 0..view.viewport.y {
        let y = normalize_pixel_coordinate(ych, view.viewport.y);
        for xch in 0..view.viewport.x {
            let x = normalize_pixel_coordinate(xch, view.viewport.x);

            // Homogeneous-coordinate endpoints of the camera ray.
            let ndc_near = Vector4::new(x, y, 1.0, 1.0);
            let ndc_far = Vector4::new(x, y, -1.0, 1.0);
            // World-space endpoints of the camera ray.
            let world_near = Point3::from_homogeneous(view_matrix * ndc_near);
            let world_far = Point3::from_homogeneous(view_matrix * ndc_far);

            let ray = Raycaster::new(world_near, world_far - world_near).within_grid(grid);
            character_from_ray(ray, space, out)?;
        }
        // End of line. Reset the color so that if the terminal is bigger, we don't
        // fill the rest of the line with the last pixel color.
        write!(out, "{}{}\n",
            color::Bg(color::Reset),
            color::Fg(color::Reset))?;
    }

    Ok(())
}

/// As per OpenGL normalized device coordinates, output ranges from -1 to 1.
fn normalize_pixel_coordinate(position: u16, size: u16) -> FreeCoordinate {
    (position as FreeCoordinate + 0.5) / (size as FreeCoordinate) * -2.0 + 1.0
}

/// Write a character corresponding to what one ray hits.
fn character_from_ray<O: io::Write>(ray :Raycaster, space: &Space, out: &mut O) -> io::Result<()> {
    fn scale(x :f32) -> u8 {
        let scale = 5.0;
        (x * scale).max(0.0).min(scale) as u8
    }

    let mut number_passed :usize = 0;
    for hit in ray {
        number_passed += 1;
        if number_passed > 1000 {
            // Abort excessively long traces.
            write!(out, "{}{}X", color::Bg(color::Red), color::Fg(color::Black))?;
            return Ok(());
        }

        let block = &space[hit.cube];
        let attributes = &block.attributes();
        let rgba = block.color().to_rgba();
        if rgba[3] > 0.5 {
            // TODO: Pick 8/256/truecolor based on what the terminal supports.
            let converted_color = color::AnsiValue::rgb(
                scale(rgba.x), scale(rgba.y), scale(rgba.z));
            write!(out, "{}{}{}",
                color::Bg(converted_color),
                color::Fg(color::Black),
                &attributes.display_name[0..1])?;
            return Ok(());
        }
    }

    // Didn't hit any blocks; write a "sky".
    if number_passed > 0 {
        // Intersected the world, but no opaque blocks.
        let c = number_passed.min(4) as u8;
        write!(out, "{}{}.",
            color::Bg(color::AnsiValue::rgb(c, c, 5)),
            color::Fg(color::AnsiValue::rgb(5, 5, 5)))?;
    } else {
        // Didn't intersect the world at all. Draw these as plain background
        write!(out, "{}{} ",
            color::Bg(color::Reset),
            color::Fg(color::Reset))?;
    }
    return Ok(());
}
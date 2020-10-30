// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use cgmath::{EuclideanSpace as _, Point3, Vector2};
use once_cell::sync::Lazy;
use ordered_float::NotNan;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::io;
use termion::color;
use termion::event::{Event, Key};

use all_is_cubes::camera::{Camera, ProjectionHelper};
use all_is_cubes::math::{FreeCoordinate, RGB, RGBA};
use all_is_cubes::raycast::{Face, Ray};
use all_is_cubes::raytracer::{ColorBuf, PixelBuf};
use all_is_cubes::space::{Grid, GridArray, PackedLight};

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

// Draw the camera's space to an ANSI terminal using raytracing.
pub fn draw_space<O: io::Write>(
    projection: &mut ProjectionHelper,
    camera: &Camera,
    out: &mut O,
) -> io::Result<()> {
    let space = &*camera.space.borrow_mut();
    projection.set_view_matrix(camera.view());
    let projection_copy_for_parallel: ProjectionHelper = *projection;

    // Diagnostic info accumulators
    let mut number_of_cubes_examined: usize = 0;

    // Copy data out of Space (whose access is not thread safe due to contained URefs).
    let grid = *space.grid();
    let indexed_block_data: Vec<TracingBlock> = space
        .distinct_blocks_unfiltered_iter()
        .map(|block_data| {
            let evaluated = block_data.evaluated();
            // TODO: For more Unicode correctness, index by grapheme cluster...
            // ...and do something clever about double-width characters.
            let character: &str = evaluated.attributes.display_name.get(0..1).unwrap_or(&" ");
            if let Some(ref voxels) = evaluated.voxels {
                TracingBlock::Recur(character, voxels)
            } else {
                TracingBlock::Atom(character, block_data.evaluated().color)
            }
        })
        .collect();
    let space_data: GridArray<TracingCubeData> =
        space.extract(grid, |index, _block, lighting| TracingCubeData {
            block: indexed_block_data[index as usize],
            lighting,
        });
    let sky = space.sky_color();

    // Construct iterator over pixel positions.
    let viewport = projection.viewport();
    let pixel_iterator = (0..viewport.y)
        .into_par_iter()
        .map(move |ych| {
            let y = projection.normalize_pixel_y(ych);
            (0..viewport.x).into_par_iter().map(move |xch| {
                let x = projection_copy_for_parallel.normalize_pixel_x(xch);
                (xch, ych, x, y)
            })
        })
        .flatten();

    let output_iterator = pixel_iterator.map(move |(xch, ych, x, y)| {
        let ray = projection_copy_for_parallel.project_ndc_into_world(x, y);
        let (text, count) = character_from_ray(ray, grid, &space_data, sky);
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
    ray: Ray,
    grid: Grid,
    space_data: &GridArray<TracingCubeData>,
    sky: RGB,
) -> TraceResult {
    let mut s: TracingState = TracingState::default();
    for hit in ray.cast().within_grid(grid) {
        if s.count_step_should_stop() {
            break;
        }

        let cube_data = &space_data[hit.cube];
        match &cube_data.block {
            TracingBlock::Atom(character, color) => {
                if color.fully_transparent() {
                    // Skip lighting lookup
                    continue;
                }

                // Find lighting.
                let lighting: RGB = space_data
                    .get(hit.previous_cube())
                    .map(|b| b.lighting.into())
                    .unwrap_or(sky);

                s.trace_through_surface(*character, *color, lighting, hit.face);
            }
            TracingBlock::Recur(character, array) => {
                // Find lighting.
                // TODO: duplicated code
                let lighting: RGB = space_data
                    .get(hit.previous_cube())
                    .map(|b| b.lighting.into())
                    .unwrap_or(sky);

                // Find where the origin in the space's coordinate system is.
                // TODO: Raycaster does not efficiently implement advancing from outside a
                // grid. Fix that to get way more performance.
                let adjusted_ray = Ray {
                    origin: Point3::from_vec(
                        (ray.origin - hit.cube.cast::<FreeCoordinate>().unwrap())
                            * FreeCoordinate::from(array.grid().size().x),
                    ),
                    ..ray
                };

                for subcube_hit in adjusted_ray.cast().within_grid(*array.grid()) {
                    if s.count_step_should_stop() {
                        break;
                    }
                    let color = array[subcube_hit.cube];
                    s.trace_through_surface(*character, color, lighting, subcube_hit.face);
                }
            }
        }
    }
    s.finish(sky)
}

#[derive(Clone, Debug)]
struct TracingCubeData<'a> {
    block: TracingBlock<'a>,
    lighting: PackedLight,
}
#[derive(Clone, Copy, Debug)]
enum TracingBlock<'a> {
    Atom(&'a str, RGBA),
    Recur(&'a str, &'a GridArray<RGBA>),
}

type TraceResult = (String, usize);

#[derive(Clone, Debug, Default)]
struct TracingState {
    /// Number of cubes traced through -- controlled by the caller, so not necessarily
    /// equal to the number of calls to trace_through_surface().
    number_passed: usize,
    pixel_buf: CharacterBuf,
}
impl TracingState {
    fn count_step_should_stop(&mut self) -> bool {
        self.number_passed += 1;
        if self.number_passed > 1000 {
            // Abort excessively long traces.
            self.pixel_buf = Default::default();
            self.pixel_buf.add(RGBA::new(1.0, 1.0, 1.0, 1.0), "X");
            true
        } else {
            self.pixel_buf.opaque()
        }
    }

    #[rustfmt::skip]
    fn finish(mut self, sky_color: RGB) -> TraceResult {
        if self.number_passed == 0 {
            // Didn't intersect the world at all. Draw these as plain background.
            // TODO: Switch to using the sky color, unless debugging options are set.
            return (format!("{}{} ", color::Bg(color::Reset), color::Fg(color::Reset)), 0);
        }

        self.pixel_buf.add(sky_color.with_alpha_one(), &" ");

        (self.pixel_buf.result(), self.number_passed)
    }

    /// Apply the effect of a given surface color.
    ///
    /// Note this is not true volumetric ray tracing: we're considering each
    /// voxel surface to be discrete.
    fn trace_through_surface(&mut self, character: &str, surface: RGBA, lighting: RGB, face: Face) {
        if surface.fully_transparent() {
            return;
        }
        let adjusted_rgb = fake_lighting_adjustment(surface.to_rgb() * lighting, face);
        self.pixel_buf
            .add(adjusted_rgb.with_alpha(surface.alpha()), character);
    }
}

fn fake_lighting_adjustment(rgb: RGB, face: Face) -> RGB {
    // TODO: notion of "one step" is less coherent ...
    let one_step = 1.0 / 5.0;
    let modifier = match face {
        Face::PY => RGB::ONE * one_step * 2.0,
        Face::NY => RGB::ONE * one_step * -1.0,
        Face::NX | Face::PX => RGB::ONE * one_step * 1.0,
        _ => RGB::ONE * 0.0,
    };
    rgb + modifier
}

/// Implements `PixelBuf` for text output.
#[derive(Clone, Debug, Default, PartialEq)]
struct CharacterBuf {
    color: ColorBuf,

    /// Character to draw, if determined yet.
    hit_text: Option<String>,
}

impl CharacterBuf {
    fn result(self) -> String {
        let final_rgb = self.color.result().to_rgb();
        // TODO: Pick 8/256/truecolor based on what the terminal supports.
        fn scale(x: NotNan<f32>) -> u8 {
            let scale = 5.0;
            (x.into_inner() * scale).max(0.0).min(scale) as u8
        }
        let converted_color = color::AnsiValue::rgb(
            scale(final_rgb.red()),
            scale(final_rgb.green()),
            scale(final_rgb.blue()),
        );
        if let Some(text) = self.hit_text {
            format!(
                "{}{}{}",
                color::Bg(converted_color),
                color::Fg(color::Black),
                text
            )
        } else {
            format!(
                "{}{}.",
                color::Bg(converted_color),
                color::Fg(color::AnsiValue::rgb(5, 5, 5))
            )
        }
    }
}

impl PixelBuf for CharacterBuf {
    fn add(&mut self, surface_color: RGBA, character: &str) {
        self.color.add(surface_color, character);

        if self.hit_text.is_none() {
            self.hit_text = Some(character.to_owned());
        }
    }

    fn opaque(&self) -> bool {
        self.color.opaque()
    }
}

static END_OF_LINE: Lazy<String> =
    Lazy::new(|| format!("{}{}\r\n", color::Bg(color::Reset), color::Fg(color::Reset)));

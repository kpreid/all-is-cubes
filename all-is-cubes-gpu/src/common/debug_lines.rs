use alloc::vec::Vec;

use all_is_cubes::character::{Character, Cursor};
use all_is_cubes::content::palette;
use all_is_cubes::math::{Face7, FreePoint, LineVertex, Rgba, Wireframe};
use all_is_cubes::space::Space;
use all_is_cubes::util::MapExtend;
use all_is_cubes_render::camera::GraphicsOptions;

/// TODO: give this trait a better name, especially now that `LineVertex` exists.
pub(crate) trait DebugLineVertex {
    fn from_position_color(position: FreePoint, color: Rgba) -> Self;
}

/// The `Character`'s space should be the given `Space` if both are present
pub(crate) fn gather_debug_lines<V: DebugLineVertex>(
    character: Option<&Character>,
    space: Option<&Space>,
    graphics_options: &GraphicsOptions,
    v: &mut Vec<V>,
    cursor_result: Option<&Cursor>,
) {
    if let Some(space) = space {
        if graphics_options.debug_behaviors {
            for item in space.behaviors().query_any(None) {
                wireframe_vertices(
                    v,
                    palette::DEBUG_BEHAVIOR_BOUNDS,
                    &item.attachment.bounds().to_free(),
                );
            }
        }

        // Show light update debug info.
        // This is enabled/disabled inside the lighting algorithm, not as a graphics
        // option.
        for cube in space.last_light_updates() {
            wireframe_vertices(v, Rgba::new(1.0, 1.0, 0.0, 1.0), &cube.aab().expand(0.005));
        }

        // Lighting trace at cursor
        if graphics_options.debug_light_rays_at_cursor {
            if let Some(cursor) = cursor_result {
                if std::ptr::eq(&*cursor.space().read().unwrap(), space) {
                    let result = space
                        .compute_lighting::<all_is_cubes::space::LightUpdateCubeInfo>(
                            cursor.preceding_cube(),
                        );
                    wireframe_vertices(v, Rgba::new(0.8, 0.8, 1.0, 1.0), &result.debug);
                }
            }
        }
    }

    if let Some(character) = character {
        if graphics_options.debug_collision_boxes {
            // Character collision box
            wireframe_vertices(
                v,
                palette::DEBUG_COLLISION_BOX,
                &character.body.collision_box_abs(),
            );
            // What it collided with
            for contact in &character.colliding_cubes {
                let color = if contact.normal() == Face7::Within {
                    palette::DEBUG_COLLISION_CUBE_WITHIN
                } else {
                    palette::DEBUG_COLLISION_CUBE_AGAINST
                };
                wireframe_vertices(v, color, contact);
            }
        }
    }
}

/// Add the wireframe of `geometry` to `vertices` (to be drawn in [`Line`](Mode::Line)
/// mode) with the given `color`.
pub(crate) fn wireframe_vertices<V, E, G>(vertices: &mut E, color: Rgba, geometry: &G)
where
    E: Extend<V>,
    V: DebugLineVertex,
    G: Wireframe,
{
    geometry.wireframe_points(&mut map_line_vertices(vertices, color))
}

pub(crate) fn map_line_vertices<'a, V: DebugLineVertex + 'a>(
    vertices: &'a mut impl Extend<V>,
    color: Rgba,
) -> impl Extend<LineVertex> + 'a {
    MapExtend::new(
        vertices,
        move |LineVertex {
                  position,
                  color: vertex_color,
                  ..
              }| V::from_position_color(position, vertex_color.unwrap_or(color)),
    )
}

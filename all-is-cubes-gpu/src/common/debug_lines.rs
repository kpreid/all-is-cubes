use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::cgmath::Point3;
use all_is_cubes::character::{Character, Cursor};
use all_is_cubes::content::palette;
use all_is_cubes::math::{Aab, FreeCoordinate, Geometry, Rgba};
use all_is_cubes::util::MapExtend;

/// TODO: give this trait a better name
pub(crate) trait DebugLineVertex {
    fn from_position_color(position: Point3<FreeCoordinate>, color: Rgba) -> Self;
}

pub(crate) fn gather_debug_lines<V: DebugLineVertex>(
    character: Option<&Character>,
    graphics_options: &GraphicsOptions,
    v: &mut Vec<V>,
    cursor_result: Option<&Cursor>,
) {
    // All of these debug visualizations currently depend on the character
    // TODO: Make it possible to render debug_behaviors with just a Space
    if let Some(character) = character {
        if graphics_options.debug_behaviors {
            if let Ok(space) = character.space.try_borrow() {
                for item in space.behaviors().query_any(None) {
                    wireframe_vertices(
                        v,
                        palette::DEBUG_BEHAVIOR_BOUNDS,
                        &Aab::from(item.attachment.bounds()),
                    );
                }
            }
        }

        if graphics_options.debug_collision_boxes {
            // Character collision box
            wireframe_vertices(
                v,
                palette::DEBUG_COLLISION_BOX,
                &character.body.collision_box_abs(),
            );
            // What it collided with
            for contact in &character.colliding_cubes {
                wireframe_vertices(v, palette::DEBUG_COLLISION_CUBES, contact);
            }
        }

        // Show light update debug info.
        // This is enabled/disabled inside the lighting algorithm, not as a graphics
        // option.
        for cube in character.space.borrow().last_light_updates.iter().copied() {
            wireframe_vertices(
                v,
                Rgba::new(1.0, 1.0, 0.0, 1.0),
                &Aab::from_cube(cube).expand(0.005),
            );
        }

        // Lighting trace at cursor
        if graphics_options.debug_light_rays_at_cursor {
            if let Some(cursor) = cursor_result {
                // TODO: We should be able to draw wireframes in the UI space too, and when we do that will enable supporting this.
                if cursor.space() == &character.space {
                    let space = character.space.borrow();
                    let (_, _, _, lighting_info) = space
                        .compute_lighting::<all_is_cubes::space::LightUpdateCubeInfo>(
                            cursor.preceding_cube(),
                        );
                    wireframe_vertices(v, Rgba::new(0.8, 0.8, 1.0, 1.0), &lighting_info);
                }
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
    G: Geometry,
{
    geometry.wireframe_points(&mut MapExtend::new(
        vertices,
        |(p, vertex_color): (Point3<FreeCoordinate>, Option<Rgba>)| {
            V::from_position_color(p, vertex_color.unwrap_or(color))
        },
    ))
}

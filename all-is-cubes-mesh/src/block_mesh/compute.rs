//! The algorithm for generating block meshes, and nothing else.

use alloc::vec::Vec;

use itertools::Itertools as _;

use all_is_cubes::block::{AnimationChange, EvaluatedBlock, Evoxel, Evoxels, Resolution};
use all_is_cubes::euclid::point2;
use all_is_cubes::math::{
    Cube, Face6, FreeCoordinate, GridAab, GridCoordinate, OpacityCategory, Rgb, Vol,
};
use all_is_cubes_render::Flaws;

use crate::block_mesh::planar::{
    greedy_mesh, push_quad, GmRect, QuadColoring, QuadTransform, VisualVoxel,
};
use crate::block_mesh::{analyze::analyze, BlockFaceMesh};
use crate::texture::{self, Tile as _};
use crate::{BlockMesh, MeshOptions, MeshTypes, Viz};

/// Generate the [`BlockMesh`] data for the given [`EvaluatedBlock`], writing it into `output`.
///
/// This private function is called by [`BlockMesh`]'s public functions.
pub(super) fn compute_block_mesh<M: MeshTypes>(
    output: &mut BlockMesh<M>,
    block: &EvaluatedBlock,
    texture_allocator: &M::Alloc,
    options: &MeshOptions,
    mut viz: Viz,
) {
    output.clear();

    // If this is true, avoid using vertex coloring even on solid rectangles.
    // We do this because:
    // * the block may be animated such that it is useful to reuse the mesh and change the
    //   texture, or
    // * the block has any light emission, which we do not support via vertex coloring.
    //   (TODO: This isn't quite the right condition, because a block might e.g. have emissive
    //   voxels only on its interior or something.)
    let prefer_textures = block.attributes().animation_hint.redefinition != AnimationChange::None
        || block.light_emission() != Rgb::ZERO;

    let flaws = &mut output.flaws;

    let resolution = block.resolution();

    // If `options.ignore_voxels` is set, substitute the block color for the
    // actual voxels.
    let tmp_block_color_voxel: Evoxels;
    let voxels: &Evoxels = if options.ignore_voxels {
        tmp_block_color_voxel = Evoxels::from_one(Evoxel::from_color(block.color()));
        &tmp_block_color_voxel
    } else {
        block.voxels()
    };

    // Short-circuit case: if we have a single voxel a.k.a. resolution 1, then we generate a
    // mesh without going through all the conversion steps.
    if let Some(mut voxel) = voxels.single_voxel() {
        voxel.color = options.transparency.limit_alpha(voxel.color);

        // TODO: Use `EvaluatedBlock::face_colors` to color each face separately.
        // (We'll need to map the faces into a texture if prefer_textures)

        // If we want to use a texture, try to allocate it.
        output.texture_used = if prefer_textures {
            texture::copy_voxels_to_new_texture(texture_allocator, voxels)
        } else {
            None
        };
        // If we successfully decided to use a texture, use that.
        let texture_plane;
        let coloring = if let Some(tile) = &output.texture_used {
            texture_plane = tile.slice(GridAab::ORIGIN_CUBE);
            QuadColoring::Texture(&texture_plane)
        } else {
            // Note: we can't reach here unless there is no emission or allocation failed,
            // so it's okay that we are not checking voxel.emission.
            QuadColoring::Solid(voxel.color)
        };

        let is_fully_opaque = voxel.color.fully_opaque();
        for (face, face_mesh) in output.face_vertices.iter_mut() {
            if voxel.opacity_category() != OpacityCategory::Invisible {
                face_mesh.vertices.reserve_exact(4);
                push_quad(
                    &mut face_mesh.vertices,
                    if is_fully_opaque {
                        face_mesh.indices_opaque.reserve_exact(6);
                        &mut face_mesh.indices_opaque
                    } else {
                        face_mesh.indices_transparent.reserve_exact(6);
                        &mut face_mesh.indices_transparent
                    },
                    &QuadTransform::new(face, Resolution::R1),
                    /* depth= */ 0.,
                    point2(0., 0.),
                    point2(1., 1.),
                    coloring,
                    &mut viz,
                    &mut face_mesh.bounding_box,
                );
            }
            face_mesh.fully_opaque = is_fully_opaque;
        }
    } else {
        viz.voxels(voxels);
        let voxels_array = voxels.as_vol_ref();

        // Exit when the voxel data is not at all in the right volume, or is empty.
        // This dodges some integer overflow cases on bad input,
        // and avoids asking the texture allocator for an empty allocation.
        // TODO: Add a test for this case
        if voxels_array
            .bounds()
            .intersection_cubes(GridAab::for_block(resolution))
            .is_none()
        {
            return;
        }

        let analysis = analyze(resolution, voxels_array, &mut viz);

        let mut used_any_vertex_colors = false;

        let block_resolution = GridCoordinate::from(resolution);

        // Construct empty output to mutate.
        for (_, face_mesh) in output.face_vertices.iter_mut() {
            // Start assuming opacity; if we find any transparent pixels we'll set
            // this to false. `Within` is always "transparent" because the algorithm
            // that consumes this structure will say "draw this face if its adjacent
            // cube's opposing face is not opaque", and `Within` means the adjacent
            // cube is ourself.
            face_mesh.fully_opaque = true;
        }
        let output_interior = &mut output.interior_vertices;

        let texture_if_needed: Option<M::Tile> = if prefer_textures || analysis.needs_texture {
            texture::copy_voxels_to_new_texture(texture_allocator, voxels)
        } else {
            None
        };

        // Walk through the planes (layers) of the block, figuring out what geometry to
        // generate for each layer and whether it needs a texture.
        for face in Face6::ALL {
            let voxel_transform = face.face_transform(block_resolution);
            let quad_transform = QuadTransform::new(face, resolution);
            let face_mesh = &mut output.face_vertices[face];

            // Rotate the voxel array's extent into our local coordinate system, so we can find
            // out what range to iterate over.
            let rotated_voxel_range = voxels_array
                .bounds()
                .transform(voxel_transform.inverse())
                .unwrap();

            // Check the case where the block's voxels don't meet its front face.
            // If they do, then we'll take care of `fully_opaque` later, but if we don't even
            // iterate over layer 0, we need this extra check.
            if !analysis.surface_is_occupied(face) {
                face_mesh.fully_opaque = false;
            }

            // Layer 0 is the outside surface of the cube and successive layers are
            // deeper below that surface.
            for (layer, occupied_rect) in analysis.occupied_planes(face) {
                if !rotated_voxel_range.z_range().contains(&layer) {
                    // TODO: This is a workaround for a bug in the analyzer; it should not be
                    // marking out-of-bounds planes as occupied.
                    continue;
                }

                // Check the case where we aren't iterating over the entire front face.
                // If they are, then we'll take care of `fully_opaque` later, but if we don't even
                // iterate over the whole face, we need this extra check.
                if layer == 0
                    && (occupied_rect.x_range() != (0..block_resolution)
                        || occupied_rect.y_range() != (0..block_resolution))
                {
                    face_mesh.fully_opaque = false;
                }

                viz.set_layer_in_progress(face, layer);

                // Becomes true if there is any voxel that is both non-fully-transparent and
                // not obscured by another voxel on top.
                let mut layer_is_visible_somewhere = false;

                // Contains a color with alpha > 0 for every voxel that _should be drawn_.
                // That is, it excludes all obscured interior volume.
                // First, we traverse the block and fill this with non-obscured voxels,
                // then we erase it as we convert contiguous rectangles of it to quads.
                let mut visible_image: Vec<VisualVoxel> =
                    Vec::with_capacity(usize::try_from(occupied_rect.area()).unwrap_or(0));

                let texture_plane_if_needed: Option<<M::Tile as texture::Tile>::Plane> =
                    if let Some(ref texture) = texture_if_needed {
                        // Compute the exact texture slice we will be accessing.
                        // TODO: It would be better if this were shrunk to the visible voxels
                        // in this specific layer, not just all voxels.
                        let slice_range = GridAab::from_ranges([
                            occupied_rect.x_range(),
                            occupied_rect.y_range(),
                            layer..layer + 1,
                        ])
                        .transform(face.face_transform(block_resolution))
                        .unwrap();

                        Some(texture.slice(slice_range))
                    } else {
                        None
                    };

                for (t, s) in occupied_rect
                    .y_range()
                    .cartesian_product(occupied_rect.x_range())
                {
                    let cube: Cube = voxel_transform.transform_cube(Cube::new(s, t, layer));
                    let evoxel = get_voxel_with_limit(voxels_array, cube, options);

                    if layer == 0 && !evoxel.color.fully_opaque() {
                        // If the first layer is transparent in any cube at all, then the face is
                        // not fully opaque
                        face_mesh.fully_opaque = false;
                    }

                    let voxel_is_visible = {
                        use OpacityCategory::{Invisible, Opaque, Partial};
                        let this_cat = evoxel.opacity_category();
                        if this_cat == Invisible {
                            false
                        } else {
                            // Compute whether this voxel is not hidden behind another
                            let obscuring_cat = get_voxel_with_limit(
                                voxels_array,
                                cube + face.normal_vector(),
                                options,
                            )
                            .opacity_category();
                            match (this_cat, obscuring_cat) {
                                // Nothing to draw no matter what
                                (Invisible, _) => false,
                                // Definitely obscured
                                (_, Opaque) => false,
                                // Completely visible.
                                (Partial | Opaque, Invisible) => true,
                                // Partially obscured, therefore visible.
                                (Opaque, Partial) => true,
                                // This is the weird one: we count transparency adjacent to
                                // transparency as if there was nothing to draw. This is
                                // because:
                                // (1) If we didn't, we would end up generating large
                                //     numbers (bad) of intersecting (also bad) quads
                                //     for any significant volume of transparency.
                                // (2) TODO: We intend to delegate responsibility for
                                //     complex transparency to the shader. Until then,
                                //     this is still better for the first reason.
                                (Partial, Partial) => false,
                            }
                        }
                    };
                    if voxel_is_visible {
                        layer_is_visible_somewhere = true;
                        visible_image.push(VisualVoxel {
                            reflectance: evoxel.color,
                            emission: evoxel.emission != Rgb::ZERO,
                        });
                    } else {
                        // All obscured voxels are treated as transparent ones, in that we don't
                        // generate geometry for them.
                        visible_image.push(VisualVoxel::INVISIBLE);
                    }
                }

                if !layer_is_visible_somewhere {
                    // No need to analyze further.
                    continue;
                }

                // Pick where we're going to store the quads.
                // Only the cube-surface faces go anywhere but `Within`.
                // (We could generalize this to blocks with concavities that still form a
                // light-tight seal against the cube face.)
                let BlockFaceMesh {
                    vertices,
                    indices_opaque,
                    indices_transparent,
                    bounding_box,
                    ..
                } = if layer == 0 {
                    &mut *face_mesh
                } else {
                    &mut *output_interior
                };
                let depth = FreeCoordinate::from(layer);

                // Traverse `visible_image` using the "greedy meshing" algorithm for
                // breaking an irregular shape into quads.
                greedy_mesh(
                    visible_image,
                    occupied_rect.x_range(),
                    occupied_rect.y_range(),
                )
                .for_each(|rect| {
                    let GmRect {
                        single_color,
                        has_alpha: rect_has_alpha,
                        low_corner,
                        high_corner,
                    } = rect;
                    // Generate quad.
                    let coloring =
                        if let Some(single_color) = single_color.filter(|_| !prefer_textures) {
                            // The quad we're going to draw has identical texels, so we might as
                            // well use a solid color and skip needing a texture.
                            QuadColoring::<<M::Tile as texture::Tile>::Plane>::Solid(single_color)
                        } else if let Some(ref plane) = texture_plane_if_needed {
                            QuadColoring::<<M::Tile as texture::Tile>::Plane>::Texture(plane)
                        } else {
                            // Texture allocation failure.
                            // Report the flaw and use block color as a fallback.
                            // Further improvement that could be had here:
                            // * Compute and use per-face colors in EvaluatedBlock
                            // * Offer the alternative of generating as much
                            //   geometry as needed.
                            *flaws |= Flaws::MISSING_TEXTURES;
                            QuadColoring::<<M::Tile as texture::Tile>::Plane>::Solid(
                                options.transparency.limit_alpha(block.color()),
                            )
                        };

                    if matches!(coloring, QuadColoring::Solid(_)) {
                        used_any_vertex_colors = true;
                    }

                    push_quad(
                        vertices,
                        if rect_has_alpha {
                            indices_transparent
                        } else {
                            indices_opaque
                        },
                        &quad_transform,
                        depth,
                        low_corner.map(FreeCoordinate::from),
                        high_corner.map(FreeCoordinate::from),
                        coloring,
                        &mut viz,
                        bounding_box,
                    );
                });
            }
        }

        viz.clear_layer_in_progress();

        output.texture_used = texture_if_needed;
        output.voxel_opacity_mask = if used_any_vertex_colors {
            None
        } else {
            Some(block.voxel_opacity_mask().clone())
        };
    }
}

fn get_voxel_with_limit(voxels: Vol<&[Evoxel]>, cube: Cube, options: &MeshOptions) -> Evoxel {
    let mut evoxel = *voxels.get(cube).unwrap_or(&Evoxel::AIR);
    evoxel.color = options.transparency.limit_alpha(evoxel.color);
    evoxel
}

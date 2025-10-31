//! The algorithm for generating block meshes, and nothing else.

use alloc::vec::Vec;

use itertools::Itertools as _;

use all_is_cubes::block::{self, AnimationChange, EvaluatedBlock, Evoxel, Evoxels, Resolution};
use all_is_cubes::euclid::Scale;
use all_is_cubes::math::{
    Cube, Face6, GridAab, GridCoordinate, GridSizeCoord, OctantMask, OpacityCategory, Rgb, Rgba,
    Vol, ZeroOne,
};
use all_is_cubes_render::Flaws;

use crate::block_mesh::analyze::{Analysis, AnalysisVertex, analyze};
use crate::block_mesh::extend::{
    BoxColoring, QuadColoring, push_box, push_full_box, push_vertices_from_iter,
};
use crate::block_mesh::planar;
use crate::texture::{self, Plane as _, Tile as _};
use crate::{BlockMesh, IndexSlice, MeshOptions, MeshTypes, SubMesh, Viz, vertex};

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

    let voxels = block.voxels();
    if voxels.bounds().is_empty() {
        // There cannot be anything to draw. Don't inflict this edge case on the rest of our math.
        return;
    }

    // If this is true, avoid using vertex coloring even on solid rectangles.
    // We do this because:
    // * the block may be animated such that it is useful to reuse the mesh and change the
    //   texture, or
    // * the block has any light emission, which we do not support via vertex coloring.
    //   (TODO: This isn't quite the right condition, because a block might e.g. have emissive
    //   voxels only on its interior or something.)
    let prefer_textures = block.attributes().animation_hint.redefinition != AnimationChange::None
        || block.light_emission() != Rgb::ZERO;

    let resolution = block.resolution();

    if resolution != Resolution::R1 && options.ignore_voxels {
        // Substitute the block color for the actual voxels.
        // Note: This discards emission, which we are considering acceptable for now.
        let face_colors = block.face_colors().map(|face, full_face_color| {
            // Increase the color's alpha to account for that we are drawing restricted bounds
            // instead of full bounds.
            // TODO: It would be more direct for `EvaluatedBlock` to provide us this value since
            // it is trivial to compute there, but is it worth storing that value always?
            let voxels_surface_area =
                block.voxels().bounds().abut(face, 1).unwrap().volume().unwrap() as f32;
            let full_surface_area = GridSizeCoord::from(resolution).pow(2) as f32;
            let partial_face_color =
                full_face_color.to_rgb().with_alpha(ZeroOne::<f32>::new_clamped(
                    full_face_color.alpha().into_inner()
                        * (full_surface_area / voxels_surface_area),
                ));

            options.transparency.limit_alpha(partial_face_color)
        });
        push_box(
            output,
            resolution,
            block.voxels().bounds(),
            face_colors
                .into_values_iter()
                .map(Rgba::opacity_category)
                .all_equal_value()
                .unwrap_or(OpacityCategory::Partial),
            &BoxColoring::Solid(face_colors),
            &mut viz,
        );
        return;
    }

    // Short-circuit case: if we have a single voxel a.k.a. resolution 1, then we generate a
    // mesh without going through all the conversion steps.
    if let Some(mut voxel) = voxels.single_voxel() {
        voxel.color = options.transparency.limit_alpha(voxel.color);

        // TODO: Use `EvaluatedBlock::face_colors` to color each face separately.
        // (We'll need to map the faces into a texture if prefer_textures)

        // If we want to use a texture, try to allocate it.
        output.texture_used = if prefer_textures {
            // TODO: this doesn't use the result of `limit_alpha()` but should.
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

        push_full_box(output, voxel.opacity_category(), coloring, &mut viz);
    } else {
        viz.voxels(voxels);
        viz.completed_step();
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

        let analysis = analyze(
            resolution,
            voxels_array,
            options.transparency_format(),
            &mut viz,
        );

        compute_block_mesh_from_analysis(
            output,
            voxels,
            &analysis,
            prefer_textures,
            texture_allocator,
            options,
            block.voxel_opacity_mask(),
            &mut viz,
        );

        if let Some(bbox) = analysis.transparent_bounding_box
            && let Some(tile) = &output.texture_used
        {
            let tile = tile.clone(); // TODO: needed to avoid borrowing conflict, but we should fix that by making push_box borrow less
            push_box(
                output,
                resolution,
                bbox,
                OpacityCategory::Partial,
                &BoxColoring::VolumeTexture(tile),
                &mut viz,
            );
        }
    }
}

/// The portion of [`compute_block_mesh()`] that has almost no special cases or initialization;
/// just starts slinging triangles.
///
/// This function assumes that `output` has been cleared and writes to every part of it.
fn compute_block_mesh_from_analysis<M: MeshTypes>(
    output: &mut BlockMesh<M>,
    voxels: &Evoxels,
    analysis: &Analysis,
    prefer_textures: bool,
    texture_allocator: &M::Alloc,
    options: &MeshOptions,
    voxel_opacity_mask: &block::VoxelOpacityMask,
    viz: &mut Viz,
) {
    let flaws = &mut output.flaws;
    let mut used_any_vertex_colors = false;

    let resolution = voxels.resolution();
    let resolution_g = GridCoordinate::from(resolution);
    let scale_to_block: Scale<f32, Cube, crate::MeshRel> = Scale::new(resolution.recip_f32());
    let voxels_array = voxels.as_vol_ref();

    for (_, sub_mesh) in output.face_vertices.iter_mut() {
        // Start assuming opacity; if we find any transparent pixels we'll set
        // this to false. `Within` is always "transparent" because the algorithm
        // that consumes this structure will say "draw this face if its adjacent
        // cube's opposing face is not opaque", and `Within` means the adjacent
        // cube is ourself.
        sub_mesh.fully_opaque = true;
    }

    let texture_if_needed: Option<M::Tile> = if prefer_textures || analysis.needs_texture {
        let allocation = texture::copy_voxels_to_new_texture(texture_allocator, voxels);
        if allocation.is_none() {
            *flaws |= Flaws::MISSING_TEXTURES;
        }
        allocation
    } else {
        None
    };

    // Triangulator contains allocations that we reuse for all planes of the block.
    // TODO: Allow reusing them across multiple blocks, if that is faster.
    let mut triangulator = planar::PlanarTriangulator::new();

    let mut vertex_subset: Vec<AnalysisVertex> = Vec::with_capacity(analysis.vertices.len() / 2);

    // Walk through the planes (layers) of the block, figuring out what geometry to
    // generate for each layer and whether it needs a texture.
    for face in Face6::ALL {
        // TODO: rename voxel_transform to be clearer about what coordinate system this is
        let voxel_transform = face.face_transform(resolution_g);
        let voxel_transform_inverse = voxel_transform.inverse();
        let face_mesh = &mut output.face_vertices[face];
        let interior_mesh = &mut output.interior_vertices[face];
        let interior_side_octant_mask = OctantMask::ALL.shift(-face);

        let mut triangulator_basis = planar::PtBasis::new(
            face,
            /* sweep_direction: */
            match face {
                Face6::NX => Face6::PY,
                Face6::NY => Face6::PX,
                Face6::NZ => Face6::PX,
                Face6::PX => Face6::PY,
                Face6::PY => Face6::PX,
                Face6::PZ => Face6::PX,
            },
            /* perpendicular_direction: */
            match face {
                Face6::NX => Face6::PZ,
                Face6::NY => Face6::PZ,
                Face6::NZ => Face6::PY,
                Face6::PX => Face6::PZ,
                Face6::PY => Face6::PZ,
                Face6::PZ => Face6::PY,
            },
            // Note: transparent will get toggled later. TODO: express this more cleanly?
            /* transparent: */
            false,
        );

        // Check the case where the block's voxels don't meet its front face.
        // If they do, then we'll take care of `fully_opaque` later, but if we don't even
        // iterate over layer 0, we need this extra check.
        if !analysis.surface_is_occupied(face) {
            face_mesh.fully_opaque = false;
        }

        // Layer 0 is the outside surface of the cube and successive layers are
        // deeper below that surface.
        // `occupied_planes()` tells us which planes have any actually visible voxel surfaces,
        // so this loop can skip fully solid or empty volumes.
        for (layer, occupied_rect) in analysis.occupied_planes(face) {
            // Check whether the block face is fully opaque and can be considered to occlude
            // adjacent blocks.
            // TODO: It would make sense to move this calculation to block evaluation, which
            // already has to do 99% of the work.
            if layer == 0 {
                let full_range: core::ops::Range<GridCoordinate> = (0..resolution_g).into();
                if occupied_rect.x_range() != full_range || occupied_rect.y_range() != full_range {
                    // Not full coverage
                    face_mesh.fully_opaque = false;
                } else {
                    // Check all the voxels
                    for (t, s) in occupied_rect.y_range().cartesian_product(occupied_rect.x_range())
                    {
                        let cube: Cube = voxel_transform.transform_cube(Cube::new(s, t, layer));
                        let evoxel = get_voxel_with_limit(voxels_array, cube, options);

                        if !evoxel.color.fully_opaque() {
                            // If the first layer is transparent in any cube at all, then the face is
                            // not fully opaque
                            face_mesh.fully_opaque = false;
                        }
                    }
                }
            }

            viz.set_layer_in_progress(face, layer);
            viz.completed_step();

            let texture_plane_if_needed: Option<<M::Tile as texture::Tile>::Plane> =
                if let Some(ref texture) = texture_if_needed {
                    // Compute the exact texture slice we will be accessing.
                    // TODO: It would be better if this were shrunk to the visible voxels
                    // in this specific layer, not just all voxels.
                    let slice_range = GridAab::from_ranges([
                        occupied_rect.x_range().into(),
                        occupied_rect.y_range().into(),
                        layer..layer + 1,
                    ])
                    .transform(face.face_transform(resolution_g))
                    .unwrap();

                    Some(texture.slice(slice_range))
                } else {
                    None
                };

            // Pick where we're going to store the quads.
            // Only the cube-surface faces go anywhere but `Within`.
            // (We could generalize this to blocks with concavities that still form a
            // light-tight seal against the cube face.)
            let SubMesh {
                vertices,
                indices_opaque,
                indices_transparent,
                bounding_box,
                has_non_rect_transparency,
                ..
            } = if layer == 0 {
                &mut *face_mesh
            } else {
                &mut *interior_mesh
            };

            for (pass_is_transparent, pass_indices, pass_bounding_box) in [
                (
                    false, // opaque
                    &mut *indices_opaque,
                    &mut bounding_box.opaque,
                ),
                (
                    true, // transparent
                    &mut *indices_transparent,
                    &mut bounding_box.transparent,
                ),
            ] {
                // Iterator over analysis vertices filtered to the current plane.
                vertex_subset.clear();
                vertex_subset.extend(analysis.vertices.iter().filter(|&v| {
                    // Filter to vertices on this layer,
                    // that have some content on this face and are not purely opposite,
                    // that are either opaque or transparent as requested.
                    (if pass_is_transparent {
                        v.renderable & !v.opaque
                    } else {
                        v.opaque
                    } & interior_side_octant_mask
                        != OctantMask::NONE)
                        && voxel_transform_inverse.transform_point(v.position).z == layer
                }));

                let index_offset = vertices.0.len().try_into().expect("vertex index overflow");

                // Append this plane's vertices to the SubMesh vertices.
                push_vertices_from_iter(
                    vertices,
                    vertex_subset.iter().map(|av| {
                        let position = scale_to_block.transform_point3d(av.position.to_f32());
                        let coloring: vertex::Coloring<<M::Tile as texture::Tile>::Point> =
                            if let Some(ref plane) = texture_plane_if_needed {
                                vertex::Coloring::Texture {
                                    pos: plane.grid_to_texcoord(
                                        av.position.to_f32().cast_unit()
                                    // offset to mid-texel for unambiguity
                                        + face.vector(-0.5f32),
                                    ),
                                    resolution,
                                }
                            } else {
                                // Pick an arbitrary inside voxel to fetch the vertex color from.
                                // Note: This operation is only valid because
                                // Analysis::needs_texture will be true if any adjacent voxels
                                // have distinct colors, even if they're not actually connected.
                                // If we were to adopt a finer-grained analysis, we wouldn't be
                                // able to get away with using only 1 mesh vertex per
                                // analysis vertex per face.
                                //
                                // It will also pick a vertex color in the case that the
                                // texture allocation failed.
                                // TODO: Use block average color instead.
                                let acceptable_voxel = Cube::from(
                                    av.position
                                        + (av.renderable & interior_side_octant_mask)
                                            .first()
                                            .unwrap_or_else(|| {
                                                panic!(
                                                    "failed to find vertex color for {av:?}\n\
                                            {interior_side_octant_mask:?}\n {face:?}"
                                                )
                                            })
                                            .to_01()
                                            .map(|c| GridCoordinate::from(c) - 1), // TODO: add an Octant op for balanced cubes
                                );

                                used_any_vertex_colors = true;

                                // Fetch color from voxel data
                                vertex::Coloring::Solid(voxels_array[acceptable_voxel].color)
                            };
                        let v =
                            <M::Vertex as vertex::Vertex>::from_block_vertex(vertex::BlockVertex {
                                position,
                                face,
                                coloring,
                            });
                        pass_bounding_box.add_point(position);
                        v
                    }),
                );

                // Compute triangles and append their indices to SubMesh.
                let indices_before_this_pass = pass_indices.len();
                triangulator_basis.transparent = pass_is_transparent;
                triangulator.triangulate(
                    viz,
                    triangulator_basis,
                    vertex_subset.iter().copied(),
                    |triangle_indices| {
                        pass_indices
                            .extend_with_offset(IndexSlice::U32(&triangle_indices), index_offset);
                    },
                );

                // If we added more than 2 transparent triangles, then they might not form
                // rectangles. Depth sorting needs to be aware of that in order to disable
                // its optimization for sorting rectangles.
                *has_non_rect_transparency |=
                    (pass_indices.len() - indices_before_this_pass > 6) & pass_is_transparent;
            }
        }
    }

    viz.clear_in_progress_markers();

    output.texture_used = texture_if_needed;
    output.voxel_opacity_mask = if used_any_vertex_colors {
        None
    } else {
        Some(voxel_opacity_mask.clone())
    };
}

fn get_voxel_with_limit(voxels: Vol<&[Evoxel]>, cube: Cube, options: &MeshOptions) -> Evoxel {
    let mut evoxel = *voxels.get(cube).unwrap_or(&Evoxel::AIR);
    // TODO: this is clunky and might get out of sync
    evoxel.color = match options.transparency_format() {
        crate::TransparencyFormat::Surfaces => options.transparency.limit_alpha(evoxel.color),
        // If we are doing volumetric transparency where there is a special bounding-box mesh,
        // then when computing the rest of the mesh, ignore everything transparent.
        crate::TransparencyFormat::BoundingBox => {
            if evoxel.color.fully_opaque() {
                evoxel.color
            } else {
                Rgba::TRANSPARENT
            }
        }
    };
    evoxel
}

//! The algorithm for generating block meshes, and nothing else.

use all_is_cubes::euclid::{Point2D, point2};
use alloc::vec::Vec;

use itertools::Itertools as _;

use all_is_cubes::block::{self, AnimationChange, EvaluatedBlock, Evoxel, Evoxels, Resolution};
use all_is_cubes::math::{
    Cube, Face6, FaceMap, GridAab, GridCoordinate, GridSizeCoord, OpacityCategory, Rgb, Rgba, Vol,
    ZeroOne,
};
use all_is_cubes_render::Flaws;

use crate::block_mesh::SubMesh;
use crate::block_mesh::analyze::{Analysis, analyze};
use crate::block_mesh::planar::{
    GmRect, QuadColoring, QuadTransform, VisualVoxel, greedy_mesh, push_quad,
};
use crate::texture::{self, Tile as _};
use crate::{BlockMesh, MeshOptions, MeshTypes, PosCoord, Viz};

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

    if resolution != Resolution::R1 && (options.ignore_voxels) {
        // Substitute the block color for the actual voxels.
        // Note: This discards emission, which we are considering acceptable for now.
        let face_colors = block.face_colors().map(|face, full_face_color| {
            // Increase the color's alpha to account for that we are drawing restricted bounds
            // instead of full bounds.
            // TODO: It would be more direct for `EvaluatedBlock` to provide us this value since
            // it is trivial to compute there, but is it worth storing that value always?
            let voxels_surface_area = block
                .voxels()
                .bounds()
                .abut(face, 1)
                .unwrap()
                .volume()
                .unwrap() as f32;
            let full_surface_area = GridSizeCoord::from(resolution).pow(2) as f32;
            let partial_face_color =
                full_face_color
                    .to_rgb()
                    .with_alpha(ZeroOne::<f32>::new_clamped(
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
            block.color(),
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

enum BoxColoring<M: MeshTypes> {
    VolumeTexture(M::Tile),
    Solid(FaceMap<Rgba>),
}

/// Append triangles to `output` which form a single box,
/// and set the `fully_opaque` flags true when appropriate.
fn push_box<M: MeshTypes>(
    output: &mut BlockMesh<M>,
    resolution: Resolution,
    aab: GridAab,
    opacity_category: OpacityCategory,
    coloring: &BoxColoring<M>,
    viz: &mut Viz,
) {
    if opacity_category == OpacityCategory::Invisible {
        return;
    }
    let fully_opaque = opacity_category == OpacityCategory::Opaque;
    for face in Face6::ALL {
        let volume_to_planar = face
            .face_transform(GridCoordinate::from(resolution))
            .inverse();
        let aab_in_face_coordinates = aab.transform(volume_to_planar).unwrap();

        let lower_bounds = aab_in_face_coordinates.lower_bounds().xy();
        let upper_bounds = aab_in_face_coordinates.upper_bounds().xy();
        let depth = aab_in_face_coordinates.lower_bounds().z;

        // Check whether this mesh layer meets the block face (face of a unit cube).
        // If it does, put these vertices in the separate sub-mesh group for the block face.
        let sub_mesh = if depth == 0 {
            &mut output.face_vertices[face]
        } else {
            &mut output.interior_vertices[face]
        };
        sub_mesh.fully_opaque |= fully_opaque
            & (depth == 0)
            & (lower_bounds == point2(0, 0))
            & (upper_bounds == Point2D::splat(resolution.into()));
        reserve_vertices(&mut sub_mesh.vertices, 4);

        // TODO: reduce duplication of code between push_box and push_full_box by factoring out
        // "reserve 6 indices" but in a way that borrow checking likes
        let plane: <M::Tile as texture::Tile>::Plane;
        push_quad(
            &mut sub_mesh.vertices,
            if fully_opaque {
                sub_mesh.indices_opaque.reserve_exact(6);
                &mut sub_mesh.indices_opaque
            } else {
                sub_mesh.indices_transparent.reserve_exact(6);
                &mut sub_mesh.indices_transparent
            },
            &QuadTransform::new(face, resolution),
            depth as f32, // TODO: eliminate this case by using a smaller integer type for `aab`?
            lower_bounds.to_f32().cast_unit(),
            upper_bounds.to_f32().cast_unit(),
            match &coloring {
                // TODO: for our actual purposes, this should be a volume and not face slices
                BoxColoring::VolumeTexture(tile) => {
                    plane = tile.slice(aab.abut(face, -1).unwrap());
                    QuadColoring::Volume {
                        plane: &plane,
                        far_depth: (aab_in_face_coordinates.upper_bounds().z as f32),
                    }
                }
                BoxColoring::Solid(face_colors) => QuadColoring::Solid(face_colors[face]),
            },
            viz,
            if fully_opaque {
                &mut sub_mesh.bounding_box.opaque
            } else {
                &mut sub_mesh.bounding_box.transparent
            },
        );
    }
}

/// Append triangles to `output` which form a single box filling the entire block cube,
/// and set the `fully_opaque` flags true when appropriate.
///
/// This is a more efficient, specialized version of [`push_box()`].
fn push_full_box<M: MeshTypes>(
    output: &mut BlockMesh<M>,
    opacity_category: OpacityCategory,
    coloring: QuadColoring<'_, <M::Tile as texture::Tile>::Plane>,
    viz: &mut Viz,
) {
    let fully_opaque = opacity_category == OpacityCategory::Opaque;
    for (face, sub_mesh) in output.face_vertices.iter_mut() {
        if opacity_category != OpacityCategory::Invisible {
            reserve_vertices(&mut sub_mesh.vertices, 4);
            push_quad(
                &mut sub_mesh.vertices,
                if fully_opaque {
                    sub_mesh.indices_opaque.reserve_exact(6);
                    &mut sub_mesh.indices_opaque
                } else {
                    sub_mesh.indices_transparent.reserve_exact(6);
                    &mut sub_mesh.indices_transparent
                },
                &QuadTransform::new(face, Resolution::R1),
                /* depth= */ 0.,
                point2(0., 0.),
                point2(1., 1.),
                coloring,
                viz,
                if fully_opaque {
                    &mut sub_mesh.bounding_box.opaque
                } else {
                    &mut sub_mesh.bounding_box.transparent
                },
            );
        }
        sub_mesh.fully_opaque |= fully_opaque;
    }
}

/// The portion of [`compute_block_mesh()`] that has almost no special cases or initialization;
/// just starts slinging triangles.
///
/// This function assumes that `output` has been cleared and writes to every part of it.
#[allow(clippy::too_many_arguments)]
fn compute_block_mesh_from_analysis<M: MeshTypes>(
    output: &mut BlockMesh<M>,
    voxels: &Evoxels,
    analysis: &Analysis,
    prefer_textures: bool,
    texture_allocator: &M::Alloc,
    options: &MeshOptions,
    placeholder_color: Rgba,
    voxel_opacity_mask: &block::VoxelOpacityMask,
    viz: &mut Viz,
) {
    let flaws = &mut output.flaws;
    let mut used_any_vertex_colors = false;

    let resolution = voxels.resolution();
    let resolution_g = GridCoordinate::from(resolution);
    let voxels_array = voxels.as_vol_ref();

    // Construct empty output to mutate.
    for (_, sub_mesh) in output.face_vertices.iter_mut() {
        // Start assuming opacity; if we find any transparent pixels we'll set
        // this to false. `Within` is always "transparent" because the algorithm
        // that consumes this structure will say "draw this face if its adjacent
        // cube's opposing face is not opaque", and `Within` means the adjacent
        // cube is ourself.
        sub_mesh.fully_opaque = true;
    }

    let texture_if_needed: Option<M::Tile> = if prefer_textures || analysis.needs_texture {
        texture::copy_voxels_to_new_texture(texture_allocator, voxels)
    } else {
        None
    };

    // Walk through the planes (layers) of the block, figuring out what geometry to
    // generate for each layer and whether it needs a texture.
    for face in Face6::ALL {
        let voxel_transform = face.face_transform(resolution_g);
        let quad_transform = QuadTransform::new(face, resolution);
        let face_mesh = &mut output.face_vertices[face];
        let interior_mesh = &mut output.interior_vertices[face];

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
        // `occupied_planes()` tells us which planes have any actually visible voxel surfaces,
        // so this loop can skip fully solid or empty volumes.
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
                && (occupied_rect.x_range() != (0..resolution_g)
                    || occupied_rect.y_range() != (0..resolution_g))
            {
                face_mesh.fully_opaque = false;
            }

            viz.set_layer_in_progress(face, layer);

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
                    .transform(face.face_transform(resolution_g))
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
                            // transparency as if there was nothing to draw. This is because if
                            // we didn't, we would end up generating large numbers (bad) of
                            // intersecting (also bad) quads for any significant volume of
                            // transparency. Also, when using `TransparencyFormat::BoundingBox`,
                            // this does not come up.
                            (Partial, Partial) => false,
                        }
                    }
                };
                if voxel_is_visible {
                    visible_image.push(VisualVoxel::from(&evoxel));
                } else {
                    // All obscured voxels are treated as transparent ones, in that we don't
                    // generate geometry for them.
                    visible_image.push(VisualVoxel::INVISIBLE);
                }
            }

            // Pick where we're going to store the quads.
            // Only the cube-surface faces go anywhere but `Within`.
            // (We could generalize this to blocks with concavities that still form a
            // light-tight seal against the cube face.)
            let SubMesh {
                vertices,
                indices_opaque,
                indices_transparent,
                bounding_box,
                ..
            } = if layer == 0 {
                &mut *face_mesh
            } else {
                &mut *interior_mesh
            };
            let depth = layer as PosCoord; // TODO: centralize this conversion

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
                let coloring = if let Some(single_color) = single_color.filter(|_| !prefer_textures)
                {
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
                        options.transparency.limit_alpha(placeholder_color),
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
                    // cannot fail or round because the maximum resolution of a block is well below
                    // f32 precision -- TODO: write a function to centralize this claim.
                    low_corner.cast::<PosCoord>(),
                    high_corner.cast::<PosCoord>(),
                    coloring,
                    viz,
                    if rect_has_alpha {
                        &mut bounding_box.transparent
                    } else {
                        &mut bounding_box.opaque
                    },
                );
            });
        }
    }

    viz.clear_layer_in_progress();

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

fn reserve_vertices<T, U>((v0, v1): &mut (Vec<T>, Vec<U>), n: usize) {
    v0.reserve_exact(n);
    v1.reserve_exact(n);
}

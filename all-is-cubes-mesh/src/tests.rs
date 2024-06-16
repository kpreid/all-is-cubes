//! Tests for the mesh system as a whole.

use alloc::vec::Vec;

use pretty_assertions::assert_eq;

use all_is_cubes::block::{self, Block, Resolution::*, AIR};
use all_is_cubes::camera::{Flaws, TransparencyOption};
use all_is_cubes::color_block;
use all_is_cubes::content::{make_some_blocks, make_some_voxel_blocks};
use all_is_cubes::euclid::{point3, Point3D, Vector3D};
use all_is_cubes::math::{
    notnan,
    Face6::{self, *},
    FaceMap, FreeCoordinate, GridAab, GridRotation, Rgba,
};
use all_is_cubes::math::{Cube, Rgb};
use all_is_cubes::space::{Space, SpacePhysics};
use all_is_cubes::universe::Universe;

use crate::testing::{mesh_blocks_and_space, Allocator, TexPoint, TextureMt};
use crate::texture::TexelUnit;
use crate::{
    block_meshes_for_space, BlockMesh, BlockMeshes, BlockVertex, Coloring, DepthOrdering,
    IndexSlice, MeshOptions, MeshTypes, SpaceMesh,
};

/// Shorthand for writing out an entire [`BlockVertex`] with solid color.
fn v_c<T>(position: [FreeCoordinate; 3], face: Face6, color: [f32; 4]) -> BlockVertex<T> {
    BlockVertex {
        position: position.into(),
        face,
        coloring: Coloring::Solid(Rgba::new(color[0], color[1], color[2], color[3])),
    }
}

/// Shorthand for writing out an entire [`BlockVertex`] with texturing.
fn v_t(position: [FreeCoordinate; 3], face: Face6, texture: [f32; 3]) -> BlockVertex<TexPoint> {
    let texture = texture.into();
    BlockVertex {
        position: position.into(),
        face,
        coloring: Coloring::Texture {
            pos: texture,
            clamp_min: texture,
            clamp_max: texture,
        },
    }
}

/// Test helper to create [`BlockMesh`] alone without a `Space`.
#[allow(clippy::needless_pass_by_value)] // convenience
pub(crate) fn test_block_mesh(block: Block) -> BlockMesh<TextureMt> {
    BlockMesh::new(
        &block.evaluate().unwrap(),
        &Allocator::new(),
        &MeshOptions {
            transparency: TransparencyOption::Volumetric,
            ..MeshOptions::dont_care_for_test()
        },
    )
}

/// Test helper to create [`BlockMesh`] alone without a `Space`,
/// and with the transparency option set to `Threshold(0.5)`.
#[allow(clippy::needless_pass_by_value)] // convenience
fn test_block_mesh_threshold(block: Block) -> BlockMesh<TextureMt> {
    BlockMesh::new(
        &block.evaluate().unwrap(),
        &Allocator::new(),
        &MeshOptions {
            transparency: TransparencyOption::Threshold(notnan!(0.5)),
            ..MeshOptions::dont_care_for_test()
        },
    )
}

fn non_uniform_fill(cube: Cube) -> &'static Block {
    const BLOCKS: &[Block] = &[color_block!(1., 1., 1., 1.), color_block!(0., 0., 0., 1.)];
    &BLOCKS[(cube.x + cube.y + cube.z).rem_euclid(2) as usize]
}

#[test]
fn excludes_hidden_faces_of_blocks() {
    let mut space = Space::empty_positive(2, 2, 2);
    space
        .fill(space.bounds(), |p| Some(non_uniform_fill(p)))
        .unwrap();
    let (_, _, space_mesh) = mesh_blocks_and_space(&space);

    assert_eq!(space_mesh.flaws(), Flaws::empty());
    // The space rendering should be a 2×2×2 cube of tiles, without any hidden interior faces.
    assert_eq!(
        Vec::<&BlockVertex<TexPoint>>::new(),
        space_mesh
            .vertices()
            .iter()
            .filter(|vertex| (vertex.position - Point3D::new(1., 1., 1.)).square_length() < 0.99)
            .collect::<Vec<&BlockVertex<TexPoint>>>(),
        "found an interior point"
    );
    assert_eq!(
        space_mesh.vertices().len(),
        4 /* vertices per face */
        * 4 /* block faces per exterior side of space */
        * 6, /* sides of space */
        "wrong number of faces"
    );
}

/// Run [`SpaceMesh::new`] with stale block data and confirm it does not panic.
#[test]
fn no_panic_on_missing_blocks() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(2, 1, 1);
    let block_meshes: BlockMeshes<TextureMt> = block_meshes_for_space(
        &space,
        &Allocator::new(),
        &MeshOptions::dont_care_for_test(),
    );
    assert_eq!(block_meshes.len(), 1); // check our assumption

    space.set([0, 0, 0], &block).unwrap(); // render data does not know about this

    // This should not panic; visual glitches are preferable to failure.
    let space_mesh = SpaceMesh::new(
        &space,
        space.bounds(),
        &MeshOptions::dont_care_for_test(),
        &*block_meshes,
    );

    // TODO: This should report Flaws::INCOMPLETE
    assert_eq!(space_mesh.flaws(), Flaws::empty());
}

/// Construct a 1x1 recursive block and test that this is equivalent in geometry
/// to an atom block.
#[test]
fn trivial_voxels_equals_atom() {
    // Construct recursive block.
    let mut u = Universe::new();
    let atom_block = color_block!(0.0, 1.0, 0.0, 1.0);
    let trivial_recursive_block = Block::builder()
        .voxels_fn(R1, |_| &atom_block)
        .unwrap()
        .build_into(&mut u);

    let (_, _, space_rendered_a) = mesh_blocks_and_space(
        &Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(atom_block)
            .build(),
    );
    let (tex, _, space_rendered_r) = mesh_blocks_and_space(
        &Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(trivial_recursive_block)
            .build(),
    );

    assert_eq!(space_rendered_a, space_rendered_r);
    assert_eq!(tex.count_allocated(), 0);
}

/// For animated blocks we always prefer textures, even for resolution 1.
#[test]
fn animated_atom_uses_texture() {
    let atom_block = Block::builder()
        .color(Rgba::new(0.0, 1.0, 0.0, 1.0))
        .light_emission(Rgb::ONE)
        .animation_hint(block::AnimationHint::redefinition(
            block::AnimationChange::ColorSameCategory,
        ))
        .build();

    let (allocator, _, mesh) = mesh_blocks_and_space(
        &Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(atom_block)
            .build(),
    );

    assert_eq!(allocator.count_allocated(), 1);
    assert_eq!(
        mesh.vertices()[0].coloring,
        Coloring::Texture {
            pos: point3(0.5, 0., 0.),
            clamp_min: point3(0.5, 0.5, 0.5),
            clamp_max: point3(0.5, 0.5, 0.5)
        }
    )
}

/// We don't encode light emission in vertex colors (because that would lead to bloated vertices),
/// so a block with light emission will use a texture, even for resolution 1.
#[test]
fn emissive_atom_uses_texture() {
    let atom_block = Block::builder()
        .color(Rgba::new(0.0, 1.0, 0.0, 1.0))
        .light_emission(Rgb::ONE)
        .build();

    let (allocator, _, mesh) = mesh_blocks_and_space(
        &Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(atom_block)
            .build(),
    );

    assert_eq!(allocator.count_allocated(), 1);
    assert_eq!(
        mesh.vertices()[0].coloring,
        Coloring::Texture {
            pos: point3(0.5, 0., 0.),
            clamp_min: point3(0.5, 0.5, 0.5),
            clamp_max: point3(0.5, 0.5, 0.5)
        }
    )
}

/// [`SpaceMesh`] of a 1×1×1 space has the same geometry as the contents.
///
/// This test compares 3 different values:
///
/// * A [`BlockMesh`] (with texture).
/// * A [`SpaceMesh`] produced from it via normal construction.
/// * A [`SpaceMesh`] produced from it via [`SpaceMesh::from`].
#[test]
fn space_mesh_equals_block_mesh() {
    // Construct recursive block.
    let resolution = R4;
    let mut u = Universe::new();
    let mut blocks = Vec::from(make_some_blocks::<2>());
    blocks.push(AIR);
    let recursive_block = Block::builder()
        .voxels_fn(resolution, |p| {
            &blocks[(p.x as usize).rem_euclid(blocks.len())]
        })
        .unwrap()
        .build_into(&mut u);
    let outer_space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(recursive_block)
        .build();

    let (tex, block_meshes, space_rendered) = mesh_blocks_and_space(&outer_space);

    eprintln!("{block_meshes:#?}");
    eprintln!("{space_rendered:#?}");

    // Compare the contents of the space mesh and block mesh.
    assert_eq!(
        space_rendered.vertices().to_vec(),
        block_meshes[0]
            .all_face_meshes()
            .flat_map(|(_, face_mesh)| face_mesh.vertices.clone().into_iter())
            .collect::<Vec<_>>()
    );
    assert_eq!(tex.count_allocated(), 1); // for striped faces

    // Compare the result of a `From<&BlockMesh>` conversion with the result of the
    // normal `SpaceMesh` creation.
    assert_eq!(space_rendered, SpaceMesh::from(&block_meshes[0]));
}

/// TODO: This test stops being meaningful when we finish migrating the texture allocator to use arbitrary-sized tiles
#[test]
fn block_resolution_greater_than_tile() {
    let block_resolution = R32;
    let mut u = Universe::new();
    let block = Block::builder()
        .voxels_fn(block_resolution, non_uniform_fill)
        .unwrap()
        .build_into(&mut u);
    let outer_space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(block)
        .build();

    let (_, _, _) = mesh_blocks_and_space(&outer_space);
    // TODO: Figure out how to make a useful assert. At least this is "it doesn't panic".
}

/// Check for hidden surfaces being given internal geometry.
/// Exercise the “shrinkwrap” logic that generates geometry no larger than necessary.
#[test]
#[rustfmt::skip]
fn shrunken_box_has_no_extras() {
    // Construct a box whose faces don't touch the outer extent of the volume.
    let resolution = R8;
    let mut u = Universe::new();
    let less_than_full_block = Block::builder()
        .voxels_fn( resolution, |cube| {
            if GridAab::from_lower_size([2, 2, 2], [4, 4, 4]).contains_cube(cube) {
                non_uniform_fill(cube)
            } else {
                &AIR
            }
        })
        .unwrap()
        .build_into(&mut u,);
    let outer_space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(less_than_full_block)
        .build();

    let (tex, _, space_rendered) = mesh_blocks_and_space(&outer_space);

    assert_eq!(tex.count_allocated(), 1);
    assert_eq!(
        space_rendered.vertices().iter().map(|&v| v.remove_clamps()).collect::<Vec<_>>(),
        vec![
            v_t([0.250, 0.250, 0.250], NX, [2.5, 2.0, 2.0]),
            v_t([0.250, 0.250, 0.750], NX, [2.5, 2.0, 6.0]),
            v_t([0.250, 0.750, 0.250], NX, [2.5, 6.0, 2.0]),
            v_t([0.250, 0.750, 0.750], NX, [2.5, 6.0, 6.0]),

            v_t([0.250, 0.250, 0.250], NY, [2.0, 2.5, 2.0]),
            v_t([0.750, 0.250, 0.250], NY, [6.0, 2.5, 2.0]),
            v_t([0.250, 0.250, 0.750], NY, [2.0, 2.5, 6.0]),
            v_t([0.750, 0.250, 0.750], NY, [6.0, 2.5, 6.0]),

            v_t([0.250, 0.250, 0.250], NZ, [2.0, 2.0, 2.5]),
            v_t([0.250, 0.750, 0.250], NZ, [2.0, 6.0, 2.5]),
            v_t([0.750, 0.250, 0.250], NZ, [6.0, 2.0, 2.5]),
            v_t([0.750, 0.750, 0.250], NZ, [6.0, 6.0, 2.5]),

            v_t([0.750, 0.750, 0.250], PX, [5.5, 6.0, 2.0]),
            v_t([0.750, 0.750, 0.750], PX, [5.5, 6.0, 6.0]),
            v_t([0.750, 0.250, 0.250], PX, [5.5, 2.0, 2.0]),
            v_t([0.750, 0.250, 0.750], PX, [5.5, 2.0, 6.0]),

            v_t([0.750, 0.750, 0.250], PY, [6.0, 5.5, 2.0]),
            v_t([0.250, 0.750, 0.250], PY, [2.0, 5.5, 2.0]),
            v_t([0.750, 0.750, 0.750], PY, [6.0, 5.5, 6.0]),
            v_t([0.250, 0.750, 0.750], PY, [2.0, 5.5, 6.0]),

            v_t([0.250, 0.750, 0.750], PZ, [2.0, 6.0, 5.5]),
            v_t([0.250, 0.250, 0.750], PZ, [2.0, 2.0, 5.5]),
            v_t([0.750, 0.750, 0.750], PZ, [6.0, 6.0, 5.5]),
            v_t([0.750, 0.250, 0.750], PZ, [6.0, 2.0, 5.5]),
        ],
    );
}

/// Exercise the case where textures are skipped because the color is uniform.
/// TODO: There are more subcases such as still using textures for irregular
/// shapes.
#[test]
#[rustfmt::skip]
fn shrunken_box_uniform_color() {
    // Construct a box whose faces don't touch the outer extent of the volume.
    let resolution = R8;
    let mut u = Universe::new();
    let filler_block = color_block!(0.0, 1.0, 0.5, 1.0);
    let less_than_full_block = Block::builder()
        .voxels_fn(resolution, |cube| {
            if GridAab::from_lower_size([2, 2, 2], [4, 4, 4]).contains_cube(cube) {
                &filler_block
            } else {
                &AIR
            }
        })
        .unwrap()
        .build_into(&mut u, );
    let outer_space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(less_than_full_block)
        .build();

    let (tex, _, space_rendered) = mesh_blocks_and_space(&outer_space);

    assert_eq!(tex.count_allocated(), 0, "should have no texture");
    assert_eq!(
        space_rendered.vertices().to_vec(),
        vec![
            v_c([0.250, 0.250, 0.250], NX, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.250, 0.250, 0.750], NX, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.250, 0.750, 0.250], NX, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.250, 0.750, 0.750], NX, [0.0, 1.0, 0.5, 1.0]),

            v_c([0.250, 0.250, 0.250], NY, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.250, 0.250], NY, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.250, 0.250, 0.750], NY, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.250, 0.750], NY, [0.0, 1.0, 0.5, 1.0]),

            v_c([0.250, 0.250, 0.250], NZ, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.250, 0.750, 0.250], NZ, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.250, 0.250], NZ, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.750, 0.250], NZ, [0.0, 1.0, 0.5, 1.0]),

            v_c([0.750, 0.750, 0.250], PX, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.750, 0.750], PX, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.250, 0.250], PX, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.250, 0.750], PX, [0.0, 1.0, 0.5, 1.0]),

            v_c([0.750, 0.750, 0.250], PY, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.250, 0.750, 0.250], PY, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.750, 0.750], PY, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.250, 0.750, 0.750], PY, [0.0, 1.0, 0.5, 1.0]),

            v_c([0.250, 0.750, 0.750], PZ, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.250, 0.250, 0.750], PZ, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.750, 0.750], PZ, [0.0, 1.0, 0.5, 1.0]),
            v_c([0.750, 0.250, 0.750], PZ, [0.0, 1.0, 0.5, 1.0]),
        ],
    );
}

fn opacities<M: MeshTypes>(mesh: &BlockMesh<M>) -> FaceMap<bool> {
    assert!(
        !mesh.interior_vertices.fully_opaque,
        "interior opacity should never be true because it doesn't mean anything"
    );
    // TODO: this could be simplified by a FaceMap by-ref operation
    FaceMap::from_fn(|face| mesh.face_vertices[face].fully_opaque)
}

#[test]
fn atom_transparency() {
    assert_eq!(
        opacities(&test_block_mesh(color_block!(Rgba::WHITE))),
        FaceMap::repeat(true)
    );
    assert_eq!(
        opacities(&test_block_mesh(color_block!(Rgba::TRANSPARENT))),
        FaceMap::repeat(false)
    );
    assert_eq!(
        opacities(&test_block_mesh(color_block!(1.0, 1.0, 1.0, 0.5))),
        FaceMap::repeat(false)
    );
}

#[test]
fn atom_transparency_thresholded() {
    // Threshold means that partial transparency should produce exactly the same mesh as 0 or 1
    assert_eq!(
        test_block_mesh_threshold(color_block!(1.0, 1.0, 1.0, 0.25)),
        test_block_mesh_threshold(color_block!(1.0, 1.0, 1.0, 0.0)),
    );
    assert_eq!(
        test_block_mesh_threshold(color_block!(1.0, 1.0, 1.0, 0.75)),
        test_block_mesh_threshold(color_block!(1.0, 1.0, 1.0, 1.0)),
    );

    // TODO: also test voxels -- including self-occlusion (thresholded voxel in front of truly opaque voxel)
}

/// Test mesh opacity flags from basic voxels.
#[test]
fn fully_opaque_voxels() {
    let resolution = R8;
    let mut u = Universe::new();
    let block = Block::builder()
        .voxels_fn(resolution, |cube| {
            // Make a cube-corner shape
            // TODO: Also test partial alpha
            if cube.x < 1 || cube.y < 1 || cube.z < 1 {
                color_block!(Rgba::BLACK)
            } else {
                AIR
            }
        })
        .unwrap()
        .build_into(&mut u);
    assert_eq!(
        opacities(&test_block_mesh(block)),
        FaceMap {
            nx: true,
            ny: true,
            nz: true,
            px: false,
            py: false,
            pz: false,
        }
    );
}

/// Test mesh opacity flags when the voxels are all individually opaque,
/// but don't fill the cube.
#[test]
fn fully_opaque_partial_block() {
    let mut u = Universe::new();
    let block = Block::builder()
        .voxels_handle(R8, {
            // The dimensions don't meet the PX face, but the blocks are all opaque.
            u.insert_anonymous(
                Space::builder(GridAab::from_lower_size([0, 0, 0], [4, 8, 8]))
                    .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                    .filled_with(color_block!(Rgba::WHITE))
                    .build(),
            )
        })
        .build();
    assert_eq!(
        opacities(&test_block_mesh(block)),
        FaceMap {
            nx: true,
            ny: false,
            nz: false,
            px: false,
            py: false,
            pz: false,
        }
    );
}

/// Test mesh opacity flags when the voxels fill the cube, but are transparent.
#[test]
fn invisible_voxel_block() {
    let mut u = Universe::new();
    let block = Block::builder()
        .voxels_handle(R8, {
            // Don't use voxels_fn() because it auto-shrinkwraps.
            u.insert_anonymous(
                Space::builder(GridAab::from_lower_size([0, 0, 0], [8, 8, 8]))
                    .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                    .build(),
            )
        })
        .build();
    assert_eq!(
        block.evaluate().unwrap().voxels.bounds(),
        GridAab::for_block(R8),
        "sanity check test data"
    );

    assert_eq!(opacities(&test_block_mesh(block)), FaceMap::repeat(false));
}

#[test]
fn transparency_split() {
    let mut space = Space::empty_positive(3, 1, 1);
    // One opaque block and one transparent block
    space
        .set([0, 0, 0], color_block!(1.0, 0.0, 0.0, 1.0))
        .unwrap();
    space
        .set([2, 0, 0], color_block!(0.0, 0.0, 1.0, 0.5))
        .unwrap();

    let (_, _, space_rendered) = mesh_blocks_and_space(&space);
    // 2 cubes...
    assert_eq!(space_rendered.vertices().len(), 6 * 4 * 2);
    // ...one of which is opaque...
    assert_eq!(space_rendered.opaque_range().len(), 6 * 6);
    // ...and one of which is transparent
    for &r in &GridRotation::ALL {
        // TODO: probably DepthOrdering should have an iteration tool directly
        assert_eq!(
            space_rendered
                .transparent_range(DepthOrdering::Direction(r))
                .len(),
            6 * 6
        );
    }
}

#[test]
fn handling_allocation_failure() {
    let resolution = R8;
    let mut u = Universe::new();
    let [atom1, atom2] = make_some_blocks();
    let complex_block = Block::builder()
        .voxels_fn(resolution, |cube| {
            if (cube.x + cube.y + cube.z).rem_euclid(2) == 0 {
                &atom1
            } else {
                &atom2
            }
        })
        .unwrap()
        .build_into(&mut u);
    let block_derived_color = complex_block.evaluate().unwrap().color;

    let space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(complex_block)
        .build();

    let mut tex = Allocator::new();
    tex.set_capacity(0);
    let block_meshes: BlockMeshes<TextureMt> =
        block_meshes_for_space(&space, &tex, &MeshOptions::dont_care_for_test());

    // Check results.
    assert_eq!(tex.count_allocated(), 0);
    assert_eq!(1, block_meshes.len());
    let mesh = &block_meshes[0];
    assert_eq!(mesh.flaws(), Flaws::MISSING_TEXTURES);

    // Check that the flaw carries over to SpaceMesh.
    // TODO: No coverage of the *normal* path of SpaceMesh::compute()
    let space_mesh = SpaceMesh::from(mesh);
    assert_eq!(space_mesh.flaws(), Flaws::MISSING_TEXTURES);

    // Check the color of the produced mesh. (Easier to do with SpaceMesh.)
    let allowed_colors = [atom1.color(), atom2.color(), block_derived_color];
    for vertex in space_mesh.vertices() {
        match vertex.coloring {
            Coloring::Solid(c) if allowed_colors.contains(&c) => { /* OK */ }
            Coloring::Solid(c) => {
                panic!("unexpected color {c:?}")
            }
            t @ Coloring::Texture { .. } => panic!("unexpected texture {t:?}"),
        }
    }
}

#[test]
fn space_mesh_empty() {
    let t = SpaceMesh::<TextureMt>::default();
    assert!(t.is_empty());
    assert_eq!(t.flaws(), Flaws::empty());
    assert_eq!(t.vertices(), &[]);
    assert_eq!(t.indices(), IndexSlice::U16(&[]));
}

#[test]
fn depth_ordering_from_view_direction() {
    let mut problems = Vec::new();
    // A coordinate range of ±3 will exercise every combination of axis orderings.
    let range = -3..3;
    for x in range.clone() {
        for y in range.clone() {
            for z in range.clone() {
                let direction = Vector3D::new(x, y, z);
                let ordering = DepthOrdering::from_view_direction(direction);
                let rotated_direction = match ordering {
                    DepthOrdering::Any => {
                        panic!("from_view_direction should not return Any")
                    }
                    DepthOrdering::Within => direction,
                    DepthOrdering::Direction(rotation) => {
                        rotation.to_rotation_matrix().transform_vector(direction)
                    }
                };
                let good = rotated_direction.x >= rotated_direction.y
                    && rotated_direction.y >= rotated_direction.z;
                println!(
                    "{:?} → {:?} → {:?}{}",
                    direction,
                    ordering,
                    rotated_direction,
                    if good { "" } else { " (wrong)" }
                );
                if !good {
                    // Defer assertions to end so we can report all cases before panicking.
                    problems.push(direction);
                }
            }
        }
    }
    assert_eq!(problems, vec![]);
}

/// Test that `clamp_min < clamp_max`, and that `pos` is within the range (± 0.5) too.
///
/// (It'd be nice if this was instead a debug assertion when constructing vertices, but
/// the data is a fully `pub` struct and enum.)
#[test]
fn texture_clamp_coordinate_ordering() {
    const ALL_TRUE: Vector3D<bool, TexelUnit> = Vector3D::new(true, true, true);

    let mut universe = Universe::new();
    let [block] = make_some_voxel_blocks(&mut universe);
    let mesh = test_block_mesh(block);
    for (face, face_mesh) in mesh.all_face_meshes() {
        for vertex in face_mesh.vertices.iter() {
            let mut had_any_textured = false;
            match vertex.coloring {
                Coloring::Solid(_) => {}
                Coloring::Texture {
                    pos,
                    clamp_min,
                    clamp_max,
                } => {
                    had_any_textured = true;
                    assert!(
                        clamp_min.zip(clamp_max, |min, max| min <= max) == ALL_TRUE,
                        "clamp should be {clamp_min:?} <= {clamp_max:?}"
                    );
                    // Texture coordinates may be outside the clamp by 0.5 because the clamp
                    // coordinates are deliberately kept within the bounds by a half-texel
                    // (which has no visual effect on the nearest-neighbor interpolated texels
                    // but ensures good numerical results).
                    assert!(
                        clamp_min.zip(pos, |min, pos| min - 0.5 <= pos) == ALL_TRUE
                            && pos.zip(clamp_max, |pos, max| pos <= max + 0.5) == ALL_TRUE,
                        "{clamp_min:?} <= {pos:?} <= {clamp_max:?}"
                    );
                }
            }
            assert!(
                had_any_textured,
                "test invalid: {face:?} has no textured vertices"
            )
        }
    }
}

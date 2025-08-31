//! Tests for the mesh system as a whole.

use alloc::vec::Vec;

use exhaust::Exhaust as _;
use pretty_assertions::assert_eq;

use all_is_cubes::block::{
    self, AIR, Block,
    Resolution::{self, *},
};
use all_is_cubes::content::{make_some_blocks, make_some_voxel_blocks};
use all_is_cubes::euclid::{Point3D, Size3D, Vector3D, point3};
use all_is_cubes::math::{
    Aab, Cube,
    Face6::{self, *},
    FaceMap, GridAab, Rgb, Rgba, zo32,
};
use all_is_cubes::space::{Space, SpacePhysics};
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::universe::Universe;
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::TransparencyOption;

use crate::testing::{Allocator, TexPoint, TextureMt, mesh_blocks_and_space};
use crate::texture::TexelUnit;
use crate::{
    BlockMesh, BlockMeshes, BlockVertex, Coloring, DepthOrdering, IndexSlice, MeshOptions,
    MeshTypes, PosCoord, SpaceMesh, block_meshes_for_space,
};

/// Shorthand for writing out an entire [`BlockVertex`] with solid color.
fn v_c<T>(position: [PosCoord; 3], face: Face6, color: [f32; 4]) -> BlockVertex<T> {
    BlockVertex {
        position: position.into(),
        face,
        coloring: Coloring::Solid(Rgba::new(color[0], color[1], color[2], color[3])),
    }
}

/// Shorthand for writing out an entire [`BlockVertex`] with texturing.
fn v_t(
    position: [PosCoord; 3],
    face: Face6,
    resolution: Resolution,
    texture: [f32; 3],
) -> BlockVertex<TexPoint> {
    let texture = texture.into();
    BlockVertex {
        position: position.into(),
        face,
        coloring: Coloring::Texture {
            pos: texture,
            clamp_min: texture,
            clamp_max: texture,
            resolution,
        },
    }
}

/// Test helper to create [`BlockMesh`] alone without a `Space`.
#[expect(clippy::needless_pass_by_value, reason = "convenience")]
pub(crate) fn test_block_mesh(universe: &Universe, block: Block) -> BlockMesh<TextureMt> {
    BlockMesh::new(
        &block.evaluate(universe.read_ticket()).unwrap(),
        &Allocator::new(),
        &MeshOptions {
            transparency: TransparencyOption::Volumetric,
            ..MeshOptions::dont_care_for_test()
        },
    )
}

/// Test helper to create [`BlockMesh`] alone without a `Space`,
/// and with the transparency option set to `Threshold(0.5)`.
#[expect(clippy::needless_pass_by_value, reason = "convenience")]
fn test_block_mesh_threshold(universe: &Universe, block: Block) -> BlockMesh<TextureMt> {
    BlockMesh::new(
        &block.evaluate(universe.read_ticket()).unwrap(),
        &Allocator::new(),
        &MeshOptions {
            transparency: TransparencyOption::Threshold(zo32(0.5)),
            ..MeshOptions::dont_care_for_test()
        },
    )
}

fn non_uniform_fill(cube: Cube) -> &'static Block {
    const BLOCKS: &[Block] = &[
        block::from_color!(1., 1., 1., 1.),
        block::from_color!(0., 0., 0., 1.),
    ];
    &BLOCKS[(cube.x + cube.y + cube.z).rem_euclid(2) as usize]
}

#[test]
fn excludes_hidden_faces_of_blocks() {
    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 2, 2]))
        .build_and_mutate(|m| {
            m.fill_all(|p| Some(non_uniform_fill(p)))?;
            Ok(())
        })
        .unwrap();
    let (_, _, space_mesh) = mesh_blocks_and_space(&space);

    assert_eq!(space_mesh.flaws(), Flaws::empty());
    // The space rendering should be a 2×2×2 cube of tiles, without any hidden interior faces.
    assert_eq!(
        Vec::<&BlockVertex<TexPoint>>::new(),
        space_mesh
            .vertices()
            .0
            .iter()
            .filter(|vertex| (vertex.position - Point3D::new(1., 1., 1.)).square_length() < 0.99)
            .collect::<Vec<&BlockVertex<TexPoint>>>(),
        "found an interior point"
    );
    assert_eq!(
        space_mesh.vertices().0.len(),
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

    space
        .mutate(ReadTicket::stub(), |m| m.set([0, 0, 0], &block))
        .unwrap(); // render data does not know about this

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
    let atom_block = block::from_color!(0.0, 1.0, 0.0, 1.0);
    let trivial_recursive_block = Block::builder()
        .voxels_fn(R1, |_| &atom_block)
        .unwrap()
        .build_into(&mut u);

    let (_, _, space_rendered_a) = mesh_blocks_and_space(
        &Space::builder(GridAab::ORIGIN_CUBE)
            .read_ticket(u.read_ticket())
            .filled_with(atom_block)
            .build(),
    );
    let (tex, _, space_rendered_r) = mesh_blocks_and_space(
        &Space::builder(GridAab::ORIGIN_CUBE)
            .read_ticket(u.read_ticket())
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
        mesh.vertices().0[0].coloring,
        Coloring::Texture {
            pos: point3(0.5, 0., 0.),
            clamp_min: point3(0.5, 0.5, 0.5),
            clamp_max: point3(0.5, 0.5, 0.5),
            resolution: R1,
        }
    )
}

/// For relevantly animated blocks we always prefer textures, even if the voxels are currently all
/// identical.
#[test]
fn animated_voxels_uses_texture() {
    let mut u = Universe::new();
    let block = Block::builder()
        .voxels_fn(R2, |_| const { &block::from_color!(Rgba::WHITE) })
        .unwrap()
        .animation_hint(block::AnimationHint::redefinition(
            block::AnimationChange::ColorSameCategory,
        ))
        .build_into(&mut u);

    let (allocator, _, mesh) = mesh_blocks_and_space(
        &Space::builder(GridAab::ORIGIN_CUBE)
            .read_ticket(u.read_ticket())
            .filled_with(block)
            .build(),
    );

    assert_eq!(allocator.count_allocated(), 1);
    assert_eq!(
        mesh.vertices().0[0].coloring,
        Coloring::Texture {
            pos: point3(0.5, 0., 0.),
            clamp_min: point3(0.5, 0.5, 0.5),
            clamp_max: point3(0.5, 1.5, 1.5),
            resolution: R2,
        }
    )
}

/// We don't encode light emission in vertex colors (because that would lead to bloated vertices),
/// so a block with light emission will use a texture, even for resolution 1.
///
/// TODO: That should be the choice of the caller, not hardcoded.
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
        mesh.vertices().0[0].coloring,
        Coloring::Texture {
            pos: point3(0.5, 0., 0.),
            clamp_min: point3(0.5, 0.5, 0.5),
            clamp_max: point3(0.5, 0.5, 0.5),
            resolution: R1,
        }
    )
}

/// Test handling of the case where `color` is transparent and `light_emission` is nonzero;
/// for an atom (resolution-1) block; it should have a nonempty mesh.
#[test]
fn emissive_only_atom() {
    let atom_block = Block::builder()
        .color(Rgba::TRANSPARENT)
        .light_emission(Rgb::ONE)
        .build();

    let (allocator, block_meshes, space_mesh) = mesh_blocks_and_space(
        &Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(atom_block)
            .build(),
    );

    assert!(!block_meshes[0].is_empty());
    assert!(!space_mesh.is_empty());
    assert_eq!(allocator.count_allocated(), 1);
    assert_eq!(
        space_mesh.vertices().0[0].coloring,
        Coloring::Texture {
            pos: point3(0.5, 0., 0.),
            clamp_min: point3(0.5, 0.5, 0.5),
            clamp_max: point3(0.5, 0.5, 0.5),
            resolution: R1,
        }
    )
}

/// Test handling of the case where `color` is transparent and `light_emission` is nonzero,
/// for a non-atom (non-resolution-1) block; it should have a nonempty mesh.
#[test]
fn emissive_only_voxels() {
    let mut u = Universe::new();
    let atom_block = Block::builder()
        .color(Rgba::TRANSPARENT)
        .light_emission(Rgb::ONE)
        .build();
    let voxel_block = Block::builder()
        .voxels_fn(R2, |cube| {
            if cube == Cube::ORIGIN {
                &atom_block
            } else {
                &AIR
            }
        })
        .unwrap()
        .build_into(&mut u);
    let (allocator, block_meshes, space_mesh) = mesh_blocks_and_space(
        &Space::builder(GridAab::ORIGIN_CUBE)
            .read_ticket(u.read_ticket())
            .filled_with(voxel_block)
            .build(),
    );

    assert!(!block_meshes[0].is_empty());
    assert!(!space_mesh.is_empty());
    assert_eq!(allocator.count_allocated(), 1);
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
        .read_ticket(u.read_ticket())
        .filled_with(recursive_block)
        .build();

    let (tex, block_meshes, space_rendered) = mesh_blocks_and_space(&outer_space);

    eprintln!("{block_meshes:#?}");
    eprintln!("{space_rendered:#?}");

    // Compare the contents of the space mesh and block mesh.
    assert_eq!(
        space_rendered.vertices().0.to_vec(),
        block_meshes[0]
            .all_sub_meshes()
            .flat_map(|sm| sm.vertices.0.clone().into_iter())
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
        .read_ticket(u.read_ticket())
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
    .read_ticket(u.read_ticket())
        .filled_with(less_than_full_block)
        .build();

    let (tex, _, space_rendered) = mesh_blocks_and_space(&outer_space);

    assert_eq!(tex.count_allocated(), 1);
    assert_eq!(
        space_rendered.vertices().0.iter().map(|&v| v.remove_clamps()).collect::<Vec<_>>(),
        vec![
            v_t([0.250, 0.250, 0.250], NX, resolution, [2.5, 2.0, 2.0]),
            v_t([0.250, 0.250, 0.750], NX, resolution, [2.5, 2.0, 6.0]),
            v_t([0.250, 0.750, 0.250], NX, resolution, [2.5, 6.0, 2.0]),
            v_t([0.250, 0.750, 0.750], NX, resolution, [2.5, 6.0, 6.0]),

            v_t([0.250, 0.250, 0.250], NY, resolution, [2.0, 2.5, 2.0]),
            v_t([0.750, 0.250, 0.250], NY, resolution, [6.0, 2.5, 2.0]),
            v_t([0.250, 0.250, 0.750], NY, resolution, [2.0, 2.5, 6.0]),
            v_t([0.750, 0.250, 0.750], NY, resolution, [6.0, 2.5, 6.0]),

            v_t([0.250, 0.250, 0.250], NZ, resolution, [2.0, 2.0, 2.5]),
            v_t([0.250, 0.750, 0.250], NZ, resolution, [2.0, 6.0, 2.5]),
            v_t([0.750, 0.250, 0.250], NZ, resolution, [6.0, 2.0, 2.5]),
            v_t([0.750, 0.750, 0.250], NZ, resolution, [6.0, 6.0, 2.5]),

            v_t([0.750, 0.750, 0.250], PX, resolution, [5.5, 6.0, 2.0]),
            v_t([0.750, 0.750, 0.750], PX, resolution, [5.5, 6.0, 6.0]),
            v_t([0.750, 0.250, 0.250], PX, resolution, [5.5, 2.0, 2.0]),
            v_t([0.750, 0.250, 0.750], PX, resolution, [5.5, 2.0, 6.0]),

            v_t([0.750, 0.750, 0.250], PY, resolution, [6.0, 5.5, 2.0]),
            v_t([0.250, 0.750, 0.250], PY, resolution, [2.0, 5.5, 2.0]),
            v_t([0.750, 0.750, 0.750], PY, resolution, [6.0, 5.5, 6.0]),
            v_t([0.250, 0.750, 0.750], PY, resolution, [2.0, 5.5, 6.0]),

            v_t([0.250, 0.750, 0.750], PZ, resolution, [2.0, 6.0, 5.5]),
            v_t([0.250, 0.250, 0.750], PZ, resolution, [2.0, 2.0, 5.5]),
            v_t([0.750, 0.750, 0.750], PZ, resolution, [6.0, 6.0, 5.5]),
            v_t([0.750, 0.250, 0.750], PZ, resolution, [6.0, 2.0, 5.5]),
        ],
    );
    assert_eq!(
        space_rendered.bounding_box().all(),
        Some(Aab::from_lower_upper([0.25, 0.25, 0.25], [0.75, 0.75, 0.75]))
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
    let filler_block = block::from_color!(0.0, 1.0, 0.5, 1.0);
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
    .read_ticket(u.read_ticket())
        .filled_with(less_than_full_block)
        .build();

    let (tex, _, space_rendered) = mesh_blocks_and_space(&outer_space);

    assert_eq!(tex.count_allocated(), 0, "should have no texture");
    assert_eq!(
        space_rendered.vertices().0.to_vec(),
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
    assert_eq!(
        space_rendered.bounding_box().all(),
        Some(Aab::from_lower_upper([0.25, 0.25, 0.25], [0.75, 0.75, 0.75]))
    );
}

fn opacities<M: MeshTypes>(mesh: &BlockMesh<M>) -> FaceMap<bool> {
    assert_eq!(
        mesh.interior_vertices.map_ref(|_, fm| fm.fully_opaque),
        FaceMap::splat(false),
        "interior opacity should never be true because it doesn't mean anything"
    );
    mesh.face_vertices.map_ref(|_, sm| sm.fully_opaque)
}

#[test]
fn atom_transparency() {
    let universe = Universe::new();
    assert_eq!(
        opacities(&test_block_mesh(&universe, block::from_color!(Rgba::WHITE))),
        FaceMap::splat(true)
    );
    assert_eq!(
        opacities(&test_block_mesh(
            &universe,
            block::from_color!(Rgba::TRANSPARENT)
        )),
        FaceMap::splat(false)
    );
    assert_eq!(
        opacities(&test_block_mesh(
            &universe,
            block::from_color!(1.0, 1.0, 1.0, 0.5)
        )),
        FaceMap::splat(false)
    );
}

#[test]
fn atom_transparency_thresholded() {
    let universe = Universe::new();
    // Threshold means that partial transparency should produce exactly the same mesh as 0 or 1
    assert_eq!(
        test_block_mesh_threshold(&universe, block::from_color!(1.0, 1.0, 1.0, 0.25)),
        test_block_mesh_threshold(&universe, block::from_color!(1.0, 1.0, 1.0, 0.0)),
    );
    assert_eq!(
        test_block_mesh_threshold(&universe, block::from_color!(1.0, 1.0, 1.0, 0.75)),
        test_block_mesh_threshold(&universe, block::from_color!(1.0, 1.0, 1.0, 1.0)),
    );

    // TODO: also test voxels -- including self-occlusion (thresholded voxel in front of truly opaque voxel)
}

/// Test mesh opacity flags from basic voxels.
#[test]
fn fully_opaque_voxels() {
    let resolution = R8;
    let mut universe = Universe::new();
    let block = Block::builder()
        .voxels_fn(resolution, |cube| {
            // Make a cube-corner shape
            // TODO: Also test partial alpha
            if cube.x < 1 || cube.y < 1 || cube.z < 1 {
                block::from_color!(Rgba::BLACK)
            } else {
                AIR
            }
        })
        .unwrap()
        .build_into(&mut universe);
    assert_eq!(
        opacities(&test_block_mesh(&universe, block)),
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
    let mut universe = Universe::new();
    let block = Block::builder()
        .voxels_handle(R8, {
            // The dimensions don't meet the PX face, but the blocks are all opaque.
            universe.insert_anonymous(
                Space::builder(GridAab::from_lower_size([0, 0, 0], [4, 8, 8]))
                    .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                    .read_ticket(universe.read_ticket())
                    .filled_with(block::from_color!(Rgba::WHITE))
                    .build(),
            )
        })
        .build();
    assert_eq!(
        opacities(&test_block_mesh(&universe, block)),
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
    let mut universe = Universe::new();
    let block = Block::builder()
        .voxels_handle(R8, {
            // Don't use voxels_fn() because it auto-shrinkwraps.
            universe.insert_anonymous(
                Space::builder(GridAab::from_lower_size([0, 0, 0], [8, 8, 8]))
                    .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                    .build(),
            )
        })
        .build();
    assert_eq!(
        block
            .evaluate(universe.read_ticket())
            .unwrap()
            .voxels()
            .bounds(),
        GridAab::for_block(R8),
        "sanity check test data"
    );

    assert_eq!(
        opacities(&test_block_mesh(&universe, block)),
        FaceMap::splat(false)
    );
}

#[test]
fn transparency_split() {
    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [3, 1, 1]))
        .build_and_mutate(|m| {
            // One opaque block and one transparent block
            m.set([0, 0, 0], block::from_color!(1.0, 0.0, 0.0, 1.0))?;
            m.set([2, 0, 0], block::from_color!(0.0, 0.0, 1.0, 0.5))?;
            Ok(())
        })
        .unwrap();

    let (_, _, space_rendered) = mesh_blocks_and_space(&space);
    // 2 cubes...
    assert_eq!(space_rendered.vertices().0.len(), 6 * 4 * 2);
    // ...one of which is opaque...
    assert_eq!(space_rendered.opaque_range().len(), 6 * 6);
    // ...and one of which is transparent
    for ordering in DepthOrdering::exhaust() {
        let num_visible_faces = match ordering.within_on_axes() {
            0 => 3,
            1 => 4,
            2 => 5,
            3 => 6,
            _ => unreachable!(),
        };
        assert_eq!(
            space_rendered.transparent_range(ordering).len(),
            6 * num_visible_faces
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
    let block_derived_color = complex_block.evaluate(u.read_ticket()).unwrap().color();

    let space = Space::builder(GridAab::ORIGIN_CUBE)
        .read_ticket(u.read_ticket())
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
    for vertex in space_mesh.vertices().0 {
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
    assert_eq!(t.vertices(), (&[][..], &[][..]));
    assert_eq!(t.indices(), IndexSlice::U16(&[]));
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
    let mesh = test_block_mesh(&universe, block);
    for (face, on_face, face_mesh) in mesh.all_sub_meshes_keyed() {
        if !on_face {
            // make_some_voxel_blocks has no interior
            continue;
        }
        let mut had_any_textured = false;
        for vertex in face_mesh.vertices.0.iter() {
            match vertex.coloring {
                Coloring::Solid(_) => {}
                Coloring::Texture {
                    pos,
                    clamp_min,
                    clamp_max,
                    resolution: _,
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
        }
        assert!(
            !on_face || had_any_textured,
            "test invalid: {face:?} has no textured vertices"
        )
    }
}

/// Test the texture coordinates for volumetric texturing.
#[test]
fn volumetric_vertices_all_transparent() {
    // A block with almost all partially transparent voxels
    let mut universe = Universe::new();
    let block = Block::builder()
        .voxels_fn(R4, |cube| {
            if cube.lower_bounds().rem_euclid(&Size3D::splat(2)) == point3(0, 0, 0) {
                block::from_color!(1.0, 0.0, 0.0, 0.9)
            } else if cube.lower_bounds().rem_euclid(&Size3D::splat(2)) == point3(1, 1, 1) {
                block::from_color!(0.0, 0.0, 1.0, 0.9)
            } else {
                block::from_color!(1.0, 1.0, 1.0, 0.12)
            }
        })
        .unwrap()
        .build_into(&mut universe);

    assert_is_using_volumetric_texturing_properly(dbg!(&test_block_mesh(&universe, block)), true);
}
/// Test mixed transparent and opaque voxels that share a surface.
/// This is a regression test for unwanted [`crate::TransparencyFormat::Surfaces`]-style triangles
/// appearing in [`crate::TransparencyFormat::BoundingBox`].
#[test]
fn volumetric_mixed_vertices() {
    let mut universe = Universe::new();
    let block = Block::builder()
        .voxels_fn(R4, |cube| {
            if cube.lower_bounds().rem_euclid(&Size3D::splat(2)) == point3(0, 0, 0) {
                if cube.lower_bounds().x == 0 {
                    // opaque that will have some transparent surface between its occurrences
                    block::from_color!(1.0, 0.0, 0.0, 1.0)
                } else {
                    block::from_color!(1.0, 0.0, 0.0, 0.9)
                }
            } else {
                block::from_color!(1.0, 1.0, 1.0, 0.12)
            }
        })
        .unwrap()
        .build_into(&mut universe);

    assert_is_using_volumetric_texturing_properly(dbg!(&test_block_mesh(&universe, block)), false);
}
/// For a full R4 block using volumetric texturing, check that the texture coordinates and
/// number of triangles are correct.
///
/// If `expect_transparent_only` is false, then opaque *untextured* vertices are allowed.
/// (This is an approximation of checking to make sure that there are no transparent surfaces
/// other than the bounding box for the volumetric rendering.)
fn assert_is_using_volumetric_texturing_properly(
    mesh: &BlockMesh<TextureMt>,
    expect_transparent_only: bool,
) {
    assert!(mesh.texture_used.is_some());

    for (face, on_face, sub_mesh) in mesh.all_sub_meshes_keyed() {
        eprintln!("Checking {face:?}...");
        if expect_transparent_only {
            assert_eq!(
                sub_mesh.vertices.0.len(),
                if on_face { 4 } else { 0 },
                "vertex count in {face:?}"
            );
        }
        for (i, vertex) in sub_mesh.vertices.0.iter().enumerate() {
            match vertex.coloring {
                Coloring::Texture {
                    pos,
                    clamp_min,
                    clamp_max,
                    resolution,
                } => {
                    assert_eq!(
                        pos,
                        vertex.position.to_f32().cast_unit() * f32::from(resolution),
                        "vertex position {face:?} {i:?}"
                    );
                    assert_eq!(
                        (clamp_min, clamp_max),
                        (point3(0., 0., 0.), point3(4., 4., 4.)),
                        "clamp {face:?} {i:?}"
                    );
                }
                _ if expect_transparent_only => {
                    panic!("not using texture {face:?} {i:?}");
                }
                Coloring::Solid(color) => {
                    assert!(color.fully_opaque(), "found transparent face {vertex:?}");
                }
            }
        }
    }
}

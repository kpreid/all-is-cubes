// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Tests for [`crate::triangulator`].

use cgmath::{MetricSpace as _, Point3, Transform as _, Vector3};
use pretty_assertions::assert_eq;

use super::*;
use crate::block::{Block, BlockAttributes, AIR};
use crate::camera::{GraphicsOptions, TransparencyOption};
use crate::content::make_some_blocks;
use crate::math::{Face, GridRotation};
use crate::math::{Face::*, FaceMap, FreeCoordinate, GridPoint, Rgba};
use crate::space::{Grid, Space, SpacePhysics};
use crate::triangulator::BlockTriangulation;
use crate::universe::Universe;

/// Shorthand for writing out an entire [`BlockVertex`] with solid color.
fn v_c(position: [FreeCoordinate; 3], face: Face, color: [f32; 4]) -> BlockVertex {
    BlockVertex {
        position: position.into(),
        face,
        coloring: Coloring::Solid(Rgba::new(color[0], color[1], color[2], color[3])),
    }
}

/// Shorthand for writing out an entire [`BlockVertex`] with texturing.
fn v_t(position: [FreeCoordinate; 3], face: Face, texture: [TextureCoordinate; 3]) -> BlockVertex {
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

/// Test helper to call `triangulate_block` alone without a `Space`.
fn test_triangulate_block(block: Block) -> BlockTriangulation<BlockVertex, TestTextureTile> {
    triangulate_block(
        &block.evaluate().unwrap(),
        &mut TestTextureAllocator::new(),
        &TriangulatorOptions {
            transparency: TransparencyOption::Volumetric,
            ..TriangulatorOptions::dont_care_for_test()
        },
    )
}

/// Test helper to call `triangulate_block` alone without a `Space`,
/// and with the transparency option set to `Threshold(0.5)`.
fn test_triangulate_block_threshold(
    block: Block,
) -> BlockTriangulation<BlockVertex, TestTextureTile> {
    triangulate_block(
        &block.evaluate().unwrap(),
        &mut TestTextureAllocator::new(),
        &TriangulatorOptions {
            transparency: TransparencyOption::Threshold(notnan!(0.5)),
            ..TriangulatorOptions::dont_care_for_test()
        },
    )
}

/// Test helper to call `triangulate_blocks` followed directly by [`triangulate_space`].
fn triangulate_blocks_and_space(
    space: &Space,
) -> (
    TestTextureAllocator,
    BlockTriangulations<BlockVertex, TestTextureTile>,
    SpaceTriangulation<BlockVertex>,
) {
    let options = &TriangulatorOptions::new(&GraphicsOptions::default());
    let mut tex = TestTextureAllocator::new();
    let block_triangulations = triangulate_blocks(space, &mut tex, options);
    let space_triangulation: SpaceTriangulation<BlockVertex> =
        triangulate_space(space, space.grid(), options, &*block_triangulations);
    (tex, block_triangulations, space_triangulation)
}

fn non_uniform_fill(cube: GridPoint) -> &'static Block {
    const BLOCKS: &[Block] = &[
        Block::Atom(BlockAttributes::default(), rgba_const!(1., 1., 1., 1.)),
        Block::Atom(BlockAttributes::default(), rgba_const!(0., 0., 0., 1.)),
    ];
    &BLOCKS[(cube.x + cube.y + cube.z).rem_euclid(2) as usize]
}

#[test]
fn excludes_hidden_faces_of_blocks() {
    let mut space = Space::empty_positive(2, 2, 2);
    space
        .fill(space.grid(), |p| Some(non_uniform_fill(p)))
        .unwrap();
    let (_, _, space_tri) = triangulate_blocks_and_space(&space);

    // The space rendering should be a 2×2×2 cube of tiles, without any hidden interior faces.
    assert_eq!(
        Vec::<&BlockVertex>::new(),
        space_tri
            .vertices()
            .iter()
            .filter(|vertex| vertex.position.distance2(Point3::new(1.0, 1.0, 1.0)) < 0.99)
            .collect::<Vec<&BlockVertex>>(),
        "found an interior point"
    );
    assert_eq!(
        space_tri.vertices().len(),
        4 /* vertices per face */
        * 4 /* block faces per exterior side of space */
        * 6, /* sides of space */
        "wrong number of faces"
    );
}

/// Run [`triangulate_space`] with stale block data and confirm it does not panic.
#[test]
fn no_panic_on_missing_blocks() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(2, 1, 1);
    let block_triangulations: BlockTriangulations<BlockVertex, _> = triangulate_blocks(
        &space,
        &mut TestTextureAllocator::new(),
        &TriangulatorOptions::dont_care_for_test(),
    );
    assert_eq!(block_triangulations.len(), 1); // check our assumption

    // This should not panic; visual glitches are preferable to failure.
    space.set((0, 0, 0), &block).unwrap(); // render data does not know about this
    triangulate_space(
        &space,
        space.grid(),
        &TriangulatorOptions::dont_care_for_test(),
        &*block_triangulations,
    );
}

/// Construct a 1x1 recursive block and test that this is equivalent in geometry
/// to an atom block.
#[test]
fn trivial_voxels_equals_atom() {
    // Construct recursive block.
    let mut u = Universe::new();
    let atom_block = Block::from(Rgba::new(0.0, 1.0, 0.0, 1.0));
    let trivial_recursive_block = Block::builder()
        .voxels_fn(&mut u, 1, |_| &atom_block)
        .unwrap()
        .build();

    let (_, _, space_rendered_a) = triangulate_blocks_and_space(&{
        let mut space = Space::empty_positive(1, 1, 1);
        space.set((0, 0, 0), &atom_block).unwrap();
        space
    });
    let (tex, _, space_rendered_r) = triangulate_blocks_and_space(&{
        let mut space = Space::empty_positive(1, 1, 1);
        space.set((0, 0, 0), &trivial_recursive_block).unwrap();
        space
    });

    assert_eq!(space_rendered_a, space_rendered_r);
    assert_eq!(tex.count_allocated(), 0);
}

/// [`triangulate_space`] of a 1×1×1 space has the same geometry as the contents.
#[test]
fn space_tri_equals_block_tri() {
    // Construct recursive block.
    let resolution = 4;
    let mut u = Universe::new();
    let mut blocks = Vec::from(make_some_blocks::<2>());
    blocks.push(AIR);
    let recursive_block = Block::builder()
        .voxels_fn(&mut u, resolution, |p| {
            &blocks[(p.x as usize).rem_euclid(blocks.len())]
        })
        .unwrap()
        .build();
    let mut outer_space = Space::empty_positive(1, 1, 1);
    outer_space.set((0, 0, 0), &recursive_block).unwrap();

    let (tex, block_triangulations, space_rendered) = triangulate_blocks_and_space(&outer_space);

    eprintln!("{:#?}", block_triangulations);
    eprintln!("{:#?}", space_rendered);

    assert_eq!(
        space_rendered.vertices().to_vec(),
        block_triangulations[0]
            .faces
            .iter()
            .flat_map(|(_face, face_render)| face_render.vertices.clone().into_iter())
            .collect::<Vec<_>>()
    );
    assert_eq!(tex.count_allocated(), 1); // for striped faces
}

/// TODO: This test stops being meaningful when we finish migrating the texture allocator to use arbitrary-sized tiles
#[test]
fn block_resolution_greater_than_tile() {
    let block_resolution = 32;
    let mut u = Universe::new();
    let block = Block::builder()
        .voxels_fn(&mut u, block_resolution, non_uniform_fill)
        .unwrap()
        .build();
    let mut outer_space = Space::empty_positive(1, 1, 1);
    outer_space.set((0, 0, 0), &block).unwrap();

    let (_, _, _) = triangulate_blocks_and_space(&outer_space);
    // TODO: Figure out how to make a useful assert. At least this is "it doesn't panic".
}

/// Check for hidden surfaces being given internal geometry.
/// Exercise the “shrinkwrap” logic that generates geometry no larger than necessary.
#[test]
#[rustfmt::skip]
fn shrunken_box_has_no_extras() {
    // Construct a box whose faces don't touch the outer extent of the volume.
    let resolution = 8;
    let mut u = Universe::new();
    let less_than_full_block = Block::builder()
        .voxels_fn(&mut u, resolution, |cube| {
            if Grid::new((2, 2, 2), (4, 4, 4)).contains_cube(cube) {
                non_uniform_fill(cube)
            } else {
                &AIR
            }
        })
        .unwrap()
        .build();
    let mut outer_space = Space::empty_positive(1, 1, 1);
    outer_space.set((0, 0, 0), &less_than_full_block).unwrap();

    let (tex, _, space_rendered) = triangulate_blocks_and_space(&outer_space);

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
    let resolution = 8;
    let mut u = Universe::new();
    let filler_block = Block::from(Rgba::new(0.0, 1.0, 0.5, 1.0));
    let less_than_full_block = Block::builder()
        .voxels_fn(&mut u, resolution, |cube| {
            if Grid::new((2, 2, 2), (4, 4, 4)).contains_cube(cube) {
                &filler_block
            } else {
                &AIR
            }
        })
        .unwrap()
        .build();
    let mut outer_space = Space::empty_positive(1, 1, 1);
    outer_space.set((0, 0, 0), &less_than_full_block).unwrap();

    let (tex, _, space_rendered) = triangulate_blocks_and_space(&outer_space);

    assert_eq!(tex.count_allocated(), 0);
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

/// Make a [`FaceMap`] with uniform values except for [`Face::Within`].
fn except_within<T: Clone>(without: T, within: T) -> FaceMap<T> {
    FaceMap::from_fn(|face| {
        if face == Face::Within {
            within.clone()
        } else {
            without.clone()
        }
    })
}

#[test]
fn atom_transparency() {
    assert_eq!(
        test_triangulate_block(Block::from(Rgba::WHITE))
            .faces
            .map(|_, ft| ft.fully_opaque),
        except_within(true, false)
    );
    assert_eq!(
        test_triangulate_block(Block::from(Rgba::TRANSPARENT))
            .faces
            .map(|_, ft| ft.fully_opaque),
        except_within(false, false)
    );
    assert_eq!(
        test_triangulate_block(Block::from(Rgba::new(1.0, 1.0, 1.0, 0.5)))
            .faces
            .map(|_, ft| ft.fully_opaque),
        except_within(false, false)
    );
}

#[test]
fn atom_transparency_thresholded() {
    // Threshold means that triangulations of partial transparency should be exactly the same as 0 or 1
    assert_eq!(
        test_triangulate_block_threshold(Block::from(Rgba::new(1.0, 1.0, 1.0, 0.25))).faces,
        test_triangulate_block_threshold(Block::from(Rgba::new(1.0, 1.0, 1.0, 0.0))).faces,
    );
    assert_eq!(
        test_triangulate_block_threshold(Block::from(Rgba::new(1.0, 1.0, 1.0, 0.75))).faces,
        test_triangulate_block_threshold(Block::from(Rgba::new(1.0, 1.0, 1.0, 1.0))).faces,
    );

    // TODO: also test voxels -- including self-occlusion (thresholded voxel in front of truly opaque voxel)
}

/// Test [`BlockTriangulation::fully_opaque`] results from basic voxels.
#[test]
fn fully_opaque_voxels() {
    let resolution = 8;
    let mut u = Universe::new();
    let block = Block::builder()
        .voxels_fn(&mut u, resolution, |cube| {
            // Make a cube-corner shape
            // TODO: Also test partial alpha
            if cube.x < 1 || cube.y < 1 || cube.z < 1 {
                Block::from(Rgba::BLACK)
            } else {
                AIR
            }
        })
        .unwrap()
        .build();
    assert_eq!(
        test_triangulate_block(block)
            .faces
            .map(|_, ft| ft.fully_opaque),
        FaceMap {
            within: false,
            nx: true,
            ny: true,
            nz: true,
            px: false,
            py: false,
            pz: false,
        }
    );
}

/// Test [`BlockTriangulation::fully_opaque`] when the voxels are all individually opaque,
/// but don't fill the cube.
#[test]
fn fully_opaque_partial_block() {
    let mut u = Universe::new();
    let block = Block::builder()
        .voxels_ref(8, {
            // The dimensions don't meet the PX face.
            let mut block_space = Space::builder(Grid::new([0, 0, 0], [4, 8, 8]))
                .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                .build_empty();
            // But the blocks are all opaque.
            block_space
                .fill_uniform(block_space.grid(), Block::from(Rgba::WHITE))
                .unwrap();
            u.insert_anonymous(block_space)
        })
        .build();
    assert_eq!(
        test_triangulate_block(block)
            .faces
            .map(|_, ft| ft.fully_opaque),
        FaceMap {
            within: false,
            nx: true,
            ny: false,
            nz: false,
            px: false,
            py: false,
            pz: false,
        }
    );
}

#[test]
fn transparency_split() {
    let mut space = Space::empty_positive(3, 1, 1);
    // One opaque block and one transparent block
    space
        .set([0, 0, 0], Block::from(Rgba::new(1.0, 0.0, 0.0, 1.0)))
        .unwrap();
    space
        .set([2, 0, 0], Block::from(Rgba::new(0.0, 0.0, 1.0, 0.5)))
        .unwrap();

    let (_, _, space_rendered) = triangulate_blocks_and_space(&space);
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
    let resolution = 8;
    let mut u = Universe::new();
    let complex_block = Block::builder()
        .voxels_fn(&mut u, resolution, |cube| {
            if (cube.x + cube.y + cube.z) % 2 == 0 {
                Rgba::WHITE.into()
            } else {
                AIR
            }
        })
        .unwrap()
        .build();

    let mut space = Space::empty_positive(1, 1, 1);
    space.set((0, 0, 0), &complex_block).unwrap();

    let mut tex = TestTextureAllocator::new();
    // TODO: Once we support tiling for high resolution blocks, make this a partial failure.
    let capacity = 0;
    tex.set_capacity(capacity);
    let block_triangulations: BlockTriangulations<BlockVertex, _> =
        triangulate_blocks(&space, &mut tex, &TriangulatorOptions::dont_care_for_test());

    // Check results.
    assert_eq!(tex.count_allocated(), capacity);
    assert_eq!(1, block_triangulations.len());
    // TODO: Check that the triangulation includes the failure marker/fallback color.
    let _complex_block_triangulation = &block_triangulations[0];
}

#[test]
fn space_triangulation_empty() {
    let t = SpaceTriangulation::<BlockVertex>::new();
    assert!(t.is_empty());
    assert_eq!(t.vertices(), &[]);
    assert_eq!(t.indices(), &[]);
}

#[test]
fn depth_ordering_from_view_direction() {
    let mut problems = Vec::new();
    // A coordinate range of ±3 will exercise every combination of axis orderings.
    let range = -3..3;
    for x in range.clone() {
        for y in range.clone() {
            for z in range.clone() {
                let direction = Vector3::new(x, y, z);
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

// TODO: more tests

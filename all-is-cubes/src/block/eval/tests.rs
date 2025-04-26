//! Tests mostly of the block evaluation algorithm.
//!
//! Tests for the [`EvaluatedBlock`] type are under [`super::evaluated::tests`].

use euclid::Vector3D;
use pretty_assertions::assert_eq;
use rstest::rstest;

use crate::block::{
    self, AIR, AnimationChange, AnimationHint, Atom, Block, BlockAttributes, BlockCollision,
    BlockDef, Cost, EvKey, EvaluatedBlock, Evoxel, Evoxels, Modifier, Primitive,
    Resolution::{self, *},
    VoxelOpacityMask, modifier,
};
use crate::math::{
    Cube, Face6, FaceMap, GridAab, GridPoint, GridVector, Intensity, OpacityCategory, Rgb, Rgba,
    Vol, zo32,
};
use crate::space::Space;
use crate::universe::{ReadTicket, Universe};

#[track_caller]
fn evaluate(block: &Block) -> EvaluatedBlock {
    block.evaluate(ReadTicket::stub()).unwrap()
}

/// `Evoxel`s are stored in large quantity, so we should think carefully any time we
/// might make it bigger. Or maybe even try to make it smaller.
#[test]
fn evoxel_size() {
    assert_eq!(
        size_of::<Evoxel>(),
        (4 + 3) * size_of::<f32>() // colors
                + 2 // flags
                + 2 // padding
    )
}

#[test]
fn visible_or_animated() {
    #[expect(clippy::needless_pass_by_value)]
    fn va(block: Block) -> bool {
        evaluate(&block).visible_or_animated()
    }
    assert!(!va(AIR));
    assert!(!va(Block::builder().color(Rgba::TRANSPARENT).build()));
    assert!(va(Block::builder().color(Rgba::WHITE).build()));
    assert!(va(Block::builder()
        .color(Rgba::TRANSPARENT)
        .light_emission(Rgb::ONE)
        .build()));
    assert!(va(Block::builder()
        .color(Rgba::TRANSPARENT)
        .animation_hint(AnimationHint::replacement(AnimationChange::Shape,))
        .build()));
}

/// Test that interior color is hidden by surface color.
///
/// TODO: This test is irregular because it bypasses constructing a `Block`, but
/// this is convenient, but it doesn't match other tests in `crate::block`. What style
/// should we use?
#[test]
fn overall_color_ignores_interior() {
    let resolution = R8;
    let outer_bounds = GridAab::for_block(resolution);
    let inner_bounds = outer_bounds.shrink(FaceMap::splat(1)).unwrap();
    let outer_color = Rgba::new(1.0, 0.0, 0.0, 1.0);
    let inner_color = Rgba::new(0.0, 1.0, 0.0, 1.0);
    let voxels = Evoxels::from_many(
        resolution,
        Vol::from_fn(outer_bounds, |p| {
            Evoxel::from_color(if inner_bounds.contains_cube(p) {
                inner_color
            } else {
                outer_color
            })
        }),
    );

    // The inner_color should be ignored because it is not visible.
    let ev = EvaluatedBlock::from_voxels(AIR, BlockAttributes::default(), voxels, Cost::ZERO);

    assert_eq!(ev.color(), outer_color);
}

#[test]
fn air_self_consistent() {
    evaluate(&AIR).consistency_check();
}

#[test]
fn air_consistent_with_constant() {
    assert_eq!(evaluate(&AIR), block::AIR_EVALUATED);
    assert_eq!(evaluate(&Block::from(Primitive::Air)), block::AIR_EVALUATED);
}

#[test]
fn air_consistent_with_evoxel_air() {
    assert_eq!(evaluate(&AIR).voxels.single_voxel().unwrap(), Evoxel::AIR);
}

#[test]
fn air_in_recursive_block() {
    let mut universe = Universe::new();
    let block = Block::builder()
        .voxels_fn(R1, |_| AIR)
        .unwrap()
        .build_into(&mut universe);
    assert_eq!(evaluate(&block).voxels.single_voxel().unwrap(), Evoxel::AIR);
}

#[test]
fn opaque_atom_and_attributes() {
    let color = Rgba::new(1.0, 2.0, 3.0, 1.0);
    let block = Block::from(Atom {
        color,
        emission: Rgb::ONE,
        collision: BlockCollision::None,
    });
    let e = evaluate(&block);
    assert_eq!(e.attributes(), BlockAttributes::DEFAULT_REF);
    assert_eq!(e.color(), color);
    assert_eq!(e.face_colors(), FaceMap::splat(color));
    assert_eq!(e.light_emission(), Rgb::ONE);
    assert_eq!(
        e.voxels,
        Evoxels::from_one(Evoxel {
            color,
            emission: Rgb::ONE,
            selectable: true,
            collision: BlockCollision::None,
        })
    );
    assert_eq!(e.resolution(), R1);
    assert_eq!(e.opaque(), FaceMap::splat(true));
    assert_eq!(e.visible(), true);
    assert_eq!(
        *e.voxel_opacity_mask(),
        VoxelOpacityMask::new_raw(R1, Vol::from_element(OpacityCategory::Opaque))
    )
}

#[test]
fn transparent_atom() {
    let color = Rgba::new(1.0, 2.0, 3.0, 0.5);
    let block = Block::from(color);
    let e = evaluate(&block);
    assert_eq!(e.color(), color);
    assert_eq!(e.face_colors(), FaceMap::splat(color));
    assert_eq!(e.light_emission(), Rgb::ZERO);
    assert!(e.voxels.single_voxel().is_some());
    assert_eq!(e.opaque(), FaceMap::splat(false));
    assert_eq!(e.visible(), true);
    assert_eq!(
        *e.voxel_opacity_mask(),
        VoxelOpacityMask::new_raw(R1, Vol::from_element(OpacityCategory::Partial))
    )
}

#[test]
fn emissive_only_atom() {
    let emissive_color = Rgb::new(1.0, 2.0, 3.0);
    let block = Block::builder()
        .color(Rgba::TRANSPARENT)
        .light_emission(emissive_color)
        .build();
    let e = evaluate(&block);
    assert_eq!(e.color(), Rgba::TRANSPARENT);
    assert_eq!(e.face_colors(), FaceMap::splat(Rgba::TRANSPARENT));
    assert_eq!(e.light_emission(), emissive_color);
    assert!(e.voxels.single_voxel().is_some());
    assert_eq!(e.opaque(), FaceMap::splat(false));
    assert_eq!(e.visible(), true);
    assert_eq!(
        *e.voxel_opacity_mask(),
        VoxelOpacityMask::new_raw(R1, Vol::from_element(OpacityCategory::Partial))
    )
}

#[test]
fn invisible_atom() {
    let block = block::from_color!(Rgba::TRANSPARENT);
    let e = evaluate(&block);
    assert_eq!(e.color(), Rgba::TRANSPARENT);
    assert_eq!(e.face_colors(), FaceMap::splat(Rgba::TRANSPARENT));
    assert!(e.voxels.single_voxel().is_some());
    assert_eq!(e.opaque(), FaceMap::splat(false));
    assert_eq!(e.visible(), false);
    assert_eq!(
        *e.voxel_opacity_mask(),
        VoxelOpacityMask::new_raw(R1, Vol::from_element(OpacityCategory::Invisible))
    );
}

#[test]
fn voxels_checked_individually() {
    let resolution = R2;
    let mut universe = Universe::new();

    let attributes = BlockAttributes {
        display_name: arcstr::literal!("hello world"),
        ..BlockAttributes::default()
    };
    let block = Block::builder()
        .attributes(attributes.clone())
        .voxels_fn(resolution, |point| {
            let point = point.lower_bounds().cast::<f32>();
            Block::from(Rgba::new(point.x, point.y, point.z, 1.0))
        })
        .unwrap()
        .build_into(&mut universe);

    let e = evaluate(&block);
    assert_eq!(e.attributes, attributes);
    assert_eq!(
        e.voxels,
        Evoxels::from_many(
            resolution,
            Vol::from_fn(GridAab::for_block(resolution), |point| {
                let point = point.lower_bounds().cast::<f32>();
                Evoxel {
                    color: Rgba::new(point.x, point.y, point.z, 1.0),
                    emission: Rgb::ZERO,
                    selectable: true,
                    collision: BlockCollision::Hard,
                }
            })
        )
    );
    assert_eq!(e.color(), Rgba::new(0.5, 0.5, 0.5, 1.0));
    assert_eq!(
        e.face_colors(),
        FaceMap {
            nx: Rgba::new(0.0, 0.5, 0.5, 1.0),
            ny: Rgba::new(0.5, 0.0, 0.5, 1.0),
            nz: Rgba::new(0.5, 0.5, 0.0, 1.0),
            px: Rgba::new(1.0, 0.5, 0.5, 1.0),
            py: Rgba::new(0.5, 1.0, 0.5, 1.0),
            pz: Rgba::new(0.5, 0.5, 1.0, 1.0),
        }
    );
    assert_eq!(e.resolution(), resolution);
    assert_eq!(e.opaque(), FaceMap::splat(true));
    assert_eq!(e.visible(), true);
    assert_eq!(
        *e.voxel_opacity_mask(),
        VoxelOpacityMask::new_raw(
            resolution,
            Vol::repeat(GridAab::for_block(resolution), OpacityCategory::Opaque,)
        )
    );
    assert_eq!(
        e.cost,
        Cost {
            components: 2,
            voxels: 8,
            recursion: 0
        }
    );
}

/// Test that light emission from voxels doesn't depend on resolution, or rather, the emission
/// is taken as an intensive property rather than an extensive property.
#[rstest]
fn voxels_emission_equivalence(
    #[values(Rgba::TRANSPARENT, Rgba::new(0.0, 0.5, 1.0, 0.5))] reflectance: Rgba,
    #[values(R1, R2, R4, R32)] resolution: Resolution,
) {
    let mut universe = Universe::new();
    let atom_emission = Rgb::new(1.0, 2.0, 3.0);
    let atom = Block::builder()
        .color(reflectance)
        .light_emission(atom_emission)
        .build();

    let voxel_block = Block::builder()
        .voxels_fn(resolution, |_| &atom)
        .unwrap()
        .build_into(&mut universe);

    let total_emission: Vector3D<f32, Intensity> = evaluate(&voxel_block).light_emission().into();
    let difference: Vector3D<f32, Intensity> = total_emission - atom_emission.into();
    assert!(
        difference.length() < 0.0001,
        "reflectance = {reflectance:?}\n\
                resolution = {resolution}\n\
                expected = {atom_emission:?}\n\
                actual = {total_emission:?}"
    );
}

#[test]
fn transparent_voxels_simple() {
    let mut universe = Universe::new();
    let resolution = R4;
    let voxel_color = Rgb::new(1.0, 0.5, 0.0);
    let alpha = 0.5;
    let block = Block::builder()
        .voxels_fn(resolution, |point| {
            Block::from(voxel_color.with_alpha(if point.x == 0 && point.z == 0 {
                zo32(alpha)
            } else {
                zo32(1.0)
            }))
        })
        .unwrap()
        .build_into(&mut universe);

    let e = evaluate(&block);
    // Transparency is (currently) computed by an orthographic view through all six
    // faces, and only two out of six faces in this test block don't fully cover
    // the light paths with opaque surfaces.
    assert_eq!(
        e.color(),
        voxel_color.with_alpha(zo32(1.0 - (alpha / (f32::from(resolution).powi(2) * 3.0))))
    );
    // This is the sum of the transparency of one voxel on one of the six faces
    let one_face_transparency =
        voxel_color.with_alpha(zo32(1.0 - (alpha / f32::from(resolution).powi(2))));
    assert_eq!(
        e.face_colors(),
        FaceMap {
            nx: voxel_color.with_alpha_one(),
            ny: one_face_transparency,
            nz: voxel_color.with_alpha_one(),
            px: voxel_color.with_alpha_one(),
            py: one_face_transparency,
            pz: voxel_color.with_alpha_one(),
        }
    );
    assert_eq!(
        e.opaque(),
        FaceMap {
            nx: false,
            ny: false,
            nz: false,
            px: true,
            py: false,
            pz: true,
        }
    );
    assert_eq!(e.visible(), true);
}

/// Check that when a block has different transparent parts with different colors, the colors
/// are correctly weighted with respect to each other.
///
/// TODO: This test doesn't pass and I'm not sure why.
#[test]
#[ignore = "not sure if code or test is wrong"]
fn transparent_voxels_weighted() {
    let mut universe = Universe::new();
    let c1 = Rgb::new(1.0, 0.0, 0.0);
    let c2 = Rgb::new(0.0, 1.0, 0.0);
    let colors = [c1.with_alpha_one(), c2.with_alpha(zo32(0.5))];
    let block = Block::builder()
        .voxels_fn(R2, |cube| Block::from(colors[cube.y as usize]))
        .unwrap()
        .build_into(&mut universe);
    let surface_area: f32 = 4. * 6.;

    let e = evaluate(&block);

    // for debugging, print the color multiplied by the surface area so that the
    // components end up round numbers
    dbg!(e.color().to_rgb() * surface_area);
    dbg!(e.face_colors().map(|_, c| c.to_rgb() * 4.));

    // each semitransparent voxel is 1/2 of a full block so its opacity is ^(1/2)
    let half_semi_alpha = 0.5_f32.powf(0.5);
    let semi_on_opaque_blend = dbg!(c1 * (1. - half_semi_alpha) + c2 * half_semi_alpha);

    assert_eq!(
        e.color().to_rgb(), // don't bother checking alpha; other tests do that
        (
            c1 * 4. // -Y face
            + semi_on_opaque_blend * 4. // +Y face
            + c1 * 8. // opaque half of other 4 faces
            + c2 * 4.
            // semitransparent half of other 4 faces
        ) * surface_area.recip()
    );
}

#[test]
fn voxels_full_but_transparent() {
    let resolution = R4;
    let mut universe = Universe::new();
    let block = Block::builder()
        .voxels_fn(resolution, |cube| {
            Block::from(Rgba::new(
                0.0,
                0.0,
                0.0,
                if cube == Cube::new(1, 1, 1) { 1.0 } else { 0.0 },
            ))
        })
        .unwrap()
        .build_into(&mut universe);

    let e = evaluate(&block);
    assert_eq!(
        e.color(),
        Rgba::new(0.0, 0.0, 0.0, 1.0 / f32::from(resolution).powi(2))
    );
    assert_eq!(e.resolution(), resolution);
    assert_eq!(e.opaque(), FaceMap::splat(false));
    assert_eq!(e.visible(), true);
}

/// Test the situation where the space is smaller than the block: in particular,
/// even if the space is all opaque, the block should not be counted as opaque.
#[test]
fn voxels_partial_not_filling() {
    let resolution = R4;
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(2, 4, 4);
    space
        .fill_uniform(space.bounds(), &block::from_color!(Rgba::WHITE))
        .unwrap();
    let space_handle = universe.insert_anonymous(space);
    let block = Block::builder()
        .voxels_handle(resolution, space_handle)
        .build();

    let e = evaluate(&block);
    // of 6 faces, 2 are opaque and 2 are half-transparent, thus there are 8 opaque half-faces.
    assert_eq!(e.color(), Rgba::new(1.0, 1.0, 1.0, 8. / 12.));
    assert_eq!(e.resolution(), resolution);
    assert_eq!(e.opaque(), FaceMap::splat(false).with(Face6::NX, true));
    assert_eq!(e.visible(), true);
}

/// Tests that the `offset` field of `Primitive::Recur` is respected.
#[test]
fn recur_with_offset() {
    let resolution = R4;
    let resolution_g = u32::from(resolution);
    let offset = GridVector::new(i32::from(resolution), 0, 0);
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(resolution_g * 2, resolution_g, resolution_g);
    space
        .fill(space.bounds(), |point| {
            let point = point.lower_bounds().cast::<f32>();
            Some(Block::from(Rgba::new(point.x, point.y, point.z, 1.0)))
        })
        .unwrap();
    let space_handle = universe.insert_anonymous(space);
    let block_at_offset = Block::from_primitive(Primitive::Recur {
        offset: offset.to_point(),
        resolution,
        space: space_handle,
    });

    let e = block_at_offset.evaluate(universe.read_ticket()).unwrap();
    assert_eq!(
        e.voxels,
        Evoxels::from_many(
            resolution,
            Vol::from_fn(GridAab::for_block(resolution), |point| {
                let point = (point.lower_bounds() + offset).cast::<f32>();
                Evoxel {
                    color: Rgba::new(point.x, point.y, point.z, 1.0),
                    emission: Rgb::ZERO,
                    selectable: true,
                    collision: BlockCollision::Hard,
                }
            })
        )
    );
}

/// Fuzzer-derived regression test for numeric overflow
#[test]
fn recur_offset_negative_overflow() {
    let mut universe = Universe::new();
    let space = Space::builder(GridAab::from_lower_upper(
        [1743229108, 939544399, -2147463345],
        [1743229109, 939544400, -2147461505],
    ))
    .build();
    let block_at_offset = Block::from_primitive(Primitive::Recur {
        offset: GridPoint::new(-414232629, -2147483648, -13697025),
        resolution: R128,
        space: universe.insert_anonymous(space),
    });

    let e = evaluate(&block_at_offset);
    assert!(!e.visible());
}

#[test]
fn recur_animation_hint_propagation() {
    let mut universe = Universe::new();
    let voxel = Block::builder()
        .color(Rgba::WHITE)
        .animation_hint(AnimationHint {
            redefinition: AnimationChange::None,
            replacement: AnimationChange::ColorSameCategory,
        })
        .build();
    let block = Block::builder()
        .voxels_fn(R4, |cube| if cube == Cube::ORIGIN { &voxel } else { &AIR })
        .unwrap()
        .build_into(&mut universe);

    let e = evaluate(&block);
    assert_eq!(
        e.attributes.animation_hint,
        AnimationHint {
            // Note that what was replacement becomes redefinition,
            // because replacement in the space is redefinition in the block
            // derived from the space.
            redefinition: AnimationChange::ColorSameCategory,
            replacement: AnimationChange::None,
        }
    );
}

#[test]
fn indirect_equivalence() {
    let resolution = R4;
    let mut universe = Universe::new();
    let mut space = Space::empty(GridAab::for_block(resolution));
    // TODO: block::Builder should support constructing indirects (by default, even)
    // and we can use the more concise version
    space
        .fill(space.bounds(), |point| {
            let point = point.lower_bounds().cast::<f32>();
            Some(Block::from(Rgba::new(point.x, point.y, point.z, 1.0)))
        })
        .unwrap();
    let space_handle = universe.insert_anonymous(space);
    let block = Block::builder()
        .voxels_handle(resolution, space_handle)
        .build();

    let eval_bare = evaluate(&block);
    let block_def_handle = universe.insert_anonymous(BlockDef::new(block));
    let indirect_block = Block::from(block_def_handle);
    let eval_def = evaluate(&indirect_block);

    assert_eq!(
        eval_def,
        EvaluatedBlock {
            cost: eval_def.cost,
            block: indirect_block.clone(),
            ..eval_bare
        }
    );
    assert_eq!(
        eval_def.cost,
        Cost {
            components: 1,
            voxels: 0, // zero because the voxels were _already_ evaluated
            recursion: 0
        }
    );

    // While we're here, test that we benefit from the `BlockDef`’s evaluation cache by
    // getting identical `EvKey`s out of separate evaluations. (`EvKey` compares voxels
    // by pointer insteed of value, as long as the resolution is not 1.)
    assert_eq!(
        EvKey::new(&eval_def),
        EvKey::new(&evaluate(&indirect_block))
    );
}

/// Check that the derived values from a `BlockDef` evaluation are getting propagated
/// out of the cache.
#[test]
fn indirect_has_derived_value_cache_internally() {
    let mut universe = Universe::new();
    let block = Block::from(universe.insert_anonymous(BlockDef::new(AIR)));
    assert!(
        block
            .evaluate_impl(&block::EvalFilter::new(universe.read_ticket()))
            .unwrap()
            .has_derived()
    );
}

/// Fuzz-discovered test case for panic during evaluation,
/// in `raytracer::apply_transmittance`.
#[test]
fn color_evaluation_regression_1() {
    let block = Block::builder()
        .color(Rgba::new(1e28, 1e28, 1e28, 1.0))
        // Modifier matters because it causes the block to become voxels
        .modifier(Modifier::Move(modifier::Move::new(Face6::NX, 0, 0)))
        .build();
    evaluate(&block);
}

/// Fuzz-discovered test case for a NaN sneaking in to a color.
#[test]
fn color_evaluation_regression_2() {
    let block = AIR.with_modifier(block::Composite::new(
        Block::builder()
            .color(Rgba::new(0.0, 0.0, 9.1835e-41, 0.0))
            .light_emission(Rgb::new(f32::INFINITY, 1.5783e-41, 0.0))
            .build(),
        block::CompositeOperator::Over,
    ));
    evaluate(&block).consistency_check();
}

//! Tests for [`Block`] as a whole.
//! The following modules also have their own tests:
//!
//! * [`super::attributes`]
//! * [`super::builder`]
//! * [`super::modifier`]

#![allow(clippy::bool_assert_comparison)]

use pretty_assertions::assert_eq;

use crate::block::{
    self, modifier, AnimationChange, AnimationHint, Atom, Block, BlockAttributes, BlockChange,
    BlockCollision, BlockDef, BlockDefTransaction, EvalBlockError, Evoxel, Evoxels, Modifier,
    Primitive, Resolution::*, AIR, AIR_EVALUATED,
};
use crate::content::make_some_blocks;
use crate::listen::{self, NullListener, Sink};
use crate::math::{
    notnan, Cube, Face6, FaceMap, GridAab, GridPoint, GridRotation, GridVector, NotNan,
    OpacityCategory, Rgb, Rgba, Vol,
};
use crate::space::{Space, SpaceTransaction};
use crate::time::DeadlineNt;
use crate::universe::{HandleError, Name, Universe};

/// Just install a listener and discard the [`EvaluatedBlock`].
///
/// TODO: Expand this to, or otherwise create, a helper which checks that the evaluation result
/// changes only with notification.
fn listen(
    block: &Block,
    listener: impl listen::Listener<BlockChange> + 'static,
) -> Result<(), EvalBlockError> {
    block
        .evaluate2(&block::EvalFilter {
            skip_eval: true,
            listener: Some(listener.erased()),
            budget: Default::default(),
        })
        .map(|_| ())
}

#[test]
fn block_is_approximately_a_pointer() {
    let block_size = size_of::<Block>();
    let ptr_size = size_of::<*const ()>();
    assert!(
        ptr_size < block_size && block_size <= 2 * ptr_size,
        "unexpected size: {block_size}",
    );
}

#[test]
fn block_static_eq_to_non_static() {
    let foo = AIR;
    let bar = Block::from_primitive(foo.primitive().clone());
    assert_eq!(foo, bar);
}

#[test]
fn block_debug_air() {
    assert_eq!(&format!("{:?}", &AIR), "Block { primitive: Air }");
}

#[test]
fn block_debug_with_modifiers() {
    assert_eq!(
        &format!(
            "{:?}",
            &Block::builder()
                .color(Rgba::new(1.0, 0.5, 0.0, 1.0))
                // TODO: When we have more modifiers, pick a different one, that isn't
                // essentially irrelevant to Primitive::Atom
                .modifier(Modifier::Rotate(GridRotation::Rxyz))
                .build()
        ),
        "Block { \
            primitive: Atom { \
                color: Rgba(1.0, 0.5, 0.0, 1.0), \
                collision: Hard }, \
            modifiers: [Rotate(Rxyz)] \
        }"
    );
}

mod eval {
    use crate::block::{Cost, EvKey, EvaluatedBlock, VoxelOpacityMask};

    use super::{assert_eq, *};

    #[test]
    fn air_self_consistent() {
        AIR.evaluate().unwrap().consistency_check();
    }

    #[test]
    fn air_consistent_with_constant() {
        assert_eq!(AIR.evaluate().unwrap(), AIR_EVALUATED);
        assert_eq!(
            Block::from(Primitive::Air).evaluate().unwrap(),
            AIR_EVALUATED
        );
    }

    #[test]
    fn air_consistent_with_evoxel_air() {
        assert_eq!(
            AIR.evaluate().unwrap().voxels.single_voxel().unwrap(),
            Evoxel::AIR
        );
    }

    #[test]
    fn air_in_recursive_block() {
        let mut universe = Universe::new();
        let block = Block::builder()
            .voxels_fn(R1, |_| AIR)
            .unwrap()
            .build_into(&mut universe);
        assert_eq!(
            block.evaluate().unwrap().voxels.single_voxel().unwrap(),
            Evoxel::AIR
        );
    }

    #[test]
    fn opaque_atom_and_attributes() {
        let color = Rgba::new(1.0, 2.0, 3.0, 1.0);
        let attributes = BlockAttributes {
            display_name: arcstr::literal!("hello world"),
            selectable: false,
            ..BlockAttributes::default()
        };
        let block = Block::from(Atom {
            attributes: attributes.clone(),
            color,
            emission: Rgb::ONE,
            collision: BlockCollision::None,
        });
        let e = block.evaluate().unwrap();
        assert_eq!(e.attributes, attributes);
        assert_eq!(e.color(), color);
        assert_eq!(e.face_colors(), FaceMap::repeat(color));
        assert_eq!(e.light_emission(), Rgb::ONE);
        assert_eq!(
            e.voxels,
            Evoxels::from_one(Evoxel {
                color,
                emission: Rgb::ONE,
                selectable: false,
                collision: BlockCollision::None,
            })
        );
        assert_eq!(e.resolution(), R1);
        assert_eq!(e.opaque(), FaceMap::repeat(true));
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
        let e = block.evaluate().unwrap();
        assert_eq!(e.color(), color);
        assert_eq!(e.face_colors(), FaceMap::repeat(color));
        assert_eq!(e.light_emission(), Rgb::ZERO);
        assert!(e.voxels.single_voxel().is_some());
        assert_eq!(e.opaque(), FaceMap::repeat(false));
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
        let e = block.evaluate().unwrap();
        assert_eq!(e.color(), Rgba::TRANSPARENT);
        assert_eq!(e.face_colors(), FaceMap::repeat(Rgba::TRANSPARENT));
        assert_eq!(e.light_emission(), emissive_color);
        assert!(e.voxels.single_voxel().is_some());
        assert_eq!(e.opaque(), FaceMap::repeat(false));
        assert_eq!(e.visible(), true);
        assert_eq!(
            *e.voxel_opacity_mask(),
            VoxelOpacityMask::new_raw(R1, Vol::from_element(OpacityCategory::Partial))
        )
    }

    #[test]
    fn invisible_atom() {
        let block = color_block!(Rgba::TRANSPARENT);
        let e = block.evaluate().unwrap();
        assert_eq!(e.color(), Rgba::TRANSPARENT);
        assert_eq!(e.face_colors(), FaceMap::repeat(Rgba::TRANSPARENT));
        assert!(e.voxels.single_voxel().is_some());
        assert_eq!(e.opaque(), FaceMap::repeat(false));
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

        let e = block.evaluate().unwrap();
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
        assert_eq!(e.opaque(), FaceMap::repeat(true));
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
                components: 1,
                voxels: 8,
                recursion: 0
            }
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
                    NotNan::new(alpha).unwrap()
                } else {
                    notnan!(1.0)
                }))
            })
            .unwrap()
            .build_into(&mut universe);

        let e = block.evaluate().unwrap();
        // Transparency is (currently) computed by an orthographic view through all six
        // faces, and only two out of six faces in this test block don't fully cover
        // the light paths with opaque surfaces.
        assert_eq!(
            e.color(),
            voxel_color.with_alpha(
                NotNan::new(1.0 - (alpha / (f32::from(resolution).powi(2) * 3.0))).unwrap()
            )
        );
        // This is the sum of the transparency of one voxel on one of the six faces
        let one_face_transparency = voxel_color
            .with_alpha(NotNan::new(1.0 - (alpha / f32::from(resolution).powi(2))).unwrap());
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
        let colors = [c1.with_alpha_one(), c2.with_alpha(notnan!(0.5))];
        let block = Block::builder()
            .voxels_fn(R2, |cube| Block::from(colors[cube.y as usize]))
            .unwrap()
            .build_into(&mut universe);
        let surface_area: f32 = 4. * 6.;

        let e = block.evaluate().unwrap();

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

        let e = block.evaluate().unwrap();
        assert_eq!(
            e.color(),
            Rgba::new(0.0, 0.0, 0.0, 1.0 / f32::from(resolution).powi(2))
        );
        assert_eq!(e.resolution(), resolution);
        assert_eq!(e.opaque(), FaceMap::repeat(false));
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
            .fill_uniform(space.bounds(), &color_block!(Rgba::WHITE))
            .unwrap();
        let space_handle = universe.insert_anonymous(space);
        let block = Block::builder()
            .voxels_handle(resolution, space_handle.clone())
            .build();

        let e = block.evaluate().unwrap();
        // of 6 faces, 2 are opaque and 2 are half-transparent, thus there are 8 opaque half-faces.
        assert_eq!(e.color(), Rgba::new(1.0, 1.0, 1.0, 8. / 12.));
        assert_eq!(e.resolution(), resolution);
        assert_eq!(e.opaque(), FaceMap::repeat(false).with(Face6::NX, true));
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
            attributes: BlockAttributes::default(),
            offset: offset.to_point(),
            resolution,
            space: space_handle.clone(),
        });

        let e = block_at_offset.evaluate().unwrap();
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
            attributes: BlockAttributes::default(),
            offset: GridPoint::new(-414232629, -2147483648, -13697025),
            resolution: R128,
            space: universe.insert_anonymous(space),
        });

        let e = block_at_offset.evaluate().unwrap();
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

        let e = block.evaluate().unwrap();
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
        // TODO: BlockBuilder should support constructing indirects (by default, even)
        // and we can use the more concise version
        space
            .fill(space.bounds(), |point| {
                let point = point.lower_bounds().cast::<f32>();
                Some(Block::from(Rgba::new(point.x, point.y, point.z, 1.0)))
            })
            .unwrap();
        let space_handle = universe.insert_anonymous(space);
        let block = Block::builder()
            .voxels_handle(resolution, space_handle.clone())
            .build();

        let eval_bare = block.evaluate().unwrap();
        let block_def_handle = universe.insert_anonymous(BlockDef::new(block));
        let indirect_block = Block::from(block_def_handle);
        let eval_def = indirect_block.evaluate().unwrap();

        assert_eq!(
            eval_def,
            EvaluatedBlock {
                cost: eval_def.cost,
                block: indirect_block.clone(),
                ..eval_bare.clone()
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
            EvKey::new(&indirect_block.evaluate().unwrap())
        );
    }

    /// Check that the derived values from a `BlockDef` evaluation are getting propagated
    /// out of the cache.
    #[test]
    fn indirect_has_derived_value_cache_internally() {
        let mut universe = Universe::new();
        let block = Block::from(universe.insert_anonymous(BlockDef::new(AIR)));
        assert!(block
            .evaluate_impl(&block::EvalFilter::default())
            .unwrap()
            .has_derived());
    }

    /// Fuzz-discovered test case for panic during evaluation,
    /// in `raytracer::apply_transmittance`.
    #[test]
    fn color_evaluation_regression() {
        let block = Block::builder()
            .color(Rgba::new(1e28, 1e28, 1e28, 1e28))
            // Modifier matters because it causes the block to become voxels
            .modifier(Modifier::Move(modifier::Move::new(Face6::NX, 0, 0)))
            .build();
        block.evaluate().unwrap();
    }
}

#[test]
fn listen_atom() {
    let block = color_block!(Rgba::WHITE);
    let sink = Sink::new();
    listen(&block, sink.listener()).unwrap();
    assert_eq!(sink.drain(), vec![]);
    // No notifications are possible, so nothing more to test.
}

#[test]
fn listen_indirect_atom() {
    let mut universe = Universe::new();
    let block_def_handle = universe.insert_anonymous(BlockDef::new(color_block!(Rgba::WHITE)));
    let indirect = Block::from(block_def_handle.clone());
    let sink = Sink::new();
    listen(&indirect, sink.listener()).unwrap();
    assert_eq!(sink.drain(), vec![]);

    // Now mutate it and we should see a notification.
    block_def_handle
        .execute(&BlockDefTransaction::overwrite(color_block!(Rgba::BLACK)))
        .unwrap();
    assert_eq!(sink.drain().len(), 1);
}

/// Testing double indirection not because it's a case we expect to use routinely,
/// but because it exercises the generality of the notification and cache mechanisms.
/// Specifically, `block_def_handle1` is updated by transaction, but `block_def_handle2`
/// is updated by universe stepping since it was not directly mutated.
#[test]
fn listen_indirect_double() {
    let mut universe = Universe::new();
    let block_def_handle1 = universe.insert_anonymous(BlockDef::new(color_block!(Rgba::WHITE)));
    let indirect1 = Block::from(block_def_handle1.clone());
    let block_def_handle2 = universe.insert_anonymous(BlockDef::new(indirect1.clone()));
    let indirect2 = Block::from(block_def_handle2.clone());
    let sink1 = Sink::new();
    let sink2 = Sink::new();
    listen(&indirect1, sink1.listener()).unwrap();
    listen(&indirect2, sink2.listener()).unwrap();
    assert_eq!(sink1.drain(), vec![]);
    assert_eq!(sink2.drain(), vec![]);

    // Mutate the first BlockDef and we should see a notification for it alone.
    block_def_handle1
        .execute(&BlockDefTransaction::overwrite(color_block!(Rgba::BLACK)))
        .unwrap();
    assert_eq!([sink1.drain().len(), sink2.drain().len()], [1, 0]);

    // Step and get the other notification.
    universe.step(false, DeadlineNt::Whenever);
    assert_eq!([sink1.drain().len(), sink2.drain().len()], [0, 1]);

    // Remove block_def_handle1 from the contents of block_def_handle2...
    block_def_handle2
        .execute(&BlockDefTransaction::overwrite(color_block!(Rgba::BLACK)))
        .unwrap();
    assert_eq!(sink2.drain().len(), 1);
    // ...and then block_def_handle1's changes should NOT be forwarded.
    block_def_handle1
        .execute(&BlockDefTransaction::overwrite(color_block!(Rgba::WHITE)))
        .unwrap();
    assert_eq!(sink2.drain(), vec![]);
}

/// Test that changes to a `Space` propagate to block listeners.
#[test]
fn listen_recur() {
    let mut universe = Universe::new();
    let [block_0, block_1] = make_some_blocks();
    let space_handle = universe.insert_anonymous(Space::empty_positive(2, 1, 1));
    let block = Block::builder()
        .voxels_handle(R1, space_handle.clone())
        .build();
    let sink = Sink::new();
    listen(&block, sink.listener()).unwrap();
    assert_eq!(sink.drain(), vec![]);

    // Now mutate the space and we should see a notification.
    space_handle
        .execute(&SpaceTransaction::set_cube([0, 0, 0], None, Some(block_0)))
        .unwrap();
    assert_eq!(sink.drain().len(), 1);

    // TODO: Also test that we don't propagate lighting changes

    // A mutation out of bounds should not trigger a notification
    space_handle
        .execute(&SpaceTransaction::set_cube([1, 0, 0], None, Some(block_1)))
        .unwrap();
    assert_eq!(sink.drain(), vec![]);
}

#[test]
fn overflow_evaluate() {
    // The primitive counts as a component.
    let too_many_modifiers: u32 = block::Budget::default().components;

    let mut block = AIR;
    block
        .modifiers_mut()
        .extend((0..too_many_modifiers).map(|_| Modifier::Rotate(GridRotation::CLOCKWISE)));
    assert_eq!(
        block.evaluate(),
        Err(EvalBlockError {
            block,
            budget: block::Budget::default().to_cost(),
            used: block::Cost {
                components: too_many_modifiers,
                voxels: 0,
                recursion: 0
            },
            kind: block::ErrorKind::BudgetExceeded,
        })
    );
}

#[test]
fn self_referential_evaluate() {
    let mut universe = Universe::new();
    let block = self_referential_block(&mut universe);
    assert_eq!(
        block.evaluate(),
        Err(EvalBlockError {
            block,
            budget: block::Budget::default().to_cost(),
            used: block::Cost {
                components: 1,
                voxels: 0,
                recursion: 0
            },
            kind: block::ErrorKind::Handle(HandleError::InUse(Name::Anonym(0)))
        })
    );
}

#[test]
fn self_referential_listen() {
    let mut universe = Universe::new();
    let block = self_referential_block(&mut universe);
    // This should *not* produce an error, because BlockDef manages its own notifier and we want
    // it to be possible to listen to a currently-erring BlockDef.
    assert_eq!(listen(&block, NullListener), Ok(()));
}

/// Helper for overflow_ tests
fn self_referential_block(universe: &mut Universe) -> Block {
    let block_def = universe.insert_anonymous(BlockDef::new(AIR));
    let indirect = Block::from(block_def.clone());
    block_def
        .execute(&BlockDefTransaction::overwrite(indirect.clone()))
        .unwrap();
    indirect
}

mod txn {
    use super::*;
    use crate::transaction::{Merge, TransactionTester};
    use pretty_assertions::assert_eq;

    #[test]
    fn causes_notification() {
        // We're using a Primitive::Indirect in addition to the BlockDef to test a more
        // realistic scenario
        let [b1, b2] = make_some_blocks();
        let mut universe = Universe::new();
        let block_def_handle = universe.insert_anonymous(BlockDef::new(b1));
        let indirect = Block::from(block_def_handle.clone());
        let sink = Sink::new();
        listen(&indirect, sink.listener()).unwrap();
        assert_eq!(sink.drain(), vec![]);

        // Now mutate it and we should see a notification.
        block_def_handle
            .execute(&BlockDefTransaction::overwrite(b2))
            .unwrap();
        assert_eq!(sink.drain().len(), 1);
    }

    #[test]
    fn merge_allows_same_new() {
        let [new] = make_some_blocks();
        let t1 = BlockDefTransaction::overwrite(new);
        assert_eq!(t1.clone().merge(t1.clone()), Ok(t1));
    }

    #[test]
    fn merge_rejects_different_new() {
        let [new1, new2] = make_some_blocks();
        let t1 = BlockDefTransaction::overwrite(new1);
        let t2 = BlockDefTransaction::overwrite(new2);
        t1.merge(t2).unwrap_err();
    }

    #[test]
    fn merge_rejects_different_old() {
        let [old1, old2] = make_some_blocks();
        let t1 = BlockDefTransaction::expect(old1);
        let t2 = BlockDefTransaction::expect(old2);
        t1.merge(t2).unwrap_err();
    }

    #[test]
    fn merge_allows_same_old() {
        let [old, new] = make_some_blocks();
        let t1 = BlockDefTransaction::replace(old.clone(), new.clone());
        let t2 = BlockDefTransaction::replace(old.clone(), new.clone());
        assert_eq!(t1.clone(), t1.clone().merge(t2).unwrap());
    }

    #[test]
    fn systematic() {
        let [b1, b2, b3] = make_some_blocks();
        TransactionTester::new()
            .transaction(BlockDefTransaction::default(), |_, _| Ok(()))
            .transaction(
                BlockDefTransaction::replace(b1.clone(), b2.clone()),
                |before, after| {
                    if *before.block() != b1 {
                        return Err("did not assert b1".into());
                    }
                    if *after.block() != b2 {
                        return Err("did not set b2".into());
                    }
                    Ok(())
                },
            )
            .transaction(
                BlockDefTransaction::replace(b1.clone(), b3.clone()),
                |before, after| {
                    if *before.block() != b1 {
                        return Err("did not assert b1".into());
                    }
                    if *after.block() != b3 {
                        return Err("did not set b3".into());
                    }
                    Ok(())
                },
            )
            .transaction(BlockDefTransaction::overwrite(b2.clone()), |_, after| {
                if *after.block() != b2 {
                    return Err("did not set b2".into());
                }
                Ok(())
            })
            .transaction(BlockDefTransaction::expect(b2.clone()), |before, _| {
                if *before.block() != b2 {
                    return Err("did not assert b2".into());
                }
                Ok(())
            })
            .transaction(BlockDefTransaction::expect(b1.clone()), |before, _| {
                if *before.block() != b1 {
                    return Err("did not assert b1".into());
                }
                Ok(())
            })
            .target(|| BlockDef::new(AIR))
            .target(|| BlockDef::new(b1.clone()))
            .target(|| BlockDef::new(b2.clone()))
            .test();
    }
}

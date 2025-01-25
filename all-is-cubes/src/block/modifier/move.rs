use crate::block::TickAction;
use crate::block::{
    self, Block, BlockAttributes, Evoxel, Evoxels, MinEval, Modifier, Resolution::R16, AIR,
};
use crate::math::{Face6, GridAab, GridCoordinate, GridRotation, GridVector, Vol};
use crate::op::Operation;
use crate::time;
use crate::universe;

/// Data for [`Modifier::Move`]; displaces the block out of the grid, cropping it.
/// A pair of `Move`s can depict a block moving between two cubes.
///
/// # Animation
///
/// * If the `distance` is zero then the modifier will remove itself, if possible,
///   on the next tick.
/// * If the `distance` and `velocity` are such that the block is out of view and will
///   never start being in view, the block will be replaced with [`AIR`].
///
/// (TODO: Define the conditions for “if possible”.)
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct Move {
    /// The direction in which the block is displaced.
    pub direction: Face6,

    /// The distance, in 1/256ths, by which it is displaced.
    pub distance: u16,

    /// The amount by which `self.distance` is changing every time
    /// `self.schedule` fires.
    pub velocity: i16,

    /// When to apply the velocity.
    ///
    /// If `self.velocity` is zero, this is ignored.
    pub schedule: time::Schedule,
}

impl Move {
    /// TODO: make a cleaner, less internals-ish constructor
    pub fn new(direction: Face6, distance: u16, velocity: i16) -> Self {
        Self {
            direction,
            distance,
            velocity,
            schedule: time::Schedule::EVERY_TICK,
        }
    }

    /// Create a pair of [`Move`]s to displace a block.
    /// The first goes on the block being moved, and the second on the air
    /// it's moving into.
    //---
    // TODO: This is going to need to change again in order to support
    // moving one block in and another out at the same time.
    //
    // TODO: This would be a good candidate for an example, once doctests are not so slow.
    #[must_use]
    pub fn into_paired(self) -> [Self; 2] {
        let complement = self.complement();
        [self, complement]
    }

    /// Calculate the modifier which should be paired with this one, and located at the adjacent
    /// cube pointed to by [`Self::direction`], to produce a complete moving block across two
    /// cubes.
    #[must_use]
    pub fn complement(&self) -> Self {
        Move {
            direction: self.direction.opposite(),
            distance: 256 - self.distance,
            velocity: -self.velocity,
            schedule: self.schedule,
        }
    }

    /// Rotate the movement direction as specified.
    #[must_use]
    pub fn rotate(mut self, rotation: GridRotation) -> Self {
        self.direction = rotation.transform(self.direction);
        self
    }

    /// Note that `Modifier::Move` does some preprocessing to keep this simpler.
    pub(super) fn evaluate(
        &self,
        block: &Block,
        this_modifier_index: usize,
        mut input: MinEval,
        filter: &block::EvalFilter,
    ) -> Result<MinEval, block::InEvalError> {
        let Move {
            direction,
            distance,
            velocity,
            schedule,
        } = *self;

        // Apply Quote to ensure that the block's own `tick_action` and other effects
        // don't interfere with movement or cause duplication.
        // (In the future we may want a more nuanced policy that allows internal changes,
        // but that will involve some sort of predicate and transformation on tick actions.)
        input = block::Quote::default().evaluate(input, filter)?;

        // TODO: short-circuit case when distance is 0

        let (input_attributes, input_voxels) = input.into_parts();
        let (original_bounds, effective_resolution) = match input_voxels.single_voxel() {
            None => (input_voxels.bounds(), input_voxels.resolution()),
            // Treat atom blocks as having a resolution of 16. TODO: Improve on this hardcoded constant
            Some(_) => (GridAab::for_block(R16), R16),
        };

        // For now, our strategy is to work in units of the block's resolution.
        // TODO: Generalize to being able to increase resolution to a chosen minimum.
        let distance_in_res =
            GridCoordinate::from(distance) * GridCoordinate::from(effective_resolution) / 256;
        let translation_in_res = direction.normal_vector() * distance_in_res;

        // This will be None if the displacement puts the block entirely out of view.
        let displaced_bounds: Option<GridAab> = original_bounds
            .translate(translation_in_res)
            .intersection_cubes(GridAab::for_block(effective_resolution));

        let animation_op: Option<Operation> = if displaced_bounds.is_none() && velocity >= 0 {
            // Displaced to invisibility; turn into just plain air.
            Some(Operation::Become(AIR))
        } else if translation_in_res == GridVector::zero() && velocity == 0
            || distance == 0 && velocity < 0
        {
            // Either a stationary displacement which is invisible, or an animated one which has finished its work.
            assert!(
                matches!(&block.modifiers()[this_modifier_index], Modifier::Move(m) if m == self)
            );
            let mut new_block = block.clone();
            new_block.modifiers_mut().remove(this_modifier_index); // TODO: What if other modifiers want to do things?
            Some(Operation::Become(new_block))
        } else if velocity != 0 {
            // Movement in progress.
            assert!(
                matches!(&block.modifiers()[this_modifier_index], Modifier::Move(m) if m == self)
            );
            let mut new_block = block.clone();
            {
                let modifiers = new_block.modifiers_mut();

                // Update the distance for the next step.
                if let Modifier::Move(Move {
                    distance, velocity, ..
                }) = &mut modifiers[this_modifier_index]
                {
                    *distance = i32::from(*distance)
                            .saturating_add(i32::from(*velocity))
                            .clamp(0, i32::from(u16::MAX))
                            .try_into()
                            .unwrap(/* clamped to range */);
                }

                // Do not include any other modifiers.
                // The modifiers themselves are responsible for doing so.
                modifiers.truncate(this_modifier_index + 1);
            }
            Some(Operation::Become(new_block))
        } else {
            // Stationary displacement; take no action
            None
        };

        let animation_hint = if animation_op.is_some() {
            input_attributes.animation_hint
                | block::AnimationHint::replacement(block::AnimationChange::Shape)
        } else {
            input_attributes.animation_hint
        };

        let attributes = BlockAttributes {
            animation_hint,
            tick_action: animation_op.map(|operation| TickAction {
                operation,
                schedule,
            }),
            ..input_attributes
        };

        Ok(match displaced_bounds {
            Some(displaced_bounds) => {
                block::Budget::decrement_voxels(
                    &filter.budget,
                    displaced_bounds.volume().unwrap(),
                )?;

                let displaced_voxels = match input_voxels.single_voxel() {
                    None => {
                        let voxels = input_voxels.as_vol_ref();
                        Evoxels::from_many(
                            effective_resolution,
                            Vol::from_fn(displaced_bounds, |cube| {
                                voxels[cube - translation_in_res]
                            }),
                        )
                    }
                    Some(voxel) => {
                        // Input block is a solid color; synthesize voxels.
                        // TODO: Also synthesize if the resolution is merely low
                        // compared to the velocity.
                        Evoxels::from_many(
                            effective_resolution,
                            Vol::from_fn(displaced_bounds, |_| voxel),
                        )
                    }
                };
                MinEval::new(attributes, displaced_voxels)
            }
            None => MinEval::new(attributes, Evoxels::from_one(Evoxel::AIR)),
        })
    }
}

impl From<Move> for Modifier {
    fn from(value: Move) -> Self {
        Modifier::Move(value)
    }
}

impl universe::VisitHandles for Move {
    fn visit_handles(&self, _visitor: &mut dyn universe::HandleVisitor) {
        let Move {
            direction: _,
            distance: _,
            velocity: _,
            schedule: _,
        } = self;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Composite, EvaluatedBlock, Resolution::*, VoxelOpacityMask};
    use crate::content::make_some_blocks;
    use crate::math::{rgba_const, zo32, FaceMap, GridPoint, OpacityCategory, Rgb, Rgba};
    use crate::space::Space;
    use crate::universe::Universe;
    use pretty_assertions::assert_eq;

    #[test]
    fn move_atom_block_evaluation() {
        let color = rgba_const!(1.0, 0.0, 0.0, 1.0);
        let original = Block::from(color);
        let moved = original.clone().with_modifier(Move {
            direction: Face6::PY,
            distance: 128, // distance 1/2 block × scale factor of 256
            velocity: 0,
            schedule: time::Schedule::EVERY_TICK,
        });

        let expected_bounds = GridAab::from_lower_size([0, 8, 0], [16, 8, 16]);

        let ev_original = original.evaluate().unwrap();
        assert_eq!(
            moved.evaluate().unwrap(),
            EvaluatedBlock {
                block: moved,
                attributes: ev_original.attributes.clone(),
                voxels: Evoxels::from_many(
                    R16,
                    Vol::repeat(expected_bounds, Evoxel::from_block(&ev_original))
                ),
                cost: block::Cost {
                    components: ev_original.cost.components + 1,
                    voxels: expected_bounds.volume_f64() as u32,
                    recursion: 0
                },
                derived: block::Derived {
                    color: color.to_rgb().with_alpha(zo32(2. / 3.)),
                    face_colors: FaceMap {
                        nx: color.to_rgb().with_alpha(zo32(0.5)),
                        ny: color.to_rgb().with_alpha(zo32(1.0)),
                        nz: color.to_rgb().with_alpha(zo32(0.5)),
                        px: color.to_rgb().with_alpha(zo32(0.5)),
                        py: color.to_rgb().with_alpha(zo32(1.0)),
                        pz: color.to_rgb().with_alpha(zo32(0.5)),
                    },
                    light_emission: Rgb::ZERO,
                    opaque: FaceMap::splat(false).with(Face6::PY, true),
                    visible: true,
                    uniform_collision: None,
                    voxel_opacity_mask: VoxelOpacityMask::new_raw(
                        R16,
                        Vol::repeat(expected_bounds, OpacityCategory::Opaque)
                    ),
                }
            }
        );
    }

    #[test]
    fn move_voxel_block_evaluation() {
        let mut universe = Universe::new();
        let resolution = R2;
        let color = rgba_const!(1.0, 0.0, 0.0, 1.0);
        let original = Block::builder()
            .voxels_fn(resolution, |_| Block::from(color))
            .unwrap()
            .build_into(&mut universe);

        let moved = original.clone().with_modifier(Move {
            direction: Face6::PY,
            distance: 128, // distance 1/2 block × scale factor of 256
            velocity: 0,
            schedule: time::Schedule::EVERY_TICK,
        });

        let expected_bounds = GridAab::from_lower_size([0, 1, 0], [2, 1, 2]);

        let ev_original = original.evaluate().unwrap();
        assert_eq!(
            moved.evaluate().unwrap(),
            EvaluatedBlock {
                block: moved,
                attributes: ev_original.attributes.clone(),
                cost: block::Cost {
                    components: ev_original.cost.components + 1,
                    voxels: 2u32.pow(3) * 3 / 2, // original recur + 1/2 block of Move
                    recursion: 0
                },
                voxels: Evoxels::from_many(
                    resolution,
                    Vol::repeat(expected_bounds, Evoxel::from_block(&ev_original))
                ),
                derived: block::Derived {
                    color: color.to_rgb().with_alpha(zo32(2. / 3.)),
                    face_colors: FaceMap {
                        nx: color.to_rgb().with_alpha(zo32(0.5)),
                        ny: color.to_rgb().with_alpha(zo32(1.0)),
                        nz: color.to_rgb().with_alpha(zo32(0.5)),
                        px: color.to_rgb().with_alpha(zo32(0.5)),
                        py: color.to_rgb().with_alpha(zo32(1.0)),
                        pz: color.to_rgb().with_alpha(zo32(0.5)),
                    },
                    light_emission: Rgb::ZERO,
                    opaque: FaceMap::splat(false).with(Face6::PY, true),
                    visible: true,
                    uniform_collision: None,
                    voxel_opacity_mask: VoxelOpacityMask::new_raw(
                        resolution,
                        Vol::repeat(expected_bounds, OpacityCategory::Opaque)
                    ),
                }
            }
        );
    }

    /// [`Modifier::Move`] incorporates [`Modifier::Quote`] to ensure that no conflicting
    /// effects happen.
    #[test]
    fn move_also_quotes() {
        let original = Block::builder()
            .color(Rgba::WHITE)
            .tick_action(Some(TickAction::from(Operation::Become(AIR))))
            .build();
        let moved = original.with_modifier(Move {
            direction: Face6::PY,
            distance: 128,
            velocity: 0,
            schedule: time::Schedule::EVERY_TICK,
        });

        assert_eq!(moved.evaluate().unwrap().attributes.tick_action, None);
    }

    /// Set up a `Modifier::Move`, let it run, and then allow assertions to be made about the result.
    fn move_block_test(direction: Face6, velocity: i16, checker: impl FnOnce(&Space, &Block)) {
        let [block] = make_some_blocks();
        let mut space = Space::empty(GridAab::from_lower_upper([-1, -1, -1], [2, 2, 2]));
        let [move_out, move_in] = Move::new(direction, 0, velocity).into_paired();
        space
            .set([0, 0, 0], block.clone().with_modifier(move_out))
            .unwrap();
        space
            .set(
                GridPoint::origin() + direction.normal_vector(),
                block.clone().with_modifier(move_in),
            )
            .unwrap();
        let mut universe = Universe::new();
        let space = universe.insert_anonymous(space);
        // TODO: We need a "step until idle" function, or for the UniverseStepInfo to convey how many blocks were updated / are waiting
        // TODO: Some tests will want to look at the partial results
        for _ in 0..257 {
            universe.step(false, time::DeadlineNt::Whenever);
        }
        checker(&space.read().unwrap(), &block);
    }

    #[test]
    fn velocity_zero() {
        move_block_test(Face6::PX, 0, |space, block| {
            assert_eq!(&space[[0, 0, 0]], block);
            assert_eq!(&space[[1, 0, 0]], &AIR);
        });
    }

    #[test]
    fn velocity_slow() {
        move_block_test(Face6::PX, 1, |space, block| {
            assert_eq!(&space[[0, 0, 0]], &AIR);
            assert_eq!(&space[[1, 0, 0]], block);
        });
    }

    #[test]
    fn velocity_whole_cube_in_one_tick() {
        move_block_test(Face6::PX, 256, |space, block| {
            assert_eq!(&space[[0, 0, 0]], &AIR);
            assert_eq!(&space[[1, 0, 0]], block);
        });
    }

    /// Check the behavior of a `Move` modifier under a `Rotate` modifier.
    /// In particular, we want to make sure the outcome doesn’t end up doubly-rotated.
    #[test]
    fn move_inside_rotation() {
        let [base] = make_some_blocks();
        const R: Modifier = Modifier::Rotate(GridRotation::CLOCKWISE);

        let block = base
            .clone()
            .with_modifier(Move {
                direction: Face6::PX,
                distance: 10,
                velocity: 10,
                schedule: time::Schedule::EVERY_TICK,
            })
            .with_modifier(R);

        let expected_after_tick = base
            .clone()
            .with_modifier(Move {
                direction: Face6::PX,
                distance: 20,
                velocity: 10,
                schedule: time::Schedule::EVERY_TICK,
            })
            .with_modifier(R);

        assert_eq!(
            block.evaluate().unwrap().attributes.tick_action,
            Some(TickAction::from(Operation::Become(expected_after_tick)))
        );
    }

    /// Test [`Move`] acting within another modifier ([`Composite`]).
    #[test]
    fn move_inside_composite_destination() {
        let [base, extra] = make_some_blocks();
        let composite = Composite::new(extra, block::CompositeOperator::Over);

        let block = base
            .clone()
            .with_modifier(Move {
                direction: Face6::PX,
                distance: 10,
                velocity: 10,
                schedule: time::Schedule::EVERY_TICK,
            })
            .with_modifier(composite.clone());

        let expected_after_tick = base
            .clone()
            .with_modifier(Move {
                direction: Face6::PX,
                distance: 20,
                velocity: 10,
                schedule: time::Schedule::EVERY_TICK,
            })
            .with_modifier(composite);

        assert_eq!(
            block.evaluate().unwrap().attributes.tick_action,
            Some(TickAction::from(Operation::Become(expected_after_tick)))
        );
    }

    /// Test [`Move`] acting within the `source` position of a [`Modifier::Composite`].
    #[test]
    fn move_inside_composite_source() {
        let [base, extra] = make_some_blocks();

        let block = extra.clone().with_modifier(Composite::new(
            base.clone().with_modifier(Move {
                direction: Face6::PX,
                distance: 10,
                velocity: 10,
                schedule: time::Schedule::EVERY_TICK,
            }),
            block::CompositeOperator::Over,
        ));

        let expected_after_tick = extra.clone().with_modifier(Composite::new(
            base.clone().with_modifier(Move {
                direction: Face6::PX,
                distance: 20,
                velocity: 10,
                schedule: time::Schedule::EVERY_TICK,
            }),
            block::CompositeOperator::Over,
        ));

        assert_eq!(
            block.evaluate().unwrap().attributes.tick_action,
            Some(TickAction::from(Operation::Become(expected_after_tick)))
        );
    }
}

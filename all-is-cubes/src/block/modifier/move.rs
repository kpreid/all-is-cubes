use crate::block::TickAction;
use crate::block::{
    self, Block, BlockAttributes, Evoxel, Evoxels, MinEval, Modifier, Resolution::R16, AIR,
};
use crate::drawing::VoxelBrush;
use crate::math::{Face6, GridAab, GridCoordinate, GridVector, Vol};
use crate::op::Operation;
use crate::universe;

/// Data for [`Modifier::Move`]; displaces the block out of the grid, cropping it.
/// A pair of `Move`s can depict a block moving between two cubes.
///
/// # Animation
///
/// * If the `distance` is zero then the modifier will remove itself, if possible,
///   on the next tick.
/// * If the `distance` and `velocity` are such that the block is out of view and will
///   never strt being in view, the block will be replaced with [`AIR`].
///
/// (TODO: Define the conditions for “if possible”.)
#[non_exhaustive] // TODO: needs a constructor instead
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Move {
    /// The direction in which the block is displaced.
    pub direction: Face6,
    /// The distance, in 1/256ths, by which it is displaced.
    pub distance: u16,
    /// The velocity **per tick** with which the displacement is changing.
    ///
    /// TODO: "Per tick" is a bad unit.
    pub velocity: i16,
}

impl Move {
    /// TODO: make a cleaner, less internals-ish constructor
    pub fn new(direction: Face6, distance: u16, velocity: i16) -> Self {
        Self {
            direction,
            distance,
            velocity,
        }
    }

    /// Create a pair of [`Modifier::Move`]s to displace a block.
    /// The first goes on the block being moved and the second on the air
    /// it's moving into.
    ///
    /// TODO: This is going to need to change again in order to support
    /// moving one block in and another out at the same time.
    pub fn paired_move(direction: Face6, distance: u16, velocity: i16) -> [Modifier; 2] {
        [
            Modifier::Move(Move {
                direction,
                distance,
                velocity,
            }),
            Modifier::Move(Move {
                direction: direction.opposite(),
                distance: 256 - distance,
                velocity: -velocity,
            }),
        ]
    }

    /// Note that `Modifier::Move` does some preprocessing to keep this simpler.
    pub(super) fn evaluate(
        &self,
        block: &Block,
        this_modifier_index: usize,
        mut input: MinEval,
        depth: u8,
        filter: &block::EvalFilter,
    ) -> Result<MinEval, block::EvalBlockError> {
        let Move {
            direction,
            distance,
            velocity,
        } = *self;

        // Apply Quote to ensure that the block's own `tick_action` and other effects
        // don't interfere with movement or cause duplication.
        // (In the future we may want a more nuanced policy that allows internal changes,
        // but that will probably involve refining tick_action processing.)
        input = Modifier::from(block::Quote::default()).evaluate(
            block,
            this_modifier_index,
            input,
            depth,
            filter,
        )?;

        let (original_bounds, effective_resolution) = match input.voxels {
            Evoxels::Many(resolution, ref array) => (array.bounds(), resolution),
            // Treat color blocks as having a resolution of 16. TODO: Improve on this hardcoded constant
            Evoxels::One(_) => (GridAab::for_block(R16), R16),
        };

        // For now, our strategy is to work in units of the block's resolution.
        // TODO: Generalize to being able to increase resolution to a chosen minimum.
        let distance_in_res =
            GridCoordinate::from(distance) * GridCoordinate::from(effective_resolution) / 256;
        let translation_in_res = direction.normal_vector() * distance_in_res;

        // This will be None if the displacement puts the block entirely out of view.
        let displaced_bounds: Option<GridAab> = original_bounds
            .translate(translation_in_res)
            .intersection(GridAab::for_block(effective_resolution));

        let animation_action: Option<TickAction> = if displaced_bounds.is_none() && velocity >= 0 {
            // Displaced to invisibility; turn into just plain air.
            Some(TickAction::from(Operation::Paint(VoxelBrush::single(AIR))))
        } else if translation_in_res == GridVector::zero() && velocity == 0
            || distance == 0 && velocity < 0
        {
            // Either a stationary displacement which is invisible, or an animated one which has finished its work.
            assert!(
                matches!(&block.modifiers()[this_modifier_index], Modifier::Move(m) if m == self)
            );
            let mut new_block = block.clone();
            new_block.modifiers_mut().remove(this_modifier_index); // TODO: What if other modifiers want to do things?
            Some(TickAction::from(Operation::Paint(VoxelBrush::single(
                new_block,
            ))))
        } else if velocity != 0 {
            // Movement in progress.
            assert!(
                matches!(&block.modifiers()[this_modifier_index], Modifier::Move(m) if m == self)
            );
            let mut new_block = block.clone();
            if let Modifier::Move(Move {
                distance, velocity, ..
            }) = &mut new_block.modifiers_mut()[this_modifier_index]
            {
                *distance = i32::from(*distance)
                            .saturating_add(i32::from(*velocity))
                            .clamp(0, i32::from(u16::MAX))
                            .try_into()
                            .unwrap(/* clamped to range */);
            }
            Some(TickAction::from(Operation::Paint(VoxelBrush::single(
                new_block,
            ))))
        } else {
            // Stationary displacement; take no action
            None
        };

        let attributes = BlockAttributes {
            tick_action: animation_action,
            ..input.attributes
        };

        Ok(match displaced_bounds {
            Some(displaced_bounds) => {
                let displaced_voxels = match &input.voxels {
                    Evoxels::Many(_, voxels) => Evoxels::Many(
                        effective_resolution,
                        Vol::from_fn(displaced_bounds, |cube| voxels[cube - translation_in_res]),
                    ),
                    &Evoxels::One(voxel) => {
                        // Input block is a solid color; synthesize voxels.
                        // TODO: Also synthesize if the resolution is merely low
                        // compared to the velocity.
                        Evoxels::Many(
                            effective_resolution,
                            Vol::from_fn(displaced_bounds, |_| voxel),
                        )
                    }
                };
                MinEval {
                    attributes,
                    voxels: displaced_voxels,
                }
            }
            None => MinEval {
                attributes,
                voxels: Evoxels::One(Evoxel::AIR),
            },
        })
    }
}

impl From<Move> for block::Modifier {
    fn from(value: Move) -> Self {
        Modifier::Move(value)
    }
}

impl universe::VisitRefs for Move {
    fn visit_refs(&self, _visitor: &mut dyn universe::RefVisitor) {
        let Move {
            direction: _,
            distance: _,
            velocity: _,
        } = self;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Block, Composite, EvaluatedBlock, Evoxel, Resolution::*};
    use crate::content::make_some_blocks;
    use crate::math::{FaceMap, GridPoint, OpacityCategory, Rgb, Rgba};
    use crate::space::Space;
    use crate::time;
    use crate::universe::Universe;
    use ordered_float::NotNan;

    #[test]
    fn move_atom_block_evaluation() {
        let color = rgba_const!(1.0, 0.0, 0.0, 1.0);
        let original = Block::from(color);
        let moved = original.clone().with_modifier(Move {
            direction: Face6::PY,
            distance: 128, // distance 1/2 block × scale factor of 256
            velocity: 0,
        });

        let expected_bounds = GridAab::from_lower_size([0, 8, 0], [16, 8, 16]);

        let ev_original = original.evaluate().unwrap();
        assert_eq!(
            moved.evaluate().unwrap(),
            EvaluatedBlock {
                attributes: ev_original.attributes.clone(),
                color: color.to_rgb().with_alpha(NotNan::new(2. / 3.).unwrap()),
                light_emission: Rgb::ZERO,
                voxels: Evoxels::Many(
                    R16,
                    Vol::repeat(expected_bounds, Evoxel::from_block(&ev_original))
                ),
                opaque: FaceMap::repeat(false).with(Face6::PY, true),
                visible: true,
                uniform_collision: None,
                voxel_opacity_mask: Some(Vol::repeat(expected_bounds, OpacityCategory::Opaque)),
            }
        );
    }

    #[test]
    fn move_voxel_block_evaluation() {
        let mut universe = Universe::new();
        let resolution = R2;
        let color = rgba_const!(1.0, 0.0, 0.0, 1.0);
        let original = Block::builder()
            .voxels_fn(&mut universe, resolution, |_| Block::from(color))
            .unwrap()
            .build();

        let moved = original.clone().with_modifier(Move {
            direction: Face6::PY,
            distance: 128, // distance 1/2 block × scale factor of 256
            velocity: 0,
        });

        let expected_bounds = GridAab::from_lower_size([0, 1, 0], [2, 1, 2]);

        let ev_original = original.evaluate().unwrap();
        assert_eq!(
            moved.evaluate().unwrap(),
            EvaluatedBlock {
                attributes: ev_original.attributes.clone(),
                color: color.to_rgb().with_alpha(NotNan::new(2. / 3.).unwrap()),
                light_emission: Rgb::ZERO,
                voxels: Evoxels::Many(
                    resolution,
                    Vol::repeat(expected_bounds, Evoxel::from_block(&ev_original))
                ),
                opaque: FaceMap::repeat(false).with(Face6::PY, true),
                visible: true,
                uniform_collision: None,
                voxel_opacity_mask: Some(Vol::repeat(expected_bounds, OpacityCategory::Opaque)),
            }
        );
    }

    /// [`Modifier::Move`] incorporates [`Modifier::Quote`] to ensure that no conflicting
    /// effects happen.
    #[test]
    fn move_also_quotes() {
        let original = Block::builder()
            .color(Rgba::WHITE)
            .tick_action(Some(TickAction::from(Operation::Paint(
                VoxelBrush::single(AIR),
            ))))
            .build();
        let moved = original.with_modifier(Move {
            direction: Face6::PY,
            distance: 128,
            velocity: 0,
        });

        assert_eq!(moved.evaluate().unwrap().attributes.tick_action, None);
    }

    /// Set up a `Modifier::Move`, let it run, and then allow assertions to be made about the result.
    fn move_block_test(direction: Face6, velocity: i16, checker: impl FnOnce(&Space, &Block)) {
        let [block] = make_some_blocks();
        let mut space = Space::empty(GridAab::from_lower_upper([-1, -1, -1], [2, 2, 2]));
        let [move_out, move_in] = Move::paired_move(direction, 0, velocity);
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
            universe.step(false, time::DeadlineStd::Whenever);
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
            })
            .with_modifier(composite.clone());

        let expected_after_tick = base
            .clone()
            .with_modifier(Move {
                direction: Face6::PX,
                distance: 20,
                velocity: 10,
            })
            .with_modifier(composite);

        assert_eq!(
            block.evaluate().unwrap().attributes.tick_action,
            Some(TickAction::from(Operation::Paint(VoxelBrush::single(
                expected_after_tick
            ))))
        );
    }

    /// Test [`Move`] acting within the `source` position of a [`Modifier::Composite`].
    ///
    /// TODO: This is not yet implemented, but should be.
    #[test]
    fn move_inside_composite_source() {
        let [base, extra] = make_some_blocks();

        let block = extra.clone().with_modifier(Composite::new(
            base.clone().with_modifier(Move {
                direction: Face6::PX,
                distance: 10,
                velocity: 10,
            }),
            block::CompositeOperator::Over,
        ));

        let expected_after_tick = extra.clone().with_modifier(Composite::new(
            base.clone().with_modifier(Move {
                direction: Face6::PX,
                distance: 10,
                velocity: 10,
            }),
            block::CompositeOperator::Over,
        ));

        if false {
            // This is what we want to happen
            assert_eq!(
                block.evaluate().unwrap().attributes.tick_action,
                Some(TickAction::from(Operation::Paint(VoxelBrush::single(
                    expected_after_tick
                ))))
            );
        } else {
            // Placeholder to fail if the current behavior changes
            assert_eq!(block.evaluate().unwrap().attributes.tick_action, None);
        }
    }
}

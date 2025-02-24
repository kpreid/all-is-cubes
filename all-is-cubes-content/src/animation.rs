//! Algorithms for animating blocks.

use alloc::boxed::Box;
use alloc::sync::Arc;
use core::f64::consts::TAU;
use core::fmt;
use core::time::Duration;

use rand::{Rng as _, SeedableRng as _};
use rand_xoshiro::Xoshiro256Plus;

use all_is_cubes::block::{self, AIR, Block, BlockCollision};
use all_is_cubes::content::palette;
use all_is_cubes::math::{
    Cube, GridAab, GridPoint, GridRotation, GridVector, Rgba, Vol, rgba_const,
};
use all_is_cubes::space::{CubeTransaction, Space, SpaceTransaction};
use all_is_cubes::time::Tick;
use all_is_cubes::transaction::Merge;
use all_is_cubes::universe::{HandleVisitor, UniverseTransaction, VisitHandles};
use all_is_cubes::{behavior, op};

#[cfg(doc)]
use all_is_cubes::time::TickSchedule;

/// A [`Behavior`] which animates a recursive block by periodically recomputing all of its
/// voxels.
// TODO: This was thrown together as a test/demo and may be too specific or too general.
#[derive(Clone, Eq, PartialEq)]
pub(crate) struct AnimatedVoxels<F> {
    /// The function to compute the voxels.
    function: F,
    /// The animation frame number, periodically incremented and fed to the function.
    frame: u64,

    /// How much time to wait before incrementing the frame counter, measured in [`TickSchedule`] ticks (i.e. steps).
    frame_period: u16,
}

impl<F: Fn(Cube, u64) -> Block + Clone + 'static> AnimatedVoxels<F> {
    pub(crate) fn new(function: F) -> Self {
        Self {
            function,
            frame: 0,
            frame_period: 4,
        }
    }

    fn paint(&self, bounds: GridAab) -> SpaceTransaction {
        SpaceTransaction::filling(bounds, |cube| {
            let block = (self.function)(cube, self.frame);
            CubeTransaction::replacing(None, Some(block.clone()))
        })
    }
}

impl<F: Fn(Cube, u64) -> Block + Clone + Send + Sync + 'static> behavior::Behavior<Space>
    for AnimatedVoxels<F>
{
    fn step(
        &self,
        context: &behavior::Context<'_, Space>,
    ) -> (UniverseTransaction, behavior::Then) {
        let txn = if context.tick.prev_phase().rem_euclid(self.frame_period) == 0 {
            let mut mut_self: AnimatedVoxels<F> = self.clone();
            mut_self.frame = mut_self.frame.wrapping_add(1);

            let paint_txn = mut_self.paint(context.attachment.bounds());
            context
                .replace_self(mut_self)
                .merge(context.bind_host(paint_txn))
                .unwrap()
        } else {
            UniverseTransaction::default()
        };

        // TODO: ask the behavior set to schedule a callback for us at our desired period
        // instead of immediately (this is not possible yet)
        (txn, behavior::Then::Step)
    }

    fn persistence(&self) -> Option<behavior::Persistence> {
        // TODO: serialize
        None
    }
}

impl<F> fmt::Debug for AnimatedVoxels<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AnimatedVoxels")
            .field("frame", &self.frame)
            .field("frame_period", &self.frame_period)
            .finish_non_exhaustive()
    }
}

impl<F> VisitHandles for AnimatedVoxels<F> {
    // No handles unless the function hides one
    fn visit_handles(&self, _visitor: &mut dyn HandleVisitor) {}
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Fire {
    blocks: [Block; 4],
    /// The bounds of this array determine the affected blocks.
    // TODO: should be using the attachment bounds instead of internally stored bounds
    fire_state: Vol<Box<[u8]>>,
    rng: Xoshiro256Plus,
    /// Time accumulation not yet equal to a whole frame.
    /// TODO: Give [`Tick`] a concept of discrete time units we can reuse instead of
    /// separate things having their own float-based clocks.
    accumulator: Duration,
}

impl Fire {
    pub(crate) fn new(bounds: GridAab) -> Self {
        let rng = Xoshiro256Plus::seed_from_u64(2385993827);
        fn fire_color(color: Rgba) -> Block {
            Block::builder()
                .color(color)
                .light_emission(color.to_rgb() * 8.0)
                .collision(BlockCollision::None)
                .build()
        }
        let blocks = [
            AIR,
            fire_color(rgba_const!(1.0, 0.5, 0.1, 1.0)),
            fire_color(rgba_const!(1.0, 0.1, 0.1, 1.0)),
            fire_color(rgba_const!(1.0, 1.0, 0.1, 1.0)),
        ];
        Self {
            blocks,
            fire_state: Vol::from_fn(bounds, |_cube| 0),
            rng,
            accumulator: Duration::ZERO,
        }
    }

    fn tick_state(&mut self, tick: Tick) -> bool {
        const PERIOD: Duration = Duration::from_nanos(1_000_000_000 / 32);
        self.accumulator += tick.delta_t();
        if self.accumulator >= PERIOD {
            self.accumulator -= PERIOD;
        } else {
            return false;
        }

        // To ripple changes upward, we need to iterate downward
        let bounds = self.fire_state.bounds();
        let y0 = bounds.lower_bounds().y;
        for z in bounds.z_range() {
            for y in bounds.y_range().rev() {
                for x in bounds.x_range() {
                    let cube = GridPoint::new(x, y, z);
                    self.fire_state[cube] = if y == y0 {
                        (self.fire_state[cube] + self.rng.gen_range(0..3))
                            .saturating_sub(1)
                            .min(self.blocks.len() as u8 - 1)
                    } else {
                        let below = self.fire_state[cube + GridVector::new(0, -1, 0)];
                        if !self.rng.gen_bool(0.25) {
                            below.saturating_sub(1)
                        } else {
                            below
                        }
                    }
                }
            }
        }

        true
    }

    fn paint(&self) -> SpaceTransaction {
        SpaceTransaction::filling(self.fire_state.bounds(), |cube| {
            CubeTransaction::replacing(
                None,
                Some(self.blocks[self.fire_state[cube] as usize].clone()),
            )
        })
    }
}

impl behavior::Behavior<Space> for Fire {
    fn step(
        &self,
        context: &behavior::Context<'_, Space>,
    ) -> (UniverseTransaction, behavior::Then) {
        let mut mut_self = self.clone();
        let txn = if mut_self.tick_state(context.tick) {
            let paint_txn = mut_self.paint();
            context
                .replace_self(mut_self)
                .merge(context.bind_host(paint_txn))
                .unwrap()
        } else {
            context.replace_self(mut_self)
        };
        (txn, behavior::Then::Step)
    }

    fn persistence(&self) -> Option<behavior::Persistence> {
        // TODO: serialize
        None
    }
}

impl VisitHandles for Fire {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        self.blocks.visit_handles(visitor)
    }
}

/// Behavior that draws a clock face that shows the progress of time, on a
/// a basis of whole seconds and individual frames.
///
/// The block must have resolution 16.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Clock {
    /// How many [`Tick`]s we've seen.
    ///
    /// TODO: Revisit this data storage once we've nailed down questions of how long a tick
    /// will ever be.
    ticks: u64,
}

impl Clock {
    pub(crate) fn new() -> Self {
        Self { ticks: 0 }
    }

    fn paint(&self) -> SpaceTransaction {
        // TODO: While these don't actually need to allocate anything, we ought to make them
        // constants to keep the per-frame cost minial.
        let rim = block::from_color!(0.7, 0.7, 0.4, 1.0);
        let marks = block::from_color!(palette::ALMOST_BLACK);
        let trail = block::from_color!(0.5, 0.5, 0.5, 1.0);
        let background = if self.ticks.rem_euclid(120) >= 60 {
            block::from_color!(0.6, 0.6, 0.6, 1.0)
        } else {
            block::from_color!(1.0, 1.0, 1.0, 1.0)
        };

        let time_angle = self.ticks.rem_euclid(60) as f64 / 60.0;
        let frame_angle = self.ticks.rem_euclid(4) as f64 / 4.0;

        SpaceTransaction::filling(GridAab::from_lower_size([0, 0, 0], [16, 16, 1]), |cube| {
            let centered_point = (cube - GridVector::new(8, 8, 0)).midpoint();
            let r = centered_point.to_vector().length();
            let block = {
                let base_angle = centered_point.x.atan2(centered_point.y) / TAU;
                if r > 8.0 {
                    // Surrounding area — do nothing
                    return CubeTransaction::default();
                } else if r > 7.0 {
                    // Outside edge
                    rim.clone()
                } else if r > 2.5 {
                    // Big sweep hand
                    let clock_angle = (time_angle - base_angle).rem_euclid(1.0);
                    if clock_angle < 0.03 {
                        marks.clone()
                    } else if clock_angle < 0.06 {
                        trail.clone()
                    } else {
                        background.clone()
                    }
                } else if r > 1.5 {
                    // Border
                    background.clone()
                } else {
                    // Advances 1 tick per frame in one of 4 patches
                    let clock_angle = (base_angle - frame_angle).rem_euclid(1.0);
                    if clock_angle < 0.25 {
                        marks.clone()
                    } else {
                        background.clone()
                    }
                }
            };
            CubeTransaction::replacing(None, Some(block))
        })
    }
}

impl behavior::Behavior<Space> for Clock {
    fn step(
        &self,
        context: &behavior::Context<'_, Space>,
    ) -> (UniverseTransaction, behavior::Then) {
        let mut mut_self = self.clone();
        mut_self.ticks += 1;
        (
            context
                .bind_host(mut_self.paint())
                .merge(context.replace_self(mut_self))
                .unwrap(),
            behavior::Then::Step,
        )
    }

    fn persistence(&self) -> Option<behavior::Persistence> {
        // TODO: serialize
        None
    }
}

impl VisitHandles for Clock {
    fn visit_handles(&self, _visitor: &mut dyn HandleVisitor) {}
}

/// Returns an [`op::Operation`] which, if used as a [`block::TickAction`], causes the block
/// to move back and forth between whatever obstacles it hits.
pub(crate) fn back_and_forth_movement(movement: block::Move) -> op::Operation {
    op::Operation::Alt(Arc::new([
        op::Operation::StartMove(movement),
        // If unable to move, turn around.
        op::Operation::AddModifiers(Arc::new([block::Modifier::Rotate(GridRotation::RxYz)])),
    ]))
}

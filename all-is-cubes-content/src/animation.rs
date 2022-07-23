//! Algorithms for animating blocks.

use std::f64::consts::TAU;
use std::fmt;

use instant::Duration;
use rand::{Rng as _, SeedableRng as _};
use rand_xoshiro::Xoshiro256Plus;

use all_is_cubes::behavior::{Behavior, BehaviorContext};
use all_is_cubes::block::{Block, AIR};
use all_is_cubes::cgmath::{EuclideanSpace as _, InnerSpace as _};
use all_is_cubes::content::palette;
use all_is_cubes::math::{cube_to_midpoint, GridAab, GridArray, GridPoint, GridVector};
use all_is_cubes::rgba_const;
use all_is_cubes::space::{Space, SpaceTransaction};
use all_is_cubes::time::Tick;
use all_is_cubes::transaction::Merge;
use all_is_cubes::universe::{RefVisitor, UniverseTransaction, VisitRefs};

/// A [`Behavior`] which animates a recursive block by periodically recomputing all of its
/// voxels.
// TODO: This was thrown together as a test/demo and may be too specific or too general.
#[derive(Clone, Eq, PartialEq)]
pub(crate) struct AnimatedVoxels<F> {
    /// The function to compute the voxels.
    function: F,
    /// The frame number, periodically incremented and fed to the function.
    frame: u64,
    /// How much time to wait before incrementing the frame counter.
    frame_period: Duration,
    /// Time accumulation not yet equal to a whole frame.
    /// Always less than `frame_period`.
    /// TODO: Give [`Tick`] a concept of discrete time units we can reuse instead of
    /// separate things having their own float-based clocks.
    accumulator: Duration,
}

impl<F: Fn(GridPoint, u64) -> Block + Clone + 'static> AnimatedVoxels<F> {
    pub(crate) fn new(function: F) -> Self {
        let frame_period = Duration::from_nanos(1_000_000_000 / 16);
        Self {
            function,
            frame: 0,
            frame_period,
            accumulator: frame_period,
        }
    }

    fn paint(&self, bounds: GridAab) -> SpaceTransaction {
        let mut txn = SpaceTransaction::default();
        for cube in bounds.interior_iter() {
            let block = (self.function)(cube, self.frame);
            txn.set(cube, None, Some(block.clone())).unwrap();
        }
        txn
    }
}

impl<F: Fn(GridPoint, u64) -> Block + Clone + Send + Sync + 'static> Behavior<Space>
    for AnimatedVoxels<F>
{
    fn step(&self, context: &BehaviorContext<'_, Space>, tick: Tick) -> UniverseTransaction {
        let mut mut_self: AnimatedVoxels<F> = self.clone();
        mut_self.accumulator += tick.delta_t();
        if mut_self.accumulator >= mut_self.frame_period {
            mut_self.accumulator -= mut_self.frame_period;
            mut_self.frame = mut_self.frame.wrapping_add(1);

            let paint_txn = mut_self.paint(context.host.bounds());
            context
                .replace_self(mut_self)
                .merge(context.bind_host(paint_txn))
                .unwrap()
        } else {
            context.replace_self(mut_self)
        }
    }

    fn alive(&self, _context: &BehaviorContext<'_, Space>) -> bool {
        true
    }

    fn ephemeral(&self) -> bool {
        false
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

impl<F> VisitRefs for AnimatedVoxels<F> {
    // No references unless the function hides one
    fn visit_refs(&self, _visitor: &mut dyn RefVisitor) {}
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Fire {
    blocks: [Block; 4],
    /// The bounds of this array determine the affected blocks.
    fire_state: GridArray<u8>,
    rng: Xoshiro256Plus,
    /// Time accumulation not yet equal to a whole frame.
    /// TODO: Give [`Tick`] a concept of discrete time units we can reuse instead of
    /// separate things having their own float-based clocks.
    accumulator: Duration,
}

impl Fire {
    pub(crate) fn new(bounds: GridAab) -> Self {
        let rng = Xoshiro256Plus::seed_from_u64(2385993827);
        let blocks = [
            AIR,
            Block::from(rgba_const!(1.0, 0.5, 0.0, 1.0)),
            Block::from(rgba_const!(1.0, 0.0, 0.0, 1.0)),
            Block::from(rgba_const!(1.0, 1.0, 0.0, 1.0)),
        ];
        Self {
            blocks,
            fire_state: GridArray::from_fn(bounds, |_cube| 0),
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
        let mut txn = SpaceTransaction::default();
        for cube in self.fire_state.bounds().interior_iter() {
            let block: &Block = &self.blocks[self.fire_state[cube] as usize];
            txn.set(cube, None, Some(block.clone())).unwrap();
        }
        txn
    }
}

impl Behavior<Space> for Fire {
    fn step(&self, context: &BehaviorContext<'_, Space>, tick: Tick) -> UniverseTransaction {
        let mut mut_self = self.clone();
        if mut_self.tick_state(tick) {
            let paint_txn = mut_self.paint();
            context
                .replace_self(mut_self)
                .merge(context.bind_host(paint_txn))
                .unwrap()
        } else {
            context.replace_self(mut_self)
        }
    }

    fn alive(&self, _context: &BehaviorContext<'_, Space>) -> bool {
        true
    }

    fn ephemeral(&self) -> bool {
        false
    }
}

impl VisitRefs for Fire {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        self.blocks.visit_refs(visitor)
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
        let rim = Block::from(rgba_const!(0.7, 0.7, 0.4, 1.0));
        let marks = Block::from(palette::ALMOST_BLACK);
        let trail = Block::from(rgba_const!(0.5, 0.5, 0.5, 1.0));
        let background = if self.ticks.rem_euclid(120) >= 60 {
            Block::from(rgba_const!(0.6, 0.6, 0.6, 1.0))
        } else {
            Block::from(rgba_const!(1.0, 1.0, 1.0, 1.0))
        };

        let time_angle = self.ticks.rem_euclid(60) as f64 / 60.0;
        let frame_angle = self.ticks.rem_euclid(4) as f64 / 4.0;

        let mut txn = SpaceTransaction::default();
        for x in 0..16 {
            for y in 0..16 {
                let cube = GridPoint::new(x, y, 0);
                let centered_point = cube_to_midpoint(cube - GridVector::new(8, 8, 0));
                let r = centered_point.to_vec().magnitude();
                let block = {
                    let base_angle = centered_point.x.atan2(centered_point.y) / TAU;
                    if r > 8.0 {
                        // Surrounding area — do nothing
                        continue;
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
                txn.set(cube, None, Some(block)).unwrap();
            }
        }
        txn
    }
}

impl Behavior<Space> for Clock {
    fn step(&self, context: &BehaviorContext<'_, Space>, _tick: Tick) -> UniverseTransaction {
        let mut mut_self = self.clone();
        mut_self.ticks += 1;
        context
            .bind_host(mut_self.paint())
            .merge(context.replace_self(mut_self))
            .unwrap()
    }

    fn alive(&self, _context: &BehaviorContext<'_, Space>) -> bool {
        true
    }

    fn ephemeral(&self) -> bool {
        false
    }
}

impl VisitRefs for Clock {
    fn visit_refs(&self, _visitor: &mut dyn RefVisitor) {}
}

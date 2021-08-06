// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for animating blocks.

use std::fmt;

use instant::Duration;
use rand::{Rng as _, SeedableRng as _};
use rand_xoshiro::Xoshiro256Plus;

use crate::apps::Tick;
use crate::behavior::{Behavior, BehaviorContext};
use crate::block::{Block, AIR};
use crate::math::{GridPoint, GridVector};
use crate::space::{Grid, GridArray, Space, SpaceTransaction};
use crate::transactions::Transaction;

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

    fn paint(&self, grid: Grid) -> SpaceTransaction {
        let mut txn = SpaceTransaction::default();
        for cube in grid.interior_iter() {
            let block = (self.function)(cube, self.frame);
            // TODO: This transaction constructing and merging is neither ergonomic nor efficient.
            // Maybe we should have a SpaceTransaction::set_cube(&mut self, cube, block) instead.
            txn = txn
                .merge(SpaceTransaction::set_cube(cube, None, Some(block.clone())))
                .unwrap();
        }
        txn
    }
}

impl<F: Fn(GridPoint, u64) -> Block + Clone + 'static> Behavior<Space> for AnimatedVoxels<F> {
    fn step(
        &self,
        context: &BehaviorContext<'_, Space>,
        tick: crate::apps::Tick,
    ) -> crate::transactions::UniverseTransaction {
        let mut mut_self: AnimatedVoxels<F> = self.clone();
        mut_self.accumulator += tick.delta_t;
        if mut_self.accumulator >= mut_self.frame_period {
            mut_self.accumulator -= mut_self.frame_period;
            mut_self.frame = mut_self.frame.wrapping_add(1);

            let paint_txn = mut_self.paint(context.host.grid());
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
    pub(crate) fn new(bounds: Grid) -> Self {
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
        self.accumulator += tick.delta_t;
        if self.accumulator >= PERIOD {
            self.accumulator -= PERIOD;
        } else {
            return false;
        }

        // To ripple changes upward, we need to iterate downward
        let grid = self.fire_state.grid();
        let y0 = grid.lower_bounds().y;
        for z in grid.z_range() {
            for y in grid.y_range().rev() {
                for x in grid.x_range() {
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
        for cube in self.fire_state.grid().interior_iter() {
            let block: &Block = &self.blocks[self.fire_state[cube] as usize];
            txn = txn
                .merge(SpaceTransaction::set_cube(cube, None, Some(block.clone())))
                .unwrap();
        }
        txn
    }
}

impl Behavior<Space> for Fire {
    fn step(
        &self,
        context: &BehaviorContext<'_, Space>,
        tick: Tick,
    ) -> crate::transactions::UniverseTransaction {
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

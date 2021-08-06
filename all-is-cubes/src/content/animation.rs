// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for animating blocks.

use std::fmt;

use instant::Duration;

use crate::behavior::{Behavior, BehaviorContext};
use crate::block::Block;
use crate::math::GridPoint;
use crate::space::{Grid, Space, SpaceTransaction};
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

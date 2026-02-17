//! Algorithms for animating blocks.

use alloc::boxed::Box;
use alloc::sync::Arc;
use core::fmt;

use rand::{RngExt as _, SeedableRng as _};
use rand_xoshiro::Xoshiro256Plus;

use all_is_cubes::block::{self, AIR, Block, BlockCollision};
use all_is_cubes::content::palette;
use all_is_cubes::math::{Cube, GridAab, GridRotation, GridVector, Rgba, Vol, rgba_const};
use all_is_cubes::space::{CubeTransaction, Space, SpaceTransaction};
use all_is_cubes::time;
use all_is_cubes::transaction::Merge;
use all_is_cubes::universe::{HandleVisitor, UniverseTransaction, VisitHandles};
use all_is_cubes::{behavior, op};

/// A [`Behavior`] which animates a recursive block by periodically recomputing all of its
/// voxels.
// TODO: This was thrown together as a test/demo and may be too specific or too general.
#[derive(Clone, Eq, PartialEq)]
pub(crate) struct AnimatedVoxels<F> {
    /// The function to compute the voxels.
    function: F,
    /// The animation frame number, periodically incremented and fed to the function.
    frame: u64,

    /// When to increment the frame number.
    schedule: time::Schedule,
}

impl<F: Fn(Cube, u64) -> Block + Clone + 'static> AnimatedVoxels<F> {
    pub(crate) fn new(function: F) -> Self {
        Self {
            function,
            frame: 0,
            schedule: time::Schedule::from_period(core::num::NonZeroU16::new(4).unwrap()),
        }
    }

    fn paint(&self, bounds: GridAab) -> SpaceTransaction {
        SpaceTransaction::filling(bounds, |cube| {
            let block = (self.function)(cube, self.frame);
            CubeTransaction::replacing(None, Some(block))
        })
    }
}

impl<F: Fn(Cube, u64) -> Block + Clone + Send + Sync + 'static> behavior::Behavior<Space>
    for AnimatedVoxels<F>
{
    fn step(
        &self,
        context: &behavior::Context<'_, '_, Space>,
    ) -> (UniverseTransaction, behavior::Then) {
        let txn = if self.schedule.contains(context.tick) {
            let mut mut_self: AnimatedVoxels<F> = self.clone();
            mut_self.frame = mut_self.frame.wrapping_add(1);

            let paint_txn = mut_self.paint(context.attachment.bounds());
            context.replace_self(mut_self).merge(context.bind_host(paint_txn)).unwrap()
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
            .field("frame_period", &self.schedule)
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
    schedule: time::Schedule,
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
            schedule: time::Schedule::from_period(core::num::NonZeroU16::new(2).unwrap()),
        }
    }

    fn tick_state(&mut self, tick: time::Tick) -> bool {
        if !self.schedule.contains(tick) {
            return false;
        }

        // To ripple changes upward, we need to iterate downward
        let bounds = self.fire_state.bounds();
        let y0 = bounds.lower_bounds().y;
        for z in bounds.z_range() {
            for y in bounds.y_range().rev() {
                for x in bounds.x_range() {
                    let cube = Cube::new(x, y, z);
                    self.fire_state[cube] = if y == y0 {
                        (self.fire_state[cube] + self.rng.random_range(0..3))
                            .saturating_sub(1)
                            .min(self.blocks.len() as u8 - 1)
                    } else {
                        let below = self.fire_state[cube + GridVector::new(0, -1, 0)];
                        if !self.rng.random_bool(0.25) {
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
        context: &behavior::Context<'_, '_, Space>,
    ) -> (UniverseTransaction, behavior::Then) {
        let mut mut_self = self.clone();
        let txn = if mut_self.tick_state(context.tick) {
            let paint_txn = mut_self.paint();
            context.replace_self(mut_self).merge(context.bind_host(paint_txn)).unwrap()
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

/// Function that returns voxels of an image of a clock face that shows the progress of time, on a
/// a basis of whole seconds and individual ticks.
///
/// The block must have resolution 16.
/// The universe [`time::TickSchedule`] must have divisor 60.
pub(crate) fn paint_clock(phase: time::Phase, cube: Cube) -> &'static Block {
    const BACKGROUND: &Block = &block::from_color!(0.7, 0.7, 0.4, 1.0);
    const MARKED: &Block = &block::from_color!(palette::ALMOST_BLACK);
    const UNMARKED: &Block = &block::from_color!(1.0, 1.0, 1.0, 1.0);

    const BG: u8 = 80;

    #[rustfmt::skip]
    const PATTERN: [[u8; 16]; 16] = [
        [ 0,  1,  2,  3,  4,  5,  6,   7,   8,  9, 10, 11, 12, 13, 14, 15],
        [59, BG, BG, BG, BG, BG, BG,  BG,  BG, BG, BG, BG, BG, BG, BG, 16],
        [58, BG,  0,  0, BG, BG, BG,  BG,  BG, BG, BG, BG, 15, 15, BG, 17],
        [57, BG,  0,  0, BG, BG, BG,  BG,  BG, BG, BG, BG, 15, 15, BG, 18],

        [56, BG, BG, BG, BG, BG, BG,  BG,  BG, BG, BG, BG, BG, BG, BG, 19],
        [55, BG, BG, BG, BG, BG, BG,  BG,  BG, BG, BG, BG, BG, BG, BG, 20],
        [54, BG, BG, BG, BG, BG, BG,  BG,  BG, BG, BG, BG, BG, BG, BG, 21],
        [53, BG, BG, BG, BG, BG, BG, 100, 101, BG, BG, BG, BG, BG, BG, 22],

        [52, BG, BG, BG, BG, BG, BG, 102, 103, BG, BG, BG, BG, BG, BG, 23],
        [51, BG, BG, BG, BG, BG, BG,  BG,  BG, BG, BG, BG, BG, BG, BG, 24],
        [50, BG, BG, BG, BG, BG, BG,  BG,  BG, BG, BG, BG, BG, BG, BG, 25],
        [49, BG, BG, BG, BG, BG, BG,  BG,  BG, BG, BG, BG, BG, BG, BG, 26],

        [48, BG, 45, 45, BG, BG, BG,  BG,  BG, BG, BG, BG, 30, 30, BG, 27],
        [47, BG, 45, 45, BG, BG, BG,  BG,  BG, BG, BG, BG, 30, 30, BG, 28],
        [46, BG, BG, BG, BG, BG, BG,  BG,  BG, BG, BG, BG, BG, BG, BG, 29],
        [45, 44, 43, 42, 41, 40, 39,  38,  37, 36, 35, 34, 33, 32, 31, 30],
    ];

    let pattern_value = PATTERN[15 - cube.y as usize][cube.x as usize];

    match pattern_value {
        0..60 => {
            if u16::from(pattern_value) == phase {
                MARKED
            } else {
                UNMARKED
            }
        }
        100..104 => {
            if u16::from(pattern_value - 100) == phase % 4 {
                MARKED
            } else {
                UNMARKED
            }
        }
        BG => BACKGROUND,
        _ => {
            // shouldn't happen
            const { &block::from_color!(1.0, 0.0, 0.0, 1.0) }
        }
    }
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

// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! VUI stands for Voxel User Interface.
//!
//! We've got all this rendering and interaction code, so let's reuse it for the
//! GUI as well as the game.

use std::time::Duration;

use crate::block::{Block, AIR};
use crate::math::{FreeCoordinate, RGBA};
use crate::space::{Grid, Space};
use crate::universe::{URef, Universe, UniverseStepInfo};

/// `Vui` builds user interfaces out of voxels. It owns a `Universe` dedicated to the
/// purpose and draws into spaces to form the HUD and menus.
#[derive(Debug)] // TODO: probably not very informative Debug as derived
pub(crate) struct Vui {
    universe: Universe,
    current_space: URef<Space>,
    hud_space: URef<Space>,
    aspect_ratio: FreeCoordinate,
}

impl Vui {
    pub fn new() -> Self {
        let mut universe = Universe::new();
        let hud_space = draw_hud_space(&mut universe);

        Self {
            universe,
            current_space: hud_space.clone(),
            hud_space,
            aspect_ratio: 4. / 3., // arbitrary placeholder assumption
        }
    }

    // TODO: It'd be more encapsulating if we could provide a _read-only_ reference...
    pub fn current_space(&self) -> &URef<Space> {
        &self.current_space
    }

    pub fn step(&mut self, timestep: Duration) -> UniverseStepInfo {
        self.universe.step(timestep)
    }
}

fn draw_hud_space(universe: &mut Universe) -> URef<Space> {
    // TODO: need to dynamically adjust aspect ratio
    // TODO: ...and when we do, make sure bad sizes don't cause us to crash
    let w = 40;
    let h = 30;
    let grid = Grid::new((-1, -1, 0), (w + 2, h + 2, 10));
    let mut space = Space::empty(grid);

    if true {
        // Visualization of the bounds of the space we're drawing.
        let frame_block = Block::from(RGBA::new(0.0, 1.0, 1.0, 1.0));
        let mut add_frame = |z| {
            space
                .fill(&Grid::new((-1, -1, z), (w + 2, h + 2, 1)), |_| {
                    Some(&frame_block)
                })
                .unwrap();
            space
                .fill(&Grid::new((0, 0, z), (w, h, 1)), |_| Some(&AIR))
                .unwrap();
        };
        add_frame(0);
        add_frame(grid.upper_bounds().z - 1);
    }

    universe.insert_anonymous(space)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vui_smoke_test() {
        let _ = Vui::new();
    }
}

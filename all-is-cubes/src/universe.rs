// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Top-level game state container.

use std::time::Duration;

use crate::space::{Grid, Space, SpaceStepInfo};
use crate::camera::Camera;
use crate::worldgen::{axes, plain_color_blocks, wavy_landscape};

/// In the future, a Universe will be a collection of named objects, which can refer
/// to each other. It will enable multiple references, garbage collection, change
/// notification, and inter-object invariants.
///
/// For now, it's a hardcoded container of all the mutable/steppable game state.
pub struct Universe {
    pub(crate) space: Space,
    pub(crate) camera: Camera,
}

impl Universe {
    pub fn new_test_universe() -> Self {
        let mut space = Space::empty(Grid::new((-10, -10, -10), (21, 21, 21)));
        let blocks = plain_color_blocks();
        wavy_landscape(&mut space, blocks, 1.0);
        axes(&mut space);

        let camera = Camera::for_grid(space.grid());

        Self { space, camera }
    }

    // TODO: will need to change this to have borrowingness
    pub fn space(&self) -> &Space { &self.space }

    // TODO: will need to change this to have explicit borrow management
    pub fn camera(&self) -> &Camera { &self.camera }

    // TODO: will need to change this to have explicit borrow management
    pub fn camera_mut(&mut self) -> &mut Camera { &mut self.camera }

    /// Advance time.
    pub fn step(&mut self, timestep: Duration) -> (SpaceStepInfo, ()) {
        let space_info = self.space.step(timestep);
        let camera_info = self.camera.step(timestep, &self.space);
        (space_info, camera_info)
    }
}

#[cfg(test)]
mod tests {
    //use super::*;

    // TODO: test that stepping works
}

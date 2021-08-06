// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use cgmath::{EuclideanSpace, InnerSpace, Point3};
use ordered_float::NotNan;

use crate::character::Spawn;
use crate::math::{FreeCoordinate, Rgb};
use crate::space::{Grid, LightPhysics, Space, SpacePhysics};

/// Tool for constructing new [`Space`]s.
///
/// To create one, call [`Space::builder(grid)`].
///
/// TODO: Allow specifying behaviors and initial block contents.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpaceBuilder {
    pub(super) grid: Grid,
    pub(super) spawn: Option<Spawn>,
    pub(super) physics: SpacePhysics,
}

impl SpaceBuilder {
    pub(super) const fn new(grid: Grid) -> Self {
        Self {
            grid,
            spawn: None,
            physics: SpacePhysics::DEFAULT,
        }
    }

    /// Sets the value for [`Space::physics`], which determines global characteristics
    /// of gravity and light in the space.
    pub fn physics(mut self, physics: SpacePhysics) -> Self {
        self.physics = physics;
        self
    }

    /// Sets the value of [`SpacePhysics::sky_color`] for the space.
    pub fn sky_color(mut self, color: Rgb) -> Self {
        self.physics.sky_color = color;
        self
    }

    /// Sets the value of [`SpacePhysics::light`] for the space, which determines the
    /// behavior of light within the space.
    pub fn light_physics(mut self, light_physics: LightPhysics) -> Self {
        self.physics.light = light_physics;
        self
    }

    /// Sets the value for [`Space::spawn`], which determines the default circumstances of
    /// new characters.
    ///
    /// If not set, the default spawn position will be [0, 0, 0].
    /// (TODO: Improve this and document it centrally.)
    pub fn spawn(mut self, spawn: Spawn) -> Self {
        self.spawn = Some(spawn);
        self
    }

    /// Sets the default spawn location of new characters.
    ///
    /// Panics if any of the given coordinates is infinite or NaN.
    #[track_caller]
    pub fn spawn_position(mut self, position: Point3<FreeCoordinate>) -> Self {
        assert!(
            position.to_vec().magnitude2().is_finite(),
            "spawn_position must be finite"
        );
        let position = position.map(|c| NotNan::new(c).unwrap());

        let grid = self.grid;
        let mut spawn = self
            .spawn
            .unwrap_or_else(|| Spawn::default_for_new_space(grid));
        spawn.position = position;
        self.spawn = Some(spawn);
        self
    }

    /// Construct a new [`Space`] filled with [`AIR`](crate::block::AIR), with the bounds
    /// and settings of this builder.
    pub fn build_empty(self) -> Space {
        Space::new_from_builder(self)
    }
}

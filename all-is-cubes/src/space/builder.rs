use cgmath::{EuclideanSpace, InnerSpace, Point3};

use crate::block::{Block, AIR};
use crate::character::Spawn;
use crate::math::{FreeCoordinate, Rgb};
use crate::space::{GridAab, LightPhysics, Space, SpacePhysics};

/// Tool for constructing new [`Space`]s.
///
/// To create one, call [`Space::builder()`](Space::builder).
///
/// TODO: Allow specifying behaviors.
///
/// # Type parameters
///
/// * `B` is either `()` or `GridAab` according to whether the bounds have been specified.
#[derive(Clone, Debug, Eq, PartialEq)]
#[must_use]
pub struct SpaceBuilder<B> {
    pub(super) bounds: B,
    pub(super) spawn: Option<Spawn>,
    pub(super) physics: SpacePhysics,
    pub(super) initial_fill: Block,
}

impl<B> SpaceBuilder<B> {
    /// Sets the [`Block`] that the space's volume will be filled with.
    ///
    /// Caution: If [evaluating](Block::evaluate) the block fails, constructing the space
    /// will panic. Future versions may improve on this.
    pub fn filled_with(mut self, block: Block) -> Self {
        self.initial_fill = block;
        self
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
}

impl<B: SpaceBuilderBounds> SpaceBuilder<B> {
    /// Set the bounds unless they have already been set.
    pub fn bounds_if_not_set(self, bounds_fn: impl FnOnce() -> GridAab) -> SpaceBuilder<GridAab> {
        // Delegate to the trait. (This method exists so the trait need not be imported.)
        SpaceBuilderBounds::bounds_if_not_set(self, bounds_fn)
    }
}

impl SpaceBuilder<()> {
    /// Use [`SpaceBuilder::default()`] as the public way to call this.
    pub(super) const fn new() -> Self {
        Self {
            bounds: (),
            spawn: None,
            physics: SpacePhysics::DEFAULT,
            initial_fill: AIR,
        }
    }

    /// Set the bounds of the space, outside which no blocks may be placed.
    pub fn bounds(self, bounds: GridAab) -> SpaceBuilder<GridAab> {
        SpaceBuilder {
            bounds,
            spawn: self.spawn,
            physics: self.physics,
            initial_fill: self.initial_fill,
        }
    }
}

impl SpaceBuilder<GridAab> {
    /// Sets the default spawn location of new characters.
    ///
    /// Panics if any of the given coordinates is infinite or NaN.
    #[track_caller]
    pub fn spawn_position(mut self, position: Point3<FreeCoordinate>) -> Self {
        assert!(
            position.to_vec().magnitude2().is_finite(),
            "spawn_position must be finite"
        );

        let bounds = self.bounds;
        let mut spawn = self
            .spawn
            .unwrap_or_else(|| Spawn::default_for_new_space(bounds));
        spawn.set_eye_position(position);
        self.spawn = Some(spawn);
        self
    }

    /// Construct a [`Space`] with the contents and settings from this builder.
    ///
    /// The builder must have had bounds specified.
    pub fn build(self) -> Space {
        Space::new_from_builder(self)
    }
}

impl Default for SpaceBuilder<()> {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper for [`SpaceBuilder::bounds_if_not_set()`]. Do not call or implement this trait.
pub trait SpaceBuilderBounds: sbb::SbbSealed + Sized {
    fn bounds_if_not_set(
        builder: SpaceBuilder<Self>,
        bounds_fn: impl FnOnce() -> GridAab,
    ) -> SpaceBuilder<GridAab>;
}

impl SpaceBuilderBounds for () {
    fn bounds_if_not_set(
        builder: SpaceBuilder<Self>,
        bounds_fn: impl FnOnce() -> GridAab,
    ) -> SpaceBuilder<GridAab> {
        builder.bounds(bounds_fn())
    }
}

impl SpaceBuilderBounds for GridAab {
    fn bounds_if_not_set(
        builder: SpaceBuilder<Self>,
        _bounds_fn: impl FnOnce() -> GridAab,
    ) -> SpaceBuilder<GridAab> {
        builder
    }
}

/// Module for sealed trait
mod sbb {
    use super::*;
    #[doc(hidden)]
    pub trait SbbSealed {}
    impl SbbSealed for () {}
    impl SbbSealed for GridAab {}
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for Space {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        use crate::content::make_some_blocks;

        // TODO: Should be reusing GridArray as Arbitrary for this.

        let bounds = GridAab::arbitrary_with_max_volume(u, 2048)?;
        let mut space = Space::builder(bounds)
            .physics(u.arbitrary()?)
            .spawn(u.arbitrary()?)
            .build();

        // Generate some blocks to put in the space
        let mut blocks = Vec::from(make_some_blocks::<2>()); // TODO: generate arbitrary blocks with attributes
        #[allow(clippy::same_item_push)]
        for _ in 0..6 {
            // Make it probable that blocks are AIR
            blocks.push(AIR);
        }

        // Fill space with blocks
        let mut failure = None;
        space
            .fill(space.bounds(), |_| {
                match u.choose(&blocks) {
                    Ok(block) => Some(block),
                    Err(e) => {
                        // We can't abort a space.fill() early unless we resort to catch_unwind.
                        failure = Some(e);
                        None
                    }
                }
            })
            .unwrap();
        if let Some(e) = failure {
            return Err(e);
        }

        Ok(space)
    }
}

#[cfg(test)]
mod tests {
    use crate::math::Rgba;

    use super::*;

    #[test]
    fn defaults() {
        let bounds = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
        let space = Space::builder(bounds).build();
        space.consistency_check();
        assert_eq!(space.bounds(), bounds);
        assert_eq!(space[bounds.lower_bounds()], AIR);
        assert_eq!(space.physics(), &SpacePhysics::default());
        assert_eq!(space.spawn(), &Spawn::default_for_new_space(bounds));
    }

    #[test]
    fn filled_with() {
        let bounds = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
        let block = Block::from(Rgba::WHITE);
        let space = Space::builder(bounds).filled_with(block.clone()).build();
        space.consistency_check();
        assert_eq!(space[bounds.lower_bounds()], block);
    }

    #[test]
    fn bounds_if_not_set_when_not_set() {
        let bounds = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
        assert_eq!(
            SpaceBuilder::new()
                .bounds_if_not_set(|| bounds)
                .build()
                .bounds(),
            bounds
        );
    }

    #[test]
    fn bounds_if_not_set_when_already_set() {
        let first_bounds = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
        let ignored_bounds = GridAab::from_lower_size([100, 2, 3], [1, 1, 1]);
        assert_eq!(
            Space::builder(first_bounds)
                .bounds_if_not_set(|| ignored_bounds)
                .build()
                .bounds(),
            first_bounds
        );
    }

    // TODO: test and implement initial fill that has a tick_action that needs to be
    // activated properly

    // TODO: test all builder features
}

use cgmath::{EuclideanSpace, InnerSpace, Point3};

use crate::character::Spawn;
use crate::math::{FreeCoordinate, Rgb};
use crate::space::{GridAab, LightPhysics, Space, SpacePhysics};

/// Tool for constructing new [`Space`]s.
///
/// To create one, call [`Space::builder()`](Space::builder).
///
/// TODO: Allow specifying behaviors and initial block contents.
#[derive(Clone, Debug, Eq, PartialEq)]
#[must_use]
pub struct SpaceBuilder {
    pub(super) bounds: GridAab,
    pub(super) spawn: Option<Spawn>,
    pub(super) physics: SpacePhysics,
}

impl SpaceBuilder {
    pub(super) const fn new(bounds: GridAab) -> Self {
        Self {
            bounds,
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

        let bounds = self.bounds;
        let mut spawn = self
            .spawn
            .unwrap_or_else(|| Spawn::default_for_new_space(bounds));
        spawn.set_eye_position(position);
        self.spawn = Some(spawn);
        self
    }

    /// Construct a new [`Space`] filled with [`AIR`](crate::block::AIR), with the bounds
    /// and settings of this builder.
    pub fn build_empty(self) -> Space {
        Space::new_from_builder(self)
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Space {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        use crate::block::AIR;
        use crate::content::make_some_blocks;

        // TODO: Should be reusing GridArray as Arbitrary for this.

        let bounds = GridAab::arbitrary_with_max_volume(u, 2048)?;
        let mut space = Space::builder(bounds)
            .physics(u.arbitrary()?)
            .spawn(u.arbitrary()?)
            .build_empty();

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

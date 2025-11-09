use core::fmt;

use bevy_ecs::prelude as ecs;
use euclid::{Vector3D, vec3};

use crate::math::{FreeCoordinate, NotNan, notnan, rgb_const};
use crate::physics::Acceleration;
use crate::space::Sky;
use crate::util::{ConciseDebug, Refmt as _};

#[cfg(doc)]
use crate::{block::Block, space::Space};

// -------------------------------------------------------------------------------------------------

/// The global characteristics of a [`Space`], more or less independent of location within
/// the block grid.
///
/// This is a separate type so that [`Space`] does not need many miscellaneous accessors,
/// and so an instance of it can be reused for similar spaces (e.g.
/// [`DEFAULT_FOR_BLOCK`](Self::DEFAULT_FOR_BLOCK)).
//---
// TODO: Consider renaming this struct to avoid `space::Space*` naming pattern
#[derive(Clone, Eq, Hash, PartialEq, ecs::Component)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct SpacePhysics {
    /// Gravity vector for moving objects, in cubes/s².
    ///
    /// TODO: Expand this to an enum which allows non-uniform gravity patterns.
    pub gravity: Vector3D<NotNan<FreeCoordinate>, Acceleration>,

    /// Light arriving from outside the space, used for light calculation
    /// and rendering the background.
    pub sky: Sky,

    /// Method used to compute the illumination of individual blocks.
    pub light: LightPhysics,
}

impl SpacePhysics {
    pub(crate) const DEFAULT: Self = Self {
        gravity: vec3(notnan!(0.), notnan!(-20.), notnan!(0.)),
        sky: Sky::DEFAULT,
        light: LightPhysics::DEFAULT,
    };

    /// Recommended defaults for spaces which are going to define a [`Block`]'s voxels.
    /// In particular, disables light since it will not be used.
    pub const DEFAULT_FOR_BLOCK: Self = Self {
        gravity: vec3(notnan!(0.), notnan!(0.), notnan!(0.)),
        sky: Sky::Uniform(rgb_const!(0.5, 0.5, 0.5)),
        light: LightPhysics::None,
    };
}

impl fmt::Debug for SpacePhysics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            gravity,
            sky,
            light,
        } = self;
        f.debug_struct("SpacePhysics")
            .field(
                "gravity",
                &gravity.map(NotNan::into_inner).refmt(&ConciseDebug),
            )
            .field("sky", &sky)
            .field("light", &light)
            .finish()
    }
}

impl Default for SpacePhysics {
    fn default() -> Self {
        Self::DEFAULT
    }
}

/// Method used to compute the illumination of individual blocks in a [`Space`].
#[non_exhaustive]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum LightPhysics {
    /// No light. All surface colors are taken exactly as displayed colors. The
    /// [`SpacePhysics::sky`] is used solely as a background image.
    None,
    /// Raycast-based light propagation and diffuse reflections.
    ///
    /// TODO: Need to provide a builder so that this can be constructed
    /// even when more parameters are added.
    // TODO: #[non_exhaustive]
    Rays {
        /// The maximum distance a simulated light ray will travel; blocks farther than
        /// that distance apart will never have direct influence on each other.
        maximum_distance: u8,
    },
}

impl LightPhysics {
    pub(crate) const DEFAULT: Self = Self::Rays {
        maximum_distance: 30,
    };
}

impl Default for LightPhysics {
    fn default() -> Self {
        Self::DEFAULT
    }
}

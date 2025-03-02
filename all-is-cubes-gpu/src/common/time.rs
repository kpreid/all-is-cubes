use core::ops;
use core::time::Duration;

use all_is_cubes_render::camera::Layers;

/// A plan for the maximum amount of time to use for each step of each frame of rendering.
///
/// Obeying the plan may consist of deferring work to the next frame, or (TODO: not
/// implemented yet) reducing the amount of content drawn, in detail or in distance.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FrameBudget {
    pub(crate) update_meshes: Layers<Duration>,
}

impl FrameBudget {
    #[doc(hidden)] // TODO: decide on good API
    pub const SIXTY_FPS: FrameBudget = FrameBudget {
        update_meshes: Layers {
            world: Duration::from_millis(5),
            ui: Duration::from_millis(3),
        },
    };

    /// No meaningful time restrictions; suitable for “offline” rendering that should
    /// always choose completeness over latency, and tests.
    pub const PRACTICALLY_INFINITE: FrameBudget = FrameBudget {
        update_meshes: Layers {
            world: VERY_LONG,
            ui: VERY_LONG,
        },
    };

    /// Create a [`FrameBudget`] presumed to be a good choice when the period between frames
    /// (reciprocal of frame rate) is this long.
    pub fn from_frame_period(frame_period: Duration) -> Self {
        // The fraction of the frame period not accounted for here is time left for:
        // * the simulation, and
        // * everything in rendering that is not explicitly budgeted.
        // TODO: In principle we should also be negotiating with the simulation.
        Self {
            update_meshes: Layers {
                world: frame_period.mul_f32(0.3),
                ui: frame_period.mul_f32(0.2),
            },
        }
    }
}

/// A Duration long enough that it is not interesting in questions of rendering, but not
/// so long that adding a reasonable number of it to an [`Instant`] will overflow.
/// TODO: Replace all this with the newer `Deadline` concept?
const VERY_LONG: Duration = Duration::from_secs(86400 * 7);

/// Wrapper to implement [`all_is_cubes::time::Instant`] for [`web_time::Instant`].
///
/// Note: This code exists in multiple locations because duplicating it is easier than
/// arranging for a shared dependency.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct AdaptedInstant(web_time::Instant);

impl all_is_cubes::time::Instant for AdaptedInstant {
    fn now() -> Self {
        Self(web_time::Instant::now())
    }

    fn saturating_duration_since(self, other: Self) -> Duration {
        web_time::Instant::saturating_duration_since(&self.0, other.0)
    }
}
impl ops::Add<Duration> for AdaptedInstant {
    type Output = Self;
    fn add(self, rhs: Duration) -> Self::Output {
        Self(self.0 + rhs)
    }
}
impl ops::Sub<Duration> for AdaptedInstant {
    type Output = Self;
    fn sub(self, rhs: Duration) -> Self::Output {
        Self(ops::Sub::sub(self.0, rhs))
    }
}

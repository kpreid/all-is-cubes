//! Items not specific to a particular GPU API.

use core::ops;
use core::time::Duration;

use all_is_cubes_render::camera::Layers;

mod debug_lines;
pub(crate) use debug_lines::*;
mod draw_to_texture;
pub(crate) use draw_to_texture::*;
mod id;
pub(crate) use id::*;
mod info;
pub use info::*;
mod msw;
pub(crate) use msw::Msw;

#[doc(hidden)] // Exported only for use by fuzz_octree
pub mod octree_alloc;

pub(crate) mod reloadable;

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

#[cfg(feature = "rerun")]
#[doc(hidden)] // not stable, just exists to support config from desktop cmdline
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)] // hidden, not stable
pub struct RerunFilter {
    /// Log performance info
    pub performance: bool,
    /// Log the rendered image.
    pub image: bool,
    /// Log the contents of the texture atlases.
    pub textures: bool,
}

/// Single-entry cache.
#[derive(Clone, Debug)]
pub(crate) struct Memo<K, V> {
    data: Option<(K, V)>,
}

impl<K: Eq, V> Memo<K, V> {
    pub fn new() -> Self {
        Self { data: None }
    }

    pub fn get_or_insert(&mut self, key: K, value_fn: impl FnOnce() -> V) -> &mut V
    where
        K: Copy,
    {
        match &mut self.data {
            Some((k, v)) => {
                if *k == key {
                    v
                } else {
                    *v = value_fn();
                    *k = key;
                    v
                }
            }
            d @ None => {
                let (_k, v) = d.insert((key, value_fn()));
                v
            }
        }
    }

    pub(crate) fn get(&self) -> Option<&V> {
        self.data.as_ref().map(|(_k, v)| v)
    }

    /// Drop the stored value, if any.
    #[cfg_attr(not(feature = "rerun"), expect(unused))] // currently not otherwise used
    pub fn clear(&mut self) {
        self.data = None;
    }
}

impl<K, V> Default for Memo<K, V> {
    fn default() -> Self {
        Self { data: None }
    }
}

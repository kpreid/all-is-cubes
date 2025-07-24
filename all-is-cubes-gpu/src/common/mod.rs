//! Items not specific to a particular GPU API.
mod debug_lines;
pub(crate) use debug_lines::*;
mod draw_to_texture;
pub(crate) use draw_to_texture::*;
mod id;
pub(crate) use id::*;
mod info;
pub use info::*;
mod memo;
pub(crate) use memo::Memo;
mod msw;
pub(crate) use msw::Msw;
pub(crate) mod reloadable;
mod time;
pub use time::FrameBudget;

#[cfg(feature = "_special_testing")] // for use by fuzz_octree
#[allow(missing_docs)]
pub mod octree_alloc;
#[cfg(not(feature = "_special_testing"))]
pub(crate) mod octree_alloc;

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

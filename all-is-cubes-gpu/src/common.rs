// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Items not specific to a particular GPU API.

use std::error::Error;

mod info;
pub use info::*;

#[doc(hidden)] // Exported only for use by fuzz_octree
pub mod octree_alloc;

pub(crate) mod reloadable;

/// Error arising when GPU/platform resources could not be obtained, or there is a bug
/// or incompatibility, and the requested graphics initialization or drawing could not be
/// completed.
#[derive(Debug, thiserror::Error)]
#[error("graphics error (in {0})", context.as_ref().map(|s| s.as_ref()).unwrap_or("?"))]
pub struct GraphicsResourceError {
    context: Option<String>,
    #[source]
    source: Box<dyn Error + Send + Sync>,
}

impl GraphicsResourceError {
    pub(crate) fn new<E: Error + Send + Sync + 'static>(source: E) -> Self {
        GraphicsResourceError {
            context: None,
            source: Box::new(source),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn _test_graphics_resource_error_is_sync()
    where
        GraphicsResourceError: Send + Sync,
    {
    }
}

//! Loading embedded assets used to define spaces and blocks.
//!
//! Use [`include_image!`] to embed PNG images.
//! Then, they can be passed to functions such as [`block_from_image()`].
//!
//! This module is in an early state of development and should be expected to change greatly.

// -------------------------------------------------------------------------------------------------

mod image;
pub(crate) use image::decode_static;
pub use image::{LazyImage, include_image};

// TODO: better name and organization
mod load_block;
pub use load_block::{Block, Expansion, PrimitiveOrSuch, Vox};

mod pixel_to_voxel;
pub use pixel_to_voxel::{BlockFromImageError, block_from_image, default_srgb, space_from_image};

// -------------------------------------------------------------------------------------------------

#[doc(no_inline)]
pub use ::imgref::ImgRef;

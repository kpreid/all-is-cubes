use std::sync::Arc;

use all_is_cubes::block::{self, Block};
use all_is_cubes::math::Rgba;

// -------------------------------------------------------------------------------------------------

#[cfg(feature = "import")]
pub(crate) fn dot_vox_palette_to_blocks(palette: &[dot_vox::Color]) -> Arc<[Block]> {
    palette
        .iter()
        .enumerate()
        .map(|(index, &dot_vox::Color { r, g, b, a })| {
            Block::builder()
                .display_name(index.to_string())
                .color(Rgba::from_srgb8([r, g, b, a]))
                .build()
        })
        .collect()
}

#[cfg(feature = "export")]
pub(crate) fn block_to_dot_vox_palette_entry(
    evaluated: &block::EvaluatedBlock,
) -> Option<dot_vox::Color> {
    // TODO: should we compare identity or color?
    if *evaluated == block::AIR_EVALUATED {
        None
    } else {
        let [r, g, b, a] = evaluated.color().to_srgb8();
        Some(dot_vox::Color { r, g, b, a })
    }
}

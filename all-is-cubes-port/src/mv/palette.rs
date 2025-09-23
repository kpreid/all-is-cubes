use std::sync::Arc;

use itertools::{EitherOrBoth, Itertools};

use all_is_cubes::arcstr;
use all_is_cubes::block::{self, Block};
use all_is_cubes::math::{Rgb, Rgb01, Rgba, ZeroOne};

// -------------------------------------------------------------------------------------------------

#[cfg(feature = "import")]
pub(crate) fn dot_vox_palette_to_blocks(
    palette: &[dot_vox::Color],
    materials: &[dot_vox::Material],
) -> Arc<[Block]> {
    palette
        .iter()
        .zip_longest(materials)
        .enumerate()
        .map(|(index, palette_color_and_or_material)| {
            let display_name = arcstr::format!("{index}");
            let (reflectance, emission) = match palette_color_and_or_material {
                EitherOrBoth::Both(&color, material) => {
                    let palette_color = color_in(color);
                    match material.material_type() {
                        Some("_blend" | "_glass") => (
                            palette_color.with_alpha(
                                ZeroOne::<f32>::try_from(material.opacity().unwrap_or(1.0))
                                    .unwrap_or(ZeroOne::ONE)
                                    .complement(),
                            ),
                            Rgb::ZERO,
                        ),
                        Some("_emit") => {
                            // Empirically, radiant_flux scales the emission by powers of 10.
                            let emission_scale = material.emission().unwrap_or(0.0)
                                * 10.0f32.powf(material.radiant_flux().unwrap_or(0.0));
                            (Rgba::BLACK, Rgb::from(palette_color) * emission_scale)
                        }
                        None | Some("_diffuse") => (palette_color.with_alpha_one(), Rgb::ZERO),
                        _ => {
                            log::warn!("unknown/unsupported .vox material type {material:?}");
                            (palette_color.with_alpha_one(), Rgb::ZERO)
                        }
                    }
                }
                EitherOrBoth::Left(&color) => (color_in(color).with_alpha_one(), Rgb::ZERO),
                EitherOrBoth::Right(_material_only) => {
                    log::warn!("material but no palette at index {index}");
                    (Rgba::WHITE, Rgb::ZERO)
                }
            };
            Block::builder()
                .display_name(display_name)
                .color(reflectance)
                .light_emission(emission)
                .build()
        })
        .collect()
}

// TODO: export materials (this will require support from dot_vox)
#[cfg(feature = "export")]
pub(crate) fn block_to_dot_vox_palette_entry(
    evaluated: &block::EvaluatedBlock,
) -> Option<dot_vox::Color> {
    // TODO: should we compare identity or color?
    if *evaluated == block::AIR_EVALUATED {
        None
    } else {
        Some(color_out(evaluated.color()))
    }
}

/// Add to a palette if there is room.
#[cfg(feature = "export")]
pub(crate) fn push(palette: &mut Vec<dot_vox::Color>, color: dot_vox::Color) -> Option<u8> {
    let i = u8::try_from(palette.len()).ok().filter(|&i| {
        // MagicaVoxel uses 1-indexing so there is no 256th entry
        i != 255
    })?;
    palette.push(color);
    Some(i)
}

#[cfg(feature = "import")]
fn color_in(dot_vox::Color { r, g, b, a: _ }: dot_vox::Color) -> Rgb01 {
    // Note: Even though the `dot_vox::Color` field carries an alpha, MagicaVoxel itself seems to
    // always ignore it, and use only transparency specified by materials. Therefore, we do too.
    Rgb01::from_srgb8([r, g, b])
}

#[cfg(feature = "export")]
pub(crate) fn color_out(color: Rgba) -> dot_vox::Color {
    let [r, g, b, a] = color.to_srgb8();
    dot_vox::Color { r, g, b, a }
}

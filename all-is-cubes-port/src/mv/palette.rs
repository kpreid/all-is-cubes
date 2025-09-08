use std::sync::Arc;

use itertools::{EitherOrBoth, Itertools};

use all_is_cubes::arcstr;
use all_is_cubes::block::{self, Block};
use all_is_cubes::math::{OpacityCategory, Rgb, Rgb01, Rgba, ZeroOne};

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

/// Subset of an All is Cubes voxel that contains only the properties that we are able to export
/// to .vox.
///
/// We use this type instead of a `dot_vox::Material` because we don’t want to construct string
/// dictionaries until necessary so this can be used efficiently for interning.
#[cfg(feature = "export")]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum MaterialKey {
    Diffuse {
        color: [u8; 3],
    },
    Blend {
        color: [u8; 3],
        transparency: ZeroOne<f32>,
    },
    Emit {
        color: Rgb,
    },
}

#[cfg(feature = "export")]
impl MaterialKey {
    /// Lossily convert the block into this representation.
    ///
    /// Returns `None` if the All is Cubes  block should be represented by the absence of a
    /// MagicaVoxel voxel.
    pub fn from_block(block: &block::EvaluatedBlock) -> Option<Self> {
        Self::from_evoxel(&block::Evoxel::from_block(block))
    }

    /// Lossily convert the voxel into this representation.
    ///
    /// Returns `None` if the All is Cubes voxel should be represented by the absence of a
    /// MagicaVoxel voxel.
    pub fn from_evoxel(evoxel: &block::Evoxel) -> Option<Self> {
        let color = evoxel.color;
        match color.opacity_category() {
            OpacityCategory::Invisible => None,
            OpacityCategory::Partial => Some(Self::Blend {
                color: color.to_rgb().clamp_01().to_srgb8(),
                transparency: color.alpha().complement(),
            }),
            OpacityCategory::Opaque => {
                if evoxel.emission == Rgb::ZERO {
                    Some(Self::Diffuse {
                        color: color.to_rgb().clamp_01().to_srgb8(),
                    })
                } else {
                    Some(Self::Emit {
                        color: evoxel.emission,
                    })
                }
            }
        }
    }

    pub fn to_color_and_material(self, id: u32) -> (dot_vox::Color, dot_vox::Material) {
        let s = <String as From<&str>>::from;

        match self {
            MaterialKey::Diffuse { color } => (
                make_color_struct(color),
                dot_vox::Material {
                    id,
                    properties: dot_vox::Dict::from([(s("_type"), s("_diffuse"))]),
                },
            ),
            MaterialKey::Blend {
                color,
                transparency,
            } => (
                make_color_struct(color),
                dot_vox::Material {
                    id,
                    properties: dot_vox::Dict::from([
                        (s("_type"), s("_blend")),
                        (s("_alpha"), format!("{transparency:.3}")),
                    ]),
                },
            ),
            MaterialKey::Emit { color } => {
                let mut luminance = color.luminance();
                let mut flux = 0.0;
                // Not sure if flux is allowed to be a non-integer value
                while luminance > 1.0 && flux < 4.0 {
                    luminance /= 10.0;
                    flux += 1.0;
                }
                (
                    make_color_struct((color * luminance.recip()).clamp_01().to_srgb8()),
                    dot_vox::Material {
                        id,
                        properties: dot_vox::Dict::from([
                            (s("_type"), s("_emit")),
                            (s("_emission"), s("_emit")),
                            (s("_emit"), format!("{luminance}")),
                            (s("_flux"), format!("{flux}")),
                        ]),
                    },
                )
            }
        }
    }
}

/// Converts and interns [`EvaluatedBlock`]s into .vox palette entries.
#[cfg(feature = "export")]
#[derive(Default)]
pub(crate) struct ExportPalette {
    colors: Vec<dot_vox::Color>,
    materials: Vec<dot_vox::Material>,
    table: std::collections::HashMap<MaterialKey, u8>,
}

#[cfg(feature = "export")]
impl ExportPalette {
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns None if the palette is full.
    pub fn get_or_insert(&mut self, key: MaterialKey) -> Option<u8> {
        match self.table.entry(key) {
            std::collections::hash_map::Entry::Occupied(oe) => Some(*oe.get()),
            std::collections::hash_map::Entry::Vacant(ve) => {
                let index = u8::try_from(self.colors.len()).ok().filter(|&i| {
                    // MagicaVoxel uses 1-indexing so there is no 256th entry
                    i != 255
                })?;
                let (color, material) = key.to_color_and_material(u32::from(index + 1));
                self.colors.push(color);
                if false {
                    // TODO: Currently, `dot_vox` exports properties in randomized HashMap order,
                    // so we disable material export to avoid this causing snapshot tests to fail.
                    //
                    // Instead, we should fix `dot_vox`, and in the interim, have a option
                    // to pick whether material export is done, but for now, it's unconditionally
                    // disabled.
                    self.materials.push(material);
                }
                ve.insert(index);
                Some(index)
            }
        }
    }

    pub(crate) fn into_parts(self) -> (Vec<dot_vox::Color>, Vec<dot_vox::Material>) {
        (self.colors, self.materials)
    }
}

#[cfg(feature = "import")]
fn color_in(dot_vox::Color { r, g, b, a: _ }: dot_vox::Color) -> Rgb01 {
    // Note: Even though the `dot_vox::Color` field carries an alpha, MagicaVoxel itself seems to
    // always ignore it, and use only transparency specified by materials. Therefore, we do too.
    Rgb01::from_srgb8([r, g, b])
}

#[cfg(feature = "export")]
fn make_color_struct([r, g, b]: [u8; 3]) -> dot_vox::Color {
    dot_vox::Color { r, g, b, a: 255 }
}

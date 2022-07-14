// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use crossterm::style::Color;

use all_is_cubes::camera::Viewport;
use all_is_cubes::cgmath::{ElementWise as _, InnerSpace as _, Vector2, Vector3};
use all_is_cubes::math::{FreeCoordinate, Rgba};

/// Options for the terminal UI.
///
/// TODO: Migrate all of this into `GraphicsOptions`? Add an extension mechanism?
/// In any case, we shouldn't have two separately-designed mechanisms, but at most two
/// parallel ones.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct TerminalOptions {
    /// Color escapes supported by the terminal.
    pub(crate) colors: ColorMode,

    pub(crate) characters: CharacterMode,
}

impl TerminalOptions {
    /// Compute viewport to use for raytracing, given the size in characters of the
    /// drawing area.
    pub(crate) fn viewport_from_terminal_size(&self, size: Vector2<u16>) -> Viewport {
        // max(1) is to keep the projection math from blowing up.
        // TODO: Remove this and make it more robust instead.
        let size = size.map(|c| c.max(1));
        Viewport {
            framebuffer_size: size
                .map(u32::from)
                .mul_element_wise(self.characters.rays_per_character().map(u32::from)),

            // Assume that characters are approximately twice as tall as they are wide.
            nominal_size: size
                .map(FreeCoordinate::from)
                .mul_element_wise(Vector2::new(0.5, 1.0)),
        }
    }
}

impl Default for TerminalOptions {
    fn default() -> Self {
        Self {
            colors: ColorMode::TwoFiftySix, // TODO: default to 16-color mode once we have it implemented
            characters: CharacterMode::Split,
        }
    }
}

/// Which type of color control sequences to use.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum ColorMode {
    None,
    // TODO: Sixteen,
    TwoFiftySix,
    Rgb,
}

impl ColorMode {
    /// Returns the “next” option for purposes of UI stepping.
    pub(crate) fn cycle(self) -> Self {
        use ColorMode::*;
        match self {
            None => TwoFiftySix,
            TwoFiftySix => Rgb,
            Rgb => None,
        }
    }

    /// Convert RGB color to a Crossterm [`Color`] value according to this mode.
    ///
    /// The input and output [`Option`]s have different meanings:
    ///
    /// * If the input color is [`None`] then the output is "reset"
    ///   (i.e. the "not colored" colors).
    /// * This function returns [`None`] if color is disabled and no color control
    ///   sequences should be produced — i.e. the input is ignored.
    pub fn convert(self, input: Option<Rgba>) -> Option<Color> {
        match (input, self) {
            // Mode None produces no output no matter what.
            (_, ColorMode::None) => None,
            // Input None means Reset.
            (None, _) => Some(Color::Reset),
            // ColorMode::Sixteen => {}
            (Some(rgba), ColorMode::TwoFiftySix) => {
                // The 256-color palette consists of
                // * the original 16 "ANSI" colors,
                // * a 216-color 6×6×6 RGB color cube (unevenly divided) starting at index 16,
                // * and a grayscale ramp from index 232 to 255 (without black or white entries).
                fn srgb_to_216(x: u8) -> u8 {
                    match x {
                        0..=47 => 0,    // 0
                        48..=114 => 1,  // 95
                        115..=154 => 2, // 135
                        155..=194 => 3, // 175
                        195..=234 => 4, // 215
                        235..=255 => 5, // 255
                    }
                }
                fn srgb_to_gray(x: u8) -> u8 {
                    // TODO: the ramp is not quite linear-in-sRGB-value at the ends, so this is not
                    // quite an accurate conversion. We could also get more shades by using the
                    // grays that appear in the color cube.
                    let gray_ramp_color = (f32::from(x) / 255. * 25.).round() as u8; // zero to 25
                    if gray_ramp_color == 0 {
                        16 // Black from the color cube
                    } else if gray_ramp_color == 25 {
                        231 // Bhite from the color cube
                    } else {
                        // The contiguous ramp range excludes black and white.
                        gray_ramp_color + 231
                    }
                }

                let luminance = rgba.luminance();
                // TODO: this is probably not how you calculate saturation
                let saturation = Vector3::from(rgba.to_rgb())
                    .map(|component| (component - luminance).abs())
                    .dot(Vector3::new(1., 1., 1.));

                // Pick the gray ramp (more shades) or the RGB cube based on whether
                // there is significant saturation (threshold picked arbitrarily).
                let ansi_color = if saturation < 0.1 {
                    let srgblum = Rgba::new(luminance, 0., 0., 0.).to_srgb8()[0];
                    srgb_to_gray(srgblum)
                } else {
                    let [r, g, b, _] = rgba.to_srgb8();
                    16 + (srgb_to_216(r) * 6 + srgb_to_216(g)) * 6 + srgb_to_216(b)
                };

                Some(Color::AnsiValue(ansi_color))
            }
            (Some(rgba), ColorMode::Rgb) => {
                let [r, g, b, _] = rgba.to_srgb8();
                let c = Color::Rgb { r, g, b };
                Some(c)
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum CharacterMode {
    /// Show the first characters of the names of the blocks.
    Names,

    /// Use the characters `  ░░▒▒▓▓██` to represent brightnesses.
    Shades,

    /// Use block drawing characters (e.g. “▄”) for double resolution and squarer pixels.
    ///
    /// This may cause artifacts in some terminals, and increases the CPU requirement
    /// since more rays are used per frame.
    Split,

    /// If colors are enabled, equivalent to [`CharacterMode::Split`].
    /// Otherwise, use block drawing characters to approximately represent brightness.
    Shapes,
}

impl CharacterMode {
    /// Returns the “next” option for purposes of UI stepping.
    pub fn cycle(self) -> Self {
        use CharacterMode::*;
        match self {
            Names => Shades,
            Shades => Split,
            Split => Shapes,
            Shapes => Names,
        }
    }

    pub fn rays_per_character(&self) -> Vector2<u8> {
        use CharacterMode::*;
        match self {
            Names | Shades => Vector2::new(1, 1),
            Split | Shapes => Vector2::new(1, 2),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn viewport_no_panic() {
        let o = TerminalOptions::default();
        o.viewport_from_terminal_size((0, 0).into());
        o.viewport_from_terminal_size((0, 1).into());
        o.viewport_from_terminal_size((1, 0).into());
        o.viewport_from_terminal_size((1, 1).into());
    }

    // TODO: add tests of color calculation
}

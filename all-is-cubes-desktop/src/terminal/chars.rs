//! Details of writing colored characters to the terminal.

use std::collections::HashMap;
use std::io;

use ratatui::backend::Backend;
use ratatui::crossterm::QueueableCommand as _;
use ratatui::crossterm::style::{Color, Colors, SetColors};
use ratatui::layout::Position;

use all_is_cubes::euclid::Vector2D;
use all_is_cubes::math::{Rgb, Rgba};
use all_is_cubes_render::camera::ImagePixel;

use super::options::{CharacterMode, ColorMode};
use super::{TextAndColor, TextRayImage};

pub(super) fn image_patch_to_character(
    image: &TextRayImage,
    char_pos: Vector2D<usize, ImagePixel>,
) -> (&str, Colors) {
    let options = &image.options;
    // This match's `get_patch()` calls must match TerminalOptions::rays_per_character.
    match (options.characters, options.colors) {
        (CharacterMode::Split | CharacterMode::Shapes, ColorMode::None) => {
            // Use "black and white" graphic characters.
            // (Split mode would otherwise be blank.)
            let [[&(ref text1, color1)], [&(_, color2)]] = image.get_patch(char_pos);

            let lum1 = color1.unwrap_or(Rgba::TRANSPARENT).luminance();
            let lum2 = color2.unwrap_or(Rgba::TRANSPARENT).luminance();
            // Checkerboard dithering.
            let [threshold1, threshold2] = if char_pos.x % 2 == 0 {
                [0.5, 0.7]
            } else {
                [0.7, 0.5]
            };

            let text = match &options.characters {
                CharacterMode::Names | CharacterMode::Shades | CharacterMode::Braille => {
                    unreachable!()
                }
                CharacterMode::Split => match (lum1 > threshold1, lum2 > threshold2) {
                    // Assume that background is black and foreground is white (this could be an option).
                    (true, true) => "█",  // U+2588 FULL BLOCK
                    (true, false) => "▀", // U+2580 UPPER HALF BLOCK
                    (false, true) => "▄", // U+2584 LOWER HALF BLOCK
                    (false, false) if lum2 > 0.2 => text1.as_str(),
                    (false, false) => " ", // U+0020 SPACE
                },
                CharacterMode::Shapes => {
                    let avg_lum = ((lum1 + lum2) / 2.0).clamp(0.0, 1.0);

                    if (lum1 - lum2).abs() < 0.05 {
                        [" ", "░", "▒", "▓", "█"][(avg_lum * 4.999) as usize]
                        //[" ", "▏", "▎", "▍", "▌", "▋", "▊", "▉", "█"][(avg_lum * 8.999) as usize]
                    } else if lum1 < lum2 {
                        [" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"][(avg_lum * 8.999) as usize]
                    } else {
                        [" ", "▔", "▘", "▘", "▀", "▀", "▛", "▛", "█"][(avg_lum * 8.999) as usize]
                    }
                }
            };

            (
                text,
                Colors {
                    foreground: None,
                    background: None,
                },
            )
        }
        (CharacterMode::Split | CharacterMode::Shapes, _) => {
            // Split with colors always divides the character cell into an upper and
            // lower half. Shapes mode currently falls back to that too.
            let [[&(_, color1)], [&(_, color2)]] = image.get_patch(char_pos);
            let color1 = options.colors.convert(color1);
            let color2 = options.colors.convert(color2);
            if color1 == color2 {
                (
                    // TODO: Offer choice of showing character sometimes. Also use characters for dithering.
                    " ", // text.as_str(),
                    Colors {
                        // Emit color at all only if we're not doing no-colors
                        foreground: color1.map(|_| Color::Black),
                        background: color1,
                    },
                )
            } else {
                (
                    "▄",
                    Colors {
                        foreground: color2,
                        background: color1,
                    },
                )
            }
        }
        (CharacterMode::Names, _) => {
            // Names mode always uses the text and the mapped color straight.
            let [[&(ref text, color)]] = image.get_patch(char_pos);
            let mapped_color = match options.colors.convert(color) {
                Some(color) => Colors::new(Color::Black, color),
                None => Colors::new(Color::Reset, Color::Reset),
            };
            (text, mapped_color)
        }
        (CharacterMode::Shades, _) => {
            // Shades mode always presents grayscale color
            let [[&(_, color)]] = image.get_patch(char_pos);
            (
                [" ", "░", "▒", "▓", "█"]
                    [(color.unwrap_or(Rgba::TRANSPARENT).luminance() * 4.999) as usize],
                Colors::new(Color::Reset, Color::Reset),
            )
        }
        (CharacterMode::Braille, _) => {
            let braille_dot_bit = |(_, color): &TextAndColor, number_z: usize| -> usize {
                if let Some(color) = color {
                    let threshold = ((number_z as f32
                        + char_pos.to_f32().dot(Vector2D::new(284.1834, 100.2384)))
                    .rem_euclid(8.0)
                        + 0.5)
                        / 8.0;
                    usize::from(color.luminance() > threshold) << number_z
                } else {
                    0
                }
            };
            fn crgb((_, color): &TextAndColor) -> Rgb {
                color.unwrap_or(Rgba::TRANSPARENT).to_rgb()
            }

            let [[one, four], [two, five], [three, six], [seven, eight]] =
                image.get_patch(char_pos);
            // The numbering of Braille dots goes
            // (1) (4)
            // (2) (5)
            // (3) (6)
            // (7) (8)
            // and the binary encoding numbers bits the same.
            // <https://en.wikipedia.org/wiki/Braille_Patterns?oldid=1074057238#Identifying,_naming_and_ordering>
            let braille_index = braille_dot_bit(one, 0)
                + braille_dot_bit(two, 1)
                + braille_dot_bit(three, 2)
                + braille_dot_bit(four, 3)
                + braille_dot_bit(five, 4)
                + braille_dot_bit(six, 5)
                + braille_dot_bit(seven, 6)
                + braille_dot_bit(eight, 7);

            let color_sum = crgb(one)
                + crgb(two)
                + crgb(three)
                + crgb(four)
                + crgb(five)
                + crgb(six)
                + crgb(seven)
                + crgb(eight);

            let hue = color_sum * (0.75 / color_sum.luminance().max(0.1));

            (
                BRAILLE_TABLE[braille_index],
                match options.colors.convert(Some(hue.with_alpha_one())) {
                    Some(color) => Colors::new(color, Color::Black),
                    None => Colors::new(Color::Reset, Color::Reset),
                },
            )
        }
    }
}

/// This is all of the Braille Unicode characters, as individual `&str`s.
#[rustfmt::skip]
static BRAILLE_TABLE: [&str; 256] = [
    "⠀","⠁","⠂","⠃","⠄","⠅","⠆","⠇","⠈","⠉","⠊","⠋","⠌","⠍","⠎","⠏",
    "⠐","⠑","⠒","⠓","⠔","⠕","⠖","⠗","⠘","⠙","⠚","⠛","⠜","⠝","⠞","⠟",
    "⠠","⠡","⠢","⠣","⠤","⠥","⠦","⠧","⠨","⠩","⠪","⠫","⠬","⠭","⠮","⠯",
    "⠰","⠱","⠲","⠳","⠴","⠵","⠶","⠷","⠸","⠹","⠺","⠻","⠼","⠽","⠾","⠿",
    "⡀","⡁","⡂","⡃","⡄","⡅","⡆","⡇","⡈","⡉","⡊","⡋","⡌","⡍","⡎","⡏",
    "⡐","⡑","⡒","⡓","⡔","⡕","⡖","⡗","⡘","⡙","⡚","⡛","⡜","⡝","⡞","⡟",
    "⡠","⡡","⡢","⡣","⡤","⡥","⡦","⡧","⡨","⡩","⡪","⡫","⡬","⡭","⡮","⡯",
    "⡰","⡱","⡲","⡳","⡴","⡵","⡶","⡷","⡸","⡹","⡺","⡻","⡼","⡽","⡾","⡿",
    "⢀","⢁","⢂","⢃","⢄","⢅","⢆","⢇","⢈","⢉","⢊","⢋","⢌","⢍","⢎","⢏",
    "⢐","⢑","⢒","⢓","⢔","⢕","⢖","⢗","⢘","⢙","⢚","⢛","⢜","⢝","⢞","⢟",
    "⢠","⢡","⢢","⢣","⢤","⢥","⢦","⢧","⢨","⢩","⢪","⢫","⢬","⢭","⢮","⢯",
    "⢰","⢱","⢲","⢳","⢴","⢵","⢶","⢷","⢸","⢹","⢺","⢻","⢼","⢽","⢾","⢿",
    "⣀","⣁","⣂","⣃","⣄","⣅","⣆","⣇","⣈","⣉","⣊","⣋","⣌","⣍","⣎","⣏",
    "⣐","⣑","⣒","⣓","⣔","⣕","⣖","⣗","⣘","⣙","⣚","⣛","⣜","⣝","⣞","⣟",
    "⣠","⣡","⣢","⣣","⣤","⣥","⣦","⣧","⣨","⣩","⣪","⣫","⣬","⣭","⣮","⣯",
    "⣰","⣱","⣲","⣳","⣴","⣵","⣶","⣷","⣸","⣹","⣺","⣻","⣼","⣽","⣾","⣿",
];

pub(crate) fn write_colored_and_measure<B: Backend + io::Write>(
    backend: &mut B,
    has_terminal_stdin: bool,
    width_table: &mut HashMap<String, u16>,
    current_color: &mut Option<Colors>,
    wanted_color: Colors,
    text: &str,
    max_width: u16,
) -> Result<u16, io::Error> {
    if *current_color != Some(wanted_color) {
        *current_color = Some(wanted_color);
        backend.queue(SetColors(wanted_color))?;
    }
    write_and_measure(backend, has_terminal_stdin, width_table, text, max_width)
}

/// Write a string and report how far this advanced the cursor,
/// unless the string was previously discovered to advance it more than `max_width`.
///
/// `width_table` is used to memoize the previously measured widths. Because of this,
/// strings should be kept short enough to be repetitive (e.g. single characters).
///
/// Returns an error if the string could not be written. If an error was encountered
/// measuring the width, returns an estimate instead.
fn write_and_measure<B: Backend + io::Write>(
    backend: &mut B,
    has_terminal_stdin: bool,
    width_table: &mut HashMap<String, u16>,
    text: &str,
    max_width: u16,
) -> Result<u16, io::Error> {
    if text.len() == 1 && (b' '..=b'~').contains(&text.as_bytes()[0]) {
        // Hardcode the ASCII case.
        if 1 <= max_width {
            backend.write_all(text.as_bytes())?;
        }
        Ok(1)
    } else if let Some(&w) = width_table.get(text) {
        // Use and report already-computed width.
        if w <= max_width {
            backend.write_all(text.as_bytes())?;
        }
        Ok(w)
    } else if !has_terminal_stdin {
        backend.write_all(text.as_bytes())?;
        Ok(fallback_measure_str(text))
    } else {
        let before = backend.get_cursor_position();
        backend.write_all(text.as_bytes())?;
        let after = backend.get_cursor_position();

        // Compute width from cursor position, if available.
        match (before, after) {
            (
                Ok(Position {
                    x: before_x,
                    y: before_y,
                }),
                Ok(Position {
                    x: after_x,
                    y: after_y,
                }),
            ) => {
                if before_y == after_y {
                    match after_x.checked_sub(before_x) {
                        Some(width) => {
                            width_table.insert(text.to_owned(), width);
                            Ok(width)
                        }
                        None => {
                            // Cursor moved leftward. Did we print a backspace, perhaps? Or did we
                            // end up on the last line and provoke scrolling? No way to recover
                            // information from this.
                            // (TODO: Add a text filter that prevents reaching this case by avoiding
                            // printing control characters.)
                            Ok(fallback_measure_str(text))
                        }
                    }
                } else {
                    // The character caused moving to a new line, perhaps because we incorrectly
                    // assumed its width was not greater than 1 and it was on the right edge.
                    Ok(fallback_measure_str(text))
                }
            }
            (_, Err(_)) | (Err(_), _) => {
                // Ignore IO error, which might be a timeout inside get_cursor ... such as due to
                // high response latency due to running this raytracer on a slow system.
                Ok(fallback_measure_str(text))
            }
        }
    }
}

fn fallback_measure_str(text: &str) -> u16 {
    unicode_width::UnicodeWidthStr::width(text)
        .try_into()
        .unwrap_or(u16::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::terminal::TerminalOptions;
    use all_is_cubes::euclid::size2;
    use all_is_cubes_render::camera::{ImageSize, Viewport, area_usize};
    use all_is_cubes_render::raytracer::RaytraceInfo;

    fn test_image(
        options: TerminalOptions,
        dimensions: ImageSize,
        characters: impl IntoIterator<Item = char>,
        colors: impl IntoIterator<Item = Rgba>,
    ) -> TextRayImage {
        let image: Vec<TextAndColor> = characters
            .into_iter()
            .map(|c| c.to_string())
            .zip(colors.into_iter().map(Some))
            .collect();
        assert_eq!(area_usize(dimensions), Some(image.len()));
        TextRayImage {
            viewport: Viewport::with_scale(1.0, dimensions), // scale is ignored
            options,
            image,
            info: RaytraceInfo::default(),
        }
    }

    /// Get one text row of the image as processed by [`image_patch_to_character`],
    /// ignoring colors.
    fn get_row(image: &TextRayImage, y: usize) -> String {
        //let reset = Colors::new(Color::Reset, Color::Reset);
        (0..image.viewport.framebuffer_size.width as usize)
            .map(|x| {
                let (text, _color) = image_patch_to_character(image, Vector2D::new(x, y));
                //assert_eq!(color, reset);
                text
            })
            .collect()
    }

    fn mode_no_color(characters: CharacterMode) -> TerminalOptions {
        TerminalOptions {
            colors: ColorMode::None,
            characters,
        }
    }

    #[test]
    fn char_mode_shades() {
        let colors: [Rgba; 10] = [0.0, 0.9, 1.1, 1.9, 2.1, 2.9, 3.1, 3.9, 4.1, 5.0]
            .map(|v| Rgba::from_luminance(v / 5.0));
        let image = &test_image(
            mode_no_color(CharacterMode::Shades),
            size2(colors.len() as u32, 1),
            std::iter::repeat('*'),
            colors,
        );
        assert_eq!(get_row(image, 0), "  ░░▒▒▓▓██");
    }

    #[test]
    fn char_mode_split_same_luminance() {
        // TODO: copy-pasted; pick break points more appropriate to this test
        let colors = [0.0, 0.9, 1.1, 1.9, 2.1, 2.9, 3.1, 3.9, 4.1, 5.0]
            .map(|v| Rgba::from_luminance(v / 5.0));
        let image = &test_image(
            mode_no_color(CharacterMode::Split),
            size2(colors.len() as u32, 2),
            std::iter::repeat('*'),
            colors.into_iter().chain(colors), // 2 rows of same color
        );
        assert_eq!(get_row(image, 0), "  ***▄▀███");
    }

    /// Shapes is the same as Shades when top and bottom are the same
    #[test]
    fn char_mode_shapes_same_luminance() {
        let colors = [0.0, 0.9, 1.1, 1.9, 2.1, 2.9, 3.1, 3.9, 4.1, 5.0]
            .map(|v| Rgba::from_luminance(v / 5.0));
        let image = &test_image(
            mode_no_color(CharacterMode::Shapes),
            size2(colors.len() as u32, 2),
            std::iter::repeat('*'),
            colors.into_iter().chain(colors), // 2 rows of same color
        );
        assert_eq!(get_row(image, 0), "  ░░▒▒▓▓██");
    }

    #[test]
    fn char_mode_split_upper() {
        // TODO: copy-pasted; pick break points more appropriate to this test
        let colors: [Rgba; 10] = [0.0, 0.9, 1.1, 1.9, 2.1, 2.9, 3.1, 3.9, 4.1, 5.0]
            .map(|v| Rgba::from_luminance(v / 5.0));
        let image = &test_image(
            mode_no_color(CharacterMode::Shapes),
            size2(colors.len() as u32, 2),
            std::iter::repeat('*'),
            [Rgba::BLACK; 10].into_iter().chain(colors), // black above ramp
        );
        // TODO: test the rest of the split by ramping to white-white
        assert_eq!(get_row(image, 0), "   ▁▁▂▂▃▃▄");
    }
}

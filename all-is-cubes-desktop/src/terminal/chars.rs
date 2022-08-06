//! Details of writing colored characters to the terminal.

use std::collections::HashMap;
use std::io;

use crossterm::style::{Color, Colors, SetColors};
use crossterm::QueueableCommand as _;

use all_is_cubes::cgmath::Vector2;
use all_is_cubes::math::Rgba;

use super::options::{CharacterMode, ColorMode};
use super::TextRayImage;

pub(super) fn image_patch_to_character(
    image: &TextRayImage,
    char_pos: Vector2<usize>,
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
                CharacterMode::Names | CharacterMode::Shades => unreachable!(),
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
    }
}

pub(crate) fn write_colored_and_measure<B: tui::backend::Backend + io::Write>(
    backend: &mut B,
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
    write_and_measure(backend, width_table, text, max_width)
}

/// Write a string and report how far this advanced the cursor,
/// unless the string was previously discovered to advance it more than `max_width`.
///
/// `width_table` is used to memoize the previously measured widths. Because of this,
/// strings should be kept short enough to be repetitive (e.g. single characters).
///
/// Returns an error if the string could not be written. If an error was encountered
/// measuring the width, returns an estimate instead.
fn write_and_measure<B: tui::backend::Backend + io::Write>(
    backend: &mut B,
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
    } else {
        let before = backend.get_cursor();
        backend.write_all(text.as_bytes())?;
        let after = backend.get_cursor();

        // Compute width from cursor position, if available.
        match (before, after) {
            (Ok((before_x, before_y)), Ok((after_x, after_y))) => {
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

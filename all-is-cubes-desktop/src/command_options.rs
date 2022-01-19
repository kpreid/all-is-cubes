// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Command line option parsing.

use std::fmt::Write as _;
use std::path::PathBuf;
use std::time::Duration;

use clap::{Arg, ArgEnum, ErrorKind};
use once_cell::sync::Lazy;
use strum::IntoEnumIterator;

use all_is_cubes::cgmath::Vector2;
use all_is_cubes_content::UniverseTemplate;

use crate::record::{RecordAnimationOptions, RecordOptions};
use crate::TITLE;

pub fn app() -> clap::App<'static> {
    clap::App::new(TITLE)
        .version(clap::crate_version!()) // TODO: include all_is_cubes library version
        .author(clap::crate_authors!())
        .about(clap::crate_description!())
        .arg(
            Arg::new("graphics")
                .long("graphics")
                .short('g')
                .possible_values(
                    GraphicsType::value_variants()
                        .iter()
                        .filter_map(|v| v.to_possible_value()),
                )
                .default_value("window")
                .value_name("MODE")
                .hide_possible_values(true)
                .help(&**GRAPHICS_HELP),
        )
        .arg(
            Arg::new("display_size")
                .long("display-size")
                .value_name("W×H")
                .default_value("auto")
                .validator(|s| parse_dimensions(s).map(|_| ()))
                .help("Window size or image size, if applicable to the selected --graphics mode."),
        )
        .arg(
            Arg::new("template")
                .long("template")
                .short('t')
                .possible_values(UniverseTemplate::iter().map(<&str>::from))
                .default_value("demo-city")
                .help(
                    "Which world template to use.\n\
                    Mutually exclusive with specifying an input file.",
                ),
        )
        .arg(
            Arg::new("precompute_light")
                .long("precompute-light")
                .help("Fully calculate light before starting the game."),
        )
        .arg(
            Arg::new("output_file")
                .allow_invalid_utf8(true)
                .long("output")
                .short('o')
                .required_if_eq("graphics", "record")
                .value_name("FILE")
                .help(
                    "Output PNG file name for 'record' mode. \
                    If animating, a frame number will be inserted.",
                ),
        )
        .arg(
            Arg::new("duration")
                .long("duration")
                .value_name("SECONDS")
                // TODO: Generalize this to "exit after this much time has passed".
                .help("Time to record video, in 'record' mode only (omit for still image)."),
        )
        .arg(
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .help("Additional logging to stderr."),
        )
        .arg(
            Arg::new("no_config_files").long("no-config-files").help(
                "Ignore all configuration files, using only defaults and command-line options.",
            ),
        )
        .arg(
            Arg::new("input_file")
                .allow_invalid_utf8(true)
                .value_name("FILE")
                .conflicts_with("template")
                .help(
                    "Existing save/document file to load. \
                    Mutually exclusive with --template. \
                    Currently supported formats:\n\
                    • MagicaVoxel .vox (partial support)",
                ),
        )
}

/// clap doesn't automatically compile the possible value help
/// (<https://github.com/clap-rs/clap/issues/3312>), so do it ourselves.
/// This is in a static so that it can become an `&'static str`.
static GRAPHICS_HELP: Lazy<String> = Lazy::new(|| {
    let pv_iter = GraphicsType::value_variants()
        .iter()
        .filter_map(|v| v.to_possible_value());

    let max_width = pv_iter
        .clone()
        .filter_map(|pv| pv.get_visible_name())
        .map(str::len)
        .max()
        .unwrap_or(0);

    let mut text = String::from("Graphics/UI mode; one of the following keywords:\n");
    for pv in pv_iter {
        // Note: There's a final newline so that clap's default value text is put on a new line.
        writeln!(
            text,
            "• {:max_width$} — {}",
            pv.get_name(),
            pv.get_help().unwrap()
        )
        .unwrap();
    }
    text
});

#[derive(
    Clone, Copy, Debug, Eq, PartialEq, clap::ArgEnum, strum::EnumString, strum::IntoStaticStr,
)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub enum GraphicsType {
    // These variants are sorted for the benefit of the help text presentation.
    #[clap(help = "Open a window (uses OpenGL)")]
    Window,
    #[clap(help = "Colored text in this terminal (uses raytracing)")]
    Terminal,
    #[clap(help = "Non-interactive; don't draw anything but only simulates")]
    Headless,
    #[clap(help = "Non-interactive; save an image or video (uses raytracing)")]
    Record,
    #[clap(help = "Non-interactive; print one frame like 'terminal' mode then exit")]
    Print,
}

pub fn parse_dimensions(input: &str) -> Result<Option<Vector2<u32>>, String> {
    if input.to_ascii_lowercase() == "auto" {
        Ok(None)
    } else {
        let dims: [u32; 2] = input
            .split(&['×', 'x', ',', ';', ' '][..])
            .map(|s| {
                s.parse::<u32>()
                    .map_err(|_| format!("{:?} not an integer or \"auto\"", s))
            })
            .collect::<Result<Vec<u32>, String>>()?
            .try_into()
            .map_err(|_| String::from("must be two integers or \"auto\""))?;
        Ok(Some(Vector2::from(dims)))
    }
}

/// Source of the universe to create/load
///
/// TODO: we will eventually want to support new/open while running and this will
/// no longer be about command line exactly, so it should move.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum UniverseSource {
    Template(UniverseTemplate),
    File(PathBuf),
}

pub fn parse_record_options(
    options: clap::ArgMatches,
    display_size: Option<Vector2<u32>>,
) -> Result<RecordOptions, clap::Error> {
    Ok(RecordOptions {
        output_path: PathBuf::from(options.value_of_os("output_file").unwrap()),
        image_size: display_size.unwrap_or_else(|| Vector2::new(640, 480)),
        animation: match options.value_of_t::<f64>("duration") {
            Ok(duration) => {
                let frame_rate = 60.0;
                Some(RecordAnimationOptions {
                    frame_count: ((duration * frame_rate).round() as usize).max(1),
                    frame_period: Duration::from_nanos((1e9 / frame_rate) as u64),
                })
            }
            Err(clap::Error {
                kind: ErrorKind::ArgumentNotFound,
                ..
            }) => None,
            Err(e) => return Err(e),
        },
    })
}

pub(crate) fn parse_universe_source(
    options: &clap::ArgMatches,
) -> Result<UniverseSource, clap::Error> {
    if let Some(file) = options.value_of_os("input_file") {
        Ok(UniverseSource::File(PathBuf::from(file)))
    } else {
        Ok(UniverseSource::Template(options.value_of_t("template")?))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_record_options() {
        fn parse(args: &[&str]) -> clap::Result<RecordOptions> {
            parse_record_options(
                app().try_get_matches_from(
                    std::iter::once("all-is-cubes").chain(args.iter().cloned()),
                )?,
                None,
            )
        }

        assert_eq!(
            parse(&["-g", "record", "-o", "output.png"]).unwrap(),
            RecordOptions {
                output_path: PathBuf::from("output.png"),
                image_size: Vector2::new(640, 480),
                animation: None,
            },
        );
        assert_eq!(
            parse(&["-g", "record", "-o", "fancy.png", "--duration", "3"]).unwrap(),
            RecordOptions {
                output_path: PathBuf::from("fancy.png"),
                image_size: Vector2::new(640, 480),
                animation: Some(RecordAnimationOptions {
                    frame_count: 180,
                    frame_period: Duration::from_nanos((1e9 / 60.0) as u64),
                }),
            },
        );

        // TODO: exercise display size, perhaps with better data flow

        // Check expected errors. TODO: Better contains-string assert
        // No output file
        assert!(parse(&["-g", "record"])
            .unwrap_err()
            .to_string()
            .contains("required arguments"));
        assert!(parse(&["-g", "record", "-o", "o.png", "--duration", "X"])
            .unwrap_err()
            .to_string()
            .contains("'X' isn't"));
    }

    fn parse_universe_test(args: &[&str]) -> clap::Result<UniverseSource> {
        parse_universe_source(
            &app().try_get_matches_from(
                std::iter::once("all-is-cubes").chain(args.iter().cloned()),
            )?,
        )
    }

    #[test]
    fn universe_default() {
        assert_eq!(
            parse_universe_test(&[]).unwrap(),
            UniverseSource::Template(UniverseTemplate::DemoCity),
        );
    }

    #[test]
    fn universe_from_template() {
        assert_eq!(
            parse_universe_test(&["--template", "cornell-box"]).unwrap(),
            UniverseSource::Template(UniverseTemplate::CornellBox),
        );
    }

    #[test]
    fn universe_from_file() {
        assert_eq!(
            parse_universe_test(&["foo/bar"]).unwrap(),
            UniverseSource::File(PathBuf::from("foo/bar")),
        );
    }

    #[test]
    fn universe_option_conflict() {
        assert_eq!(
            parse_universe_test(&["--template", "demo-city", "foo"])
                .unwrap_err()
                .kind,
            clap::ErrorKind::ArgumentConflict
        );
    }

    #[test]
    fn universe_option_invalid_template() {
        assert_eq!(
            parse_universe_test(&["--template", "foo"])
                .unwrap_err()
                .kind,
            clap::ErrorKind::InvalidValue
        );
    }

    #[test]
    fn test_parse_dimensions() {
        let err = |s: &str| Err(s.to_owned());
        assert_eq!(parse_dimensions("1,2"), Ok(Some(Vector2::new(1, 2))));
        assert_eq!(parse_dimensions("30x93"), Ok(Some(Vector2::new(30, 93))));
        assert_eq!(parse_dimensions("30×93"), Ok(Some(Vector2::new(30, 93))));
        assert_eq!(parse_dimensions(""), err("\"\" not an integer or \"auto\""));
        assert_eq!(
            parse_dimensions("1"),
            err("must be two integers or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("a"),
            err("\"a\" not an integer or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("1a1"),
            err("\"1a1\" not an integer or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("1×1×1"),
            err("must be two integers or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("a×b"),
            err("\"a\" not an integer or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("1×b"),
            err("\"b\" not an integer or \"auto\"")
        );
    }
}

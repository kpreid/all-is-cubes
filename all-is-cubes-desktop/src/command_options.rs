// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Command line option parsing.

use std::path::PathBuf;
use std::time::Duration;

use clap::{value_t, Arg, ErrorKind};
use strum::IntoEnumIterator;

use all_is_cubes::cgmath::Vector2;
use all_is_cubes_content::UniverseTemplate;

use crate::record::{RecordAnimationOptions, RecordOptions};
use crate::TITLE;

pub fn app() -> clap::App<'static, 'static> {
    clap::App::new(TITLE)
        .version(clap::crate_version!()) // TODO: include all_is_cubes library version
        .author(clap::crate_authors!())
        .about(clap::crate_description!())
        .arg(
            Arg::with_name("graphics")
                .long("graphics")
                .short("g")
                .possible_values(&GraphicsType::iter().map(<&str>::from).collect::<Vec<_>>())
                .default_value("window")
                .value_name("MODE")
                .hide_possible_values(true)
                .help(
                    "Graphics/UI mode; one of the following keywords:\n\
                    window   - Open a window (uses OpenGL)\n\
                    terminal - Colored text in this terminal (uses raytracing)\n\
                    headless - Non-interactive; don't draw anything but only simulates\n\
                    record   - Non-interactive; save an image or video (uses raytracing)\n\
                    print    - Non-interactive; print one frame like 'terminal' mode then exit\
                    ",
                ),
        )
        .arg(
            Arg::with_name("display_size")
                .long("display-size")
                .value_name("W×H")
                .default_value("auto")
                .validator(|s| parse_dimensions(&s).map(|_| ()))
                .help("Window size or image size, if applicable to the selected --graphics mode."),
        )
        .arg(
            Arg::with_name("template")
                .long("template")
                .short("t")
                .possible_values(
                    &UniverseTemplate::iter()
                        .map(<&str>::from)
                        .collect::<Vec<_>>(),
                )
                .default_value("demo-city")
                .help("Which world template to use."),
        )
        .arg(
            Arg::with_name("precompute_light")
                .long("precompute-light")
                .help("Fully calculate light before starting the game."),
        )
        .arg(
            Arg::with_name("output_file")
                .long("output")
                .short("o")
                .required_if("graphics", "record")
                .value_name("FILE")
                .help(
                    "Output PNG file name for 'record' mode. \
                    If animating, a frame number will be inserted.",
                ),
        )
        .arg(
            Arg::with_name("duration")
                .long("duration")
                .value_name("SECONDS")
                // TODO: Generalize this to "exit after this much time has passed".
                .help("Time to record video, in 'record' mode only (omit for still image)."),
        )
        .arg(
            Arg::with_name("verbose")
                .long("verbose")
                .short("v")
                .help("Additional logging to stderr."),
        )
        .arg(
            Arg::with_name("no_config_files")
                .long("no-config-files")
                .help(
                    "Ignore all configuration files, using only defaults and command-line options.",
                ),
        )
}

#[derive(Debug, PartialEq, strum::EnumString, strum::EnumIter, strum::IntoStaticStr)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub enum GraphicsType {
    Headless,
    Window,
    Terminal,
    Record,
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

pub fn parse_record_options(
    options: clap::ArgMatches<'_>,
    display_size: Option<Vector2<u32>>,
) -> Result<RecordOptions, clap::Error> {
    Ok(RecordOptions {
        output_path: PathBuf::from(options.value_of_os("output_file").unwrap()),
        image_size: display_size.unwrap_or_else(|| Vector2::new(640, 480)),
        animation: match value_t!(options, "duration", f64) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_record_options() {
        fn parse(args: &[&str]) -> clap::Result<RecordOptions> {
            parse_record_options(
                app().get_matches_from_safe(
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

// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Command line option parsing.

use std::ffi::OsStr;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time::Duration;

use clap::{ArgEnum, Parser};
use once_cell::sync::Lazy;
use strum::IntoEnumIterator;

use all_is_cubes::cgmath::Vector2;
use all_is_cubes_content::UniverseTemplate;

use crate::record::{RecordAnimationOptions, RecordFormat, RecordOptions};
use crate::TITLE;

#[derive(Clone, Debug, Parser)]
#[clap(name = TITLE, author, about, version)]
pub(crate) struct AicDesktopArgs {
    #[clap(
        long = "graphics",
        short = 'g',
        default_value = "window",
        value_name = "mode",
        hide_possible_values = true,
        help = "Graphics/UI mode",
        long_help = &**GRAPHICS_HELP_LONG,
    )]
    pub(crate) graphics: GraphicsType,

    /// Window size or image size, if applicable to the selected --graphics mode.
    #[clap(long = "display-size", value_name = "W×H", default_value = "auto")]
    pub(crate) display_size: DisplaySizeArg,

    /// Which world template to use.
    ///
    /// Mutually exclusive with specifying an input file.
    #[clap(
        long = "template",
        short = 't',
        default_value = "demo-city",
        possible_values = UniverseTemplate::iter().map(<&'static str>::from).collect::<Vec<&'static str>>(),
    )]
    pub(crate) template: UniverseTemplate,

    /// Fully calculate light before starting the game.
    #[clap(long = "precompute-light")]
    pub(crate) precompute_light: bool,

    /// Output file name for 'record' mode.
    ///
    /// The file name must have an extension specifying the type; currently only PNG is supported
    /// ('.png' or '.apng').
    #[clap(
        long = "output",
        short = 'o',
        required_if_eq("graphics", "record"),
        value_name = "FILE",
        parse(from_os_str),
        validator_os = validate_output_file,
    )]
    pub(crate) output_file: Option<PathBuf>,

    // TODO: Generalize this to "exit after this much time has passed".
    /// Length of time to simulate.
    ///
    /// * In 'record' mode, sets duration of video (or still image if absent).
    /// * In 'headless' mode, sets a time to exit rather than running infinitely.
    /// * In all other modes, does nothing.
    #[clap(long = "duration", value_name = "SECONDS", verbatim_doc_comment)]
    pub(crate) duration: Option<f64>,

    /// Additional logging to stderr.
    #[clap(long = "verbose", short = 'v')]
    pub(crate) verbose: bool,

    /// Ignore all configuration files, using only defaults and command-line options.
    #[clap(long = "no-config-files")]
    pub(crate) no_config_files: bool,

    /// Existing save/document file to load. If not specified, a template will be used
    /// instead.
    ///
    /// Currently supported formats:
    ///
    /// * MagicaVoxel .vox (partial support)
    #[clap(conflicts_with = "template", value_name = "FILE")]
    pub(crate) input_file: Option<PathBuf>,
}

impl AicDesktopArgs {
    /// Construct [`RecordOptions`].
    ///
    /// Returns an error if options were inconsistent with each other.
    ///
    /// Panics if `output_path` is not set or validated (this should have been checked at parse time).
    pub fn record_options(&self) -> clap::Result<RecordOptions> {
        let output_path = self
            .output_file
            .clone()
            .expect("output_file should be present");
        let output_format = determine_record_format(&output_path)
            .expect("output_file should have been validated to specify a format");
        Ok(RecordOptions {
            output_path: self.output_file.clone().unwrap(),
            output_format,
            image_size: self
                .display_size
                .0
                .unwrap_or_else(|| Vector2::new(640, 480)),
            animation: match self.duration {
                Some(duration) => {
                    let frame_rate = 60.0;
                    Some(RecordAnimationOptions {
                        frame_count: ((duration * frame_rate).round() as usize).max(1),
                        frame_period: Duration::from_nanos((1e9 / frame_rate) as u64),
                    })
                }
                None => None,
            },
        })
    }
}

/// clap doesn't automatically compile the possible value help
/// (<https://github.com/clap-rs/clap/issues/3312>), so do it ourselves.
/// This is in a static so that it can become an `&'static str`.
static GRAPHICS_HELP_LONG: Lazy<String> = Lazy::new(|| {
    let pv_iter = GraphicsType::value_variants()
        .iter()
        .filter_map(|v| v.to_possible_value())
        .filter(|pv| !pv.is_hide_set());

    let max_width = pv_iter
        .clone()
        .map(|pv| pv.get_name())
        .map(str::len)
        .max()
        .unwrap_or(0);

    let mut text = String::from("Graphics/UI mode; one of the following keywords:\n");
    for pv in pv_iter {
        write!(
            text,
            "\n* {:max_width$} — {}",
            pv.get_name(),
            pv.get_help().unwrap_or_default()
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
    #[clap(help = "EXPERIMENTAL: Open a window (uses `wgpu` for GPU)")]
    WindowWgpu,
    #[clap(help = "EXPERIMENTAL: Open a window (uses CPU raytracing)")]
    WindowRt,
    #[clap(help = "Colored text in this terminal (uses raytracing)")]
    Terminal,
    #[clap(help = "Non-interactive; don't draw anything but only simulates")]
    Headless,
    #[clap(help = "Non-interactive; save an image or video (uses raytracing)")]
    Record,
    #[clap(help = "Non-interactive; print one frame like 'terminal' mode then exit")]
    Print,
}

/// This is just to hide the `Option` from `clap` because we don't want it to mean optional-argument.
#[derive(Clone, Copy, Debug)]
pub(crate) struct DisplaySizeArg(pub Option<Vector2<u32>>);

impl FromStr for DisplaySizeArg {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_dimensions(s).map(DisplaySizeArg)
    }
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

pub fn determine_record_format(output_path: &Path) -> Result<RecordFormat, &'static str> {
    if let Some(extension) = output_path.extension() {
        match extension.to_str() {
            // When updating this match, also update the docs for output_file!
            Some("png" | "PNG") => return Ok(RecordFormat::PngOrApng),
            Some("apng" | "APNG") => return Ok(RecordFormat::PngOrApng),
            Some("gltf" | "GLTF") => return Ok(RecordFormat::Gltf),
            _ => {}
        }
    }
    // TODO: Have a separate option for choosing file type as a fallback
    Err("file name must have an extension specifying the type; one of 'png', 'apng', or 'gltf'")
}

fn validate_output_file(path_str: &OsStr) -> Result<(), &'static str> {
    determine_record_format(Path::new(path_str)).map(|_| ())
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

// TODO: express the inputs here as a sub-struct of AicDesktopArgs
pub(crate) fn parse_universe_source(
    input_file: Option<PathBuf>,
    template: UniverseTemplate,
) -> UniverseSource {
    if let Some(file) = input_file {
        UniverseSource::File(file)
    } else {
        UniverseSource::Template(template)
    }
}

#[cfg(test)]
mod tests {
    use clap::error::ContextValue;

    use super::*;

    fn parse(args: &[&str]) -> clap::Result<AicDesktopArgs> {
        AicDesktopArgs::try_parse_from(std::iter::once("all-is-cubes").chain(args.iter().cloned()))
    }

    #[test]
    fn record_options_image() {
        assert_eq!(
            parse(&["-g", "record", "-o", "output.png"])
                .unwrap()
                .record_options()
                .unwrap(),
            RecordOptions {
                output_path: PathBuf::from("output.png"),
                output_format: RecordFormat::PngOrApng,
                image_size: Vector2::new(640, 480),
                animation: None,
            },
        );
    }
    #[test]
    fn record_options_animation() {
        assert_eq!(
            parse(&["-g", "record", "-o", "fancy.png", "--duration", "3"])
                .unwrap()
                .record_options()
                .unwrap(),
            RecordOptions {
                output_path: PathBuf::from("fancy.png"),
                output_format: RecordFormat::PngOrApng,
                image_size: Vector2::new(640, 480),
                animation: Some(RecordAnimationOptions {
                    frame_count: 180,
                    frame_period: Duration::from_nanos((1e9 / 60.0) as u64),
                }),
            },
        );
    }

    // TODO: exercise record display size

    #[test]
    fn record_options_missing_file() {
        let e = parse(&["-g", "record"]).unwrap_err();
        assert_eq!(e.kind(), clap::ErrorKind::MissingRequiredArgument);
        assert_eq!(
            e.context()
                .find(|&(k, _)| k == clap::error::ContextKind::InvalidArg),
            Some((
                clap::error::ContextKind::InvalidArg,
                &ContextValue::Strings(vec![String::from("--output <FILE>")])
            ))
        );
    }

    #[test]
    fn record_options_invalid_duration() {
        let e = parse(&["-g", "record", "-o", "o.png", "--duration", "X"]).unwrap_err();
        assert_eq!(e.kind(), clap::ErrorKind::ValueValidation);
        assert_eq!(
            e.context()
                .find(|&(k, _)| k == clap::error::ContextKind::InvalidArg),
            Some((
                clap::error::ContextKind::InvalidArg,
                &ContextValue::String(String::from("--duration <SECONDS>"))
            ))
        );
    }

    fn parse_universe_test(args: &[&str]) -> clap::Result<UniverseSource> {
        let AicDesktopArgs {
            template,
            input_file,
            ..
        } = parse(args)?;
        // TODO: make this a method on AicDesktopArgs
        Ok(parse_universe_source(input_file, template))
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
                .kind(),
            clap::ErrorKind::ArgumentConflict
        );
    }

    #[test]
    fn universe_option_invalid_template() {
        assert_eq!(
            parse_universe_test(&["--template", "foo"])
                .unwrap_err()
                .kind(),
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

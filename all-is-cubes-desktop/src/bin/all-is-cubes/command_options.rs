//! Command line option parsing.

use std::fmt::Write as _;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::LazyLock;
#[allow(unused_imports)]
use std::time::Duration;

use clap::builder::{PossibleValue, PossibleValuesParser, TypedValueParser};
use clap::{Parser, ValueEnum};
use strum::IntoEnumIterator;

use all_is_cubes::euclid::Size2D;
use all_is_cubes::math::{GridSize, GridSizeCoord};
use all_is_cubes_content::{TemplateParameters, UniverseTemplate};
use all_is_cubes_desktop::logging::LoggingArgs;
use all_is_cubes_render::camera;

use all_is_cubes_desktop::UniverseSource;
#[cfg(feature = "record")]
use all_is_cubes_desktop::record::{RecordAnimationOptions, RecordFormat, RecordOptions};

#[derive(Clone, Debug, Parser)]
#[command(
    name = crate::TITLE, author, about, version,
    next_display_order = None, // causes alphabetical sorting -- TODO: revisit
    help_template = "\
{name} {version}
{author}
{about-with-newline}
{usage-heading}
    {usage}

{all-args}{after-help}",
)]
pub(crate) struct AicDesktopArgs {
    #[arg(
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
    #[arg(long = "display-size", value_name = "W×H", default_value = "auto")]
    pub(crate) display_size: DisplaySizeArg,

    /// Make the window fullscreen.
    ///
    /// Causes --display-size to be ignored unless fullscreen is not possible.
    #[arg(long)]
    pub(crate) fullscreen: bool,

    /// Which world template to use.
    ///
    /// Mutually exclusive with specifying an input file.
    #[arg(
        long = "template",
        short = 't',
        default_value = "demo-city",
        value_parser = PossibleValuesParser::new(
            UniverseTemplate::iter().map(|t| {
                PossibleValue::new(<&str>::from(t.clone())).hide(!t.include_in_lists())
            }),
        ).map(|string| UniverseTemplate::from_str(&string).unwrap()),
    )]
    pub(crate) template: UniverseTemplate,

    /// Seed value for randomized components of the world template.
    ///
    /// May be an integer between 0 and 18446744073709551615 (2⁶⁴ - 1).
    ///
    /// If not specified, a randomly chosen seed will be used.
    #[arg(long = "seed")]
    pub(crate) seed: Option<u64>,

    /// Dimensions for the space the template generates.
    ///
    /// Not all templates support this option, and some may restrict the size to a
    /// minimum or multiples.
    ///
    /// If not specified, a template-specific default size will be used.
    #[arg(
        long = "template-size",
        value_name = "X,Y,Z",
        default_value = "default"
    )]
    pub(crate) template_size: SpaceSizeArg,

    /// Fully calculate light before starting the game.
    #[arg(long = "precompute-light")]
    pub(crate) precompute_light: bool,

    /// Output file name for 'record' mode.
    ///
    /// The file name must have an extension specifying the format to use:
    ///
    /// * “.alliscubesjson” — All is Cubes native save file format.
    /// * “.png” or “.apng” — export rendered scene.
    /// * “.gltf” — export scene as meshes in glTF format
    ///   (has accompanying “.glbin” data files).
    /// * “.vox” — export world to MagicaVoxel .vox format.
    #[cfg(feature = "record")]
    #[arg(
        long = "output",
        short = 'o',
        required_if_eq("graphics", "record"),
        value_name = "FILE",
        value_parser = clap::builder::PathBufValueParser::new().try_map(|value| {
            let format = determine_record_format(&value)?;
            Ok::<(PathBuf, RecordFormat), &str>((value, format))
        }),
        verbatim_doc_comment,
    )]
    pub(crate) output_file_and_format: Option<(PathBuf, RecordFormat)>,

    /// Whether to record/export everything, rather than just the displayed scene.
    #[cfg(feature = "record")]
    #[arg(long, requires = "output_file_and_format")]
    pub(crate) save_all: bool,

    // TODO: Generalize this to "exit after this much time has passed".
    /// Length of time to simulate.
    ///
    /// * In 'record' mode, sets duration of video (or still image if absent).
    /// * In 'headless' mode, sets a time to exit rather than running infinitely.
    /// * In all other modes, does nothing.
    #[arg(long = "duration", value_name = "SECONDS", verbatim_doc_comment)]
    pub(crate) duration: Option<f64>,

    #[command(flatten)]
    pub(crate) logging: LoggingArgs,

    /// Ignore all configuration files, using only defaults and command-line options.
    #[arg(long = "no-config-files")]
    pub(crate) no_config_files: bool,

    /// Existing save/document file to load. If not specified, a template will be used
    /// instead.
    ///
    /// Currently supported formats:
    ///
    /// * MagicaVoxel .vox (partial support)
    #[arg(
        conflicts_with = "template",
        conflicts_with = "template_size",
        conflicts_with = "seed",
        value_name = "FILE"
    )]
    pub(crate) input_file: Option<PathBuf>,
}

impl AicDesktopArgs {
    /// Construct [`RecordOptions`].
    ///
    /// Returns an error if options were inconsistent with each other.
    ///
    /// Returns `Ok(None)` if recording was not requested (`output_file_and_format` not set).
    ///
    /// Panics if `output_path` is not validated (this should have been checked at parse time).
    #[cfg(feature = "record")]
    #[allow(clippy::unnecessary_wraps, reason = "*currently* no error can happen")]
    pub fn record_options(&self) -> Result<Option<RecordOptions>, anyhow::Error> {
        let Some((output_path, output_format)) = self.output_file_and_format.clone() else {
            return Ok(None);
        };
        let options = RecordOptions {
            output_path,
            output_format,
            image_size: self
                .display_size
                .0
                .unwrap_or_else(|| Size2D::new(640, 480))
                .cast_unit(), // nominal = physical here
            save_all: self.save_all,
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
        };

        Ok(Some(options))
    }

    /// Whether the command arguments requested “headless” (no real-time UI) operation.
    ///
    /// Concretely, this is expected to imply not contacting audio or display devices.
    pub(crate) fn is_headless(&self) -> bool {
        self.graphics.is_headless()
    }
}

/// clap doesn't automatically compile the possible value help
/// (<https://github.com/clap-rs/clap/issues/3312>), so do it ourselves.
/// This is in a static so that it can become an `&'static str`.
static GRAPHICS_HELP_LONG: LazyLock<String> = LazyLock::new(|| {
    let pv_iter = GraphicsType::value_variants()
        .iter()
        .filter_map(|v| v.to_possible_value())
        .filter(|pv| !pv.is_hide_set());

    let max_width = pv_iter
        .clone()
        .map(|pv| pv.get_name().len())
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
    Clone, Copy, Debug, Eq, PartialEq, clap::ValueEnum, strum::EnumString, strum::IntoStaticStr,
)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub(crate) enum GraphicsType {
    // These variants are sorted for the benefit of the help text presentation.
    #[value(help = "Open a window (uses GPU rendering)")]
    Window,
    #[value(help = "EXPERIMENTAL: Open a window (uses CPU raytracing)")]
    WindowRt,

    #[cfg(feature = "terminal")]
    #[value(help = "Colored text in this terminal (uses raytracing)")]
    Terminal,

    #[value(help = "Non-interactive; don't draw anything but only simulates")]
    Headless,

    #[value(help = "Non-interactive; save an image or video (uses raytracing)")]
    #[cfg(feature = "record")]
    Record,

    #[cfg(feature = "terminal")]
    #[value(help = "Non-interactive; print one frame like 'terminal' mode then exit")]
    Print,
}

impl GraphicsType {
    /// Whether this graphics type implies “headless” (no real-time UI) operation.
    ///
    /// Concretely, this is expected to imply not contacting display devices,
    /// including the terminal.
    pub(crate) fn is_headless(self) -> bool {
        match self {
            GraphicsType::Window => false,
            GraphicsType::WindowRt => false,
            #[cfg(feature = "terminal")]
            GraphicsType::Terminal => false,

            GraphicsType::Headless => true,
            #[cfg(feature = "record")]
            GraphicsType::Record => true,
            #[cfg(feature = "terminal")]
            GraphicsType::Print => true,
        }
    }

    /// Whether this graphics type makes use of stdin/stdout/stderr in a way
    /// which is incompatible with other subsystems printing to stderr.
    pub(crate) fn uses_terminal(self) -> bool {
        match self {
            #[cfg(feature = "terminal")]
            GraphicsType::Terminal => true,
            _ => false,
        }
    }
}

/// Window/image size, parseable in a variety of formats, and with `None` referring to
/// “automatic”, not “optional”.
#[derive(Clone, Copy, Debug)]
pub(crate) struct DisplaySizeArg(pub Option<Size2D<u32, camera::NominalPixel>>);

impl FromStr for DisplaySizeArg {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("auto") {
            Ok(DisplaySizeArg(None))
        } else {
            let dims: [u32; 2] = s
                .split(&['×', 'x', ',', ';', ' '][..])
                .map(|s| {
                    s.parse::<u32>()
                        .map_err(|_| format!("{s:?} not an integer or \"auto\""))
                })
                .collect::<Result<Vec<u32>, String>>()?
                .try_into()
                .map_err(|_| String::from("must be two integers or \"auto\""))?;
            Ok(DisplaySizeArg(Some(Size2D::from(dims))))
        }
    }
}

/// Template generation size.
#[derive(Clone, Copy, Debug)]
pub(crate) struct SpaceSizeArg(pub Option<GridSize>);

impl FromStr for SpaceSizeArg {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("default") {
            Ok(SpaceSizeArg(None))
        } else {
            let dims: [GridSizeCoord; 3] = s
                .split(&['×', 'x', ',', ';', ' '][..])
                .map(|s| {
                    let i = s
                        .parse::<GridSizeCoord>()
                        .map_err(|_| format!("{s:?} not an integer or \"default\""))?;
                    if i < 1 {
                        return Err(format!("{s:?} not an integer or \"default\""));
                    }
                    Ok(i)
                })
                .collect::<Result<Vec<GridSizeCoord>, String>>()?
                .try_into()
                .map_err(|_| String::from("must be three integers or \"default\""))?;
            Ok(SpaceSizeArg(Some(GridSize::from(dims))))
        }
    }
}

#[cfg(feature = "record")]
pub fn determine_record_format(
    output_path: &std::path::Path,
) -> Result<RecordFormat, &'static str> {
    use all_is_cubes_port::Format;

    if let Some(extension) = output_path.extension() {
        match extension.to_str() {
            // When updating this match, also update the docs for output_file!
            // TODO: RecordFormat and port::Format should be merged?
            Some("alliscubesjson" | "ALLISCUBESJSON") => {
                return Ok(RecordFormat::Export(Format::AicJson));
            }
            Some("png" | "PNG") => return Ok(RecordFormat::PngOrApng),
            Some("apng" | "APNG") => return Ok(RecordFormat::PngOrApng),
            Some("gltf" | "GLTF") => return Ok(RecordFormat::Gltf),
            Some("stl" | "STL") => return Ok(RecordFormat::Export(Format::Stl)),
            Some("vox" | "VOX") => return Ok(RecordFormat::Export(Format::DotVox)),
            _ => {}
        }
    }
    // TODO: Have a separate option for choosing file type as a fallback
    Err(
        "file name must have an extension specifying the type; one of \
        'alliscubesjson', 'png', 'apng', 'gltf', 'stl', or 'vox'",
    )
}

// TODO: express the inputs here as a sub-struct of AicDesktopArgs
pub(crate) fn parse_universe_source(
    input_file: Option<PathBuf>,
    template: UniverseTemplate,
    SpaceSizeArg(size): SpaceSizeArg,
    seed: Option<u64>,
) -> UniverseSource {
    if let Some(file) = input_file {
        UniverseSource::File(file)
    } else {
        UniverseSource::Template(template, TemplateParameters { seed, size })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::euclid::size3;
    #[allow(unused_imports)]
    use clap::error::{ContextValue, ErrorKind};

    fn parse(args: &[&str]) -> clap::error::Result<AicDesktopArgs> {
        AicDesktopArgs::try_parse_from(std::iter::once("all-is-cubes").chain(args.iter().copied()))
    }

    #[cfg(feature = "record")]
    fn error_context(
        error: &clap::Error,
        wanted_kind: clap::error::ContextKind,
    ) -> Option<&ContextValue> {
        error
            .context()
            .find_map(|(k, v)| if k == wanted_kind { Some(v) } else { None })
    }

    #[test]
    #[cfg(feature = "record")]
    fn record_options_image() {
        assert_eq!(
            parse(&["-g", "record", "-o", "output.png"])
                .unwrap()
                .record_options()
                .unwrap()
                .unwrap(),
            RecordOptions {
                output_path: PathBuf::from("output.png"),
                output_format: RecordFormat::PngOrApng,
                save_all: false,
                image_size: Size2D::new(640, 480),
                animation: None,
            },
        );
    }

    #[test]
    #[cfg(feature = "record")]
    fn record_options_animation() {
        assert_eq!(
            parse(&["-g", "record", "-o", "fancy.png", "--duration", "3"])
                .unwrap()
                .record_options()
                .unwrap()
                .unwrap(),
            RecordOptions {
                output_path: PathBuf::from("fancy.png"),
                output_format: RecordFormat::PngOrApng,
                save_all: false,
                image_size: Size2D::new(640, 480),
                animation: Some(RecordAnimationOptions {
                    frame_count: 180,
                    frame_period: Duration::from_nanos((1e9 / 60.0) as u64),
                }),
            },
        );
    }

    // TODO: exercise record display size

    #[test]
    #[cfg(feature = "record")]
    fn record_options_missing_file() {
        let e = parse(&["-g", "record"]).unwrap_err();
        assert_eq!(e.kind(), ErrorKind::MissingRequiredArgument);
        assert_eq!(
            error_context(&e, clap::error::ContextKind::InvalidArg),
            Some(&ContextValue::Strings(vec![String::from(
                "--output <FILE>"
            )]))
        );
    }

    #[test]
    #[cfg(feature = "record")]
    fn record_options_missing_extension() {
        let e = parse(&["-g", "record", "-o", "foo"]).unwrap_err();
        assert_eq!(e.kind(), ErrorKind::ValueValidation);
        assert_eq!(
            error_context(&e, clap::error::ContextKind::InvalidArg),
            Some(&ContextValue::String(String::from("--output <FILE>")))
        );
        assert!(
            e.to_string()
                .contains("file name must have an extension specifying the type; one of"),
            "{e}\n{e:?}"
        );
    }

    #[test]
    #[cfg(feature = "record")]
    fn record_options_invalid_duration() {
        let e = parse(&["-g", "record", "-o", "o.png", "--duration", "X"]).unwrap_err();
        assert_eq!(e.kind(), ErrorKind::ValueValidation);
        assert_eq!(
            error_context(&e, clap::error::ContextKind::InvalidArg),
            Some(&ContextValue::String(String::from("--duration <SECONDS>")))
        );
    }

    fn parse_universe_test(args: &[&str]) -> clap::error::Result<UniverseSource> {
        let AicDesktopArgs {
            template,
            input_file,
            seed,
            template_size: size,
            ..
        } = parse(args)?;
        // TODO: make this a method on AicDesktopArgs
        Ok(parse_universe_source(input_file, template, size, seed))
    }

    #[test]
    fn universe_default() {
        assert_eq!(
            parse_universe_test(&[]).unwrap(),
            UniverseSource::Template(UniverseTemplate::DemoCity, TemplateParameters::default()),
        );
    }

    #[test]
    fn universe_from_template() {
        assert_eq!(
            parse_universe_test(&["--template", "cornell-box"]).unwrap(),
            UniverseSource::Template(UniverseTemplate::CornellBox, TemplateParameters::default()),
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
            ErrorKind::ArgumentConflict
        );
    }

    #[test]
    fn universe_option_invalid_template() {
        assert_eq!(
            parse_universe_test(&["--template", "foo"])
                .unwrap_err()
                .kind(),
            ErrorKind::InvalidValue
        );
    }

    #[test]
    fn display_size_parse() {
        let parse = |s: &str| s.parse::<DisplaySizeArg>().map(|DisplaySizeArg(size)| size);
        let err = |s: &str| Err(s.to_owned());
        assert_eq!(parse("1,2"), Ok(Some(Size2D::new(1, 2))));
        assert_eq!(parse("30x93"), Ok(Some(Size2D::new(30, 93))));
        assert_eq!(parse("30×93"), Ok(Some(Size2D::new(30, 93))));
        assert_eq!(parse(""), err("\"\" not an integer or \"auto\""));
        assert_eq!(parse("1"), err("must be two integers or \"auto\""));
        assert_eq!(parse("a"), err("\"a\" not an integer or \"auto\""));
        assert_eq!(parse("1a1"), err("\"1a1\" not an integer or \"auto\""));
        assert_eq!(parse("1×1×1"), err("must be two integers or \"auto\""));
        assert_eq!(parse("a×b"), err("\"a\" not an integer or \"auto\""));
        assert_eq!(parse("1×b"), err("\"b\" not an integer or \"auto\""));
    }

    #[test]
    fn space_size_parse() {
        let parse = |s: &str| s.parse::<SpaceSizeArg>().map(|SpaceSizeArg(size)| size);
        let err = |s: &str| Err(s.to_owned());
        assert_eq!(parse("1,2,3"), Ok(Some(size3(1, 2, 3))));
        assert_eq!(parse("10x20x30"), Ok(Some(size3(10, 20, 30))));
        assert_eq!(parse("10×20×30"), Ok(Some(size3(10, 20, 30))));
        assert_eq!(parse(""), err("\"\" not an integer or \"default\""));
        assert_eq!(parse("1"), err("must be three integers or \"default\""));
        assert_eq!(parse("a"), err("\"a\" not an integer or \"default\""));
        assert_eq!(
            parse("1a1a1"),
            err("\"1a1a1\" not an integer or \"default\"")
        );
        assert_eq!(parse("1×1"), err("must be three integers or \"default\""));
        assert_eq!(parse("a×bxc"), err("\"a\" not an integer or \"default\""));
        assert_eq!(parse("1×b×c"), err("\"b\" not an integer or \"default\""));
    }
}

//! Copies and compresses an image file from the test-renderers output directory
//! to the `expected/` directory.

use std::num::NonZeroU8;

use clap::Parser as _;

use test_renderers::{ImageId, RendererId, SuiteId, TestId, image_path};

#[derive(Debug, clap::Parser)]
#[command(author, about, version)]
struct BlessArgs {
    /// Whether to name the new files as being renderer-specific.
    /// If not specified, then they are named as "all renderers" outputs.
    #[arg(long)]
    specific: bool,

    /// Name of the test suite, e.g. "renderers".
    suite_id: SuiteId,

    /// Which renderer's output to copy, e.g. "ray".
    src_id: RendererId,

    /// Names of the test cases, e.g. "fog-None".
    test_names: Vec<String>,
}

fn main() -> Result<(), anyhow::Error> {
    {
        use simplelog::LevelFilter::{Info, Off, Trace};
        simplelog::WriteLogger::init(
            Info,
            simplelog::ConfigBuilder::new()
                .set_target_level(Trace)
                .set_location_level(Off)
                .build(),
            std::io::stderr(),
        )?;
    }

    let BlessArgs {
        specific,
        suite_id,
        src_id,
        test_names,
    } = BlessArgs::parse();

    let dst_id = if specific { src_id } else { RendererId::All };

    for test_name in test_names {
        let test_id = TestId {
            suite: suite_id,
            test: test_name,
        };
        let src_path = image_path(
            &ImageId {
                test_id: test_id.clone(),
                renderer: src_id,
                // Kludge: serial number 1 equals "no suffix" so we can not care about whether
                // the original file really had a number
                serial_number: 1,
            },
            test_renderers::Version::Actual,
        );
        let dst_path = image_path(
            &ImageId {
                test_id,
                renderer: dst_id,
                serial_number: 1,
            },
            test_renderers::Version::ExpectedSrc,
        );

        oxipng::optimize(
            &oxipng::InFile::Path(src_path),
            &oxipng::OutFile::from_path(dst_path),
            &oxipng::Options {
                // Even if no optimization is found, write the file to the new path.
                force: true,

                fix_errors: false,
                optimize_alpha: false,
                color_type_reduction: true,
                palette_reduction: true,
                grayscale_reduction: true,
                idat_recoding: true,
                deflate: oxipng::Deflaters::Zopfli {
                    iterations: NonZeroU8::new(15).unwrap(),
                },
                fast_evaluation: false,
                timeout: None,
                ..oxipng::Options::default()
            },
        )?;
    }

    Ok(())
}

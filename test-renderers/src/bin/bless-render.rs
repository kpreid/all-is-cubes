//! Copies and compresses an image file from the test-renderers output directory
//! to the `expected/` directory.
//!
//! Requires the `pngcrush` command-line program to be installed.

use std::process::Command;

use clap::Parser as _;

use test_renderers::{image_path, ImageId, RendererId};

#[derive(Debug, clap::Parser)]
#[command(author, about, version)]
struct BlessArgs {
    /// Whether to name the new files as being renderer-specific.
    /// If not specified, then they are named as "all renderers" outputs.
    #[arg(long)]
    specific: bool,

    /// Which renderer's output to copy, e.g. "ray".
    src_id: RendererId,

    /// Names of the test cases, e.g. "fog-None".
    test_id: Vec<String>,
}

fn main() {
    let BlessArgs {
        specific,
        src_id,
        test_id: test_ids,
    } = BlessArgs::parse();

    let dst_id = if specific { src_id } else { RendererId::All };

    for test_id in test_ids {
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

        let status = Command::new("pngcrush")
            .arg("-brute")
            .arg(src_path)
            .arg(dst_path)
            .status()
            .expect("failed to spawn `pngcrush`");
        if !status.success() {
            eprintln!("pngcrush returned an error; exiting.");
            std::process::exit(status.code().unwrap_or(1));
        }
    }
}

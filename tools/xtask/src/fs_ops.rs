use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use anyhow::Context;

/// Test if some input files are newer than some output files, or if the output files don't exist.
#[track_caller]
pub fn newer_than<P1, P2>(inputs: impl AsRef<[P1]>, outputs: impl AsRef<[P2]>) -> bool
where
    P1: AsRef<Path>,
    P2: AsRef<Path>,
{
    let newest_input: SystemTime = inputs
        .as_ref()
        .iter()
        .map(|path| {
            fs::metadata(path)
                .unwrap_or_else(|e| {
                    panic!(
                        "IO error while checking timestamp of {p}: {e}",
                        p = path.as_ref().display()
                    )
                })
                .modified()
                .expect("mtime support required")
        })
        .max()
        .expect("Must have at least one input file");
    let oldest_output: Option<SystemTime> = outputs
        .as_ref()
        .iter()
        .map(|path| {
            let path: &Path = path.as_ref();
            match fs::metadata(path) {
                Ok(metadata) => Some(metadata.modified().unwrap()),
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
                Err(e) => panic!(
                    "IO error while checking timestamp of {p}: {e}",
                    p = path.display()
                ),
            }
        })
        .min()
        .expect("Must have at least one output file");

    match oldest_output {
        None => true, // some file did not exist
        Some(t) => newest_input >= t,
    }
}

/// Return a set of all paths within `search_dir`,
///
/// * with directories named before their contents
/// * excluding dot-files
/// * relative to `base_dir` (panics if `base_dir` is not equal to or a parent of `search_dir`)
pub fn directory_tree_contents(
    base_dir: &Path,
    search_dir: &Path,
) -> Result<BTreeSet<PathBuf>, walkdir::Error> {
    walkdir::WalkDir::new(search_dir)
        .min_depth(1)
        .follow_links(false)
        .contents_first(false)
        .into_iter()
        .filter(|entry_result| {
            entry_result.as_ref().map_or(true, |entry| {
                !entry.file_name().to_string_lossy().starts_with('.')
            })
        })
        .map(|entry_result| {
            entry_result.map(|entry| {
                entry
                    .path()
                    .strip_prefix(base_dir)
                    .expect("strip_prefix shouldn't fail")
                    .to_owned()
            })
        })
        .collect()
}

/// Copy a file or create a directory, and if there is an error then give details.
pub fn copy_file_with_context(src: &Path, dst: &Path) -> Result<(), crate::ActionError> {
    if fs::metadata(src)
        .with_context(|| format!("checking copy source {}", src.to_string_lossy()))?
        .is_dir()
    {
        fs::create_dir_all(dst)
            .with_context(|| format!("creating directory {}", dst.to_string_lossy()))?;
    } else {
        // TODO: preserve modification times
        fs::copy(src, dst).with_context(|| {
            format!(
                "copying {} to {}",
                src.to_string_lossy(),
                dst.to_string_lossy()
            )
        })?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_directory_tree_contents() {
        let p = &Path::new(env!("CARGO_MANIFEST_DIR"));
        assert_eq!(
            directory_tree_contents(p, p).unwrap(),
            BTreeSet::from([
                "Cargo.toml".into(),
                "src".into(),
                "src/args.rs".into(),
                "src/context.rs".into(),
                "src/fs_ops.rs".into(),
                "src/main.rs".into(),
                "src/reporting.rs".into(),
            ])
        );
    }
}

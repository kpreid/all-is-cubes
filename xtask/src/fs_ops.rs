use std::collections::BTreeSet;
use std::ffi::OsString;
use std::path::Path;
use std::time::SystemTime;
use std::{fs, io};

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

pub fn dir_file_names(dir: &Path) -> Result<BTreeSet<OsString>, io::Error> {
    fs::read_dir(dir)?
        .map(|entry_result| entry_result.map(|entry| entry.file_name()))
        .collect::<Result<BTreeSet<OsString>, io::Error>>()
}

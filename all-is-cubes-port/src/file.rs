//! File system abstraction, for importing data that does not necessarily
//! live on the file system that [`std::fs`] accesses.

use std::fmt;
use std::io;
use std::path::PathBuf;

/// A “file” that we can load things from and which has a name,
/// without being tied to the current OS file system.
pub trait Fileish: fmt::Debug + Send + Sync {
    /// Path of the file, for display purposes such as in a window title.
    fn document_name(&self) -> String;

    /// Path of the file, for display purposes such as in an error, not a path that can
    /// necessarily be opened.
    fn display_full_path(&self) -> String;

    /// Obtains the file contents.
    ///
    /// TODO: This should probably be async.
    fn read(&self) -> Result<Vec<u8>, io::Error>;
}

// TODO: when Rust has generic associated types we will no longer
// need to implement Fileish for references.

impl Fileish for PathBuf {
    fn document_name(&self) -> String {
        match self.file_stem() {
            Some(n) => n.to_string_lossy(),
            None => self.to_string_lossy(),
        }
        .into_owned()
    }

    fn display_full_path(&self) -> String {
        self.display().to_string()
    }

    fn read(&self) -> Result<Vec<u8>, io::Error> {
        std::fs::read(self)
    }
}

/// General-purpose implementation of [`Fileish`].
pub struct NonDiskFile<O> {
    name: String,
    opener: O,
}

impl<O> fmt::Debug for NonDiskFile<O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { name, opener: _ } = self;
        f.debug_struct("NonDiskFile")
            .field("name", name)
            .finish_non_exhaustive()
    }
}

impl<O> NonDiskFile<O> {
    /// Construct a new [`NonDiskFile`] from its parts.
    pub fn from_name_and_data_source(name: String, opener: O) -> Self {
        Self { name, opener }
    }
}

impl<O> Fileish for NonDiskFile<O>
where
    O: Fn() -> Result<Vec<u8>, io::Error> + Send + Sync,
{
    fn document_name(&self) -> String {
        self.name.clone()
    }

    fn display_full_path(&self) -> String {
        self.name.clone()
    }

    fn read(&self) -> Result<Vec<u8>, io::Error> {
        (self.opener)()
    }
}

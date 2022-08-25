//! File system abstraction, for importing data that does not necessarily
//! live on the file system that [`std::fs`] accesses.

use std::fmt;
use std::io;
use std::path;

/// A “file” that we can load things from and which has a name,
/// without being tied to the current OS file system.
pub trait Fileish {
    /// Type returned by [`Self::display_full_path()`].
    type Display: fmt::Display;

    /// Path of the file, for display purposes.
    fn display_full_path(&self) -> Self::Display;

    /// Obtains the file contents.
    ///
    /// TODO: This should probably be async.
    fn read(&self) -> Result<Vec<u8>, io::Error>;
}

// TODO: when Rust has generic associated types we will no longer
// need to implement Fileish for references.

impl<'a> Fileish for &'a path::Path {
    type Display = path::Display<'a>;

    fn display_full_path(&self) -> Self::Display {
        self.display()
    }

    fn read(&self) -> Result<Vec<u8>, io::Error> {
        std::fs::read(self)
    }
}

/// General-purpose implementation of [`Fileish`].
#[derive(Debug)]
pub struct NonDiskFile<O> {
    name: String,
    opener: O,
}

impl<O> NonDiskFile<O> {
    /// Construct a new [`NonDiskFile`] from its parts.
    pub fn from_name_and_data_source(name: String, opener: O) -> Self {
        Self { name, opener }
    }
}

impl<'a, O> Fileish for &'a NonDiskFile<O>
where
    O: Fn() -> Result<Vec<u8>, io::Error>,
{
    type Display = &'a str;

    fn display_full_path(&self) -> Self::Display {
        &self.name
    }

    fn read(&self) -> Result<Vec<u8>, io::Error> {
        (self.opener)()
    }
}

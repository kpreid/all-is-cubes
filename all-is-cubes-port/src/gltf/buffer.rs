//! Helper for writing glTF buffer data, either to disk or to memory for testing.

use std::collections::HashSet;
use std::ffi::OsString;
use std::fs::File;
use std::io;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use descriptive_unwrap::OptionExt as _;
use gltf_json::Index;
use gltf_json::validation::USize64;

use super::glue::{Lef32, create_accessor};

// -------------------------------------------------------------------------------------------------

/// Designates the location where glTF buffer data (meshes, textures) should be written
/// (either to disk files or inline in the glTF JSON).
///
/// If cloned, the clone will provide equivalent access to the same destination and may be
/// used interchangeably.
///
/// TODO: Add support for combining buffers and writing `.glb` combined files.
#[derive(Clone, Debug)]
pub struct GltfDataDestination(Arc<Shared>);

#[derive(Debug)]
struct Shared {
    /// If true, all data is unconditionally discarded. For testing only.
    discard: bool,

    /// Buffers whose byte length is less than or equal to this will be inlined as `data:` URLs.
    maximum_inline_bytes: usize,

    /// Path (possibly with extension which will be stripped) to use as a base name for data files
    /// beside the glTF file.
    ///
    /// If this is `None` and `maximum_inline_length` does not permit inlining, an error will be
    /// reported on any attempt to write a buffer.
    file_base_path: Option<PathBuf>,

    /// Filename suffixes (the 'bar' in `foo-bar.glbin`) that have already been used,
    /// tracked to ensure uniqueness.
    suffix_uses: Mutex<HashSet<String>>,

    /// Buffers that have been created by [`GltfDataDestination::write()`] calls.
    /// These will later be moved into the [`gltf_json::Root`].
    /// This vector is only appended to, so its indices are stable.
    buffers: Mutex<Vec<gltf_json::Buffer>>,
}

impl GltfDataDestination {
    /// `maximum_inline_length` is the maximum length of data which will be stored inline in the
    /// glTF file as a `data:` URL rather than separately.
    ///
    /// `file_base_path` is the file path (optionally with extension which will be stripped) to
    /// use as a base name for data files beside the glTF file.
    /// For example, if `file_base_path` is `foo/bar.gltf`, then buffer files will be written to
    /// paths like `foo/bar-buffername.glbin`.
    /// If it is `None`, then buffers may not exceed `maximum_inline_length`.
    ///
    /// # Panics
    ///
    /// Panics if `file_base_path` does not contain a file name.
    pub fn new(file_base_path: Option<PathBuf>, maximum_inline_bytes: usize) -> Self {
        if let Some(file_base_path) = &file_base_path {
            assert!(
                file_base_path.file_stem().is_some(),
                "file_base_path must include a file name, but does not: “{}”",
                file_base_path.display()
            );
        }

        Self(Arc::new(Shared {
            discard: false,
            maximum_inline_bytes,
            file_base_path,
            suffix_uses: Mutex::new(HashSet::new()),
            buffers: Mutex::new(Vec::new()),
        }))
    }

    /// Creates a [`GltfDataDestination`] that discards all data.
    ///
    /// This is only useful for testing, when buffer contents are not relevant to the test.
    pub fn null() -> GltfDataDestination {
        Self(Arc::new(Shared {
            discard: true,
            maximum_inline_bytes: 0,
            file_base_path: None,
            suffix_uses: Mutex::new(HashSet::new()),
            buffers: Mutex::new(Vec::new()),
        }))
    }

    /// Write glTF buffer data, then return the buffer index and offset into that buffer where
    /// the provided data will be found.
    ///
    /// * `contents_fn` will be called with a buffered writer to write the data to.
    /// * `buffer_object_name` is the `name` that may be given to the [`gltf_json::Buffer`] object
    ///   if it is not shared with other data.
    /// * `proposed_file_name` will be included in the name of the generated data file,
    ///   if there is one for this data alone; for example, `foo.gltf` will have data files named
    ///   like `foo-{proposed_file_name}-20.{proposed_file_extension}`.
    /// * `proposed_file_extension` should be `glbin` or an image format.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    ///
    /// * An IO error occurs while writing.
    /// * The data file path constructed using `self`'s base file path is not UTF-8.
    ///
    /// The outcome is not specified if IO errors from the writer given to `contents_fn`
    /// are ignored rather than propagated.
    //
    // ---
    // TODO: Add context (filename) to the IO error
    pub(crate) fn write<F>(
        &self,
        buffer_object_name: String,
        proposed_file_name: &str,
        proposed_file_extension: &str,
        contents_fn: F,
    ) -> io::Result<BufferAddress>
    where
        F: FnOnce(&mut dyn io::Write) -> io::Result<()>,
    {
        // Refuse characters which could change the interpretation of the path.
        // TODO: filter them out instead
        assert!(
            !proposed_file_name.contains(['/', '\0', '%']),
            "Invalid character in buffer file name {proposed_file_name:?}"
        );

        let mut implementation = if self.0.discard {
            SwitchingWriter::Null { bytes_written: 0 }
        } else if let Some(file_base_path) = &self.0.file_base_path {
            // Ensure uniqueness of the file suffix.
            // TODO: Only do this if we exceed the in-memory limit?
            let unique_file_suffix: String = {
                let mut suffix_uses = self.0.suffix_uses.lock().map_err(dispose_of_poison)?;
                make_unique_name(proposed_file_name, &mut suffix_uses)
            };

            // Construct the file name (which is also the _relative_ path from gltf to data file).
            let mut buffer_file_name: OsString =
                file_base_path.file_stem().none_is_unreachable().to_owned();
            buffer_file_name.push(format!("-{unique_file_suffix}.{proposed_file_extension}"));

            // Construct the relative URL the glTF file will contain.
            let relative_url = file_name_to_relative_url(&buffer_file_name)?;

            // Construct the absolute path which we are going to write to.
            let mut buffer_file_path = file_base_path.clone();
            buffer_file_path.set_file_name(&buffer_file_name);

            SwitchingWriter::Memory {
                buffer: Vec::new(),
                limit: self.0.maximum_inline_bytes,
                path: Some(buffer_file_path),
                future_file_uri: Some(relative_url),
            }
        } else {
            SwitchingWriter::Memory {
                buffer: Vec::new(),
                limit: self.0.maximum_inline_bytes,
                path: None,
                future_file_uri: None,
            }
        };

        // Write data to file
        contents_fn(&mut implementation)?;
        let (uri, byte_length) = implementation.close()?;

        // Create buffer object.
        let buffer = gltf_json::Buffer {
            name: Some(buffer_object_name),
            byte_length: USize64::from(byte_length),
            uri,
            extensions: Default::default(),
            extras: Default::default(),
        };
        let buffer_index = Index::push(
            &mut *self.0.buffers.lock().map_err(dispose_of_poison)?,
            buffer,
        );

        Ok(BufferAddress {
            buffer: buffer_index,
            byte_offset: USize64(0),
            byte_length: USize64::from(byte_length),
        })
    }

    /// Returns the buffers that should go into the glTF JSON.
    ///
    /// If clones of this destination exist, it is still usable to make glTF with more content,
    /// but this is not the intended usage pattern and will incur an additional clone of the data.
    pub(crate) fn into_buffers(self) -> io::Result<Vec<gltf_json::Buffer>> {
        match Arc::try_unwrap(self.0) {
            Ok(inner) => inner.buffers.into_inner().map_err(dispose_of_poison),
            Err(arc) => Ok(arc.buffers.lock().map_err(dispose_of_poison)?.clone()),
        }
    }
}

impl PartialEq for GltfDataDestination {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

// -------------------------------------------------------------------------------------------------

/// A buffer and an offset in it, created by [`GltfDataDestination::write()`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct BufferAddress {
    /// The index of the buffer in the glTF asset.
    pub buffer: Index<gltf_json::Buffer>,
    /// The offset of the beginning of the data from the beginning of the buffer.
    pub byte_offset: USize64,
    /// Length of the data in bytes.
    /// (We include this because it is handy, not because it is required.)
    pub byte_length: USize64,
}

impl BufferAddress {
    /// Byte zero of buffer zero.
    #[cfg(test)]
    pub(crate) fn from_length_at_zero(len: usize) -> Self {
        Self {
            buffer: Index::new(0),
            byte_offset: USize64(0),
            byte_length: USize64::from(len),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// An implementation of [`io::Write`] which can dynamically switch from
/// an in-memory buffer to a file based on the length, and in any case
/// remembers the length written and encodes the final URI of the data.
///
/// Does not guarantee the length is correct if `write()` is called after
/// an IO error was previously returned.
#[derive(Debug)]
enum SwitchingWriter {
    Null {
        bytes_written: usize,
    },
    Memory {
        buffer: Vec<u8>,
        limit: usize,
        future_file_uri: Option<String>,
        path: Option<PathBuf>,
    },
    File {
        file: io::BufWriter<File>,
        bytes_written: usize,
        file_uri: Option<String>,
    },
}

impl SwitchingWriter {
    /// Close the file (if any) and return the uri and the bytes written.
    fn close(self) -> io::Result<(Option<String>, usize)> {
        match self {
            SwitchingWriter::Null { bytes_written } => Ok((None, bytes_written)),
            SwitchingWriter::Memory { buffer, .. } => {
                use base64::Engine as _;

                let prefix = "data:application/gltf-buffer;base64,";
                let mut url = String::with_capacity(prefix.len() + buffer.len() * 6 / 8 + 3);
                url += prefix;
                // Note: The so-called “URL_SAFE” character set is *not* the correct
                // format for data URLs; standard base64 is correct. The URL safety
                // in question is for e.g. base64 components within ordinary URLs or
                // file names.
                base64::engine::general_purpose::STANDARD.encode_string(&buffer, &mut url);
                Ok((Some(url), buffer.len()))
            }
            SwitchingWriter::File {
                bytes_written,
                file,
                file_uri,
                ..
            } => {
                // TODO: use crate::export::close_buffered_file() for consistency
                let file = file.into_inner()?;
                file.sync_data()?;
                // clippy false positive when this code is compiled for wasm
                // should be fixed by <https://github.com/rust-lang/rust/pull/162444>
                #[allow(clippy::drop_non_drop)]
                drop(file);
                Ok((file_uri, bytes_written))
            }
        }
    }
}

impl io::Write for SwitchingWriter {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        match *self {
            SwitchingWriter::Null {
                ref mut bytes_written,
            } => {
                *bytes_written += bytes.len();
                Ok(bytes.len())
            }
            SwitchingWriter::Memory {
                ref mut buffer,
                limit,
                ref path,
                ref future_file_uri,
            } => {
                let n = buffer.write(bytes)?;
                if buffer.len() > limit {
                    let path = path.as_ref().ok_or_else(|| {
                        io::Error::other(format!(
                            "no destination was provided for glTF buffers > {limit} bytes"
                        ))
                    })?;
                    // TODO: refuse to overwrite existing files unless we are also overwriting a corresponding .gltf
                    let file = crate::open_buffered_file(path).map_err(io::Error::other)?;
                    let mut new_writer = SwitchingWriter::File {
                        file,
                        bytes_written: 0,
                        file_uri: future_file_uri.clone(),
                    };
                    new_writer.write_all(buffer)?;
                    *self = new_writer;
                }
                Ok(n)
            }
            SwitchingWriter::File {
                ref mut file,
                ref mut bytes_written,
                file_uri: _,
            } => {
                let n = file.write(bytes)?;
                *bytes_written += n;
                Ok(n)
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            SwitchingWriter::Null { .. } => Ok(()),
            SwitchingWriter::Memory { .. } => Ok(()),
            SwitchingWriter::File { file, .. } => file.flush(),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Store the given data in a buffer, and return an accessor to the data.
///
/// The `data_source` iterator should be cheap to clone,
/// as it will be consulted multiple times.
///
/// This function only creates non-interleaved and non-concatenated buffers,
/// and does not set the `target`, so it is not suitable for vertices.
pub(crate) fn create_buffer_and_accessor<I, const COMPONENTS: usize>(
    root: &mut gltf_json::Root,
    dest: &GltfDataDestination,
    name: String,
    file_suffix: &str,
    data_source: I,
) -> io::Result<Index<gltf_json::Accessor>>
where
    I: IntoIterator<Item = [f32; COMPONENTS], IntoIter: ExactSizeIterator> + Clone,
    [Lef32; COMPONENTS]: bytemuck::Pod,
{
    let BufferAddress {
        buffer,
        byte_offset,
        byte_length,
    } = dest.write(name.clone(), file_suffix, "glbin", |w| {
        for item in data_source.clone() {
            w.write_all(bytemuck::bytes_of(&item.map(Lef32::from)))?;
        }
        Ok(())
    })?;

    let buffer_view = root.push(gltf_json::buffer::View {
        buffer,
        byte_length,
        byte_offset: Some(byte_offset),
        byte_stride: None,
        name: Some(name.clone()),
        target: None,
        extensions: Default::default(),
        extras: Default::default(),
    });

    let accessor_index = root.push(create_accessor(name, buffer_view, 0, data_source));

    Ok(accessor_index)
}

fn make_unique_name(proposed: &str, used: &mut HashSet<String>) -> String {
    let chosen = if used.contains(proposed) {
        let mut i = 2;
        loop {
            let new_suffix = format!("{proposed}-{i}");
            if !used.contains(&new_suffix) {
                break new_suffix;
            } else {
                i += 1;
            }
        }
    } else {
        proposed.to_owned()
    };
    used.insert(chosen.clone());
    chosen
}

/// Convert the name (not path) of a file that we are writing to a relative URL
/// that may appear in the glTF data.
fn file_name_to_relative_url(buffer_file_name: &std::ffi::OsStr) -> Result<String, io::Error> {
    // Strictly speaking, “UTF-8” is not the actual requirement here. However, if we were to support
    // a platform-specific-encoded file name, we’d need to get the specific right bytes to put in a
    // file URL, which are not what `OsStr` bytes are, and is not available in a cross-platform
    // way. So, we say “if UTF-8 is not right, we don’t support that case”.
    //
    // TODO: Ideally we’d raise this error well before the export process begins.
    let unencoded_str = buffer_file_name.to_str().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "glTF file name must be valid UTF-8, but “{}” was not",
                buffer_file_name.to_string_lossy()
            ),
        )
    })?;

    // This set is conservative, consisting only of common characters that are known to be OK.
    const CHAR_SET_TO_ESCAPE: &percent_encoding::AsciiSet =
        &percent_encoding::NON_ALPHANUMERIC.remove(b'-').remove(b'_').remove(b'.');

    Ok(percent_encoding::percent_encode(unencoded_str.as_bytes(), CHAR_SET_TO_ESCAPE).to_string())
}

fn dispose_of_poison<G>(_: std::sync::PoisonError<G>) -> io::Error {
    io::Error::other("previous panic while using GltfDataDestination; cannot continue")
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Write one byte to make the buffer nonempty.
    fn write1(w: &mut dyn io::Write) -> io::Result<()> {
        w.write_all(&[0])
    }

    #[test]
    fn discard() {
        let d = GltfDataDestination::null();

        let buffer_index_and_offset =
            d.write("foo".into(), "bar", "glbin", |w| w.write_all(&[1, 2, 3])).unwrap();

        assert_eq!(
            buffer_index_and_offset,
            BufferAddress::from_length_at_zero(3)
        );
        let [buffer_object] =
            <[gltf_json::Buffer; 1]>::try_from(d.into_buffers().unwrap()).unwrap();
        assert_eq!(buffer_object.name, Some("foo".into()));
        assert_eq!(buffer_object.uri, None);
        assert_eq!(buffer_object.byte_length, USize64(3));
    }

    #[test]
    fn inline_only_success() {
        let d = GltfDataDestination::new(None, usize::MAX);

        let buffer_index_and_offset =
            d.write("foo".into(), "bar", "glbin", |w| w.write_all(&[1, 2, 255])).unwrap();

        assert_eq!(
            buffer_index_and_offset,
            BufferAddress::from_length_at_zero(3)
        );
        let [buffer_object] =
            <[gltf_json::Buffer; 1]>::try_from(d.into_buffers().unwrap()).unwrap();
        assert_eq!(buffer_object.name, Some("foo".into()));
        assert_eq!(
            buffer_object.uri.as_deref(),
            Some("data:application/gltf-buffer;base64,AQL/") // AQL/ = 000000 010000 001011 111111
        );
        assert_eq!(buffer_object.byte_length, USize64(3));
    }

    #[test]
    fn inline_only_failure() {
        let d = GltfDataDestination::new(None, 1);

        let error = d
            .write("foo".into(), "bar", "glbin", |w| w.write_all(&[1, 2, 255]))
            .unwrap_err();

        assert_eq!(
            error.to_string(),
            "no destination was provided for glTF buffers > 1 bytes"
        );
    }

    #[test]
    fn switch_to_file() {
        let temp_dir = tempfile::tempdir().unwrap();
        let mut file_base_path = temp_dir.path().to_owned();
        file_base_path.push("basepath.gltf");
        println!("Base path: {}", file_base_path.display());

        let d = GltfDataDestination::new(Some(file_base_path), 3);
        let buffer_index_and_offset = d
            .write("foo".into(), "bar", "glbin", |w| {
                w.write_all(&[1, 2, 3])?;
                w.write_all(&[4, 5, 6])?;
                Ok(())
            })
            .unwrap();

        assert_eq!(
            buffer_index_and_offset,
            BufferAddress::from_length_at_zero(6)
        );
        let [buffer_object] =
            <[gltf_json::Buffer; 1]>::try_from(d.into_buffers().unwrap()).unwrap();
        assert_eq!(buffer_object.name, Some("foo".into()));
        // Note that the URL is relative, not including the temp dir.
        assert_eq!(buffer_object.uri.as_deref(), Some("basepath-bar.glbin"));
        assert_eq!(buffer_object.byte_length, USize64(6));
    }

    #[test]
    fn non_unique_suffixes() {
        let temp_dir = tempfile::tempdir().unwrap();
        let mut file_base_path = temp_dir.path().to_owned();
        file_base_path.push("basepath.gltf");
        println!("Base path: {}", file_base_path.display());

        let d = GltfDataDestination::new(Some(file_base_path), 0);
        d.write("foo".into(), "bar", "glbin", write1).unwrap();
        d.write("foo".into(), "bar", "glbin", write1).unwrap();

        let [e1, e2] = <[gltf_json::Buffer; 2]>::try_from(d.into_buffers().unwrap()).unwrap();
        // These two file names must be distinct.
        assert_eq!(e1.uri.as_deref(), Some("basepath-bar.glbin"));
        assert_eq!(e2.uri.as_deref(), Some("basepath-bar-2.glbin"));
    }

    #[test]
    fn url_encoding() {
        let temp_dir = tempfile::tempdir().unwrap();
        let mut file_base_path = temp_dir.path().to_owned();
        file_base_path.push("base path.gltf");

        let d = GltfDataDestination::new(Some(file_base_path), 0);
        d.write("object name".into(), "object file", "glbin", write1).unwrap();

        let [buffer_object] =
            <[gltf_json::Buffer; 1]>::try_from(d.into_buffers().unwrap()).unwrap();
        assert_eq!(
            buffer_object.name,
            Some("object name".into()),
            "name should not be escaped"
        );
        assert_eq!(
            buffer_object.uri.as_deref(),
            Some("base%20path-object%20file.glbin")
        );
    }
}

//! Helper for writing glTF buffer data, either to disk or to memory for testing.

use std::collections::HashSet;
use std::ffi::OsString;
use std::fmt::Write as _;
use std::fs::File;
use std::io::{self, Seek as _};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use descriptive_unwrap::{OptionExt as _, ResultExt as _};
use gltf_json::Index;
use gltf_json::validation::USize64;

use super::glue::{Lef32, create_accessor};

// -------------------------------------------------------------------------------------------------

/// Designates the location where glTF buffer data (meshes, textures) should be written
/// (either to disk files or inline in the glTF JSON).
///
/// If cloned, the clone will provide equivalent access to the same destination and may be
/// used interchangeably.
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

    /// * `None`: each data segment created by a single [`GltfDataDestination::write()`] call is a
    ///   separate buffer, either inlined or written to a new file.
    /// * `Some`: all data segments are written to this file, which is the contents of buffer 0.
    ///
    /// Note: While it is possible to write to a `File` by reference without using a mutex,
    /// that would allow undesired interleaving of data.
    shared_buffer_file: Option<Mutex<File>>,
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
    /// If `multiple_files` is `true`, each piece of data is written to a separate `.glbin` file.
    /// If it is `false`, exactly one file is used.
    ///
    /// # Errors
    ///
    /// Returns an error if accessing the file system fails.
    ///
    /// # Panics
    ///
    /// Panics if `file_base_path` does not contain a file name,
    /// or is missing when `multiple_files` is `false`.
    pub fn new(
        file_base_path: Option<PathBuf>,
        maximum_inline_bytes: usize,
        multiple_files: bool,
    ) -> io::Result<Self> {
        if let Some(file_base_path) = &file_base_path {
            assert!(
                file_base_path.file_stem().is_some(),
                "file_base_path must include a file name, but does not: “{}”",
                file_base_path.display()
            );
        }

        // TODO: check for non-collision. also it would be better if we just took this path as a parameter
        let shared_data_file_path = if !multiple_files {
            Some(
                file_base_path
                    .as_ref()
                    .expect("must have a file_base_path in single-file mode")
                    .with_extension("glbin"),
            )
        } else {
            None
        };

        Ok(Self(Arc::new(Shared {
            discard: false,
            suffix_uses: Mutex::new(HashSet::new()),
            buffers: Mutex::new(
                if let Some(shared_data_file_path) = &shared_data_file_path {
                    vec![gltf_json::Buffer {
                        byte_length: USize64(0), // replaced later
                        name: None,
                        uri: Some(file_name_to_relative_url(
                            shared_data_file_path.file_name().none_is_unreachable(),
                        )?),
                        extensions: None,
                        extras: Default::default(),
                    }]
                } else {
                    Vec::new()
                },
            ),
            shared_buffer_file: if let Some(shared_data_file_path) = shared_data_file_path {
                Some(Mutex::new(File::create(shared_data_file_path)?))
            } else {
                None
            },

            maximum_inline_bytes,
            file_base_path,
        })))
    }

    /// Creates a [`GltfDataDestination`] that writes all data to a single file, assuming that
    /// their contents will separately be transferred to the binary chunk of a GLB file afterward.
    ///
    /// In this mode, as per [glTF 2.0 § 3.6.1.2], the first buffer object produced will have no
    /// `"uri"` property and be taken to refer to the contents of the GLB binary chunk.
    ///
    /// [glTF 2.0 § 3.6.1.2]: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#glb-stored-buffer
    pub fn for_glb(temporary_file: File) -> Self {
        Self(Arc::new(Shared {
            discard: false,
            suffix_uses: Mutex::new(HashSet::new()),
            buffers: Mutex::new(vec![gltf_json::Buffer {
                byte_length: USize64(0), // replaced later
                name: None,
                uri: None, // for GLB, buffer 0 has no `uri`
                extensions: None,
                extras: Default::default(),
            }]),
            shared_buffer_file: Some(Mutex::new(temporary_file)),
            maximum_inline_bytes: 0,
            file_base_path: None,
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
            shared_buffer_file: None,
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
    /// * `data_type` should specify the type of data being written, and will be used to make
    ///   decisions about how and where the data is stored.
    ///
    /// Note: If a shared data file is in use, then this will take a lock on that shared file,
    /// and therefore will block until writes from other threads complete.
    /// Accordingly, `contents_fn` should complete its work quickly and avoid blocking on anything
    /// but the writes it must perform.
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
        data_type: DataType,
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
        } else if let Some(shared_file_mutex) = &self.0.shared_buffer_file {
            // We could recover from this PoisonError, because we don’t care about whether the
            // *other* contents of the file are valid, just where they are, but there is no point
            // in bothering.
            let mut file = shared_file_mutex.lock().map_err(dispose_of_poison)?;

            // Ensure that the new data has sufficient alignment, regardless of where the
            // previous data ended.
            let end_of_previous_data: u64 = file.stream_position()?;
            let start_of_new_data: u64 =
                end_of_previous_data.next_multiple_of(data_type.minimum_alignment());
            if start_of_new_data != end_of_previous_data {
                file.seek(io::SeekFrom::Start(start_of_new_data))?;
            }

            SwitchingWriter::SharedFile {
                offset: start_of_new_data,
                bytes_written: 0,
                file,
            }
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
            write!(
                buffer_file_name,
                "-{unique_file_suffix}.{extension}",
                extension = data_type.extension()
            )
            .err_is_unreachable();

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
                data_type,
            }
        } else {
            SwitchingWriter::Memory {
                buffer: Vec::new(),
                limit: self.0.maximum_inline_bytes,
                path: None,
                future_file_uri: None,
                data_type,
            }
        };

        // Write data to file
        contents_fn(&mut implementation)?;
        let (uri, byte_offset, byte_length) = implementation.close()?;

        // Create buffer object, if and only if we aren’t using a shared buffer.
        let buffer_index = if self.0.shared_buffer_file.is_some() {
            // The shared buffer is always buffer index 0
            Index::<gltf_json::Buffer>::new(0)
        } else {
            let buffer = gltf_json::Buffer {
                name: Some(buffer_object_name),
                byte_length: USize64::from(byte_length),
                uri,
                extensions: Default::default(),
                extras: Default::default(),
            };
            Index::push(
                &mut *self.0.buffers.lock().map_err(dispose_of_poison)?,
                buffer,
            )
        };

        Ok(BufferAddress {
            buffer: buffer_index,
            byte_offset,
            byte_length: USize64::from(byte_length),
        })
    }

    /// Returns the buffers that should go into the glTF JSON.
    ///
    /// If clones of this destination exist, it is still usable to make glTF with more content,
    /// but this is not the intended usage pattern and will incur an additional clone of the data.
    pub(crate) fn into_buffers(self) -> io::Result<Vec<gltf_json::Buffer>> {
        let shared_buffer_file_length = if let Some(shared_file_mutex) = &self.0.shared_buffer_file
        {
            Some(shared_file_mutex.lock().map_err(dispose_of_poison)?.metadata()?.len())
        } else {
            None
        };

        let mut buffers = match Arc::try_unwrap(self.0) {
            Ok(inner) => inner.buffers.into_inner().map_err(dispose_of_poison)?,
            Err(arc) => arc.buffers.lock().map_err(dispose_of_poison)?.clone(),
        };

        if let Some(shared_buffer_file_length) = shared_buffer_file_length {
            buffers[0].byte_length = USize64::from(shared_buffer_file_length);
        }

        Ok(buffers)
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

/// Types of data that may appear in a glTF "buffer" or "image".
#[derive(Clone, Copy, Debug)]
pub(crate) enum DataType {
    /// Vertex and index data.
    Mesh,
    /// PNG encoded image data.
    Png,
}

impl DataType {
    /// Returns the file extension for this data type, as specified by
    /// [glTF 2.0 § 2.6](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#file-extensions-and-media-types).
    pub fn extension(self) -> &'static str {
        match self {
            DataType::Mesh => "glbin",
            DataType::Png => "png",
        }
    }

    /// Returns the media type (MIME type) for this data type, as specified by
    /// [glTF 2.0 § 2.6](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#file-extensions-and-media-types).
    pub fn mime_type(self) -> &'static str {
        match self {
            DataType::Mesh => "application/gltf-buffer",
            DataType::Png => "image/png",
        }
    }

    /// Returns a sufficient alignment for this type of data.
    fn minimum_alignment(self) -> u64 {
        match self {
            // “The offset ... MUST be a multiple of the size of the accessor’s component type.”
            // — https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#data-alignment
            // Our biggest component type is f32.
            DataType::Mesh => const { size_of::<f32>() as u64 },

            // Images stored in glTF have no alignment requirements.
            DataType::Png => 1,
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
enum SwitchingWriter<'f> {
    Null {
        bytes_written: usize,
    },
    Memory {
        buffer: Vec<u8>,
        limit: usize,
        future_file_uri: Option<String>,
        path: Option<PathBuf>,
        data_type: DataType,
    },
    File {
        file: io::BufWriter<File>,
        bytes_written: usize,
        file_uri: Option<String>,
    },
    SharedFile {
        file: std::sync::MutexGuard<'f, File>,
        /// Offset in the file at which this data segment starts.
        offset: u64,
        bytes_written: usize,
    },
}

impl SwitchingWriter<'_> {
    /// Close the file (if any) and return the uri and the bytes written.
    fn close(self) -> io::Result<(Option<String>, USize64, usize)> {
        match self {
            SwitchingWriter::Null { bytes_written } => Ok((None, USize64(0), bytes_written)),
            SwitchingWriter::Memory {
                buffer, data_type, ..
            } => {
                use base64::Engine as _;

                let mut url = String::with_capacity(
                    const { "data:;base64,".len() }
                        + data_type.mime_type().len()
                        + buffer.len() * 6 / 8
                        + 3,
                );
                write!(url, "data:{};base64,", data_type.mime_type()).err_is_unreachable();

                // Note: The so-called “URL_SAFE” character set is *not* the correct
                // format for data URLs; standard base64 is correct. The URL safety
                // in question is for e.g. base64 components within ordinary URLs or
                // file names.
                base64::engine::general_purpose::STANDARD.encode_string(&buffer, &mut url);
                Ok((Some(url), USize64(0), buffer.len()))
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
                Ok((file_uri, USize64(0), bytes_written))
            }
            SwitchingWriter::SharedFile {
                file,
                offset,
                bytes_written,
                ..
            } => {
                drop(file); // release mutex guard
                Ok((None, USize64(offset), bytes_written))
            }
        }
    }
}

impl io::Write for SwitchingWriter<'_> {
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
                data_type: _,
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
            SwitchingWriter::SharedFile {
                ref mut file,
                ref mut bytes_written,
                offset: _,
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
            SwitchingWriter::SharedFile { file, .. } => file.flush(),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Store the given data in a buffer view, and return an accessor to the data.
///
/// The `data_source` iterator should be cheap to clone,
/// as it will be consulted multiple times.
///
/// This function only creates non-interleaved buffer views,
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
    } = dest.write(name.clone(), file_suffix, DataType::Mesh, |w| {
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
    use std::fs;
    use std::io::Read as _;

    /// Write one byte to make the buffer nonempty.
    fn write1(w: &mut dyn io::Write) -> io::Result<()> {
        w.write_all(&[0])
    }

    #[test]
    fn discard() {
        let d = GltfDataDestination::null();

        let buffer_index_and_offset = d
            .write("foo".into(), "bar", DataType::Mesh, |w| {
                w.write_all(&[1, 2, 3])
            })
            .unwrap();

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
        let d = GltfDataDestination::new(None, usize::MAX, true).unwrap();

        let buffer_index_and_offset = d
            .write("foo".into(), "bar", DataType::Mesh, |w| {
                w.write_all(&[1, 2, 255])
            })
            .unwrap();

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
        let d = GltfDataDestination::new(None, 1, true).unwrap();

        let error = d
            .write("foo".into(), "bar", DataType::Mesh, |w| {
                w.write_all(&[1, 2, 255])
            })
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

        let d = GltfDataDestination::new(Some(file_base_path), 3, true).unwrap();
        let buffer_index_and_offset = d
            .write("foo".into(), "bar", DataType::Mesh, |w| {
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

        let d = GltfDataDestination::new(Some(file_base_path), 0, true).unwrap();
        d.write("foo".into(), "bar", DataType::Mesh, write1).unwrap();
        d.write("foo".into(), "bar", DataType::Mesh, write1).unwrap();

        let [e1, e2] = <[gltf_json::Buffer; 2]>::try_from(d.into_buffers().unwrap()).unwrap();
        // These two file names must be distinct.
        assert_eq!(e1.uri.as_deref(), Some("basepath-bar.glbin"));
        assert_eq!(e2.uri.as_deref(), Some("basepath-bar-2.glbin"));
    }

    /// Tests single-file mode and that alignment is performed within the file.
    #[test]
    fn single_file_and_alignment() {
        let temp_dir = tempfile::tempdir().unwrap();
        let mut file_base_path = temp_dir.path().to_owned();
        file_base_path.push("basepath.gltf");
        println!("Base path: {}", file_base_path.display());

        let d = GltfDataDestination::new(Some(file_base_path), 0, false).unwrap();
        let addr1 = d
            .write("p1".into(), "p3", DataType::Mesh, |w| {
                w.write_all(&[1, 2, 3])
            })
            .unwrap();
        let addr2 = d.write("p2".into(), "p3", DataType::Mesh, |w| w.write_all(&[4])).unwrap();
        let addr3 = d.write("p3".into(), "p3", DataType::Mesh, |w| w.write_all(&[5])).unwrap();

        assert_eq!(
            (addr1, addr2, addr3),
            (
                BufferAddress {
                    buffer: Index::new(0),
                    byte_offset: USize64(0),
                    byte_length: USize64(3),
                },
                BufferAddress {
                    buffer: Index::new(0),
                    byte_offset: USize64(4),
                    byte_length: USize64(1),
                },
                BufferAddress {
                    buffer: Index::new(0),
                    byte_offset: USize64(8),
                    byte_length: USize64(1),
                },
            )
        );
        let [buffer_object] =
            <[gltf_json::Buffer; 1]>::try_from(d.into_buffers().unwrap()).unwrap();
        // These two file names must be distinct.
        assert_eq!(buffer_object.uri.as_deref(), Some("basepath.glbin"));
        assert_eq!(
            fs::read(temp_dir.path().join("basepath.glbin")).unwrap(),
            vec![1, 2, 3, 0, 4, 0, 0, 0, 5]
        );
    }

    /// Tests GLB-export mode, which is distinct because it does not give a `uri` to the buffer
    /// and does not use a base path.
    #[test]
    fn for_glb() {
        let mut temp_file = tempfile::tempfile().unwrap();
        let d = GltfDataDestination::for_glb(temp_file.try_clone().unwrap());

        let addr1 = d
            .write("p1".into(), "p3", DataType::Mesh, |w| {
                w.write_all(&[1, 2, 3])
            })
            .unwrap();
        let addr2 = d.write("p2".into(), "p3", DataType::Mesh, |w| w.write_all(&[4])).unwrap();

        assert_eq!(
            (addr1, addr2),
            (
                BufferAddress {
                    buffer: Index::new(0),
                    byte_offset: USize64(0),
                    byte_length: USize64(3),
                },
                BufferAddress {
                    buffer: Index::new(0),
                    byte_offset: USize64(4),
                    byte_length: USize64(1),
                },
            )
        );
        let [buffer_object] =
            <[gltf_json::Buffer; 1]>::try_from(d.into_buffers().unwrap()).unwrap();
        // These two file names must be distinct.
        assert_eq!(buffer_object.byte_length, USize64(5));
        assert_eq!(buffer_object.uri, None);

        temp_file.seek(io::SeekFrom::Start(0)).unwrap();
        let mut contents_of_temp_file = Vec::new();
        temp_file.read_to_end(&mut contents_of_temp_file).unwrap();
        assert_eq!(contents_of_temp_file, vec![1, 2, 3, 0, 4]);
    }

    #[test]
    fn url_encoding() {
        let temp_dir = tempfile::tempdir().unwrap();
        let mut file_base_path = temp_dir.path().to_owned();
        file_base_path.push("base path.gltf");

        let d = GltfDataDestination::new(Some(file_base_path), 0, true).unwrap();
        d.write("object name".into(), "object file", DataType::Mesh, write1).unwrap();

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

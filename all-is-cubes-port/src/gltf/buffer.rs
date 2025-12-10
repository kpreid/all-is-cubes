//! Helper for writing glTF buffer data, either to disk or to memory for testing.

use std::collections::HashSet;
use std::ffi::OsString;
use std::fs::File;
use std::io;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use gltf_json::Index;
use gltf_json::validation::USize64;

use super::glue::{Lef32, create_accessor};

/// Designates the location where glTF buffer data (meshes, textures) should be written
/// (either to disk files or inline in the glTF JSON).
///
/// If cloned, the clone will provide equivalent access to the same destination and may be
/// used interchangeably.
///
/// TODO: Add support for `.glb` combined files.
#[derive(Clone, Debug)]
pub struct GltfDataDestination(Arc<Inner>);

#[derive(Debug)]
struct Inner {
    /// If true, all data is unconditionally discarded. For testing only.
    discard: bool,

    /// Buffers whose byte length is less than or equal to this will be inlined as `data:` URLs.
    maximum_inline_length: usize,

    /// Path (possibly with extension which will be stripped) to use as a base name for data files
    /// beside the glTF file.
    ///
    /// If this is `None` and `maximum_inline_length` does not permit inlining, an error will be
    /// reported on any attempt to write a buffer.
    file_base_path: Option<PathBuf>,

    /// Filename suffixes (the 'bar' in `foo-bar.glbin`) that have already been used,
    /// tracked to ensure uniqueness.
    suffix_uses: Mutex<HashSet<String>>,
}

impl GltfDataDestination {
    #[cfg(test)]
    pub(crate) fn null() -> GltfDataDestination {
        Self(Arc::new(Inner {
            discard: true,
            maximum_inline_length: 0,
            file_base_path: None,
            suffix_uses: Mutex::new(HashSet::new()),
        }))
    }

    /// `maximum_inline_length` is the maximum length of data which will be stored inline in the
    /// glTF file as a `data:` URL rather than separately.
    ///
    /// `file_base_path` is the file path (optionally with extension which will be stripped) to use as a
    /// base name for data files beside the glTF file. For example, if `file_base_path` is
    /// `foo/bar.gltf`, then buffer files will be written to paths like `foo/bar-buffername.glbin`.
    /// If it is `None`, then buffers may not exceed `maximum_inline_length`.
    pub fn new(file_base_path: Option<PathBuf>, maximum_inline_length: usize) -> Self {
        if let Some(file_base_path) = &file_base_path {
            assert!(
                file_base_path.file_stem().is_some(),
                "file_base_path must include a file name, but does not: “{}”",
                file_base_path.display()
            );
        }

        Self(Arc::new(Inner {
            discard: false,
            maximum_inline_length,
            file_base_path,
            suffix_uses: Mutex::new(HashSet::new()),
        }))
    }

    /// Write glTF buffer data, then return a [`gltf_json::Buffer`] pointing to it by
    /// one of the permitted means.
    ///
    /// * `contents_fn` will be called with a buffered writer to write the data to.
    /// * `buffer_entity_name` is the `name` that will be in the returned [`gltf_json::Buffer`]
    ///   entity.
    /// * `proposed_file_name` will be included in the name of the generated data file,
    ///   if there is one; for example, `foo.gltf` will have data files named like
    ///   `foo-{proposed_file_name}-20.{proposed_file_extension}`.
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
    pub fn write<F>(
        &self,
        buffer_entity_name: String,
        proposed_file_name: &str,
        proposed_file_extension: &str,
        contents_fn: F,
    ) -> io::Result<gltf_json::Buffer>
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
                let mut suffix_uses = self.0.suffix_uses.lock().map_err(|_| {
                    io::Error::other("previous panic while using GltfDataDestination")
                })?;
                make_unique_name(proposed_file_name, &mut suffix_uses)
            };

            // Construct the file name (which is also the _relative_ path from gltf to data file).
            let mut buffer_file_name: OsString = file_base_path.file_stem().unwrap().to_owned();
            buffer_file_name.push(format!("-{unique_file_suffix}.{proposed_file_extension}"));

            // Construct the relative URL the glTF file will contain.
            // TODO: this path needs URL-encoding (excepting slashes)
            let relative_url = buffer_file_name
                .to_str()
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!(
                            "glTF file path must be valid UTF-8, but “{}” was not",
                            buffer_file_name.to_string_lossy()
                        ),
                    )
                })?
                .to_string();

            // Construct the absolute path which we are going to write to.
            let mut buffer_file_path = file_base_path.clone();
            buffer_file_path.set_file_name(&buffer_file_name);

            SwitchingWriter::Memory {
                buffer: Vec::new(),
                limit: self.0.maximum_inline_length,
                path: Some(buffer_file_path),
                future_file_uri: Some(relative_url),
            }
        } else {
            SwitchingWriter::Memory {
                buffer: Vec::new(),
                limit: self.0.maximum_inline_length,
                path: None,
                future_file_uri: None,
            }
        };

        // Write data to file
        contents_fn(&mut implementation)?;
        let (uri, byte_length) = implementation.close()?;

        Ok(gltf_json::Buffer {
            byte_length: USize64::from(byte_length),
            name: Some(buffer_entity_name),
            uri,
            extensions: Default::default(),
            extras: Default::default(),
        })
    }
}

impl PartialEq for GltfDataDestination {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

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
                let file = file.into_inner()?;
                file.sync_all()?;
                // clippy false positive when this code is compiled for wasm -- TODO: remove the file support when compiling for wasm
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
                    let file = File::create(path)?;
                    let mut new_writer = SwitchingWriter::File {
                        file: io::BufWriter::new(file),
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

/// Create a buffer from the given data, and return an accessor to it.
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
    let length = data_source.clone().into_iter().len();
    let buffer = dest.write(name.clone(), file_suffix, "glbin", |w| {
        for item in data_source.clone() {
            w.write_all(bytemuck::bytes_of(&item.map(Lef32::from)))?;
        }
        Ok(())
    })?;
    let buffer_index = root.push(buffer);

    let buffer_view = root.push(gltf_json::buffer::View {
        buffer: buffer_index,
        byte_length: (length * size_of::<[Lef32; COMPONENTS]>()).into(),
        byte_offset: None,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discard() {
        let d = GltfDataDestination::null();
        let buffer_entity =
            d.write("foo".into(), "bar", "glbin", |w| w.write_all(&[1, 2, 3])).unwrap();
        assert_eq!(buffer_entity.name, Some("foo".into()));
        assert_eq!(buffer_entity.uri, None);
        assert_eq!(buffer_entity.byte_length, USize64(3));
    }

    #[test]
    fn inline_only_success() {
        let d = GltfDataDestination::new(None, usize::MAX);
        let buffer_entity =
            d.write("foo".into(), "bar", "glbin", |w| w.write_all(&[1, 2, 255])).unwrap();
        assert_eq!(buffer_entity.name, Some("foo".into()));
        assert_eq!(
            buffer_entity.uri.as_deref(),
            Some("data:application/gltf-buffer;base64,AQL/") // AQL/ = 000000 010000 001011 111111
        );
        assert_eq!(buffer_entity.byte_length, USize64(3));
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
        let buffer_entity = d
            .write("foo".into(), "bar", "glbin", |w| {
                w.write_all(&[1, 2, 3])?;
                w.write_all(&[4, 5, 6])?;
                Ok(())
            })
            .unwrap();
        assert_eq!(buffer_entity.name, Some("foo".into()));
        // Note that the URL is relative, not including the temp dir.
        assert_eq!(buffer_entity.uri.as_deref(), Some("basepath-bar.glbin"));
        assert_eq!(buffer_entity.byte_length, USize64(6));
    }

    #[test]
    fn non_unique_suffixes() {
        let temp_dir = tempfile::tempdir().unwrap();
        let mut file_base_path = temp_dir.path().to_owned();
        file_base_path.push("basepath.gltf");
        println!("Base path: {}", file_base_path.display());

        let d = GltfDataDestination::new(Some(file_base_path), 0);
        let e1 = d.write("foo".into(), "bar", "glbin", write1).unwrap();
        let e2 = d.write("foo".into(), "bar", "glbin", write1).unwrap();

        // These two file names must be distinct.
        assert_eq!(e1.uri.as_deref(), Some("basepath-bar.glbin"));
        assert_eq!(e2.uri.as_deref(), Some("basepath-bar-2.glbin"));
    }

    /// Write one byte to make the buffer nonempty.
    fn write1(w: &mut dyn io::Write) -> io::Result<()> {
        w.write_all(&[0])
    }
}

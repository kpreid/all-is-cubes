use axum::routing::Router;

/// Where to obtain the WebAssembly+JS code for the All is Cubes in-browser game engine.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum AicClientSource {
    /// Use the copy embedded in this server binary.
    #[cfg(feature = "embed")]
    Embedded,

    /// Fetch the code live from `../all-is-cubes-wasm/`.
    ///
    /// This option will only succeed if the the original All is Cubes development
    /// workspace is present, and not in an installed binary.
    /// TODO: Disable this in release builds? Or some other way to distinguish
    Workspace,
}

impl AicClientSource {
    /// Returns a [`Router`] for paths to all client resources.
    pub(crate) fn client_router(&self) -> Router {
        let static_service = match self {
            #[cfg(feature = "embed")]
            AicClientSource::Embedded => axum::routing::get_service(
                tower_http::services::ServeDir::with_backend("", embedded::EmbeddedClient),
            ),
            AicClientSource::Workspace => axum::routing::get_service(
                tower_http::services::ServeDir::new(concat!(env!("AIC_CLIENT_BUILD_DIR"), "/")),
            ),
        };

        Router::new()
            .route("/", static_service.clone())
            .route("/{*path}", static_service)
    }
}

#[cfg(feature = "embed")]
mod embedded {
    use std::future::{Ready, ready};
    use std::io;
    use std::path::{Path, PathBuf};
    use std::pin::Pin;
    use std::task::{Context, Poll};

    use include_dir::DirEntry;
    use tokio::io::{AsyncRead, AsyncSeek};
    use tower_http::services::fs;

    // TODO: need to support optionally embedding the release build rather than the dev build
    static CLIENT_STATIC: DirEntry<'static> =
        DirEntry::Dir(include_dir::include_dir!("$AIC_CLIENT_BUILD_DIR"));

    fn client_static_lookup(path: &Path) -> Option<&'static DirEntry<'static>> {
        if path == "./" {
            Some(&CLIENT_STATIC)
        } else {
            CLIENT_STATIC.as_dir().unwrap().get_entry(path.strip_prefix("./").unwrap())
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub(crate) struct EmbeddedClient;

    #[derive(Debug)]
    pub(crate) struct OpenAdapter {
        cursor: io::Cursor<&'static [u8]>,
        metadata: MetadataAdapter,
    }

    #[derive(Clone, Copy, Debug)]
    pub(crate) struct MetadataAdapter(&'static DirEntry<'static>);

    /// Adapts [`include_dir::Dir`] to [`fs::Backend`].
    impl fs::Backend for EmbeddedClient {
        type File = OpenAdapter;
        type Metadata = MetadataAdapter;
        type OpenFuture = Ready<io::Result<Self::File>>;
        type MetadataFuture = Ready<io::Result<Self::Metadata>>;

        fn open(&self, path: PathBuf) -> Self::OpenFuture {
            ready(match client_static_lookup(&path) {
                Some(entry @ DirEntry::File(file)) => Ok(OpenAdapter {
                    cursor: io::Cursor::new(file.contents()),
                    metadata: MetadataAdapter(entry),
                }),
                None => Err(io::ErrorKind::NotFound.into()),
                Some(DirEntry::Dir(_)) => Err(io::ErrorKind::IsADirectory.into()),
            })
        }

        fn metadata(&self, path: PathBuf) -> Self::MetadataFuture {
            ready(match client_static_lookup(&path) {
                Some(file) => Ok(MetadataAdapter(file)),
                None => Err(io::ErrorKind::NotFound.into()),
            })
        }
    }

    impl fs::File for OpenAdapter {
        type Metadata = MetadataAdapter;
        type MetadataFuture<'a> = Ready<io::Result<MetadataAdapter>>;
        fn metadata(&self) -> Self::MetadataFuture<'_> {
            ready(Ok(self.metadata))
        }
    }

    impl OpenAdapter {
        fn project_cursor(self: Pin<&mut Self>) -> Pin<&mut io::Cursor<&'static [u8]>> {
            Pin::new(&mut self.get_mut().cursor)
        }
    }

    impl AsyncRead for OpenAdapter {
        fn poll_read(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut tokio::io::ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
            self.project_cursor().poll_read(cx, buf)
        }
    }

    impl AsyncSeek for OpenAdapter {
        fn start_seek(self: Pin<&mut Self>, position: io::SeekFrom) -> io::Result<()> {
            self.project_cursor().start_seek(position)
        }

        fn poll_complete(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<u64>> {
            self.project_cursor().poll_complete(cx)
        }
    }

    impl fs::Metadata for MetadataAdapter {
        fn is_dir(&self) -> bool {
            self.0.as_dir().is_some()
        }

        fn modified(&self) -> io::Result<std::time::SystemTime> {
            Err(io::ErrorKind::Unsupported.into())
        }

        fn len(&self) -> u64 {
            match self.0 {
                DirEntry::Dir(_) => 0,
                DirEntry::File(file) => file.contents().len() as u64,
            }
        }
    }
}

// for unused-dependencies analysis
#[cfg(not(feature = "embed"))]
mod embedded {
    use include_dir as _;
}

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
            AicClientSource::Embedded => axum::routing::get(embedded::client),
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
    use axum::body;
    use axum::extract::Path;
    use axum::http::StatusCode;
    use axum::response::{IntoResponse, Response};

    // TODO: need to support optionally embedding the release build rather than the dev build
    static CLIENT_STATIC: include_dir::Dir<'static> =
        include_dir::include_dir!("$AIC_CLIENT_BUILD_DIR");

    /// Handler for client static files
    pub(crate) async fn client(path: Option<Path<String>>) -> impl IntoResponse {
        // based on example code https://bloerg.net/posts/serve-static-content-with-axum/
        let path = match &path {
            None => "index.html",
            Some(Path(path_string)) => {
                // TODO: validate that this is url-escaping-correct (i.e. over-escaped characters should still match)
                path_string.as_str()
            }
        };
        match CLIENT_STATIC.get_file(path) {
            None => Response::builder()
                .status(StatusCode::NOT_FOUND)
                .body(body::Body::from(String::from("Not found")))
                .unwrap(),
            Some(file) => {
                // Note that we're matching the *file's statically known path*, not the provided URL,
                // to ensure that this is not controlled by the request.
                // TODO: I'd rather have a hardcoded list for our purposes than mime_guess, but
                // I want to match the non-embedded option and `tower_http::services::ServeDir`
                // doesn't offer an option that isn't mime_guess.
                let content_type = mime_guess::from_path(file.path()).first_or_octet_stream();
                Response::builder()
                    .status(StatusCode::OK)
                    .header("Content-Type", content_type.as_ref())
                    // TODO: caching headers
                    .body(body::Body::from(file.contents()))
                    .unwrap()
            }
        }
    }
}

// for unused-dependencies analysis
#[cfg(not(feature = "embed"))]
mod embedded {
    use include_dir as _;
    use mime_guess as _;
}

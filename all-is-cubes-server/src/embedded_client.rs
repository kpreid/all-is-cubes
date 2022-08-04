use axum::body;
use axum::extract::Path;
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};

static CLIENT_STATIC: include_dir::Dir<'static> =
    include_dir::include_dir!("$CARGO_MANIFEST_DIR/../all-is-cubes-wasm/dist");

/// Handler for client static files
pub(crate) async fn client(Path(path): Path<String>) -> impl IntoResponse {
    // based on example code https://bloerg.net/posts/serve-static-content-with-axum/
    let path = if path == "/" {
        "index.html"
    } else {
        // TODO: validate that this is url-escaping-correct (i.e. over-escaped characters should still match)
        path.trim_start_matches('/') // absolute to relative
    };
    match CLIENT_STATIC.get_file(path) {
        None => Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(body::boxed(body::Body::from(String::from("Not found"))))
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
                // TODO: content type
                .body(body::boxed(body::Full::from(file.contents())))
                .unwrap()
        }
    }
}

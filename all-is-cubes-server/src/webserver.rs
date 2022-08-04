//! Top-level code for server that serves the All is Cubes web client as well as being a game
//! server in the multiplayer sense (eventually).

use std::ffi::OsStr;

use axum::body::{self};
use axum::extract::Path;
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use axum::routing::get;

/// Run the All is Cubes web server on port 8833.
///
/// TODO: configurability of ports, etc.
pub async fn server_main() -> Result<(), anyhow::Error> {
    // TODO: serve static at well defined subdir
    let app = axum::Router::new().route("/*path", get(client_static));

    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], 8833));

    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await?;
    Ok(())
}

static CLIENT_STATIC: include_dir::Dir<'static> =
    include_dir::include_dir!("$CARGO_MANIFEST_DIR/static-all-is-cubes-wasm/");
async fn client_static(Path(path): Path<String>) -> impl IntoResponse {
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
            let content_type = match file.path().extension().and_then(OsStr::to_str) {
                Some("css") => "text/css;charset=utf-8",
                Some("html") => "text/html;charset=utf-8",
                Some("js") => "text/javascript;charset=utf-8",
                Some("json") => "application/json",
                Some("wasm") => "application/wasm",
                _ => "application/octet-stream", // TODO: log unknowns
            };
            Response::builder()
                .status(StatusCode::OK)
                .header("Content-Type", content_type)
                // TODO: content type
                .body(body::boxed(body::Full::from(file.contents())))
                .unwrap()
        }
    }
}

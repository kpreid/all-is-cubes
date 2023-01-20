//! Top-level code for server that serves the All is Cubes web client as well as being a game
//! server in the multiplayer sense (eventually).

use std::future::Future;
use std::io;
use std::net::SocketAddr;

use axum::http::StatusCode;
use axum::response::IntoResponse;

/// Run the All is Cubes web server on an arbitrary local port.
///
/// Returns the base URL to access it, and [TODO: explain the future, and have a shutdown plan]
pub fn start_server(
    bind_addr: SocketAddr,
) -> Result<(String, impl Future<Output = Result<(), anyhow::Error>>), anyhow::Error> {
    #[cfg(feature = "embed")]
    let static_service = axum::routing::get(crate::embedded_client::client);
    #[cfg(not(feature = "embed"))]
    let static_service = axum::routing::get_service(tower_http::services::ServeDir::new(concat!(
        // TODO: should be a run-time specified path and not depend on the workspace.
        env!("CARGO_MANIFEST_DIR"),
        "/../all-is-cubes-wasm/dist/"
    )))
    .handle_error(handle_error);

    // TODO: serve static at well defined subdir separate from root, so that we have
    // more division of responsibility in which urls mean what
    let app = axum::Router::new()
        .route("/", static_service.clone())
        .route("/*path", static_service);

    let server = axum::Server::bind(&bind_addr).serve(app.into_make_service());
    // TODO: refactor so stdout writing isn't hardcoded into this function
    let url = format!("http://{}/", server.local_addr());

    Ok((url, async move { Ok(server.await?) }))
}

#[allow(dead_code)] // currently unused if feature="embed"
async fn handle_error(_err: io::Error) -> impl IntoResponse {
    (
        StatusCode::INTERNAL_SERVER_ERROR,
        "An internal error occurred.",
    )
}

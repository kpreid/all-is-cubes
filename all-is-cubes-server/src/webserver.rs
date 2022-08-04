//! Top-level code for server that serves the All is Cubes web client as well as being a game
//! server in the multiplayer sense (eventually).

use std::io;

use axum::http::StatusCode;
use axum::response::IntoResponse;

/// Run the All is Cubes web server on port 8833.
///
/// TODO: configurability of ports, etc.
pub async fn server_main() -> Result<(), anyhow::Error> {
    #[cfg(feature = "embed")]
    let static_service = axum::routing::get(crate::embedded_client::client);
    #[cfg(not(feature = "embed"))]
    let static_service = axum::routing::get_service(tower_http::services::ServeDir::new(concat!(
        // TODO: should be a run-time specified path
        env!("CARGO_MANIFEST_DIR"),
        "/static-all-is-cubes-wasm/"
    )))
    .handle_error(handle_error);

    // TODO: serve static at well defined subdir
    let app = axum::Router::new().route("/*path", static_service);

    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], 8833));

    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await?;
    Ok(())
}

#[allow(dead_code)] // currently unused if feature="embed"
async fn handle_error(_err: io::Error) -> impl IntoResponse {
    (
        StatusCode::INTERNAL_SERVER_ERROR,
        "An internal error occurred.",
    )
}

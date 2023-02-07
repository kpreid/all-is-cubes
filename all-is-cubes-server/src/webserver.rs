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
    client_source: &AicClientSource,
) -> Result<(String, impl Future<Output = Result<(), anyhow::Error>>), anyhow::Error> {
    let static_service = client_source.static_service();

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
    fn static_service(&self) -> axum::routing::MethodRouter {
        match self {
            #[cfg(feature = "embed")]
            AicClientSource::Embedded => axum::routing::get(crate::embedded_client::client),
            AicClientSource::Workspace => {
                axum::routing::get_service(tower_http::services::ServeDir::new(concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/../all-is-cubes-wasm/dist/"
                )))
                .handle_error(handle_error)
            }
        }
    }
}

async fn handle_error(_err: io::Error) -> impl IntoResponse {
    (
        StatusCode::INTERNAL_SERVER_ERROR,
        "An internal error occurred.",
    )
}

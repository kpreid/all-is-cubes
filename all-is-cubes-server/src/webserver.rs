//! Top-level code for server that serves the All is Cubes web client as well as being a game
//! server in the multiplayer sense (eventually).

use std::future::Future;
use std::net::SocketAddr;

/// Run the All is Cubes web server on an arbitrary local port.
///
/// Returns the base URL to access it, and [TODO: explain the future, and have a shutdown plan]
pub async fn start_server(
    bind_addr: SocketAddr,
    client_source: &crate::client_static::AicClientSource,
) -> Result<
    (
        String,
        impl Future<Output = Result<(), anyhow::Error>> + use<>,
    ),
    anyhow::Error,
> {
    let static_router = client_source.client_router();

    // TODO: serve static at well defined subdir separate from root, so that we have
    // more division of responsibility in which urls mean what
    let app = static_router;

    let listener = tokio::net::TcpListener::bind(bind_addr).await?;
    let url = format!("http://{}/", listener.local_addr()?);
    let server = axum::serve(listener, app.into_make_service());

    Ok((url, async move { Ok(server.await?) }))
}

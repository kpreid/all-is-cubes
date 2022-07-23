//! Top-level code for server that serves the All is Cubes web client as well as being a game
//! server in the multiplayer sense (eventually).

use static_dir::static_dir;

/// Run the All is Cubes web server on port 8833.
///
/// TODO: configurability of ports, etc.
pub async fn server_main() {
    warp::serve(static_dir!("./static-all-is-cubes-wasm/"))
        .run(([127, 0, 0, 1], 8833))
        .await;
}

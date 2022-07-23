//! main() for a server that serves the All is Cubes client as well as being a game
//! server.

use all_is_cubes_server::webserver::server_main;

#[tokio::main]
async fn main() {
    server_main().await;
}

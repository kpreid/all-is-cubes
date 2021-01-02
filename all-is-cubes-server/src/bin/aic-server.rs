// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! main() for a server that serves the All is Cubes client as well as being a game
//! server.

use all_is_cubes_server::webserver::server_main;

#[tokio::main]
async fn main() {
    server_main().await;
}

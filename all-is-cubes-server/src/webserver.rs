// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Top-level code for server that serves the All is Cubes web client as well as being a game
//! server in the multiplayer sense (eventually).

use static_dir::static_dir;

pub async fn server_main() {
    // TODO: configurability of ports, etc.

    warp::serve(static_dir!("../all-is-cubes-wasm/dist/"))
        .run(([127, 0, 0, 1], 8833))
        .await;
}

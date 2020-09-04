// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Top-level code for server that serves the All is Cubes web client as well as being a game
//! server in the multiplayer sense (eventually).

pub async fn server_main() {
    // TODO: configurability of ports, etc.

    // TODO: need build script glue to guarantee this is up to date.
    // Also, look into embedding files directly in the binary, e.g.
    // https://crates.io/crates/static_dir claims to be able to do that.

    warp::serve(warp::fs::dir("all-is-cubes-wasm/dist/"))
        .run(([127, 0, 0, 1], 8833))
        .await;
}

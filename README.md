All is Cubes
============

This project is (will be) a game engine for worlds made of cubical blocks (“blocky voxels”). The particular features of this engine are that each ordinary block is itself made out of blocks, and all game mechanics are defined by data within the world that can be interactively edited.

Or rather, that's the plan; for now, it mainly supports rendering of said content, and I am aiming to duplicate and expand on the functionality of my previous work [Cubes](https://github.com/kpreid/cubes/). That was written in JavaScript; this compiles to WebAssembly so the result will still run in the browser, but also someday support a multiplayer server and storage backend.

![](https://annex.switchb.org/2023/10-24-all-is-cubes-atrium.jpg)

Above: a partial, procedural voxel recreation of the Sponza Atrium computer graphics test scene, assembled and rendered using All is Cubes.

Project organization
--------------------

This repository is divided into several Rust packages:

* `all-is-cubes/` is a library containing all of the fundamental data types and simulation algorithms.
* `all-is-cubes-render/` is a library containing the CPU raytracer rendering code, and abstract rendering types and traits.
* `all-is-cubes-mesh/` is a library containing algorithms to generate triangle meshes from the voxel data.
* `all-is-cubes-gpu/` is a library containing the GPU-based rendering code, depending on [`wgpu`](https://wgpu.rs/).
* `all-is-cubes-ui/` is a library containing a widget framework and basic user interface functions (not platform-specific).
* `all-is-cubes-content/` is a library containing “game content” — procedural generation and demos — that does not need to be part of the core library but is used by all of the below binaries.
* `all-is-cubes-port/` is a library implementing import and export to various file formats.
* `all-is-cubes-desktop/` is a standalone game app which uses `winit` and `wgpu` for platform windowing & graphics, or ASCII-art raytracing in the terminal.
* `all-is-cubes-wasm/` is the game app code for the browser/WebAssembly environment (if compiled outside of `wasm32` architecture, it will be an empty crate).
* `all-is-cubes-server/` is to be a network server for the game, but right now only contains a preconfigured HTTP static file server.
* `all-is-cubes-base/` is for internal use only.

The following packages and other directories are for testing:

* `fuzz/` is a package containing fuzz tests.
* `test-renderers/` is a package containing image-comparison tests.
* `tools/` contains miscellaneous files relating to development.

Building and running
--------------------

**First time setup:** Unless you don't intend to build the WebAssembly/WebGL version of all-is-cubes or run the full test suite, you will need to install

* `wasm-pack` (`cargo install wasm-pack`)
* [`cargo-about`] (`cargo install cargo-about`)

(If you would like to avoid all of this, then descend into the `all-is-cubes` or `all-is-cubes-desktop` directories as applicable and work using plain `cargo [build|test|run]` there, since those directories contain no web dependencies.)

Because of the complication of having some wasm-specific code, some commands for building, testing, and running the entire project are collected in the `xtask` build tool for convenience and to ensure all non-Rust files are built when needed.

(**You _must_ use `cargo xtask` to build the workspace at least once** for `all-is-cubes-server` to build correctly, as it expects to embed files that are built and copied in by previous steps.)

*   Run all tests: `cargo xtask test`

*   Build and lint all the code: `cargo xtask lint`

*   Wasm development server: `cargo xtask run-dev`

*   Run the desktop/console game: `cargo run --bin all-is-cubes -- <options>`

*   Run the non-dev-mode game server: `cargo xtask run-game-server`

Stability and versioning
------------------------

All is Cubes is an ambitious hobby project; many features necessary to be “complete” are not yet implemented, and implementing them will require incompatible changes. During development, library APIs change regularly, and the version numbering will mark these versions as incompatible (e.g. 0.3 to 0.4); there will be no 1.0 version until I am confident that future versions will at least have save data compatibility. However, I have made some attempt to document API elements that are _more likely_ to change in future versions.

MSRV policy: The current release accurately documents its `rust-version` in `Cargo.toml`.
Future releases will typically require the current stable Rust version as of that release.

License
-------

All source code and other materials are Copyright © 2020-2025 Kevin Reid, and licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option. 

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.


[`cargo-about`]: https://crates.io/crates/cargo-about

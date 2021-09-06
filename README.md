All is Cubes
============

This project is (will be) a game engine for worlds made of cubical blocks. The unique feature of this engine is that each ordinary block is itself made out of blocks; each block can be directly edited in the same fashion as the outside world.

Or rather, that's the plan; for now, this is just my very first project written in Rust, and I am aiming to duplicate and expand on the functionality of my previous work [Cubes](https://github.com/kpreid/cubes/). That was written in JavaScript; this compiles to WebAssembly so the result will still run in the browser (hopefully faster), but also support a multiplayer server and storage backend.

![](https://switchb.org/kpreid/2020/all-is-cubes-10-13-progress.png)

Project organization
--------------------

This repository is divided into three Rust packages, split roughly according to different sets of dependencies:

* `all-is-cubes/` contains all of the fundamental data types and algorithms. It is intended to be usable as a library but also contains the non-platform-specific top-level application logic.
* `all-is-cubes-desktop/` is a standalone game app which will use [GLFW](https://www.glfw.org/) for platform windowing & graphics, or ASCII-art raytracing in the terminal.
* `all-is-cubes-wasm/` is the game app code for the browser/WebAssembly environment (if compiled outside of `wasm32` architecture, it will be empty). It is also a NPM package, which embeds the Rust code by way of `wasm-pack`. In order to use this, you must use either webpack-dev-server or the web server described next.
* `all-is-cubes-server/` is to be a network server for the game, but right now only contains a preconfigured HTTP static file server.

Building and running
--------------------

**First time setup:** Unless you don't intend to build the WebAssembly/WebGL version of all-is-cubes or run the full test suite, you will need to install `npm` (use your choice of source) and `wasm-pack` (I suggest `cargo install wasm-pack`). (If you would like to avoid this, then descend into the `all-is-cubes` or `all-is-cubes-desktop` directories as applicable and work using plain `cargo [build|test|run]` there, since those directories contain no web dependencies.)

Because of the complication of having some wasm-specific code, some commands for building, testing, and running the entire project are collected in the `xtask` build tool for convenience and to ensure all non-Rust files are built when needed.

(**You _must_ use `cargo xtask` to build the workspace at least once** for `all-is-cubes-server` to build correctly, as it expects to embed files that are built and copied in by previous steps.)

*   Run all tests: `cargo xtask test`

*   Build and lint all the code: `cargo xtask lint`

*   Development server: `cargo xtask run-dev`

    Note: the webpack-dev-server will automatically rebuild when files change *unless those files are outside of `all-is-cubes-wasm/`,* unfortunately.

*   Run the desktop/console game: `cargo run --bin all-is-cubes -- <options>`

*   Run the non-dev-mode game server: `cargo xtask run-game-server`

Stability and versioning
------------------------

All is Cubes is just getting started; many core features are not yet implemented, and implementing them will require incompatible changes. During development, library APIs change regularly, and the version numbering will mark these versions as incompatible (e.g. 0.3 to 0.4); there will be no 1.0 version until I am confident that future versions will at least have save data compatibility. However, I have made some attempt to mark API elements that are _more likely_ to change in future versions.

MSRV: The minimum supported Rust version is the current _stable_ version. This policy may change after a future 1.0 release.

License
-------

Except as otherwise noted, all source code and other materials are Copyright © 2020-2021 Kevin Reid, and licensed as follows (the “MIT License”):

> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
> 
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

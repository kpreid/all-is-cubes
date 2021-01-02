All is Cubes
============

This project is (will be) a game engine for worlds made of cubical blocks. The unique feature of this engine is that each ordinary block is itself made out of blocks; each block can be directly edited in the same fashion as the outside world.

Or rather, that's the plan; for now, this is just my very first project written in Rust, and I am aiming to duplicate and expand on the functionality of my previous work [Cubes](https://github.com/kpreid/cubes/). That was written in JavaScript; this compiles to WebAssembly so the result will still run in the browser (hopefully faster), but also support a multiplayer server and storage backend.

Project organization and development
------------------------------------

This repository is divided into three packages (crates):

* `all-is-cubes/` contains all of the fundamental data types and algorithms. It is intended to be usable as a library but also contains the non-platform-specific application logic.
* `all-is-cubes-wasm/` is the web UI/renderer which will only run in the browser/WebAssembly environment (if compiled outside of `wasm32` architecture, it will be empty). It is also a NPM package, which embeds the Rust code by way of `wasm-pack`.
* `all-is-cubes-server/` is to be a network server for the game, but right now only contains a static file server and code to render worlds as colored ASCII art (which is in this category because it depends on the `termion` package which will not compile to WebAssembly).

Because of the complication of having some wasm-specific code, some commands for building, testing, and running the entire project are collected in the `Makefile` for convenience and to ensure all non-Rust files are built when needed:

*   Test: `make test`

*   Lint: `make lint`

*   Development server: `make run-dev`

    Note: the webpack-dev-server will automatically rebuild when files change *unless those files are outside of `all-is-cubes-wasm/`.*

*   Run the standalone game server: `make run-server`

*   Run the terminal raytracer version: `make run-console`

License
-------

Except as otherwise noted, all source code and other materials are Copyright © 2020-2021 Kevin Reid, and licensed as follows (the “MIT License”):

> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
> 
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

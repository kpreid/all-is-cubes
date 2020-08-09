All Is Cubes
============

This project is (will be) a game engine for worlds made of cubical blocks. The unique feature of this engine is that each ordinary block is itself made out of blocks; each block can be directly edited in the same fashion as the outside world.

Or rather, that's the plan; for now, this is just my very first project written in Rust, and I am aiming to duplicate and expand on the functionality of my previous work [Cubes](https://github.com/kpreid/cubes/). That was written in JavaScript; this compiles to WebAssembly so the result will still run in the browser (hopefully faster), but also support a multiplayer server and storage backend.

Project organization and development
------------------------------------

The files in this directory define both a Rust package and a NPM package; the latter depends on the former via `wasm-pack`. The Rust package has two *features* controlling necessary conditional compilation for the target platform: `console` for things that only work in a binary talking to a text-mode terminal, and `wasm` for things that only work in WebAssembly.

**Testing:** Right now, all the tests are pure Rust tests that will be run appropriately if you `cargo test --features=console`. In the future there will likely be browser-only tests too, in which case `npm test` will do both.

**Development server:** Run `npm start`.

License
-------

Except as otherwise noted, all source code and other materials are Copyright © 2020 Kevin Reid, and licensed as follows (the “MIT License”):

> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
> 
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

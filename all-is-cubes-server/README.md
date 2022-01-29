All is Cubes (Web Edition)
==========================

A “voxel game” where each block is made out of smaller blocks (one level of recursion).

This is a work in progress; it's my very first project written in Rust, and I am aiming to duplicate and expand on my previous work [Cubes](https://github.com/kpreid/cubes/). For more information about the currently implemented functionality, see the [core library crate `all-is-cubes`.][all-is-cubes]

This crate is the “web edition”: it currently consists of a simple web server which serves the prebuilt WASM and JavaScript files for the game. Eventually I intend it to become a multiplayer (or at least remotely-saved-data) game server in addition to this, but none of that networking functionality is implemented at the monent.

You can also run the [“desktop” version][all-is-cubes-desktop] without any of this web server nonsense.

[all-is-cubes]: https://crates.io/crates/all-is-cubes
[all-is-cubes-desktop]: https://crates.io/crates/all-is-cubes-desktop

Stability and versioning
------------------------

All is Cubes is just getting started; many core features are not yet implemented, and implementing them will require incompatible changes. The 0.x version number will be incremented as needed to indicate incompatible releases. There will be no 1.0 version until I am confident that future versions will at least have save data compatibility.

MSRV: The minimum supported Rust version is the current _stable_ version. This policy may change after a future 1.0 release.

License
-------

Except as otherwise noted, all source code and other materials are Copyright © 2020-2022 Kevin Reid, and licensed as follows (the “MIT License”):

> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
> 
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

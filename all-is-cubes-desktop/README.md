All is Cubes (Desktop Edition)
==============================

A “voxel game” where each block is made out of smaller blocks (one level of recursion).

This is a work in progress; it's my very first project written in Rust, and I am aiming to duplicate and expand on my previous work [Cubes](https://github.com/kpreid/cubes/). For more information about the currently implemented functionality, see the [core library crate `all-is-cubes`.][all-is-cubes]

This crate is the “desktop edition” that you can build and run to “play the game” on your own computer, as much as there is one right now (you can only admire the scenery, and place and remove blocks). You can also consider running the [web version][all-is-cubes-server].

[all-is-cubes]: https://crates.io/crates/all-is-cubes
[all-is-cubes-server]: https://crates.io/crates/all-is-cubes-server

Requirements
------------

Your system must have these installed:

* OpenGL. (Note: This excludes macOS on ARM computers.)
* The `cmake` build tool. (This is used to build GLFW.)

Usage
-----

By default, running `all-is-cubes` will open a window with a newly created world containing various test cases (there is no saving yet). Run `all-is-cubes --help` to find out options, including different starting templates and different graphics modes (including a terminal-based mode).


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

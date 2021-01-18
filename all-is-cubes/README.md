All is Cubes
============

A “voxel game” engine where each block is made out of smaller blocks (one level of recursion).

This is a work in progress; it's my very first project written in Rust, and I am aiming to duplicate and expand on my previous work [Cubes](https://github.com/kpreid/cubes/). Currently implemented:

* Core data model of [Blocks] and [Spaces] (subject to change).
* Rendering using [luminance] (OpenGL bindings) or a software raytracer.
* Basic user interface (not yet able to edit all aspects of the data).
* 2D drawing into the voxel space, built on top of the [embedded-graphics] library.

[blocks]: https://docs.rs/all-is-cubes/0.1.2/all_is_cubes/block/enum.Block.html
[spaces]: https://docs.rs/all-is-cubes/0.1.2/all_is_cubes/space/struct.Space.html
[luminance]: https://github.com/phaazon/luminance-rs/
[embedded-graphics]: https://crates.io/crates/embedded-graphics

This library is intended to be somewhat reusable for working with block/voxel data, but it does not currently have a particularly stable interface as I am still changing it as requirements are discovered.

Related crates
--------------

*   [`all-is-cubes-desktop`](https://crates.io/crates/all-is-cubes-desktop)
    is a binary you can build and run to “play the game”.
*   [`all-is-cubes-server`](https://crates.io/crates/all-is-cubes-server)
    is to be a network server for the game, but right now only contains a preconfigured HTTP static file server for the web version of the “game”.

“The game” is in quotes because all you can do for the moment is place and remove blocks.

License
-------

Except as otherwise noted, all source code and other materials are Copyright © 2020-2021 Kevin Reid, and licensed as follows (the “MIT License”):

> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
> 
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

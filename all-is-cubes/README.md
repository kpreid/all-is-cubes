All is Cubes
============

A “voxel game” engine where each block is made out of smaller blocks (one level of recursion).

This is a work in progress. Currently implemented:

* Core data model of [Blocks] and [Spaces] (subject to change).
* Some ways for these things to change and interact with each other.
* Foundations of rendering using a CPU-only raytracer. (See `all-is-cubes-gpu` for GPU.)
* 2D drawing into the voxel space, built on top of the [embedded-graphics] library.

For more information on the capabilities, limitations, and requirements of the library,
please consult [the crate documentation].

This library is intended to be somewhat reusable for working with block/voxel data,
but its API is not stable and it makes many specific assumptions.

[the crate documentation]: https://docs.rs/all-is-cubes/0.10.0/all_is_cubes/
[Blocks]: https://docs.rs/all-is-cubes/0.10.0/all_is_cubes/block/enum.Block.html
[Spaces]: https://docs.rs/all-is-cubes/0.10.0/all_is_cubes/space/struct.Space.html
[embedded-graphics]: https://crates.io/crates/embedded-graphics

Related crates
--------------

*   [`all-is-cubes-mesh`](https://crates.io/crates/all-is-cubes-mesh)
    generates triangle meshes from `all-is-cubes` voxel data.
*   [`all-is-cubes-gpu`](https://crates.io/crates/all-is-cubes-gpu)
    renders using GPU functionality rather than CPU-only.
*   [`all-is-cubes-ui`](https://crates.io/crates/all-is-cubes-ui)
    contains a widget framework and basic user interface functions (not platform-specific)
*   [`all-is-cubes-content`](https://crates.io/crates/all-is-cubes-content)
    contains procedural generation and data for concrete “game content” as opposed to the engine.
*   [`all-is-cubes-port`](https://crates.io/crates/all-is-cubes-port)
    provides import and export to various formats.
*   [`all-is-cubes-desktop`](https://crates.io/crates/all-is-cubes-desktop)
    is a binary you can build and run to “play the game”.
*   [`all-is-cubes-server`](https://crates.io/crates/all-is-cubes-server)
    is to be a network server for the game, but right now only contains a preconfigured HTTP static file server for the web version of the “game”.

“The game” is in quotes because all you can do for the moment is place and remove blocks and look at the scenery.

Stability and versioning
------------------------

All is Cubes is an ambitious hobby project; many features necessary to be “complete” are not yet implemented, and implementing them will require incompatible changes. During development, library APIs change regularly, and the version numbering will mark these versions as incompatible (e.g. 0.3 to 0.4); there will be no 1.0 version until I am confident that future versions will at least have save data compatibility. However, I have made some attempt to document API elements that are _more likely_ to change in future versions.

MSRV policy: The current release accurately documents its `rust-version` in `Cargo.toml`.
Future releases will typically require the current stable Rust version as of that release.

License
-------

All source code and other materials are Copyright © 2020-2026 Kevin Reid, and licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option. 

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

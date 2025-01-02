All is Cubes (Desktop Edition)
==============================

A “voxel game” where each block is made out of smaller blocks (one level of recursion).

This is a work in progress. For more information about the currently implemented functionality, see the [core library crate `all-is-cubes`.][all-is-cubes]

This package is the “desktop edition” that you can build and run to “play the game” on your own computer, as much as there is one right now (you can only admire the scenery, and place and remove blocks). You can also consider running the [web version][all-is-cubes-server].

This package includes a library crate, but it is not yet a good general-purpose library.
it currently only exists as a library so that additional development tools can reuse the same UI code.
Use at your own risk. Documentation is lacking.

[all-is-cubes]: https://crates.io/crates/all-is-cubes
[all-is-cubes-server]: https://crates.io/crates/all-is-cubes-server

Requirements
------------

Your system must have these installed:

* If on Linux, then the libraries that Ubuntu calls `libxrandr-dev`, `xorg-dev`, `libx11-xcb-dev`, `libwayland-dev`, and `libasound2-dev`.

Usage
-----

By default, running `all-is-cubes` will open a window with a newly created world containing various test cases. Run `all-is-cubes --help` to find out options, including different starting templates and different graphics modes (including a terminal-based mode).

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

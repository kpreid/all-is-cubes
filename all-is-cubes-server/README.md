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

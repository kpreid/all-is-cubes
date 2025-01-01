All is Cubes Mesh Generation
============================

Data structures and algorithms for converting [`all-is-cubes`] voxel data to triangle meshes for rendering or export.

All of the algorithms are independent of graphics API, but they require providing vertex and texture data types suitable for the API or data format you wish to use.

If you are looking for a renderer which produces images,
you may use the raytracer built in to [`all-is-cubes`],
or send the meshes to a GPU with [`all-is-cubes-gpu`].

[`all-is-cubes`]: https://crates.io/crates/all-is-cubes
[`all-is-cubes-gpu`]: https://crates.io/crates/all-is-cubes-gpu

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

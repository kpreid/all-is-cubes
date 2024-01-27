# Changelog

## Unreleased

### Added

- Functionality:
    - Sound effects for character/world collisions.

- Graphics:
    - Improved initial startup behavior of mesh generation; chunks will be filled in sooner but lacking detail.
    - Windowed raytracing mode now uses background threads for increased throughput.

- `all-is-cubes` library:
    - Now `no_std` compatible (with caveats), if the `std` feature is disabled.

    - New variant `block::Primitive::Text` allows displaying text as part of a block, without having to separately draw the text as blocks first.

    - Expanded `block::BlockAttributes::tick_action` can now specify a time period rather than always occurring on the next tick.

    - New `block::CompositeOperator` variants `Atop` and `Out`, as per the standard Porter-Duff compositing operators.

    - New field `block::EvaluatedBlock::face_colors` provides colors which may be distinct for each face of the block. These colors may be used for low-detail rendering.

    - New method `Camera::near_plane_distance()` returns the (currently hard-coded) near plane distance used in the calculated projection matrix.

    - New type `linking::Provider` is `BlockProvider` but generalized to non-block types.

    - New type `math::Axis` is an enum of coordinate axes.

    - New type `math::Cube` represents a unit cube on the grid; it replaces many previous uses of `GridPoint` to identify cubes.

    - New constructor function `math::GridAab::from_ranges()` can make it more convenient to construct `GridAab`s where different axes are handled differently.
    - New method `math::GridAab::volume_f64()` returns the volume as a float, for calculations where volume is a scaling factor.

    - New type `math::Gridgid` represents rigid transformations (including reflection), a useful subset of what `GridMatrix` already could do.

        The following new functions return `Gridgid`:
        
        - `math::Face6::face_transform()`
        - `math::GridRotation::to_positive_octant_transform()`
    
    - New type `math::Vol` is a more general replacement for `math::GridArray` which allows choice of the data container type (including `&[T]` for borrowing without additional indirection), and also a replacement for uses of `math::GridAab` which care about volume and linearization.

    - New type `op::Operation` describes local transformations of a `Space` and an interacting agent with an inventory.
      It is now the type for specifying `BlockAttributes::tick_action` effects, and in the future will be used by `Tool`s as well.

    - `universe::UniverseTransaction::insert_anonymous()` and `universe::UniverseTransaction::insert_mut()` allow conveniently building transactions that insert multiple members without needing `merge()`s. Such transactions allow building universe content without passing around a `&mut Universe`.

- Demo content and `all-is-cubes-content` library:
    - The `atrium` scene now includes more elements.

- The `all-is-cubes-desktop` package now contains a library, which in principle can be used to create applications with different behavior than the included binary (such as creating and displaying a universe from some data source other than files and the provided templates). However, it does not yet have a well-designed API.

- `all-is-cubes-ui` library:
    - The UI optionally presents a “Quit” button, if configured via the `SessionBuilder`.
    - New type `vui::widgets::ButtonLabel` allows giving `ActionButton` and `ToggleButton` text labels in addition to or instead of icons.
    - New function `Session::set_character()` allows changing the player character without also changing the universe.
    - New function `vui::leaf_widget()` allows easier, less type-error-prone construction of `WidgetTree`s.
      It should generally be used in place of `LayoutTree::leaf()`.

### Changed

- `all-is-cubes` library:
    - Block schema and behavior changes:
        - Light emission is now a property of `Atom` blocks instead of `BlockAttributes`.
        - `EvaluatedBlock`’s `color` field is computed more accurately, ignoring voxels hidden by other voxels.
        - `BlockAttributes::tick_action` is now a dedicated `TickAction` type instead of `VoxelBrush`.
        - `BlockDef` now caches evaluation of its contained block. The cache is updated during `Universe::step()`.

    - All vector and matrix types from the library `cgmath` have been replaced with the library `euclid`.

    - All functions manipulating volume data in `Space`, `GridArray`/`Vol`, `Evoxels`, etc. have changed signature to use the new type `math::Cube` instead of `math::GridPoint`.

    - All functions using `usize` to identify a coordinate axis now use `math::Axis` instead.
      `Face6::axis_number()` and `Face7::axis_number()` are now called `axis()`.

    - `space::SpaceChange` now includes block indices, not just positions, in cube changes;
      and all variants have been renamed for clarity.

    - `space::SpaceTransaction` now exposes its components as individual `CubeTransaction`s, which can be mutated using `SpaceTransaction::at()`.

    - In block names and universe member names, `Cow<'static, str>` and `Arc<str>` have been replaced with `arcstr::ArcStr`.
      This type allows both refcounting and static literal strings.

    - The following functions have changed signature to use the new type `math::Gridgid`:
        - `math::GridAab::transform()`
        - `math::GridMatrix::decompose()`
        - `space::Space::draw_target()`
        - `space::SpaceTransaction::draw_target()`

    - The trait method `Behavior::step()` now must return a value of type `behavior::Then`, which specifies when the behavior should next be stepped.

    - `block::BlockBuilder::voxels_fn()` no longer takes a `&mut Universe` parameter.
      Instead, either the universe or a transaction must be supplied through a call to `.build_into()` or `.build_txn()`.

    - `block::EvalBlockError::StackOverflow` has been replaced with `block::EvalBlockError::BudgetExceeded`,
      which reflects a new, better-fitting block evaluation limit system.

    - `math::GridAab::volume()` now returns `Option<usize>`, in preparation for a future change where `GridAab` will not have a limit on volume.
    - `math::GridArray` is now `math::Vol` and allows choice of the data container type.

    - `camera::HeadlessRenderer` now returns a custom image container type `Rendering` instead of using `image::RgbaImage`.
      (This way, no dependency on `image` is needed.)

    - `linking::BlockProvider::install()` now requires a `&mut UniverseTransaction` instead of a `&mut Universe`.

    - `drawing::VoxelBrush::transform()` is renamed to `rotate()` and only accepts a rotation.
      This avoids confusion between points in space and cube-identifying coordinates.
    - Renamed `raytracer::PixelBuf` trait to `Accumulate`.

    - `all_is_cubes::util::CustomFormat` has been split into a separate library called `manyfmt`,
      reexported at `all_is_cubes::util::manyfmt`.
      The `CustomFormat` trait is now split into two traits, `Fmt` (for implementing) and `Refmt` (extension).

- `all-is-cubes-gpu` library:
    - Block textures are now fully dynamically allocated; it is no longer possible to run out of texture space.

- `all-is-cubes-mesh` library:
    - The new trait `MeshTypes` is now used to combine declaration of vertex and texture types.
      `BlockMesh`, `SpaceMesh`, `ChunkedSpaceMesh`, and `GetBlockMesh` all have a single `M: MeshTypes` parameter in place of multiple generics.

    - Major changes to texturing:
        - It is now possible for texture allocator implementations to use purely 2D texturing; `Tile`s are now subdivided into `Plane`s before usage, and each plane may provide its own texture coordinates independent of other planes in the volume.
        - Texture allocators may opt out of supporting multiple writes, and have only immutable allocations.

        - Texture allocators are expected to be able to store emissive color in addition to reflectance.
          The new type `texture::Channels` communicates which properties each allocation must actually store,
          so that allocators can conserve memory when it is not necessary.

        - Texture writes are now given data using `Vol<&[Evoxel]>` rather than pre-converted sRGB data;
        it is up to the texture implementation to convert and shuffle data as needed.

        - Trait method `GetBlockMesh::get_block_mesh()` takes additional arguments which support instanced rendering of blocks. (They are not yet used.)

        - Renamed `TextureAllocator` to `texture::Allocator`.
        - Renamed `TextureTile` to `texture::Tile`.
        - Renamed `NoTexture` to `texture::NoTexture`.
        - Renamed `NoTextures` to `texture::NoTextures`.

### Removed

- `all-is-cubes` library:
    - No longer depends on the `instant` library; WebAssembly builds do not need to depend on it to set its features.
    - `block::BlockBuilder::into_named_definition()` no longer exists.
      Instead, create the `BlockDef` separately after using the builder.
    - `block::BlockDef` no longer implements `Deref<Target = Block>`.
      Instead, call `BlockDef::block()` when needed.
    - `math::Aab::from_cube()` no longer exists. Use `Cube::aab()` instead.
    - `math::Face7::matrix()` no longer exists. Use `Face6::face_transform()` instead.
    - `math::GridAab::contains_cube()` no longer accepts `impl Into<GridPoint>`.
      Call sites should be changed to pass only `Cube`.
    - `math::GridAab::index()` no longer exists. Use `Vol::index()` instead.
    - `math::GridRotation::to_positive_octant_matrix()` no longer exists. Use `to_positive_octant_transform()` instead.
    - `math::cube_to_midpoint()` no longer exists. Use `Cube::midpoint()` instead.
    - `math::point_to_enclosing_cube()` no longer exists. Use `Cube::containing()` instead.
    - `space::SpaceTransaction::set_overwrite()` no longer exists. Use `transaction.at(cube).overwrite(block)` instead.

- `all-is-cubes-mesh` library:
    - No longer depends on the `instant` library; WebAssembly builds do not need to depend on it to set its features.
    - `TextureCoordinate` type alias no longer exists.
      Its only use was when implementing `TextureTile`; simply use `f32` instead.
    - `Texel` type alias no longer exists; callers are free to choose their own texel data type.

### Performance improvements

- Block voxel data is kept to smaller bounds where possible:
    - `BlockBuilder::voxels_fn()` trims empty space surrounding the produced voxels.
    - `all_is_cubes::block::Modifier::Composite` produces voxel data bounds that are the union or intersection of the inputs, as appropriate, rather than a full block.
- Various tweaks to reduce the size of compiled code.


## 0.6.0 (2023-07-29)

### Added

- Command line application (crate `all-is-cubes-desktop`) functionality:
    - Exporting universes to files; see below section on `all-is-cubes-port` for supported formats.
    - Option `--template-size` allows controlling the size of spaces produced by universe templates.
    - Window titles name the data source (file or template) for the contained universe.

- `all-is-cubes` library:
    - Many types, including `Universe` and its components, now support serialization via `serde`.
      This serialization support is still a work in progress and long-term save data compatibility is planned but not currently guaranteed.

    - `block::Atom`, a struct for the data of the `Primitive::Atom` enum variant.
    - `block::CompositeOperator::In` allows masking one block shape by another.
    - `block::Modifier::Composite` has a new option `disassemblable`, which causes `Block::unspecialize()` (and its callers such as `Tool::RemoveBlock`) to return the composed blocks separately instead of the composite.
    - `linking::BlockProvider` now has methods `subset()` (replace keys) and `map()` (replace values) to allow using `BlockProvider`s in more ways.
    - `math::GridAab::iter()` to iterate over `(cube, &item)`.
    - `save::WhenceUniverse`, a trait for associating a `Universe` with disk files or other persistent storage. `Universe` now keeps an `Arc<dyn WhenceUniverse>`.
    - `space::SpaceBuilder::palette_and_contents()` allows efficiently specifying arbitrary contents for a newly created `Space`.
    - `universe::RefVisitor` is now implemented for all `FnMut(&dyn URefErased)`, allowing visitors to simply be functions.
    - `universe::Universe::universe_id()` returns a unique identifier for this `Universe`.

- `all-is-cubes-port` library:
    - Import and export of a “native” file format (serialized `Universe`s).
    - Export to `.stl` meshes (commonly used for 3D printing).
    - Export to MagicaVoxel `.vox` files.
    - `ImportError` type for precise error reporting.

- `all-is-cubes-ui` library:
    - `vui::LayoutTree::Shrink` allows a subtree to be shrunk to only be as big as needed, rather than filling available space, allowing for “shrink wrapped” layouts such as framed dialog boxes.
    - `vui::widgets::Frame::as_background_of()` allows conveniently making dialog frames.

### Changed

- Graphics:
    - `Space` light propagation no longer updates deterministically, but as much as possible given available time for the computation.

- New crate `all-is-cubes-mesh` contains the former contents of `all_is_cubes::mesh`, except for `LineVertex` which is now in `all_is_cubes::math`.
    - `SpaceMesh` indices are now either `u16` or `u32` depending on the size of the mesh, rather than always `u32`. The new enum `all_is_cubes_mesh::IndexSlice` is used to work with them.
    - `SpaceMesh` index ranges and other metadata are now kept in a sub-struct `MeshMeta`.
    - Newly public/documented module `dynamic` provides support for incrementally updated meshes for interactive rendering.
    - Renamed: `BlockMeshProvider` is now `GetBlockMesh`. This aligns with the general principle of naming traits for the action that they enable.

- `all-is-cubes` library:
    - Block collision control has been redesigned:
        - `BlockCollision` is now stored in `Atom`s and not in `BlockAttributes`; `Primitive::Recur` blocks always get their collision from their component voxels.
        - `BlockCollision::Recur` no longer exists and `BlockCollision::Hard` cannot be used to disregard voxels' own collision.
        This change should be an overall simplification of the semantics and eliminates the common mistake of forgetting to specify `Recur`.
    
    - Universes are now considered to have an inherent, fixed time step.
        - This time step cannot yet be changed; it is currently always 1/60 second.
        - Within a single second, instants and `Tick`s have a “phase” value which cycles from 0 to 59.
          This will enable simulations to run on an intentionally slow but consistent schedule by skipping some ticks.
        - The `time` module contains new types `TickSchedule` and `Clock` to support this.

    - `block::Block::listen()` is now `evaluate_and_listen()` which includes a simultaneous `evaluate()`.
    - `block::Block::unspecialize()` now returns `Vec<Block>`, to allow for cases where a block comes apart into multiple parts, such as with `Modifier::Composite`.
    - `block::EvaluatedBlock` now has a `voxels` field of the new type `Evoxels`, which replaces the previous `resolution` and `voxels` fields.
      This simplifies the data model, in that there is now _always_ a set of `Evoxel`s defining a block's shape, even if there's only one of them, and it is always found in the `voxels` field. This is a breaking change for code that accesses `EvaluatedBlock` data.
    - `block::Modifier::attach()` has been replaced by `block::Block::with_modifier()`.
      This is intended to be more convenient in all cases.
    - `block::Primitive::Atom` now contains a `struct Atom` instead of individual fields.
    - `block::AIR` now has its own dedicated primitive, `Primitive::Air`. The behavior is unchanged.
    - `camera::Flaws` now implements `Display`. Use this instead of `Debug` for printing the flaws.
    - `math::Geometry::wireframe_points()` now produces a new type `mesh::LineVertex` instead of a tuple (with the same position and color data).
    - `space::Space::extract()` now passes one argument instead of three to the callback function; it is of the new type `Extract` which has methods to return all the previously available data. This is intended to be more extensible and potentially more efficient.
    - `transaction::Transaction` can now produce any number of `Output`s, delivered through a callback.
    - `transaction::Merge` now has an associated type `Conflict` for more informative conflict errors.
    - `universe::Name` now has a variant `Pending` for not-yet-assigned names.
    - `universe::URef::name()` now returns an owned instead of borrowed `Name`.
    - `universe::UniverseIndex` is no longer a public trait; the relevant methods are now inherent methods on `Universe`.
    - `universe::UniverseTransaction::insert()` now takes a `URef` created by `URef::new_pending()`, instead of a bare value.
      This allows associations between the new member and other objects to be created within the same transaction.
    - Types that previously had a `pub fn listen(&self, impl Listener)` now implement the `listen::Listen` trait instead.
      `listen::DirtyFlag::listening()` now expects `impl Listen` instead of a closure.

- `all-is-cubes-port` library:
    - `load_universe_from_file` returns its own `ImportError` instead of `anyhow::Error`.

- `all-is-cubes-ui` library:
    - `vui::LayoutGrant` now takes an additional parameter, `enlarge_for_symmetry`. Existing calls should be changed to pass `false` to get the prior behavior.

### Removed

- `all_is_cubes::block::BlockCollision::Recur` no longer exists; it is the default behavior.
- `all_is_cubes::transaction::TransactionConflict` no longer exists; the `Transaction` trait has a `Conflict` associated type instead.

## 0.5.1 (2022-12-29)

- Fixed packaging error in `all-is-cubes-desktop`.

## 0.5.0 (2022-12-28)

This is a large revision with many breaking changes. This log does not record all of them; I have chosen “get a release out” over “document everything”.

### Added

- New crate `all-is-cubes-port` contains import/export routines (not very many, for now):
    - Import MagicaVoxel `.vox` files.
    - Export glTF 3D model files. (This support is very incomplete and currently is not feasible to use outside of the export feature in `all-is-cubes-desktop`).

- New crate `all-is-cubes-ui` contains the user interface components which used to be in `all-is-cubes`. This separation should improve compilation times.

- Graphics:
    - The `all-is-cubes-gpu` renderer now fully supports blocks with a resolution greater than 16.

- `all-is-cubes` library:
    - `block::Modifier::Move`, for drawing blocks in motion or off the grid.
    - `block::Modifier::Zoom`, for creating multi-block structures from a single block definition.
    - `camera::HeadlessRenderer`, for easily creating images using the raytracer or other renderers.
    - `math::point_to_enclosing_cube()`
    - `math::Face6`, which is like `math::Face` (now) `Face7` but without the `Within` variant.
    - `DirtyFlag::listening()` which simplifies typical usage.
    - `GridArray::repeat()` for constructing arrays with uniform contents.
    - `GridArray::from_element()` for constructing single-element arrays.
    - `GridRotation::ALL_BUT_REFLECTIONS`, as `GridRotation::ALL` but excluding reflections.
    - `SpaceBuilder::filled_with()` allows specifying the initial block in a new `Space`.
    - `SpaceTransaction::draw_target()` allows 2.5D drawing into a transaction in the same way `Space::draw_target()` works on `&mut Space`.
    - `StandardCameras::world_space()`, so that clients don't need to consult the `Character` each frame.
    - `Universe::get_any()` allows looking up universe members without knowing their type.
    - `UniverseTransaction::insert()` allows inserting objects into a `Universe` via transaction rather than directly.
    - `VoxelBrush::transform()` allows rotating a `VoxelBrush`.
    - `VoxelBrush::with_thickness()` allows easily constructing a brush with a Z-axis stack of one block.
    - The previously-existing widget system is now publicly available in the `vui` module.
    - Most `Listener` implementations now also implement `Clone` and `Debug`. This allows taking a listener and registering it with more than one `Notifier`. Relatedly, some uses of `-> impl Listener` have been replaced with concrete types.

### Changed

- `all-is-cubes` library:
    - Breaking: The `Block` type has been substantially redesigned (though it still has existing functionality).
        - It is no longer an `enum`; it is an opaque type.
        - It is logically made up of two components, a `block::Primitive` and any number of `block::Modifier`s.
            - `block::Primitive` is an enum which has `Atom`, `Recur`, and `Indirect` variants as before.
            - The `Rotated` variant is now `Modifier::Rotate`.
        - It internally uses reference counting to be predictably cheap to `clone()`; this is intended to help use cases such as transactions which mean that even a block which only exists once in the universe is frequently cloned.
        - Many changes to the functions, associated methods, and `BlockBuilder` were made to support this new structure.

    - Breaking: `mesh::TextureAllocator` implementations may now choose an arbitrary type to represent their texture coordinates. `mesh::BlockVertex` is now generic to accomodate this.

    - Breaking: `behavior::BehaviorSet` now stores additional data, the “attachment”, with each behavior, which must be specified when adding a behavior.

    - Breaking: `apps::Session::new()` (formerly `AllIsCubesAppState::new()`) has been replaced with `SessionBuilder`, whose `build()` is now an async function.
    - Breaking: `apps::StandardCameras` now works with a `ListenableSource<Viewport>` instead of a `Viewport` and `set_viewport()` method, and it is constructed using `Session::create_cameras()` instead of `StandardCameras::from_session()`.
    - Breaking: `block::AnimationHint` (from `BlockAttributes::animation_hint`) has been redesigned to be more systematic.
    - Breaking: `linking::BlockProvider::new()` is now an async function.
    - Breaking: The `linking::BlockModule` trait now requires the [`exhaust::Exhaust`](https://docs.rs/exhaust/latest/exhaust/trait.Exhaust.html) trait in place of `strum::IntoEnumIterator`. This allows implementors to use enums with fields (or non-enums).
    - Breaking: `raytracer::SpaceRaytracer::trace_scene_to_image()` now expects a buffer rather than allocating one.

    - Renamed: `space::Grid` is now `math::GridAab`.
        - Methods named `grid()` that return the bounding box of something are now called `bounds()`.
        - `Grid::contains_grid()` is now `GridAab::contains_box()`.
        - `Raycaster::within_grid()` is now `Raycaster::within()`.
        - `Grid::new()` is now `GridAab::from_lower_size()`.
        - `Grid::checked_new()` is now `GridAab::checked_from_lower_size()`.
    
    - Renamed: `apps::AllIsCubesAppState` to `apps::Session`.
    - Renamed: `math::Face` is now `math::Face7`.
    - Renamed: `mesh::SpaceMesh::new()` is now `mesh::SpaceMesh::default()`.
    - Renamed: `mesh::triangulate_block()` is now `mesh::BlockMesh::new()`.
    - Renamed: `mesh::triangulate_blocks()` is now `mesh::block_meshes_for_space()`.
    - Renamed: `mesh::triangulate_space()` is now `mesh::SpaceMesh::new()`.
    - Renamed: `space::SpaceBuilder::build_empty()` is now `build()`.
    - Renamed: `transaction::UniverseTransaction` is now `universe::UniverseTransaction`.
    - Renamed: `vui::Icons` is now `inv::Icons`.

    - Moved to `all-is-cubes-ui`: the modules `apps` and `vui`.

    - Renamed: The feature `"rayon"` is now `"threads"`.

- `all-is-cubes-gpu` library:
    - No longer supports `luminance` GPU API; uses `wgpu` instead.

### Removed

- `all-is-cubes` library:
    - `math::NoiseFnExt` (no longer public).

## 0.4.0 (2022-01-29)

### Added

- Functionality:
    - New universe template, "dungeon", generating an enclosed maze of rooms. I hope to build on this further to create some sort of gameplay (e.g. finding keys to unlock doors) and a tutorial for All is Cubes' functionality from a player's perspective; currently, all the rooms are the same.
    - New universe template, "menger-sponge". Fractals are cool.
    - There is now some amount of clickable user interface: pause and mouselook functions are visible as clickable buttons. Soon we may have actual menu screens!

- Graphics:
    - The software raytracer now implements volumetric transparency; blocks whose colors have alpha between 0 and 1 may be rendered as a solid volume of translucent material, rather than the alpha being applied at the surface, if the graphics options request it. However, the light model does not yet have a reasonable interpretation of how to light these volumes, so blocks may have unreasonably dark parts. I hope that future work will address this and also better handling of material surfaces (e.g. light reflections at the surface of glass, vs the lack of them in fog or at the meeting surfaces of adjacent similar blocks).

- `all-is-cubes` library:
    - New major items:
        - `apps::StandardCameras` manages the `Camera`s needed for rendering, given the necessary inputs. It is intended to reduce duplicated code and coupling in each renderer.
        - `universe::VisitRefs` trait allows traversing the `URef` graph inside a `Universe`.
        - `util::YieldProgress` is an interface for long-running async tasks to report their progress.
    - New features in existing types:
        - `Aab::center()` returns the center point of the box.
        - `BehaviorSet::query()` for looking up existing behaviors.
        - `BehaviorSetTransaction::insert()` for adding a behavior via transaction.
        - `BlockAttributes::rotation_rule` and `RotationPlacementRule` define how a block should be automatically rotated when placed.
        - `Camera::exposure` for controlling displayed brightness. Both renderers implement it. Nothing sets it yet.
        - `Camera::post_process_color()` applies the new exposure and tone-mapping settings to a color value.
        - `Evoxel::from_block()` for performing the same conversion from a full block to a voxel that `Block::Recur(...).evaluate()` does.
        - `Face::dot()`, for dot product with a vector without constructing an intermediate unit vector.
        - `GraphicsOptions::tone_mapping` and `ToneMappingOperator` (not yet providing good operators, just the mechanisms to have the feature at all).
        - `Rgb::from_srgb8()`.
        - `Rgb::luminance()` and `Rgba::luminance()`.
        - `SpaceMesh` now remembers the `TextureAllocator::Tile`s it was constructed using, just like `BlockMesh` does, so callers no longer need to do so.
        - `SpaceMesh::blocks_used_iter()` reports which block indices went into the mesh.
        - `SpaceTransaction::set()`, for faster construction of large transactions instead of many small ones. (This will likely be changed further to have better naming and convenience.)
        - `SpaceTransaction::nonconserved()` requests that merging two transactions with the same effect on some cube should not be considered a conflict.
        - `math::cube_to_midpoint()` function for the pattern of converting to float and adding 0.5.
    - The following functions are now `const fn`:
        - `Grid::for_block()`
        - `Rgb::from_srgb8()`
        - `Rgba::from_srgb8()`

- `all-is-cubes-content` library:
    - `UniverseTemplate::build` is now an async function, which accepts a `YieldProgress` hook parameter. This means that there can be progress bars (currently implemented in desktop but not web) and that building doesn't hang the web event loop.

- `all-is-cubes-desktop`:
    - Can now “open data files”. Currently the only file format supported is MagicaVoxel `.vox` scene files; when we have a native file format that will be supported too. Files can be opened by passing them on the command line or by dropping them on an open window.
    - Terminal mode now has mouse support for placing and removing blocks.
    - Terminal mode no longer goes blank if asked to use graphic characters but not colors.

### Changed

- Game mechanics:
    - `Character`s no longer inherently have the ability to fly; they can only do so if they have a `Tool::Jetpack` in their inventory.
    - It is no longer possible to jump multiple times in the same frame.

- Graphics:
    - Fixed a bug in the GPU renderer where dark areas would always have a noticeable minimum brightness (`PackedLight` did not have the same interpretation in the Rust and GLSL code for a value of zero).
    - Block meshes are no longer computed all at once, reducing maximum frame time during startup (or, in the future, when moving from viewing one `Space` to another).
    - Optimized construction and depth-sorting of meshes for transparent blocks. They still don't draw correctly, but they are less slow.
    - Various performance improvements to the raytracer.

- `all_is_cubes` library:
    - The new minimum supported Rust version is 1.56.0 (2021 edition).
    - Breaking: The items previously in `all_is_cubes::lum` are now in a separate crate `all_is_cubes_gpu`, and the `"lum"` feature no longer exists.
    - Breaking: The `triangulator` module has been renamed to `mesh`. All types with `Triangulation` in the name now use `Mesh` instead.
        - `BlockTriangulation` → `BlockMesh`
        - `BlockTriangulations` → `BlockMeshes`
        - `SpaceTriangulation` → `SpaceMesh`
            - `SpaceMesh` takes an additional type parameter for the texture type.
        - `BlockTriangulationProvider` → `BlockMeshProvider`
        - `TriangulatorOptions` → `MeshOptions`
    - Breaking: The raytracer is now more flexible and allows custom data to pass into the tracing process.
        - Instead of `PixelBuf` having a single output type, the raytracer returns the `PixelBuf` and lets the caller convert it (or provide a conversion function, for `trace_scene_to_image` which returns a slice of converted values).
        - Block data is computed via a separate trait, `RtBlockData`, and has access to `GraphicsOptions` and an arbitrary extra parameter. Multiple types of `PixelBuf` may be used with the same `SpaceRaytracer` as long as they agree on a concrete type of `RtBlockData`.

    - Breaking: The color values written into `all_is_cubes::mesh::TextureTile` are now in sRGB; previously, they were linear.
    - Breaking: `GLRenderer::new()` now expects a `StandardCameras` (instead of the components of one).
    - Breaking: `AllIsCubesAppState::update_cursor()` now expects a `&StandardCameras` instead of two `Camera`s.
    - Breaking: `Camera::set_view_matrix()` is now `Camera::set_view_transform()`, and expects a `cgmath::Decomposed` transform, inverted from the matrix version.
    - Breaking: `Space::spawn_mut()` replaced by `Space::set_spawn()`.
    - The `Name` enum is now intended to be reliably cheap to clone:
        - `Name::Specific`'s field holds an `Arc<str>` instead of a `String`. 
        - `Universe`-related errors now hold `Name` instead of `Arc<Name>`.
    - Renamed: `Evoxel::new()` to `Evoxel::from_color()`.
    - Renamed: `apps::Tick` is now `time::Tick`. 
    - Renamed: `Rgba::from_srgb_32bit()` is now `Rgba::from_srgb8()`, and `Rgba::to_srgb_32bit()` is now `Rgba::to_srgb8()`. This is more consistent with what I understand to be common terminology (specifying the component size instead of the total size), and allows the same name to be used sensibly for `Rgb`.

## 0.3.0 (2021-10-09)

### Added

- Demo content: Multiple scenes are now available, through the command-line option `--template`, web page URL parameter `?template=`, or in code as `all_is_cubes_content::UniverseTemplate`. The available templates include:
    - `demo-city`: The default in the previous version: a collection of many examples of specific functionality. (Now with more stuff, of course.)
    - `atrium`: A voxel interpretation of the Sponza Atrium rendering test scene (from scratch, not a data conversion).
    - `blank`: No data at all. Mostly useless except for testing.
    - `cornell-box`: A version of the [Cornell box](https://en.wikipedia.org/wiki/Cornell_box) rendering test scene.

- `all-is-cubes-desktop`:
    - New graphics mode `-g record` for rendering images or video (currently only in PNG or APNG format).
    - New option `--display-size` to choose the window or image size.
    - New option `--precompute-light` to fully compute `Space` light before displaying anything.
    - Terminal mode (`-g terminal`):
        - Pipelined simulation and rendering for more CPU utilization.
        - Keyboard controls over the text and color modes used.
        - Double-resolution mode using “▄” graphic characters.
        - UI has more HUD elements.
        - Implemented using the `crossterm` library instead of `termion`, and should therefore support Windows (not tested).
    - Reads graphics options from `graphics.json` in whatever [directories-next](https://docs.rs/directories-next/2.0.0/directories_next/) thinks is a platform-appropriate app configuration directory.
    - Added info text overlay like the web version.

- Graphics:
    - Color values produced in all modes are now sRGB instead of linear, which should be more correct on most displays.
    - Various graphics options are now configurable without recompiling, through the `GraphicsOptions` type.
    - “Smooth lighting”, interpolated across block faces.
    - Triangulator and  `all-is-cubes::lum` (GPU triangle-based renderer):
        - Blocks with resolution greater than 16 are now drawn with correct shapes, though not textures unless they have solid-colored faces because the texture allocator is still limited.
        - Added frustum culling.
        - Added distance fog.
        - Now supports transparent blocks, as the raytracer already did. This support is complete for `Block::Atom` blocks but poor to nonexistent for `Block::Recur` blocks.
        - Light changes no longer require rebuilding chunk triangulations.
    - Fixed several bugs in light calculation, including one that could lead to runaway brightness.

- Physics and game mechanics:
    - Character now has limited reach (can no longer interact with arbitrarily distant blocks).
    - Gravity is now a property of individual `Space`s.
    - Characters collide against the voxel shapes of blocks, not just their unit cube bounds. This happens only if the block's `BlockCollision` attribute is `Recur`.
    - Inventory items are now stackable, and “finite rather than infinite” block items exist — you can place a block in a `Space` and lose it from your inventory, or the reverse.

- User interface:
    - Mouselook support.
    - Cursor markers no longer lag behind the camera.
    - The HUD now displays a “tooltip” with the name of the selected tool/block.

- `all_is_cubes` library API:
    - `Universe`, `URef`, `Space`, `Listener`, and so on now implement `Send` and `Sync`. They are not fully equipped for useful multithreaded operation (in particular, there is are no principles for deadlock-free lock acquisition of various `URef`s), but it is now possible to, for example, construct a `Universe` on one thread and send it to another.
    - Transactions (`all_is_cubes::transaction`). Transactions describe modifications to a game object or many within the same `Universe`. They enable:
        - Ensuring that a set of modifications is committed or entirely rejected (preventing “item duplication” kinds of bugs).
        - Deferring mutations to solve borrowing problems.
        - For example, `Tool`s' modifications of `Space`s are now exclusively performed as transactions.
    - Behaviors (`all_is_cubes::behavior`). The `Behavior` trait can be implemented to add custom behavior to a `Space` or `Character` (a sort of “scripting”). Its functionality is very limited for now and may be substantially revised.
    - Inventories and items (`all_is_cubes::inv`) are now public.
    - `Aab` implements `From<Grid>`.
    - `Aab::contains()`
    - `Aab::face_coordinate()`: returns one of the box's six coordinates.
    - `Aab::random_point()`
    - `Aab::round_up_to_grid()`
    - `Aab::scale()`
    - `Block::rotate()`: generates a canonical `Block::Rotated` version of a block.
    - `Block::unspecialize()` to undo rotation and, in the future, other changes that are considered specific to a block's particular placement in a `Space` (such as being due to interactions with other blocks).
    - `BlockAttributes::animation_hint`
    - `Face` implements `TryFrom<GridVector>`.
    - `FaceMap::iter()`
    - `FaceMap::repeat()`
    - `GenError` and `InGenError` types for classifying worldgen failures.
    - `Grid::checked_new()`: returns an error instead of panicking.
    - `Grid::abut()`: constructs a grid adjacent to an existing one (useful for making walls).
    - `Grid::axis_range()`: as `x_range()`, `y_range()`, `z_range()` but with a parameter for the axis.
    - `Grid::expand()`: enlarges a grid by moving its faces.
    - `Grid::intersection()`
    - `Grid::unsigned_size()`
    - `GridArray` implements `IndexMut`.
    - `GridArray::from_elements()`
    - `GridArray::map()`
    - `GridMatrix::decompose()` to decompose into a translation and rotation.
    - `GridMatrix::inverse_transform()`
    - `GridMatrix::transform_cube()`
    - `GridRotation::{ALL, IDENTITY, CLOCKWISE, COUNTERCLOCKWISE}`
    - `GridRotation::from_to()` (a “look at” operation of sorts)
    - `GridRotation::inverse()`
    - `GridRotation::is_reflection()`
    - `GridRotation::iterate()` to repeat a rotation exactly until it loops to the start.
    - `GridRotation::to_rotation_matrix()`
    - `Rgb::clamp()`, `Rgba::clamp()`
    - `Space::builder()`: returns `SpaceBuilder` for configuring a new space's properties.
    - `Space::evaluate_light()` and `Space::fast_evaluate_light()` can be used for immediate light evaluation.
    - `Space::fill_uniform()`: faster than `Space::fill()` when filling with only one block.
    - `SpaceRaytracer::trace_scene_to_image()` can be used to produce an RGBA image.
    - `TextureAllocator`, the trait through which the triangulator outputs textures, is now expected to offer 3D textures instead of 2D. This significantly reduces the number of redundant texels for complex blocks (at the cost of more never-seen texels for simple blocks).

- Some information is logged using the `log` crate (startup progress and some nonfatal errors).

### Changed

- `all_is_cubes` library:
    - No longer depends on the `luminance-front` crate.

    - Expansions:
        - `Space` can now store up to `u16::MAX` (65536) distinct blocks, instead of 256.
        - `Grid` now allows zero-volume grids.

    - Breaking parameter/field/type changes:
        - `draw_to_blocks()` now takes options for Z dimensions and the attributes of the resulting blocks.
        - `GridRotation` is now an enum, so its size is 1 byte and it cannot have invalid values.
        - `EvaluatedBlock` now has an explicit `.resolution` field. The dimensions of the `.voxels` field no longer implicitly define the resolution; if `.voxels` is smaller than the block then the missing voxels should be treated as `AIR`.
        - The API of `all_is_cubes::triangulator` has been substantially revised to support depth sorting and separation of transparent triangles, and to remove unnecessary complexity.
        - New type `GraphicsOptions` stores global user-settable rendering options, and is required by most graphics-related functions.
        - `make_some_blocks()` returns an array rather than a vector, taking the size as a const generic parameter.
        - `Universe::get_default_character()` returns `Option` instead of panicking.
        - `Face::axis_number()` returns `Option` instead of panicking.
        - `all_is_cubes::listen::Sink` now stores messages in order without duplicates, and offers the method `.drain()` instead of implementing `Iterator`.
    
    - Breaking: The integration with `embedded_graphics` now uses API version 0.7. The drawing target type has been renamed from `VoxelDisplayAdapter` to `DrawingPlane`.
    - Breaking: `all_is_cubes::util::ConciseDebug` is now generalized as `all_is_cubes::util::CustomFormat` which takes one of several formatting types.

    - Renamed: The modules `all_is_cubes::blockgen`, `all_is_cubes::worldgen`, and `all_is_cubes::content` have all been replaced by the separate crate `all_is_cubes_content`.
    - Renamed: `all_is_cubes::camera::Camera` to `all_is_cubes::character::Character`, and `all_is_cubes::camera::ProjectionHelper` to `Camera`.
    - Renamed: `all_is_cubes::camera::InputProcessor` to `all_is_cubes::apps::InputProcessor`.
    - Renamed: `all_is_cubes::lum::glrender::Viewport` to `all_is_cubes::camera::Viewport`.
    - Renamed: `all_is_cubes::universe::FrameClock` to `all_is_cubes::apps::FrameClock`.
    - Renamed: `GridArray::generate()` and `FaceMap::generate()` to `from_fn()`.
    - Renamed: `Aab::expand()` to `Aab::enlarge()`
    
    - `Grid::index()` no longer overflows on some out of range coordinates.
    - `Raycaster` will stop iteration rather than overflowing if the ray exits the range of possible cube coordinates (`i32`).

- Development tools:
    - The project's custom build scripts are now provided using the [cargo xtask pattern](https://github.com/matklad/cargo-xtask) rather than a Makefile. This means that the `make` tool is not needed, and instead of e.g. `make run-dev`, you would now write `cargo xtask run-dev`.

### Removed

- Most of `all_is_cubes::lum` is no longer public as I have decided those items should be considered implementation details. The top-level struct `GLRenderer` (which needs a better name) is no longer in the `glrender` submodule.
- `FaceMap::values()`: use `FaceMap::iter()` instead.
- `Universe::get_default_space()`

## 0.2.1 (2021-01-18)

A changelog was not kept for this and earlier releases.
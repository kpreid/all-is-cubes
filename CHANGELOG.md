# Changelog

## Unreleased

### Added

### Changed

* `all-is-cubes` library:
    * `chunking::point_to_chunk()` now returns `None` in case of out-of-range input instead of panicking.

### Removed

## 0.10.0 (2025-12-11)

### Major change: ECS architecture

In order to resolve various design problems interfering with development, the `all-is-cubes` core crate has been substantially revised to use ECS (Entity-Component-System) data storage and execution. Specifically, it is now built on top of [`bevy_ecs`](https://docs.rs/bevy_ecs/0.16.1/bevy_ecs/). (This is largely an implementation detail, though currently leaks in some places. There are no current plans to make All is Cubes into a Bevy plugin.) This, and further related changes, have resulted in some significant API changes.

* It is no longer possible to read from a `universe::Handle` without providing a `universe::ReadTicket` — a new type that denotes read access to a `Universe` (or, in some cases, part of it).

* `universe::Handle<T>::read()` no longer returns a guard object that dereferences to `T`, but instead a `<T as UniverseMember>::Read` which does not dereference to `T` but serves the same function.

* Methods such as `Character::step()` and `Space::step()` no longer exist; stepping must be performed on a whole `Universe`.

* All types that were generic over `<I: all_is_cubes::time::Instant>` no longer are, and the `Instant` trait no longer exists.
  Instead, `all_is_cubes::time::Instant` is a re-export of the type `bevy_platform::time::Instant`, which is itself a re-export of `std::time::Instant` when possible, and on platforms not supported by default, can have its time source installed at run time.

* Previously, when the `"std"` feature of `all-is-cubes` was disabled, many of its types did not implement `Send + Sync`; now they always do.
  However, under this condition, spinlocks are used for synchronization (as a portable placeholder), so if `all-is-cubes` types are being used from multiple threads, the `"std"` feature should be enabled to avoid poor performance.

[Issue #620](https://github.com/kpreid/all-is-cubes/issues/620) contains more information on the motivation for this change.

### Added

* User-visible functionality:
    * The interior volumes of transparent blocks with resolution greater than 1 are now rendered more correctly.
    * In the desktop and web applications, changes to graphics options are now persisted across sessions.
    * `.vox` import (but not export) now supports:
        * Files containing a scene graph, or just multiple models.
          Positioning of models is currently flawed.
        * Light emission and transparency (“Emit”, “Blend”, and “Glass” materials).

* `all-is-cubes` library:
    * `block::Block::find_inventory()`
    * `block::BlockAttributes::ambient_sound`, sound that is heard continuously when near blocks.
    * `block::Modifier::Tag`, for marking blocks as being of particular kinds.
      Does not interact with anything else yet.
    * `block::Resolution::log2()`
    * `block::Resolution::recip_{f32,f64}()`
    * `inv::Ix`
    * `inv::InvInBlock` and `inv::IconRow` now have public `new()` functions.
    * `inv::InventoryTransaction::is_empty()`
    * `linking::BlockProvider::new_installed_cyclic()` allows construction of modules whose blocks refer to each other.
    * `math::Aab::corner_point()` allows obtaining any one of the corners of the box.
    * `math::BoxPart::centered_on()`
    * `math::BoxPart::to_face()`
    * `math::chebyshev_length()`
    * `math::Face6::{clockwise, counterclockwise, r180}` for constructing rotations.
    * `math::Face[67]::vector()`, like `normal_vector()` but with an arbitrary length.
    * `math::FaceMap::sum()`
    * `math::FaceMap::values_mut()`
    * `math::FaceMap` supports assignment operators (like `+=`).
    * `math::FaceMap` implements `IntoIterator`
    * `math::GridAab::size()` is now a `const fn`.
    * `math::GridAab::tiny()` allows constructing `GridAab`s in `const` contexts when the bounds cannot overflow.
    * `math::GridAab::volume()` is now a `const fn`.
    * `math::Gridgid::checked_transform_point()`
    * `impl From<math::Octant> for math::OctantMask`
    * `math::PositiveSign::INFINITY`
    * `math::PositiveSign` and `math::ZeroOne` support assignment operators.
    * `math::Rgb01` is a float, linear color type that is restricted to the range 0 to 1.
    * `math::Vol::tiny()` and `from_tiny_elements()` allow constructing `Vol`s in `const` contexts when the bounds cannot overflow.
    * `op::Operation::MoveInventory`
    * `raycast::Raycaster::within()` allows optional inclusion of the step where the raycast *exits* the given bounds.
    * `space::Builder::build_and_mutate()`
    * `space::Builder::try_build()` produces recoverable out-of-memory errors where `build()` does not.
    * `space::Mutation`, a new interface for performing bulk changes to a `Space`.
    * `sound::Ambient`, used to define sounds that are heard when near blocks.
    * `sound::SoundDef`, allows defining short synthesized sounds.
    * `tag` module, for use with `block::Modifier::Tag`.
    * Universe garbage collection is now fully implemented.
      `Handle`s not stored inside a universe no longer prevent a member from being collected; use the new type `StrongHandle` for this purpose.
    * `universe::HandleError` is now a `struct` instead of an `enum`.
      * `universe::HandleError::gone()` may be used to learn the reason why a handle is defunct.
    * `universe::Universe::set_clock()`
    * `universe::UniverseMember` is a trait which bounds various operations on `Universe` and `Handle`. It previously existed but was not public.
    * `universe::UniverseTransaction::is_empty()`
    * `universe::UniverseTransaction::insert_without_value()` allows the construction of cyclic structures of handles (e.g. multiple `BlockDef`s which refer to each other).

* `all-is-cubes-content` library:
    * `BoxPart::{is_face, is_edge, is_corner}()`
    * `UniverseTemplate` now allows custom sizes for the `CornellBox` and `MengerSponge` templates.
    * `UniverseTemplate::DemoCity` has more exhibits, and hills at the edges.
    * `UniverseTemplate::Dungeon` now requires the player to actually find a key to unlock gates.

* `all-is-cubes-gpu` library:
    * Support for `TransparencyOption::Volumetric` now extends to blocks with resolutions higher than 1, using raymarching.
      Quality is poor but will be improved in the future.
    * `SurfaceRenderer::render_frame()` now takes a parameter of type `FrameBudget`, which allows controlling the amount of time spent on calculations.
    * Bloom rendering has been improved.
    * Edges between adjacent voxels on a surface are now antialiased, if requested.

* `all-is-cubes-mesh` library:
    * Supports generating meshes suitable for raymarched volumetric transparency.
      This feature is controlled by the `transparency` in the provided `GraphicsOptions`;
      when it is set to `TransparencyOption::Volumetric`, a single bounding box will be generated for all transparent voxels, rather than generating a mesh for the boundary of the transparent volume.

    * Supports generating vertices which have “secondary data”, an associated type which makes up a second “column” of vertex attribute data that is stored separately in meshes.
      Data may be arbitrarily subdivided between primary and secondary, except that the primary type must contain the vertex’s position. This allows vertex positions to be stored more densely in memory, which may improve performance for depth sorting and rendering.

    * `BlockMesh::bounding_box()` and `SpaceMesh::bounding_box()` now return the new type `Aabbs` which provides separate boxes for opaque and transparent geometry. This allows precise culling when rendering in separate opaque and transparent passes.
    * `Coloring::Texture` now includes the `Resolution` of the texture/voxels.
    * New type aliases `Position` and `PosCoord` for vertex positions.
    * `SpaceMesh`es’ opaque triangles are now semi-sorted; for each face/normal, triangles with that normal will be in roughly front-to-back order.
    * We now use `euclid`’s static coordinate system checking for distinguishing between points in a `Space` and points in a mesh. The new marker type `MeshRel` identifies the coordinates used in a mesh.
    * `dynamic::ChunkMesh::depth_ordering_for_view()`

* `all-is-cubes-port` library:
    * `export_to_path()` no longer accesses the `Universe` while its future is running, but instead only when initially called. This allows the latter portion of the export operation to run concurrently with other uses of the `Universe`.
    * `.vox` import now partially supports materials and scenes.
    * glTF export now preserves the translation of an exported `Space` instead of translating it so that its lower corner is at (0, 0, 0).
    
* `all-is-cubes-render` library:
    * Raytracer now supports distance fog.
    * `camera::GraphicsOptions::maximum_intensity` may be used to specify the maximum brightness of the output device and thus properly apply tone mapping to HDR output. It may also be used to choose to restrict output to SDR.
    * `camera::LightingOption::Bounce` requests per-pixel rather than per-block illumination.
      This option is currently only supported by the CPU raytracer (in which case it adds 4 secondary rays to the primary ray, without any denoising).
    * `raytracer::Accumulate` is implemented for tuples of up to 2 elements.
    * `raytracer::Accumulate::add()` takes a new struct type `Hit` instead of separate arguments,
      and `Hit` includes cube position, distance/depth information, and a new enum `Exception`.
    * `raytracer::DepthBuf` can be used to produce depth images.
    * `raytracer::Exception`, part of `Hit` which distinguishes special situations such as “hitting” the sky.
    * `raytracer::Hit`, used by `Accumulate` implementors.
    * `raytracer::RtRenderer` now takes separate custom options for the world and UI layers.

* `all-is-cubes-ui` library:
    * `apps::MainTaskContext::add_custom_command()` allows adding arbitrary command buttons to the UI; currently, they only appear in the pause menu.
    * `apps::MainTaskContext::set_universe_async()` allows telling the UI about a long-running world generation or loading process, displaying a progress bar to the user.
    * `apps::Settings` manages user-editable settings that eventually will be more than just the graphics options.
    * `apps::SessionBuilder::settings()` links a possibly-shared `Settings` to the created session.
    * `widgets::TextBox` for displaying changing text (not yet editable)

### Changed

* `all-is-cubes` library:
    * `block::BlockAttributes` is now a non-exhaustive struct.
      `BlockAttributes` literals should be replaced with `block::Builder::build_attributes()`.
    * `block::EvaluatedBlock::with_inventory()` now accepts `IntoIterator` instead of `Iterator`.
    
    * Inventories are now indexed using the new type `inv::Ix`.
    * `inv::Inventory` no longer has public fields. Use the new accessor functions `slots()` or `get()` instead.
    * `inv::InvInBlock` is now more correctly affected by block rotation.

    * `math::Axis::color()` now returns `Rgb01` instead of `Rgb`.
    * `math::Rgb::clamp()` now takes a parameter for the upper bound.
    * `math::Wireframe` has been renamed to `math::lines::Wireframe` and now produces vertices in pairs, representing whole line segments instead of each end separately.

    * `transaction::Transaction::commit()` and `execute()` take the transaction by value instead of reference. This allows transactions to be committed more efficiently by moving instead of cloning data that they contain.

    * `universe::Handle<T>::read()` no longer returns a guard object that dereferences to `T`, but instead a `<T as UniverseMember>::Read` which does not dereference to `T` but serves the same function.
    * `universe::Universe::new()` now returns `Box<Universe>` instead of `Universe`, to make it easier to avoid unnecessarily large futures and stack frames.
    * `universe::UniverseTransaction::{insert, insert_mut}()` now take a name and value rather than a `Handle`.
    * `universe::WhenceUniverse::load()` now returns `Box<Universe>` instead of `Universe`.

    * Renamed `block::Move::to_paired()` to `into_paired()`.
    * Renamed `block::Text::to_builder()` to `into_builder()`.
    * Renamed `all_is_cubes::color_block!()` to `all_is_cubes::block::from_color!()`.
    * Renamed `math::Cube::midpoint()` to `center()`.
    * Renamed `math::LineVertex` to `math::lines::Vertex`.

* `all-is-cubes-content` library:
    * `UniverseTemplate::build()` now returns `Box<Universe>` instead of `Universe`.

* `all-is-cubes-mesh` library:
    * The block mesh construction algorithm has been significantly improved.
      Block meshes will no longer contain “T-junctions” which may create pixel-sized gaps when rendered, or otherwise interfere with further processing of the mesh, and they will have fewer vertices.

      However, it can no longer be assumed that triangles will appear in pairs that form rectangles.

    * Depth sorting of transparent meshes has been near-completely redesigned to be more correct and more efficient.
      
      * All depth sorting is now lazy.
        This reduces the immediate cost of constructing a `SpaceMesh`, since it does not need to be sorted for a particular view point unless and until that view is needed.
        You must now call `SpaceMesh::depth_sort_for_view()` for all rendering of transparent triangles, not just those cases where the view point is inside the mesh.
      * `DepthOrdering` is now a `struct`, and the variants `DepthOrdering::{Any, Within}` have been replaced with constants.
        Additionally, there are now fewer distinct `DepthOrdering`s, reducing the amount of index data required for transparent meshes.
      * The return type of `BlockMesh::bounding_box()` and `SpaceMesh::bounding_box()` has changed from `Option<Aab>` to `Aabbs`.
      * The return type of `depth_sort_for_view()` functions has changed from `bool` to a new `struct DepthSortResult` (which contains that boolean).
      * `DepthOrdering::from_view_direction()` is now called `from_view_of_aabb()` and takes a viewpoint and bounding box instead of only a view direction.
      * `dynamic::RenderDataUpdate`’s `indices_only` field now specifies the range of indices to update, allowing you to copy less data.


    * Renamed `GfxVertex` to `Vertex`.
    * Vertices are now constructed via the `Vertex` trait’s function `from_block_vertex()` instead of `impl From<Vertex> for YourVertexType`.
    * Vertex position coordinates are now conveyed as `f32` (you may use the new type alias `PosCoord`) instead of `f64`.
      (This will not reduce the precision of any mesh of reasonable size, because vertex positions are relative to the bounds of the individual mesh.)
      Furthermore, their coordinate system marker type is now `MeshRel` instead of `Cube`, indicating that the coordinate system origin for a mesh is not the same as that of the `Space` it was created from.

* `all-is-cubes-render` library:
    * `HeadlessRenderer::update()` is no longer async.
    * `raytracer::Accumulate::add()` takes a new struct type `Hit` instead of separate arguments.
    * `raytracer::Raytracer::trace_ray()` now takes an `&mut` reference to an `Accumulator` instead of constructing a new accumulator using `Default`. This allows specifying the initial state of the accumulator (which need not implement `Default`), and composition of the results of multiple traces.
    * `raytracer::RtBlockData::{error,sky}()` have been replaced with a single function `exception()` which takes an `Exception` enum describing the situation for which data should be constructed.

* `all-is-cubes-ui` library:
    * `apps::Session::settings()` replaces `graphics_options_mut()`.

### Removed

* `all-is-cubes` library:
    * The public functions `Character::step()` and `Space::step()` no longer exist.
      In general, to make time pass, create a `Universe` and `step()` it instead.
    * Behaviors are deprecated. They are no longer supported by `Character` and `Universe`.
    * `behavior::BehaviorWaker` no longer exists; use the standard library type `core::task::Waker` instead.
    * `math::Cube` no longer implements the `Borrow` trait (incorrectly).
    * `math::GridRotation::{CLOCKWISE, COUNTERCLOCKWISE}` have been replaced by `Face6::PY.{clockwise, counterclockwise}()`.
    * `math::Rgb::UNIFORM_LUMINANCE_*` have been replaced by constants on `Rgb01`.
    * `math::Rgb` can no longer be converted to `Block`, `Primitive`, or `Atom`.
      Use `Rgb01` instead.
    * `math::Vol` no longer implements `ops::Index` generically, only for `Cube` (preferred), and
    `[GridCoordinate; 3]` (convenience).
      This restriction is intended to minimize confusion between corner points and cubes which can result in bugs in the presence of rotation.
    * `physics::Body` can no longer be constructed with NaN values.
    * `space::Space` has several direct mutation methods removed and relocated to the `Mutation` type.
      Use `Space::mutate()` to access them.
    * `universe::Handle::execute()` has been replaced by `Universe::execute_1()`.
    * `universe::Handle::new_pending()` has been replaced by `UniverseTransaction::insert()`.
      Handles are now always associated with some container, either `Universe` or `UniverseTransaction`, except for those in the “gone” defunct state.
    * `universe::Handle::try_modify()` no longer exists.
      Use `Universe::mutate_space()` for `Space`s, and transactions for other cases.
    * `universe::UniverseTransaction` no longer implements `Clone`.
    * It is no longer possible to mutate pending `Handle`s.
      In order to construct cyclic structures, use `UniverseTransaction::insert_without_value()`.

* `all-is-cubes-mesh` library:
    * Texture “clamp” coordinates are no longer produced.
    * The associated type `GfxVertex::Coordinate` no longer exists.
      `GfxVertex` (now `Vertex`) implementors may still use whatever coordinate storage format they wish, but must be able to convert it back to `f32`.

* `all-is-cubes-render` library:
    * `raytracer::Accumulate::hit_nothing()` no longer exists.
      Use `Hit::exception` to detect the opposite of this condition instead.

## 0.9.0 (2025-01-01)

### Added

* `all-is-cubes` library:
    * Block inventories are now more functional.
        * `block::EvaluatedBlock::with_inventory()` attaches inventory to a block.
        * `inv::InvInBlock`, stored in `block::BlockAttributes::inventory`, describes the size and rendering such inventories should have.
    * `block::EvaluatedBlock::block()` returns the original block that was evaluated.
    * `block::Modifier::Attributes` allows overriding block attributes.

    * `listen::Store` and `listen::StoreLock` help implement `Listener` with less boilerplate.
    
    * `math::Aab` now implements `Eq`.
    * `math::Aab::union()`.
    * `math::Aab::union_point()`.
    * `math::Cube` implements `core::ops::Add<Face6>` to obtain adjacent cubes.
    * `math::Face6::rotation_from_nz()` produces the same transformation as `Face6::face_transform()`, but expressed as a rotation value.
    * `math::PositiveSign` is a floating-point numeric type restricted to values which have a positive sign bit.
    * `math::ZeroOne` is a floating-point numeric type restricted to values between 0.0 and 1.0.
    
    * `op::Operation::Alt` allows operations to try alternatives.
    * `op::Operation::Replace` allows replacing a specific block and not any other.
    * `op::Operation::StartMove` allows setting up an animated move into an adjacent cube.

    * `time::Schedule` describes the timing of repeated events, now used in `block::TickAction` and `block::Move`.

* `all-is-cubes-mesh` library:
    * Is now `no_std` compatible, if the `dynamic` feature is not enabled.
    * `BlockMesh::bounding_box()`
    * `SpaceMesh::bounding_box()`

* `all-is-cubes-render` library:
    * `Resolution` implements `RtBlockData`.

* `all-is-cubes-ui` library:
    * Is now `no_std` compatible, if the `session` feature is not enabled.

### Changed

* Graphics:
    * The handling of voxels with light emission is now more consistent.
      In particular, emission is *not* modified by alpha; it always describes light emitted from the surface of the material regardless of the alpha, or transmittance, of the volume behind that surface.

* `all-is-cubes` library:
    * `block::EvaluatedBlock`’s fields are now private. Use methods instead.
    * `block::EvaluatedBlock::voxel_opacity_mask` now has its own data type, `VoxelOpacityMask`.
    * `block::EvalBlockError` is now a `struct` with an inner `ErrorKind` enum, instead of an enum, and contains more information.
    * `block::Evoxels` is no longer an enum; use its constructors and accessor functions instead.
    * `block::Move`’s means of construction have been changed to be more systematic and orthogonal. In particular, paired moves are constructed from unpaired ones.
    * The interactions of `Move` and `Composite` with other block modifiers occurring after them, and attributes such as `tick_action` have been redesigned and should be more consistent, though not yet entirely free of quirks.
    * `block::TickAction`’s `period` field is now of type `time::Schedule`

    * The `listen` module is now a reexport of the separate library [`nosy`](https://docs.rs/nosy).
      Many items have changed in name and signature.

    * `math::FaceMap::repeat()` has been renamed to `splat()`, for consistency with the same concept in the `euclid` vector types which we use.
    * `math::Geometry` is now `math::Wireframe`, and its `translate()` method has been replaced with inherent methods on its implementors.
    * `math::GridAab::expand()` now takes unsigned values; use `GridAab::shrink()` instead of negative ones. This allows both versions to never panic.
    * `math::{Rgb, Rgba}` now use more restricted numeric types for their components, `math::PositiveSign` and `math::ZeroOne`.
      This prevents various arithmetic edge cases from arising.
    * `math::Vol::subdivide()` now returns an array instead of a tuple, and the `Vol<&mut [_]>` version takes a filter function.
      The filter should make it easier to use in cases where the mutable subdivisions need to meet some size condition.

    * Renamed `behavior::BehaviorContext` to `behavior::Context`.
    * Renamed `behavior::BehaviorHost` to `behavior::Host`.
    * Renamed `behavior::BehaviorPersistence` to `behavior::Persistence`.

    * Renamed `block::BlockBuilder` to `block::Builder`.
    * Renamed `space::SpaceBuilder` to `space::Builder`.

* `all-is-cubes-content` library:
    * `BoxStyle`’s functionality has been expanded; `BoxStyle::from_fn()` and the new companion type `BoxPart` allows constructing arbitrary `BoxStyle`s rather than only the specific patterns supported by previous constructors.

* `all-is-cubes-gpu` library:
    * Everything in the `in_wgpu` module is now located at the root of the crate instead.
    * `in_wgpu::SurfaceRenderer::new()` requires `wgpu::Adapter` instead of `&wgpu::Adapter`.

* `all-is-cubes-mesh` library:
    * `texture::Allocator` implementations are now permitted to reject zero-volume allocations.

* `all-is-cubes-port` library:
    * All functionality is now conditional on feature flags, to allow omitting unneeded formats and operations.
    * `ExportFormat` is now named `Format`.

* `all-is-cubes-render` library:
    * The trait method `raytracer::Accumulate::add()` now accepts the surface color via a `ColorBuf` (which acts essentially as a form of premultiplied alpha) rather than `Rgba`. This enables more consistent handling of emissive materials. See the method documentation for details.
    * Raytracer update operations return whether any changes were actually found.
      This can be used to avoid rerendering an unchanged scene.

* `all-is-cubes-ui` library:
    * Visual changes:
        * Better dialog box backgrounds, and addition of titles.

### Removed

* `all-is-cubes` library:
    * `block::Primitive` no longer contains `BlockAttributes` in any of its variants.
      The new means of specifying attributes is `block::Modifier::Attributes`.

* `all-is-cubes-gpu` library:
    * The `"wgpu"` crate feature no longer exists; it is not optional.

## 0.8.0 (2024-07-08)

### Added

* New library crate `all-is-cubes-render`, which replaces `all_is_cubes::camera` and `all-is-cubes::raytracer`.

* `all-is-cubes` library:
    * `color_block!` macro is a constant, non-allocating version of `Block::from(Rgba::new(...))`.

    * `block::Resolution::MAX` provides the maximum available resolution.
      This value is not planned to change, 
    * `block::AnimationHint` now offers constructor functions `redefinition()` and `replacement()` for the common case of setting one of its fields to a specific `AnimationChange`.
    * `block::Modifier::Inventory` is a new modifier allowing blocks to carry inventory.
      It does not do anything yet.

    * `listen::Notifier::buffer()` and `listen::Buffer` allows creating and sending batches of messages, to improve efficiency over dispatching each message individually to all listeners.

    * `math::FaceMap` now implements `Exhaust`.
    * `math::GridAab::intersection_box()` is a new definition of box intersection which returns a bigger box in some zero-volume/flat cases.
    * `math::GridAab::union_cubes()` is a new definition of union which returns a smaller box in zero-volume/flat cases.
    * `math::GridAab::union_cube()` is a convenience for expanding the box to include one cube.
    * `math::GridAab::to_free()` is another name for `Aab::from()`, useful for method chaining and not importing `Aab`.
    * `math::GridRotation::transform_size()` rotates `GridSize` values, like `transform_vector()` without negation.
    * `math::GridSize` type alias for `euclid::Size3D<u32, Cube>`, the sizes of `GridAab`s.
    * `math::Octant` is a new type identifying an octant.
    * `math::OctantMap` is a new type storing eight values associated with octants (like `FaceMap` is to `Face6`).
    * `math::Vol::subdivide()` splits a `Vol`'s bounds and data into parts that can be processed in parallel.

    * `op::Operation::AddModifiers` allows an operation to add modifiers rather than replacing a block.
    * `op::Operation::Neighbors` allows an operation to modify nearby blocks.
    * `op::Operation::DestroyTo` allows an operation to express replacing a block when the replacement is less important than `Operation::Become`.

    * `raycast::AaRay` and `raycast::AxisAlignedRaycaster` represent rays which are always axis-aligned. This new raycaster is slightly simpler and faster.

    * `raytracer::SpaceRaytracer::trace_axis_aligned_ray()` does what it says and is slightly faster than the non-axis-aligned version.

    * `transaction::Transaction::check()` now returns an error type specified by the new `Mismatch` associated type, rather than necessarily `PreconditionFailed`.
    * `transaction::Transactional::transact()` provides a convenient way to create and immediately execute transactions.

* `all-is-cubes-mesh` library:
    * New features in `dynamic::ChunkedSpaceMesh`:
        * Blocks may now be rendered via instancing.
          `dynamic::DynamicMeshTypes::MAXIMUM_MERGED_BLOCK_MESH_SIZE` controls whether this is done; set it to `usize::MAX` if you don't want instances.
          `dynamic::ChunkMesh::block_instances()` returns the instance data.
        * Parallelism for block and chunk mesh calculations:
            * Block mesh updates may be executed in the background rather than strictly during `update()`.
            This must be externally driven; if you wish to do so, clone the `ChunkedSpaceMesh::job_queue()`, and create one or more tasks/threads which take work from it.
            * Chunk mesh updates are performed in parallel using `rayon` if the `"auto-threads"` feature is enabled. They are not performed in the background, because they currently still require read access to the `Space` being rendered.

* `all-is-cubes-ui` library:
    * New widget `ProgressBar`.
    * New module `notification` and method `Session::show_notification()` for displaying notifications to the user (within the window, not platform notification integration).

### Changed

* The `"threads"` feature has been renamed to `"auto-threads"` in those libraries which have it, for clarity.

* `all-is-cubes` library:
    * `chunking::OctantMask` has been moved to the `math` module.

    * `listen::Listener::receive()` now accepts batches of messages instead of single messages, and the `alive()` method has been replaced with a return value from `receive()`. Implementors should review the new requirements documentation.

    * `math::FaceMap`’s `Debug` deduplicates identical elements.
    * `math::GridAab` is now allowed to be bigger than `i32::MAX`, up to `i32::MAX - i32::MIN`.
    * `math::GridAab` no longer has a limit on total volume (only `Vol` does).
    * `math::GridAab::from_lower_size()` now takes a `GridSize` instead of `GridVector`.
    * `math::GridAab::size()` and `math::Aab::size()` now return `GridSize` instead of `GridVector`.
    * `math::GridAab::intersection()` has been renamed to `intersection_cubes()`, and its precise behavior with zero-volume boxes clarified.
      If the alternative behavior is desired, see `intersection_box()`.
    * `math::GridAab::union()` has been renamed to `union_box()`, and is no longer fallible.
    * `math::GridArray` has been removed. Uses of `GridArray<T>` may be replaced with `Vol<Box<[T]>>`.
    
    * `transaction::ExecuteError` is now generic over the type of transaction it is an error from.
    * `transaction::Transaction` now has an associated type `Target` instead of a type parameter; it is no longer possible for a transaction type to be used with more than one target type.

    * `universe::Handle::execute()` no longer takes a useless `outputs` parameter.

    * Renamed `universe::URef` to `universe::Handle`. Related items have also been renamed:
        * `URefErased` to `ErasedHandle`
        * `AnyURef` to `AnyHandle`
        * `VisitRefs` to `VisitHandles`
        * `RefVisitor` to `HandleVisitor`
        
    * The macros `rgb_const!`, `rgba_const!`, and `notnan!` have been moved to the `math` module.

* `all-is-cubes-gpu` library:
    * `in_wgpu::headless::Builder::from_adapter()` requires `wgpu::Adapter` instead of `Arc<wgpu::Adapter>`.

* `all-is-cubes-mesh` library:
    * The return type of `GetBlockMesh::get_block_mesh()` has changed to `Option<&BlockMesh>`.
      `None` is to be returned when the implementor intends to request that the block be _omitted_ from a produced `SpaceMesh` (such as when it is being rendered separately) rather than the mesh merely being empty (invisible).
    * Renamed `dynamic::ChunkedSpaceMesh::update_blocks_and_some_chunks()` to `update()`, which is shorter and also more accurate nowadays since it isn't guaranteed to update all blocks.
    * The caller-supplied types in `MeshTypes` and `DynamicMeshTypes` must meet `Send + Sync` bounds.
    * `dynamic::ChunkedSpaceMesh::update()` requires the `render_data_updater` callback to be a `Fn`, not just a `FnMut`.
    * `dynamic::ChunkedSpaceMesh` requires a texture allocator passed to `new()` instead of `update()`.

* `all-is-cubes-render` library (formerly part of `all-is-cubes`):
    * `camera::Camera::projection_matrix()` now produces a depth range of 0 to 1 instead of −1 to 1.
    * `camera::ImageSize` is now an `euclid::Size2D` instead of `euclid::Vector2D`.
    * `camera::Viewport`'s fields are now `euclid::Size2D` instead of `euclid::Vector2D`.
    * Renamed `camera::Camera` methods:
        * `projection()` to `projection_matrix()`
        * `get_view_transform()` to `view_transform()`

    * `raytracer::SpaceRaytracer::trace_scene_to_text()` and `trace_scene_to_string()` have been replaced with the single method `to_text()`.

    * `HeadlessRenderer` implementors must always produces futures that are `Send`.

* `all-is-cubes-ui` library:
    * `apps::Session::set_universe_async()` has been replaced with the more flexible `set_main_task()`.
      It can do several things via the `MainTaskContext` type.
    * `vui::widgets::LargeText` now uses `all_is_cubes::block::text::Font` as the font type.

### Removed

* `all-is-cubes` library:
    * The modules `camera` and `raytracer` have been removed — or rather, made pseudo-private.
      Use the new library `all-is-cubes-render` instead.

    * `block::AnimationHint::{TEMPORARY, CONTINUOUS}` constants have been removed.
      Use the new constructor functions and `AnimationChange` instead.

    * `drawing::draw_to_blocks()` has been removed. There is no replacement.

    * `From<Rgb> for Cow<Block>` and `From<Rgba> for Cow<Block>` have been removed.
      Use explicit conversion through `Block` instead.
      Consider using the new `color_block!()` macro for constant colors.

    * `listen::Sink::take_equal()`

    * `op::Operation::Paint` has been removed. Use `Operation::Neighbors` and `Operation::DestroyTo` to get the same effect.

    * `transaction::PreconditionFailed` no longer exists. It has been replaced with error types specific to each transaction type.

## 0.7.1 (2024-01-27)

* `all-is-cubes` library:
    * Fixed compilation error in the case where the `save` feature is enabled (and no other crate is enabling `serde/alloc`).

## 0.7.0 (2024-01-27)

### Added

* Functionality:
    * Sound effects for character/world collisions.

* Graphics:
    * Improved initial startup behavior of mesh generation; chunks will be filled in sooner but lacking detail.
    * Windowed raytracing mode now uses background threads for increased throughput.

* `all-is-cubes` library:
    * Now `no_std` compatible (with caveats), if the `std` feature is disabled.

    * New variant `block::Primitive::Text` allows displaying text as part of a block, without having to separately draw the text as blocks first.

    * Expanded `block::BlockAttributes::tick_action` can now specify a time period rather than always occurring on the next tick.

    * New `block::CompositeOperator` variants `Atop` and `Out`, as per the standard Porter-Duff compositing operators.

    * New field `block::EvaluatedBlock::face_colors` provides colors which may be distinct for each face of the block. These colors may be used for low-detail rendering.

    * New method `Camera::near_plane_distance()` returns the (currently hard-coded) near plane distance used in the calculated projection matrix.

    * New type `linking::Provider` is `BlockProvider` but generalized to non-block types.

    * New type `math::Axis` is an enum of coordinate axes.

    * New type `math::Cube` represents a unit cube on the grid; it replaces many previous uses of `GridPoint` to identify cubes.

    * New constructor function `math::GridAab::from_ranges()` can make it more convenient to construct `GridAab`s where different axes are handled differently.
    * New method `math::GridAab::volume_f64()` returns the volume as a float, for calculations where volume is a scaling factor.

    * New type `math::Gridgid` represents rigid transformations (including reflection), a useful subset of what `GridMatrix` already could do.

        The following new functions return `Gridgid`:
        
        * `math::Face6::face_transform()`
        * `math::GridRotation::to_positive_octant_transform()`
    
    * New type `math::Vol` is a more general replacement for `math::GridArray` which allows choice of the data container type (including `&[T]` for borrowing without additional indirection), and also a replacement for uses of `math::GridAab` which care about volume and linearization.

    * New type `op::Operation` describes local transformations of a `Space` and an interacting agent with an inventory.
      It is now the type for specifying `BlockAttributes::tick_action` effects, and in the future will be used by `Tool`s as well.

    * `universe::UniverseTransaction::insert_anonymous()` and `universe::UniverseTransaction::insert_mut()` allow conveniently building transactions that insert multiple members without needing `merge()`s. Such transactions allow building universe content without passing around a `&mut Universe`.

* Demo content and `all-is-cubes-content` library:
    * The `atrium` scene now includes more elements.

* The `all-is-cubes-desktop` package now contains a library, which in principle can be used to create applications with different behavior than the included binary (such as creating and displaying a universe from some data source other than files and the provided templates). However, it does not yet have a well-designed API.

* `all-is-cubes-ui` library:
    * The UI optionally presents a “Quit” button, if configured via the `SessionBuilder`.
    * New type `vui::widgets::ButtonLabel` allows giving `ActionButton` and `ToggleButton` text labels in addition to or instead of icons.
    * New function `Session::set_character()` allows changing the player character without also changing the universe.
    * New function `vui::leaf_widget()` allows easier, less type-error-prone construction of `WidgetTree`s.
      It should generally be used in place of `LayoutTree::leaf()`.

### Changed

* `all-is-cubes` library:
    * Block schema and behavior changes:
        * Light emission is now a property of `Atom` blocks instead of `BlockAttributes`.
        * `EvaluatedBlock`’s `color` field is computed more accurately, ignoring voxels hidden by other voxels.
        * `BlockAttributes::tick_action` is now a dedicated `TickAction` type instead of `VoxelBrush`.
        * `BlockDef` now caches evaluation of its contained block. The cache is updated during `Universe::step()`.

    * All vector and matrix types from the library `cgmath` have been replaced with the library `euclid`.

    * All functions manipulating volume data in `Space`, `GridArray`/`Vol`, `Evoxels`, etc. have changed signature to use the new type `math::Cube` instead of `math::GridPoint`.

    * All functions using `usize` to identify a coordinate axis now use `math::Axis` instead.
      `Face6::axis_number()` and `Face7::axis_number()` are now called `axis()`.

    * `space::SpaceChange` now includes block indices, not just positions, in cube changes;
      and all variants have been renamed for clarity.

    * `space::SpaceTransaction` now exposes its components as individual `CubeTransaction`s, which can be mutated using `SpaceTransaction::at()`.

    * In block names and universe member names, `Cow<'static, str>` and `Arc<str>` have been replaced with `arcstr::ArcStr`.
      This type allows both refcounting and static literal strings.

    * The following functions have changed signature to use the new type `math::Gridgid`:
        * `math::GridAab::transform()`
        * `math::GridMatrix::decompose()`
        * `space::Space::draw_target()`
        * `space::SpaceTransaction::draw_target()`

    * The trait method `Behavior::step()` now must return a value of type `behavior::Then`, which specifies when the behavior should next be stepped.

    * `block::BlockBuilder::voxels_fn()` no longer takes a `&mut Universe` parameter.
      Instead, either the universe or a transaction must be supplied through a call to `.build_into()` or `.build_txn()`.

    * `block::EvalBlockError::StackOverflow` has been replaced with `block::EvalBlockError::BudgetExceeded`,
      which reflects a new, better-fitting block evaluation limit system.

    * `math::GridAab::volume()` now returns `Option<usize>`, in preparation for a future change where `GridAab` will not have a limit on volume.
    * `math::GridArray` is now `math::Vol` and allows choice of the data container type.

    * `camera::HeadlessRenderer` now returns a custom image container type `Rendering` instead of using `image::RgbaImage`.
      (This way, no dependency on `image` is needed.)

    * `linking::BlockProvider::install()` now requires a `&mut UniverseTransaction` instead of a `&mut Universe`.

    * `drawing::VoxelBrush::transform()` is renamed to `rotate()` and only accepts a rotation.
      This avoids confusion between points in space and cube-identifying coordinates.
    * Renamed `raytracer::PixelBuf` trait to `Accumulate`.

    * `all_is_cubes::util::CustomFormat` has been split into a separate library called `manyfmt`,
      reexported at `all_is_cubes::util::manyfmt`.
      The `CustomFormat` trait is now split into two traits, `Fmt` (for implementing) and `Refmt` (extension).

* `all-is-cubes-gpu` library:
    * Block textures are now fully dynamically allocated; it is no longer possible to run out of texture space.

* `all-is-cubes-mesh` library:
    * The new trait `MeshTypes` is now used to combine declaration of vertex and texture types.
      `BlockMesh`, `SpaceMesh`, `ChunkedSpaceMesh`, and `GetBlockMesh` all have a single `M: MeshTypes` parameter in place of multiple generics.

    * Major changes to texturing:
        * It is now possible for texture allocator implementations to use purely 2D texturing; `Tile`s are now subdivided into `Plane`s before usage, and each plane may provide its own texture coordinates independent of other planes in the volume.
        * Texture allocators may opt out of supporting multiple writes, and have only immutable allocations.

        * Texture allocators are expected to be able to store emissive color in addition to reflectance.
          The new type `texture::Channels` communicates which properties each allocation must actually store,
          so that allocators can conserve memory when it is not necessary.

        * Texture writes are now given data using `Vol<&[Evoxel]>` rather than pre-converted sRGB data;
        it is up to the texture implementation to convert and shuffle data as needed.

        * Trait method `GetBlockMesh::get_block_mesh()` takes additional arguments which support instanced rendering of blocks. (They are not yet used.)

        * Renamed `TextureAllocator` to `texture::Allocator`.
        * Renamed `TextureTile` to `texture::Tile`.
        * Renamed `NoTexture` to `texture::NoTexture`.
        * Renamed `NoTextures` to `texture::NoTextures`.

### Removed

* `all-is-cubes` library:
    * No longer depends on the `instant` library; WebAssembly builds do not need to depend on it to set its features.
    * `block::BlockBuilder::into_named_definition()` no longer exists.
      Instead, create the `BlockDef` separately after using the builder.
    * `block::BlockDef` no longer implements `Deref<Target = Block>`.
      Instead, call `BlockDef::block()` when needed.
    * `math::Aab::from_cube()` no longer exists. Use `Cube::aab()` instead.
    * `math::Face7::matrix()` no longer exists. Use `Face6::face_transform()` instead.
    * `math::GridAab::contains_cube()` no longer accepts `impl Into<GridPoint>`.
      Call sites should be changed to pass only `Cube`.
    * `math::GridAab::index()` no longer exists. Use `Vol::index()` instead.
    * `math::GridRotation::to_positive_octant_matrix()` no longer exists. Use `to_positive_octant_transform()` instead.
    * `math::cube_to_midpoint()` no longer exists. Use `Cube::midpoint()` instead.
    * `math::point_to_enclosing_cube()` no longer exists. Use `Cube::containing()` instead.
    * `space::SpaceTransaction::set_overwrite()` no longer exists. Use `transaction.at(cube).overwrite(block)` instead.

* `all-is-cubes-mesh` library:
    * No longer depends on the `instant` library; WebAssembly builds do not need to depend on it to set its features.
    * `TextureCoordinate` type alias no longer exists.
      Its only use was when implementing `TextureTile`; simply use `f32` instead.
    * `Texel` type alias no longer exists; callers are free to choose their own texel data type.

### Performance improvements

* Block voxel data is kept to smaller bounds where possible:
    * `BlockBuilder::voxels_fn()` trims empty space surrounding the produced voxels.
    * `all_is_cubes::block::Modifier::Composite` produces voxel data bounds that are the union or intersection of the inputs, as appropriate, rather than a full block.
* Various tweaks to reduce the size of compiled code.


## 0.6.0 (2023-07-29)

### Added

* Command line application (crate `all-is-cubes-desktop`) functionality:
    * Exporting universes to files; see below section on `all-is-cubes-port` for supported formats.
    * Option `--template-size` allows controlling the size of spaces produced by universe templates.
    * Window titles name the data source (file or template) for the contained universe.

* `all-is-cubes` library:
    * Many types, including `Universe` and its components, now support serialization via `serde`.
      This serialization support is still a work in progress and long-term save data compatibility is planned but not currently guaranteed.

    * `block::Atom`, a struct for the data of the `Primitive::Atom` enum variant.
    * `block::CompositeOperator::In` allows masking one block shape by another.
    * `block::Modifier::Composite` has a new option `disassemblable`, which causes `Block::unspecialize()` (and its callers such as `Tool::RemoveBlock`) to return the composed blocks separately instead of the composite.
    * `linking::BlockProvider` now has methods `subset()` (replace keys) and `map()` (replace values) to allow using `BlockProvider`s in more ways.
    * `math::GridAab::iter()` to iterate over `(cube, &item)`.
    * `save::WhenceUniverse`, a trait for associating a `Universe` with disk files or other persistent storage. `Universe` now keeps an `Arc<dyn WhenceUniverse>`.
    * `space::SpaceBuilder::palette_and_contents()` allows efficiently specifying arbitrary contents for a newly created `Space`.
    * `universe::RefVisitor` is now implemented for all `FnMut(&dyn URefErased)`, allowing visitors to simply be functions.
    * `universe::Universe::universe_id()` returns a unique identifier for this `Universe`.

* `all-is-cubes-port` library:
    * Import and export of a “native” file format (serialized `Universe`s).
    * Export to `.stl` meshes (commonly used for 3D printing).
    * Export to MagicaVoxel `.vox` files.
    * `ImportError` type for precise error reporting.

* `all-is-cubes-ui` library:
    * `vui::LayoutTree::Shrink` allows a subtree to be shrunk to only be as big as needed, rather than filling available space, allowing for “shrink wrapped” layouts such as framed dialog boxes.
    * `vui::widgets::Frame::as_background_of()` allows conveniently making dialog frames.

### Changed

* Graphics:
    * `Space` light propagation no longer updates deterministically, but as much as possible given available time for the computation.

* New crate `all-is-cubes-mesh` contains the former contents of `all_is_cubes::mesh`, except for `LineVertex` which is now in `all_is_cubes::math`.
    * `SpaceMesh` indices are now either `u16` or `u32` depending on the size of the mesh, rather than always `u32`. The new enum `all_is_cubes_mesh::IndexSlice` is used to work with them.
    * `SpaceMesh` index ranges and other metadata are now kept in a sub-struct `MeshMeta`.
    * Newly public/documented module `dynamic` provides support for incrementally updated meshes for interactive rendering.
    * Renamed: `BlockMeshProvider` is now `GetBlockMesh`. This aligns with the general principle of naming traits for the action that they enable.

* `all-is-cubes` library:
    * Block collision control has been redesigned:
        * `BlockCollision` is now stored in `Atom`s and not in `BlockAttributes`; `Primitive::Recur` blocks always get their collision from their component voxels.
        * `BlockCollision::Recur` no longer exists and `BlockCollision::Hard` cannot be used to disregard voxels' own collision.
        This change should be an overall simplification of the semantics and eliminates the common mistake of forgetting to specify `Recur`.
    
    * Universes are now considered to have an inherent, fixed time step.
        * This time step cannot yet be changed; it is currently always 1/60 second.
        * Within a single second, instants and `Tick`s have a “phase” value which cycles from 0 to 59.
          This will enable simulations to run on an intentionally slow but consistent schedule by skipping some ticks.
        * The `time` module contains new types `TickSchedule` and `Clock` to support this.

    * `block::Block::listen()` is now `evaluate_and_listen()` which includes a simultaneous `evaluate()`.
    * `block::Block::unspecialize()` now returns `Vec<Block>`, to allow for cases where a block comes apart into multiple parts, such as with `Modifier::Composite`.
    * `block::EvaluatedBlock` now has a `voxels` field of the new type `Evoxels`, which replaces the previous `resolution` and `voxels` fields.
      This simplifies the data model, in that there is now _always_ a set of `Evoxel`s defining a block's shape, even if there's only one of them, and it is always found in the `voxels` field. This is a breaking change for code that accesses `EvaluatedBlock` data.
    * `block::Modifier::attach()` has been replaced by `block::Block::with_modifier()`.
      This is intended to be more convenient in all cases.
    * `block::Primitive::Atom` now contains a `struct Atom` instead of individual fields.
    * `block::AIR` now has its own dedicated primitive, `Primitive::Air`. The behavior is unchanged.
    * `camera::Flaws` now implements `Display`. Use this instead of `Debug` for printing the flaws.
    * `math::Geometry::wireframe_points()` now produces a new type `mesh::LineVertex` instead of a tuple (with the same position and color data).
    * `space::Space::extract()` now passes one argument instead of three to the callback function; it is of the new type `Extract` which has methods to return all the previously available data. This is intended to be more extensible and potentially more efficient.
    * `transaction::Transaction` can now produce any number of `Output`s, delivered through a callback.
    * `transaction::Merge` now has an associated type `Conflict` for more informative conflict errors.
    * `universe::Name` now has a variant `Pending` for not-yet-assigned names.
    * `universe::URef::name()` now returns an owned instead of borrowed `Name`.
    * `universe::UniverseIndex` is no longer a public trait; the relevant methods are now inherent methods on `Universe`.
    * `universe::UniverseTransaction::insert()` now takes a `URef` created by `URef::new_pending()`, instead of a bare value.
      This allows associations between the new member and other objects to be created within the same transaction.
    * Types that previously had a `pub fn listen(&self, impl Listener)` now implement the `listen::Listen` trait instead.
      `listen::DirtyFlag::listening()` now expects `impl Listen` instead of a closure.

* `all-is-cubes-port` library:
    * `load_universe_from_file` returns its own `ImportError` instead of `anyhow::Error`.

* `all-is-cubes-ui` library:
    * `vui::LayoutGrant` now takes an additional parameter, `enlarge_for_symmetry`. Existing calls should be changed to pass `false` to get the prior behavior.

### Removed

* `all_is_cubes::block::BlockCollision::Recur` no longer exists; it is the default behavior.
* `all_is_cubes::transaction::TransactionConflict` no longer exists; the `Transaction` trait has a `Conflict` associated type instead.

## 0.5.1 (2022-12-29)

* Fixed packaging error in `all-is-cubes-desktop`.

## 0.5.0 (2022-12-28)

This is a large revision with many breaking changes. This log does not record all of them; I have chosen “get a release out” over “document everything”.

### Added

* New crate `all-is-cubes-port` contains import/export routines (not very many, for now):
    * Import MagicaVoxel `.vox` files.
    * Export glTF 3D model files. (This support is very incomplete and currently is not feasible to use outside of the export feature in `all-is-cubes-desktop`).

* New crate `all-is-cubes-ui` contains the user interface components which used to be in `all-is-cubes`. This separation should improve compilation times.

* Graphics:
    * The `all-is-cubes-gpu` renderer now fully supports blocks with a resolution greater than 16.

* `all-is-cubes` library:
    * `block::Modifier::Move`, for drawing blocks in motion or off the grid.
    * `block::Modifier::Zoom`, for creating multi-block structures from a single block definition.
    * `camera::HeadlessRenderer`, for easily creating images using the raytracer or other renderers.
    * `math::point_to_enclosing_cube()`
    * `math::Face6`, which is like `math::Face` (now) `Face7` but without the `Within` variant.
    * `DirtyFlag::listening()` which simplifies typical usage.
    * `GridArray::repeat()` for constructing arrays with uniform contents.
    * `GridArray::from_element()` for constructing single-element arrays.
    * `GridRotation::ALL_BUT_REFLECTIONS`, as `GridRotation::ALL` but excluding reflections.
    * `SpaceBuilder::filled_with()` allows specifying the initial block in a new `Space`.
    * `SpaceTransaction::draw_target()` allows 2.5D drawing into a transaction in the same way `Space::draw_target()` works on `&mut Space`.
    * `StandardCameras::world_space()`, so that clients don't need to consult the `Character` each frame.
    * `Universe::get_any()` allows looking up universe members without knowing their type.
    * `UniverseTransaction::insert()` allows inserting objects into a `Universe` via transaction rather than directly.
    * `VoxelBrush::transform()` allows rotating a `VoxelBrush`.
    * `VoxelBrush::with_thickness()` allows easily constructing a brush with a Z-axis stack of one block.
    * The previously-existing widget system is now publicly available in the `vui` module.
    * Most `Listener` implementations now also implement `Clone` and `Debug`. This allows taking a listener and registering it with more than one `Notifier`. Relatedly, some uses of `-> impl Listener` have been replaced with concrete types.

### Changed

* `all-is-cubes` library:
    * Breaking: The `Block` type has been substantially redesigned (though it still has existing functionality).
        * It is no longer an `enum`; it is an opaque type.
        * It is logically made up of two components, a `block::Primitive` and any number of `block::Modifier`s.
            * `block::Primitive` is an enum which has `Atom`, `Recur`, and `Indirect` variants as before.
            * The `Rotated` variant is now `Modifier::Rotate`.
        * It internally uses reference counting to be predictably cheap to `clone()`; this is intended to help use cases such as transactions which mean that even a block which only exists once in the universe is frequently cloned.
        * Many changes to the functions, associated methods, and `BlockBuilder` were made to support this new structure.

    * Breaking: `mesh::TextureAllocator` implementations may now choose an arbitrary type to represent their texture coordinates. `mesh::BlockVertex` is now generic to accomodate this.

    * Breaking: `behavior::BehaviorSet` now stores additional data, the “attachment”, with each behavior, which must be specified when adding a behavior.

    * Breaking: `apps::Session::new()` (formerly `AllIsCubesAppState::new()`) has been replaced with `SessionBuilder`, whose `build()` is now an async function.
    * Breaking: `apps::StandardCameras` now works with a `ListenableSource<Viewport>` instead of a `Viewport` and `set_viewport()` method, and it is constructed using `Session::create_cameras()` instead of `StandardCameras::from_session()`.
    * Breaking: `block::AnimationHint` (from `BlockAttributes::animation_hint`) has been redesigned to be more systematic.
    * Breaking: `linking::BlockProvider::new()` is now an async function.
    * Breaking: The `linking::BlockModule` trait now requires the [`exhaust::Exhaust`](https://docs.rs/exhaust/latest/exhaust/trait.Exhaust.html) trait in place of `strum::IntoEnumIterator`. This allows implementors to use enums with fields (or non-enums).
    * Breaking: `raytracer::SpaceRaytracer::trace_scene_to_image()` now expects a buffer rather than allocating one.

    * Renamed: `space::Grid` is now `math::GridAab`.
        * Methods named `grid()` that return the bounding box of something are now called `bounds()`.
        * `Grid::contains_grid()` is now `GridAab::contains_box()`.
        * `Raycaster::within_grid()` is now `Raycaster::within()`.
        * `Grid::new()` is now `GridAab::from_lower_size()`.
        * `Grid::checked_new()` is now `GridAab::checked_from_lower_size()`.
    
    * Renamed: `apps::AllIsCubesAppState` to `apps::Session`.
    * Renamed: `math::Face` is now `math::Face7`.
    * Renamed: `mesh::SpaceMesh::new()` is now `mesh::SpaceMesh::default()`.
    * Renamed: `mesh::triangulate_block()` is now `mesh::BlockMesh::new()`.
    * Renamed: `mesh::triangulate_blocks()` is now `mesh::block_meshes_for_space()`.
    * Renamed: `mesh::triangulate_space()` is now `mesh::SpaceMesh::new()`.
    * Renamed: `space::SpaceBuilder::build_empty()` is now `build()`.
    * Renamed: `transaction::UniverseTransaction` is now `universe::UniverseTransaction`.
    * Renamed: `vui::Icons` is now `inv::Icons`.

    * Moved to `all-is-cubes-ui`: the modules `apps` and `vui`.

    * Renamed: The feature `"rayon"` is now `"threads"`.

* `all-is-cubes-gpu` library:
    * No longer supports `luminance` GPU API; uses `wgpu` instead.

### Removed

* `all-is-cubes` library:
    * `math::NoiseFnExt` (no longer public).

## 0.4.0 (2022-01-29)

### Added

* Functionality:
    * New universe template, "dungeon", generating an enclosed maze of rooms. I hope to build on this further to create some sort of gameplay (e.g. finding keys to unlock doors) and a tutorial for All is Cubes' functionality from a player's perspective; currently, all the rooms are the same.
    * New universe template, "menger-sponge". Fractals are cool.
    * There is now some amount of clickable user interface: pause and mouselook functions are visible as clickable buttons. Soon we may have actual menu screens!

* Graphics:
    * The software raytracer now implements volumetric transparency; blocks whose colors have alpha between 0 and 1 may be rendered as a solid volume of translucent material, rather than the alpha being applied at the surface, if the graphics options request it. However, the light model does not yet have a reasonable interpretation of how to light these volumes, so blocks may have unreasonably dark parts. I hope that future work will address this and also better handling of material surfaces (e.g. light reflections at the surface of glass, vs the lack of them in fog or at the meeting surfaces of adjacent similar blocks).

* `all-is-cubes` library:
    * New major items:
        * `apps::StandardCameras` manages the `Camera`s needed for rendering, given the necessary inputs. It is intended to reduce duplicated code and coupling in each renderer.
        * `universe::VisitRefs` trait allows traversing the `URef` graph inside a `Universe`.
        * `util::YieldProgress` is an interface for long-running async tasks to report their progress.
    * New features in existing types:
        * `Aab::center()` returns the center point of the box.
        * `BehaviorSet::query()` for looking up existing behaviors.
        * `BehaviorSetTransaction::insert()` for adding a behavior via transaction.
        * `BlockAttributes::rotation_rule` and `RotationPlacementRule` define how a block should be automatically rotated when placed.
        * `Camera::exposure` for controlling displayed brightness. Both renderers implement it. Nothing sets it yet.
        * `Camera::post_process_color()` applies the new exposure and tone-mapping settings to a color value.
        * `Evoxel::from_block()` for performing the same conversion from a full block to a voxel that `Block::Recur(...).evaluate()` does.
        * `Face::dot()`, for dot product with a vector without constructing an intermediate unit vector.
        * `GraphicsOptions::tone_mapping` and `ToneMappingOperator` (not yet providing good operators, just the mechanisms to have the feature at all).
        * `Rgb::from_srgb8()`.
        * `Rgb::luminance()` and `Rgba::luminance()`.
        * `SpaceMesh` now remembers the `TextureAllocator::Tile`s it was constructed using, just like `BlockMesh` does, so callers no longer need to do so.
        * `SpaceMesh::blocks_used_iter()` reports which block indices went into the mesh.
        * `SpaceTransaction::set()`, for faster construction of large transactions instead of many small ones. (This will likely be changed further to have better naming and convenience.)
        * `SpaceTransaction::nonconserved()` requests that merging two transactions with the same effect on some cube should not be considered a conflict.
        * `math::cube_to_midpoint()` function for the pattern of converting to float and adding 0.5.
    * The following functions are now `const fn`:
        * `Grid::for_block()`
        * `Rgb::from_srgb8()`
        * `Rgba::from_srgb8()`

* `all-is-cubes-content` library:
    * `UniverseTemplate::build` is now an async function, which accepts a `YieldProgress` hook parameter. This means that there can be progress bars (currently implemented in desktop but not web) and that building doesn't hang the web event loop.

* `all-is-cubes-desktop`:
    * Can now “open data files”. Currently the only file format supported is MagicaVoxel `.vox` scene files; when we have a native file format that will be supported too. Files can be opened by passing them on the command line or by dropping them on an open window.
    * Terminal mode now has mouse support for placing and removing blocks.
    * Terminal mode no longer goes blank if asked to use graphic characters but not colors.

### Changed

* Game mechanics:
    * `Character`s no longer inherently have the ability to fly; they can only do so if they have a `Tool::Jetpack` in their inventory.
    * It is no longer possible to jump multiple times in the same frame.

* Graphics:
    * Fixed a bug in the GPU renderer where dark areas would always have a noticeable minimum brightness (`PackedLight` did not have the same interpretation in the Rust and GLSL code for a value of zero).
    * Block meshes are no longer computed all at once, reducing maximum frame time during startup (or, in the future, when moving from viewing one `Space` to another).
    * Optimized construction and depth-sorting of meshes for transparent blocks. They still don't draw correctly, but they are less slow.
    * Various performance improvements to the raytracer.

* `all_is_cubes` library:
    * The new minimum supported Rust version is 1.56.0 (2021 edition).
    * Breaking: The items previously in `all_is_cubes::lum` are now in a separate crate `all_is_cubes_gpu`, and the `"lum"` feature no longer exists.
    * Breaking: The `triangulator` module has been renamed to `mesh`. All types with `Triangulation` in the name now use `Mesh` instead.
        * `BlockTriangulation` → `BlockMesh`
        * `BlockTriangulations` → `BlockMeshes`
        * `SpaceTriangulation` → `SpaceMesh`
            * `SpaceMesh` takes an additional type parameter for the texture type.
        * `BlockTriangulationProvider` → `BlockMeshProvider`
        * `TriangulatorOptions` → `MeshOptions`
    * Breaking: The raytracer is now more flexible and allows custom data to pass into the tracing process.
        * Instead of `PixelBuf` having a single output type, the raytracer returns the `PixelBuf` and lets the caller convert it (or provide a conversion function, for `trace_scene_to_image` which returns a slice of converted values).
        * Block data is computed via a separate trait, `RtBlockData`, and has access to `GraphicsOptions` and an arbitrary extra parameter. Multiple types of `PixelBuf` may be used with the same `SpaceRaytracer` as long as they agree on a concrete type of `RtBlockData`.

    * Breaking: The color values written into `all_is_cubes::mesh::TextureTile` are now in sRGB; previously, they were linear.
    * Breaking: `GLRenderer::new()` now expects a `StandardCameras` (instead of the components of one).
    * Breaking: `AllIsCubesAppState::update_cursor()` now expects a `&StandardCameras` instead of two `Camera`s.
    * Breaking: `Camera::set_view_matrix()` is now `Camera::set_view_transform()`, and expects a `cgmath::Decomposed` transform, inverted from the matrix version.
    * Breaking: `Space::spawn_mut()` replaced by `Space::set_spawn()`.
    * The `Name` enum is now intended to be reliably cheap to clone:
        * `Name::Specific`'s field holds an `Arc<str>` instead of a `String`. 
        * `Universe`-related errors now hold `Name` instead of `Arc<Name>`.
    * Renamed: `Evoxel::new()` to `Evoxel::from_color()`.
    * Renamed: `apps::Tick` is now `time::Tick`. 
    * Renamed: `Rgba::from_srgb_32bit()` is now `Rgba::from_srgb8()`, and `Rgba::to_srgb_32bit()` is now `Rgba::to_srgb8()`. This is more consistent with what I understand to be common terminology (specifying the component size instead of the total size), and allows the same name to be used sensibly for `Rgb`.

## 0.3.0 (2021-10-09)

### Added

* Demo content: Multiple scenes are now available, through the command-line option `--template`, web page URL parameter `?template=`, or in code as `all_is_cubes_content::UniverseTemplate`. The available templates include:
    * `demo-city`: The default in the previous version: a collection of many examples of specific functionality. (Now with more stuff, of course.)
    * `atrium`: A voxel interpretation of the Sponza Atrium rendering test scene (from scratch, not a data conversion).
    * `blank`: No data at all. Mostly useless except for testing.
    * `cornell-box`: A version of the [Cornell box](https://en.wikipedia.org/wiki/Cornell_box) rendering test scene.

* `all-is-cubes-desktop`:
    * New graphics mode `-g record` for rendering images or video (currently only in PNG or APNG format).
    * New option `--display-size` to choose the window or image size.
    * New option `--precompute-light` to fully compute `Space` light before displaying anything.
    * Terminal mode (`-g terminal`):
        * Pipelined simulation and rendering for more CPU utilization.
        * Keyboard controls over the text and color modes used.
        * Double-resolution mode using “▄” graphic characters.
        * UI has more HUD elements.
        * Implemented using the `crossterm` library instead of `termion`, and should therefore support Windows (not tested).
    * Reads graphics options from `graphics.json` in whatever [directories-next](https://docs.rs/directories-next/2.0.0/directories_next/) thinks is a platform-appropriate app configuration directory.
    * Added info text overlay like the web version.

* Graphics:
    * Color values produced in all modes are now sRGB instead of linear, which should be more correct on most displays.
    * Various graphics options are now configurable without recompiling, through the `GraphicsOptions` type.
    * “Smooth lighting”, interpolated across block faces.
    * Triangulator and  `all-is-cubes::lum` (GPU triangle-based renderer):
        * Blocks with resolution greater than 16 are now drawn with correct shapes, though not textures unless they have solid-colored faces because the texture allocator is still limited.
        * Added frustum culling.
        * Added distance fog.
        * Now supports transparent blocks, as the raytracer already did. This support is complete for `Block::Atom` blocks but poor to nonexistent for `Block::Recur` blocks.
        * Light changes no longer require rebuilding chunk triangulations.
    * Fixed several bugs in light calculation, including one that could lead to runaway brightness.

* Physics and game mechanics:
    * Character now has limited reach (can no longer interact with arbitrarily distant blocks).
    * Gravity is now a property of individual `Space`s.
    * Characters collide against the voxel shapes of blocks, not just their unit cube bounds. This happens only if the block's `BlockCollision` attribute is `Recur`.
    * Inventory items are now stackable, and “finite rather than infinite” block items exist — you can place a block in a `Space` and lose it from your inventory, or the reverse.

* User interface:
    * Mouselook support.
    * Cursor markers no longer lag behind the camera.
    * The HUD now displays a “tooltip” with the name of the selected tool/block.

* `all_is_cubes` library API:
    * `Universe`, `URef`, `Space`, `Listener`, and so on now implement `Send` and `Sync`. They are not fully equipped for useful multithreaded operation (in particular, there is are no principles for deadlock-free lock acquisition of various `URef`s), but it is now possible to, for example, construct a `Universe` on one thread and send it to another.
    * Transactions (`all_is_cubes::transaction`). Transactions describe modifications to a game object or many within the same `Universe`. They enable:
        * Ensuring that a set of modifications is committed or entirely rejected (preventing “item duplication” kinds of bugs).
        * Deferring mutations to solve borrowing problems.
        * For example, `Tool`s' modifications of `Space`s are now exclusively performed as transactions.
    * Behaviors (`all_is_cubes::behavior`). The `Behavior` trait can be implemented to add custom behavior to a `Space` or `Character` (a sort of “scripting”). Its functionality is very limited for now and may be substantially revised.
    * Inventories and items (`all_is_cubes::inv`) are now public.
    * `Aab` implements `From<Grid>`.
    * `Aab::contains()`
    * `Aab::face_coordinate()`: returns one of the box's six coordinates.
    * `Aab::random_point()`
    * `Aab::round_up_to_grid()`
    * `Aab::scale()`
    * `Block::rotate()`: generates a canonical `Block::Rotated` version of a block.
    * `Block::unspecialize()` to undo rotation and, in the future, other changes that are considered specific to a block's particular placement in a `Space` (such as being due to interactions with other blocks).
    * `BlockAttributes::animation_hint`
    * `Face` implements `TryFrom<GridVector>`.
    * `FaceMap::iter()`
    * `FaceMap::repeat()`
    * `GenError` and `InGenError` types for classifying worldgen failures.
    * `Grid::checked_new()`: returns an error instead of panicking.
    * `Grid::abut()`: constructs a grid adjacent to an existing one (useful for making walls).
    * `Grid::axis_range()`: as `x_range()`, `y_range()`, `z_range()` but with a parameter for the axis.
    * `Grid::expand()`: enlarges a grid by moving its faces.
    * `Grid::intersection()`
    * `Grid::unsigned_size()`
    * `GridArray` implements `IndexMut`.
    * `GridArray::from_elements()`
    * `GridArray::map()`
    * `GridMatrix::decompose()` to decompose into a translation and rotation.
    * `GridMatrix::inverse_transform()`
    * `GridMatrix::transform_cube()`
    * `GridRotation::{ALL, IDENTITY, CLOCKWISE, COUNTERCLOCKWISE}`
    * `GridRotation::from_to()` (a “look at” operation of sorts)
    * `GridRotation::inverse()`
    * `GridRotation::is_reflection()`
    * `GridRotation::iterate()` to repeat a rotation exactly until it loops to the start.
    * `GridRotation::to_rotation_matrix()`
    * `Rgb::clamp()`, `Rgba::clamp()`
    * `Space::builder()`: returns `SpaceBuilder` for configuring a new space's properties.
    * `Space::evaluate_light()` and `Space::fast_evaluate_light()` can be used for immediate light evaluation.
    * `Space::fill_uniform()`: faster than `Space::fill()` when filling with only one block.
    * `SpaceRaytracer::trace_scene_to_image()` can be used to produce an RGBA image.
    * `TextureAllocator`, the trait through which the triangulator outputs textures, is now expected to offer 3D textures instead of 2D. This significantly reduces the number of redundant texels for complex blocks (at the cost of more never-seen texels for simple blocks).

* Some information is logged using the `log` crate (startup progress and some nonfatal errors).

### Changed

* `all_is_cubes` library:
    * No longer depends on the `luminance-front` crate.

    * Expansions:
        * `Space` can now store up to `u16::MAX` (65536) distinct blocks, instead of 256.
        * `Grid` now allows zero-volume grids.

    * Breaking parameter/field/type changes:
        * `draw_to_blocks()` now takes options for Z dimensions and the attributes of the resulting blocks.
        * `GridRotation` is now an enum, so its size is 1 byte and it cannot have invalid values.
        * `EvaluatedBlock` now has an explicit `.resolution` field. The dimensions of the `.voxels` field no longer implicitly define the resolution; if `.voxels` is smaller than the block then the missing voxels should be treated as `AIR`.
        * The API of `all_is_cubes::triangulator` has been substantially revised to support depth sorting and separation of transparent triangles, and to remove unnecessary complexity.
        * New type `GraphicsOptions` stores global user-settable rendering options, and is required by most graphics-related functions.
        * `make_some_blocks()` returns an array rather than a vector, taking the size as a const generic parameter.
        * `Universe::get_default_character()` returns `Option` instead of panicking.
        * `Face::axis_number()` returns `Option` instead of panicking.
        * `all_is_cubes::listen::Sink` now stores messages in order without duplicates, and offers the method `.drain()` instead of implementing `Iterator`.
    
    * Breaking: The integration with `embedded_graphics` now uses API version 0.7. The drawing target type has been renamed from `VoxelDisplayAdapter` to `DrawingPlane`.
    * Breaking: `all_is_cubes::util::ConciseDebug` is now generalized as `all_is_cubes::util::CustomFormat` which takes one of several formatting types.

    * Renamed: The modules `all_is_cubes::blockgen`, `all_is_cubes::worldgen`, and `all_is_cubes::content` have all been replaced by the separate crate `all_is_cubes_content`.
    * Renamed: `all_is_cubes::camera::Camera` to `all_is_cubes::character::Character`, and `all_is_cubes::camera::ProjectionHelper` to `Camera`.
    * Renamed: `all_is_cubes::camera::InputProcessor` to `all_is_cubes::apps::InputProcessor`.
    * Renamed: `all_is_cubes::lum::glrender::Viewport` to `all_is_cubes::camera::Viewport`.
    * Renamed: `all_is_cubes::universe::FrameClock` to `all_is_cubes::apps::FrameClock`.
    * Renamed: `GridArray::generate()` and `FaceMap::generate()` to `from_fn()`.
    * Renamed: `Aab::expand()` to `Aab::enlarge()`
    
    * `Grid::index()` no longer overflows on some out of range coordinates.
    * `Raycaster` will stop iteration rather than overflowing if the ray exits the range of possible cube coordinates (`i32`).

* Development tools:
    * The project's custom build scripts are now provided using the [cargo xtask pattern](https://github.com/matklad/cargo-xtask) rather than a Makefile. This means that the `make` tool is not needed, and instead of e.g. `make run-dev`, you would now write `cargo xtask run-dev`.

### Removed

* Most of `all_is_cubes::lum` is no longer public as I have decided those items should be considered implementation details. The top-level struct `GLRenderer` (which needs a better name) is no longer in the `glrender` submodule.
* `FaceMap::values()`: use `FaceMap::iter()` instead.
* `Universe::get_default_space()`

## 0.2.1 (2021-01-18)

A changelog was not kept for this and earlier releases.
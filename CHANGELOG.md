# Changelog

## Unreleased

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
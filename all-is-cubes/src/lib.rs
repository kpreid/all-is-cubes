#![feature(const_clone)]
#![feature(const_default)]
#![feature(const_convert)]
#![feature(const_trait_impl)]
#![feature(doc_cfg)]
#![feature(doc_notable_trait)]
#![feature(final_associated_functions)]
#![feature(fmt_debug)]
#![feature(impl_trait_in_assoc_type)]
#![feature(large_assignments)]
#![move_size_limit = "2000"]
#![feature(macro_attr)]
#![feature(negative_impls)]
#![feature(never_type)]
#![feature(new_range, new_range_api)]
#![feature(try_blocks)]

//! All is Cubes is a game/engine for worlds made of cubical blocks, where the blocks
//! are themselves made of “smaller” blocks (voxels) that define their appearance and
//! behavior.
//!
//! This crate, `all_is_cubes`, defines the data model and simulation rules.
//! (User interface components, rendering, platform glue, and game content are kept in other
//! crates.)
//! This crate is designed to be a reusable library for simulating and rendering voxel
//! world/scenes, but is not yet mature; many pieces of functionality are only sketched out
//! or missing entirely.
//!
//! ## Capabilities
//!
//! Here is a brief summary of the concepts and capabilities (actual or planned) of
//! All is Cubes, so that you may evaluate whether it suits your needs. Many of these
//! details are subject to change; this is currently a one-author project with a rather
//! chaotic vision.
//!
//! ### Data model
//!
//! Note that there is not currently any networking/multiplayer, or any save files.
//! Both of these are planned, but may result in substantal revisions to the data model.
//!
//! * [`Universe`] manages a graph of interrelated game objects. It is intended to
//!   be the run-time form of what a player might call “a save file” or “a world”.
//!   Time advances at a uniform rate throughout a `Universe`.
//! * A [`Space`] is a 3D array of [`Block`]s forming the physical space/scene which
//!   player characters can move through and interact with.
//!   * `Space`s currently have fixed boundaries (no infinite procedural generation),
//!     can have dimensions in the range of an [`i32`], and are kept in memory
//!     uncompressed at 6 bytes per cube. They can contain at most 65536 distinct
//!     blocks (subject to change).
//!   * The light falling on surfaces within a `Space` is computed using a global
//!     illumination algorithm (optionally).
//!   * In addition to their [`Block`] contents, `Space`s have global physical
//!     properties such as gravity and illumination. They also can have [`Behavior`]s
//!     attached for miscellaneous “scripting” purposes, though this feature is
//!     to be redesigned.
//!   * There can be arbitrarily many spaces per [`Universe`]. Eventually there will
//!     be ways to teleport between spaces; for now, this mainly assists [`Block`]s.
//! * A [`Block`] is a cubical object which can be placed in a [`Space`] or carried in
//!   a [`Character`]'s inventory.
//!   * Blocks' shape and appearance (their [`Primitive`]) is either a cube of a single
//!     color, or recursively defined by a [`Space`] of smaller blocks (at a reduced
//!     size of [between 1/2 and 1/128][block::Resolution] of a full cube).
//!     Each of these voxels can be transparent.
//!     * The player will be able to enter these [`Space`]s and interactively edit
//!       blocks. This is not currently implemented but that is a lack in the user
//!       interface rather than core data structures.
//!     * In the future, there may be ways for blocks to be fully procedurally defined
//!       rather than working through [`Space`]s, such as for the purposes of
//!       animation or deriving variations from a single template or formula.
//!   * Blocks can have [`Modifier`]s applied to them which change the basic shape and
//!     behavior in a particular instance, such as being rotated into different
//!     orientations.
//!   * Blocks have various forms of active behavior and responses to their environment,
//!     such as [periodically taking an action][block::BlockAttributes::tick_action]
//!     and [playing sound][block::BlockAttributes::ambient_sound].
//! * A [`Character`] can move around a [`Space`], acting as the player's viewpoint, and
//!   carry [`Tool`]s (items) with which to affect the space.
//!     * There can be multiple characters, but not yet any multiplayer, NPC AI, or even
//!       being able to see another character. Some of these this may be in the future.
//!     * A character's ability to modify the [`Space`] depends entirely on the provided
//!       tools, so that there can be entirely free editing or only actions following
//!       gameplay rules.
//!
//! ### Coordinate system
//!
//! Currently, there are some assumptions of a specific coordinate system.
//! All of these might be generalized in the future.
//!
//! * [`Space`] and [`Block`] use 3-dimensional integer coordinates with no assumptions
//!   about axes. However, the default [`SpacePhysics`] configuration currently has a
//!   gravity vector in the &minus;Y direction.
//! * [`Character`] and [`Body`] have a look direction and corresponding transformation
//!   matrix which assumes OpenGL-style coordinates: +X right, +Y up, +Z towards viewer —
//!   a “right-handed” coordinate system.
//!   Jumping is also hardcoded to work in the +Y direction.
//!   Future versions may support arbitrary character orientation.
//!
//! ## Package features
//!
//! This package, `all-is-cubes`, defines the following feature flags:
//!
//! * `"save"`:
//!   Enable [`serde`] serialization of [`Universe`]s and some other types.
//! * `"auto-threads"`:
//!   Enable use of threads for parallel and background processing, including via
//!   [`rayon`]’s global thread pool.
//!   This feature does not affect the public API (except via enabling other features),
//!   only performance and dependencies.
//! * `"arbitrary"`: Adds implementations of the [`arbitrary::Arbitrary`] trait for
//!   fuzzing / property testing on types defined by this crate.
//! * `"std"` (enabled by default):
//!   If disabled, the library becomes `no_std` compatible, at this cost:
//!   * Some trait implementations for [`std`] types are removed.
//!   * Floating-point functions are provided by [`libm`] instead of [`std`].
//!   * Spinlocks may be used in cases where that is a poor choice, reducing
//!     multithreaded performance.
//!   * Certain data calculations are not memoized.
//!
//! ## Platform compatibility
//!
//! * Compatible with web `wasm32-unknown-unknown`, whether or not the `std` feature is active.
//!   That is, the parts of `std` it uses are the thread-safety and floating-point parts,
//!   not IO (and not creating threads, unless requested).
//!   However, you must enable the `"web"` feature of [`bevy_platform`].
//!
//! * `usize` must be at least 32 bits (that is, not 16).
//!
//! ## Dependencies and global state
//!
//! `all_is_cubes` avoids having any global state, for the most part. However, it does write
//! log messages using the [`log`] crate and is therefore subject to that global
//! configuration.
//!
//! `all_is_cubes` depends on and re-exports the following crates as part of its public
//! API:
//!
//! * [`arcstr`] for string data (as `all_is_cubes::arcstr`).
//! * [`euclid`] for vector math (as `all_is_cubes::euclid`).
//! * [`ordered_float`] (only the type [`all_is_cubes::math::NotNan`][crate::math::NotNan]).
//! * [`embedded_graphics`] (as `all_is_cubes::drawing::embedded_graphics`).
//!
//! [`Behavior`]: crate::behavior::Behavior
//! [`Block`]: crate::block::Block
//! [`Body`]: crate::physics::Body
//! [`Character`]: crate::character::Character
//! [`Modifier`]: crate::block::Modifier
//! [`Primitive`]: crate::block::Primitive
//! [`Space`]: crate::space::Space
//! [`SpacePhysics`]: crate::space::SpacePhysics
//! [`Tool`]: crate::inv::Tool
//! [`Universe`]: crate::universe::Universe
#![cfg_attr(
    not(feature = "arbitrary"),
    doc = "[`arbitrary::Arbitrary`]: https://docs.rs/arbitrary/1.0.2/arbitrary/trait.Arbitrary.html"
)]
#![cfg_attr(
    not(feature = "auto-threads"),
    doc = "[`rayon`]: https://docs.rs/rayon/"
)]
#![no_std]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
// * This crate contains some unsafe code and therefore does not `forbid(unsafe_code)`.
//   All of it is located in the `universe` module and pertains to ECS extensions.
#![cfg_attr(
    not(any(test, feature = "arbitrary")),
    warn(clippy::std_instead_of_core, clippy::std_instead_of_alloc)
)]
#![cfg_attr(not(feature = "std"), allow(clippy::arc_with_non_send_sync))]
#![cfg_attr(feature = "_special_testing", allow(private_interfaces))]

#[cfg(any(feature = "std", test))]
#[cfg_attr(test, macro_use)]
extern crate std;
#[macro_use]
extern crate alloc;

#[doc(inline)]
pub use all_is_cubes_base::raycast;

pub mod behavior;
pub mod block;
#[doc(hidden)] // Exported only for use by all_is_cubes_render
pub mod camera;
pub mod character;
pub mod chunking;
#[doc(hidden)] // Exported only for use by all_is_cubes_content
pub mod content;
pub mod drawing;
pub mod fluff;
pub mod inv;
pub mod linking;
pub mod listen;
pub mod math;
pub mod op;
pub mod physics;
#[doc(hidden)]
pub mod raytracer_components;
#[doc(hidden)]
#[cfg_attr(not(feature = "rerun"), path = "rerun_glue_disabled.rs")]
pub mod rerun_glue;
pub mod save;
pub mod sound;
pub mod space;
pub mod tag;
pub mod time;
pub mod transaction;
pub mod universe;
pub mod util;

/// Re-export the version of the `arcstr` string type library we're using.
pub use arcstr;
/// Re-export the version of the `euclid` vector math library we're using.
pub use euclid;

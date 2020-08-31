// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

#![allow(clippy::collapsible_if)]

// TODO: consider exporting individual symbols instead of the modules, because
// the modules are mostly per-data-type rather than being convenient usage bundles.
// Or have modules reexport by API consumer (world-builder versus renderer etc.)

pub mod block;
pub mod blockgen;
pub mod camera;
pub mod demo_content;
pub mod drawing;
mod lighting;
pub mod math;
mod physics;
pub mod raycast;
pub mod space;
pub mod triangulator;
pub mod universe;
mod util;
pub mod worldgen;

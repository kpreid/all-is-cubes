//! Miscellanous demonstrations of capability and manual test-cases.
//! The exhibits defined in this module are combined into [`crate::demo_city()`].

use all_is_cubes::math::GridCoordinate;

mod prelude;

mod animation;
mod chunking;
mod collision;
mod color;
mod composite;
mod destruction;
mod elevator;
mod images;
mod inventory;
mod knot;
mod make_some_blocks;
mod move_modifier;
mod resolutions;
mod rotation;
mod smallest;
mod text_blocks;
mod transparency;
mod trees;
mod ui;
mod zoom;

pub(crate) const NEEDED_RADIUS: GridCoordinate = 60;

/// All exhibits which will show up in [`crate::UniverseTemplate::DemoCity`].
///
/// Ordered by distance from the center.
pub(crate) static DEMO_CITY_EXHIBITS: &[prelude::Exhibit] = &[
    elevator::ELEVATOR,
    inventory::INVENTORY,
    transparency::TRANSPARENCY_VOX,
    knot::KNOT,
    transparency::TRANSPARENCY_WHOLE_BLOCK,
    transparency::TRANSPARENCY_GLASS_AND_WATER,
    collision::COLLISION,
    resolutions::RESOLUTIONS,
    transparency::TRANSPARENCY_SHRUNKEN_BLOCK,
    move_modifier::PROJECTILE,
    animation::ANIMATION,
    make_some_blocks::MAKE_SOME_BLOCKS,
    composite::DASHED_BOXES,
    composite::COMPOSITE,
    destruction::DESTRUCTION,
    move_modifier::MOVED_BLOCKS,
    rotation::ROTATIONS,
    ui::UI_BLOCKS,
    ui::UI_PROGRESS_BAR,
    trees::TREES,
    chunking::CHUNK_CHART,
    color::COLOR_LIGHTS,
    color::COLORED_BOUNCE,
    color::SPOTLIGHT,
    images::IMAGES,
    smallest::SMALLEST,
    transparency::SWIMMING_POOL,
    color::COLORS,
    text_blocks::TEXT,
    zoom::ZOOM,
    animation::BECOME,
];

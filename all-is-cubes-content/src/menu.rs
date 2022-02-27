// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use all_is_cubes::block::{space_to_blocks, Block, BlockAttributes};
use all_is_cubes::cgmath::{EuclideanSpace, One, Vector3, Zero};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::mono_font::iso_8859_1::FONT_9X18_BOLD;
use all_is_cubes::drawing::embedded_graphics::mono_font::MonoTextStyle;
use all_is_cubes::drawing::embedded_graphics::prelude::Point;
use all_is_cubes::drawing::embedded_graphics::text::{Baseline, Text};
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{GridCoordinate, GridMatrix, GridPoint, GridVector};
use all_is_cubes::space::{Grid, Space, SpacePhysics};
use all_is_cubes::universe::Universe;
use strum::IntoEnumIterator;

use crate::{
    draw_text_in_blocks,
    logo::{logo_text, logo_text_extent},
    space_to_space_copy, UniverseTemplate,
};

pub(crate) fn template_menu(universe: &mut Universe) -> Result<Space, InGenError> {
    let template_iter = UniverseTemplate::iter().filter(UniverseTemplate::include_in_lists);

    let logo_resolution = 4;
    let logo_extent = logo_text_extent();
    let small_logo_space = {
        let mut drawing_space = Space::empty(logo_extent);
        logo_text(GridMatrix::one(), &mut drawing_space)?;
        space_to_blocks(
            6,
            BlockAttributes::default(),
            universe.insert_anonymous(drawing_space),
        )?
    };

    let button_label_width = 10;
    let logo_height = small_logo_space.grid().size().y;
    let overall_width = button_label_width.max(logo_extent.size().x / logo_resolution) + 2;

    let bounds = Grid::new(
        [-overall_width / 2, 0, -2],
        [
            overall_width,
            logo_height + template_iter.clone().count() as GridCoordinate * 2,
            4,
        ],
    );
    let mut space = Space::builder(bounds)
        .physics({
            let mut p = SpacePhysics::default();
            p.sky_color = palette::MENU_BACK.to_rgb();
            p.gravity = Vector3::zero();
            p
        })
        .spawn({
            let mut spawn = Spawn::looking_at_space(bounds, [0., 0., 1.]);
            spawn.set_inventory(vec![Tool::Activate.into()]);
            spawn
        })
        .build_empty();

    // TODO: we need a widget-layout system to use here
    let mut y_top;

    // Small logo
    // TODO: This should be simpler to set up
    // TODO: a slight truncation is happening; check bounds calculation
    {
        space_to_space_copy(
            &small_logo_space,
            small_logo_space.grid(),
            &mut space,
            GridMatrix::from_translation([
                0,
                bounds.upper_bounds().y - small_logo_space.grid().upper_bounds().y,
                0,
            ]),
        )?;
        y_top = bounds.upper_bounds().y - logo_height;
    }

    for template in template_iter {
        y_top -= 2;
        let button_lower_bounds = GridPoint::new(-button_label_width / 2, y_top, 0);
        space.fill_uniform(
            Grid::new(button_lower_bounds, [button_label_width, 1, 1]),
            Block::builder()
                .display_name(template.to_string())
                .color(palette::MENU_FRAME)
                .build(),
        )?;
        // TODO: draw_text_in_blocks needs justification options
        draw_text_in_blocks(
            universe,
            &mut space,
            20,
            button_label_width,
            GridMatrix::from_translation(button_lower_bounds.to_vec() + GridVector::new(0, 0, 1)),
            &Text::with_baseline(
                &template.to_string(),
                Point::new(0, 0),
                MonoTextStyle::new(&FONT_9X18_BOLD, palette::ALMOST_BLACK),
                Baseline::Bottom,
            ),
        )?;
    }

    Ok(space)
}

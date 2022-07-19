// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::sync::Arc;

use strum::IntoEnumIterator;

use all_is_cubes::{
    block::{Block, BlockAttributes},
    cgmath::{Vector3, Zero as _},
    character::Spawn,
    content::palette,
    drawing::{
        draw_to_blocks,
        embedded_graphics::{
            mono_font::{iso_8859_1::FONT_9X18_BOLD, MonoTextStyle},
            prelude::Point,
            text::{Baseline, Text},
        },
    },
    inv::Tool,
    linking::InGenError,
    math::{Face6, GridAab, GridMatrix, GridVector},
    space::{Space, SpacePhysics, SpaceTransaction},
    transaction::{Merge, Transaction as _},
    universe::Universe,
    vui::{
        self, widgets::FrameWidget, LayoutGrant, LayoutRequest, LayoutTree, Layoutable, Widget,
        WidgetController,
    },
};

use crate::{logo::LogoTextLarge, UniverseTemplate};

#[derive(Debug)]
struct TemplateButtonWidget {
    // template: UniverseTemplate,
    background_block: Block,
    /// TODO: Instead of pregenerated text we should be able to synthesize it as-needed,
    /// but that requires new facilities in the Universe (insert transactions and GC)
    text_blocks: Space,
}

impl TemplateButtonWidget {
    fn new(universe: &mut Universe, template: UniverseTemplate) -> Result<Self, InGenError> {
        let background_block = Block::builder()
            .display_name(template.to_string())
            .color(palette::MENU_FRAME)
            .build();

        // TODO: We should be doing this when the widget is instantiated, not now, but that's not yet possible.
        let text_blocks = draw_to_blocks(
            universe,
            16,
            0,
            0..1,
            BlockAttributes::default(),
            &Text::with_baseline(
                &template.to_string(),
                Point::new(0, 0),
                MonoTextStyle::new(&FONT_9X18_BOLD, palette::ALMOST_BLACK),
                Baseline::Bottom,
            ),
        )?;

        Ok(Self {
            // template,
            background_block,
            text_blocks,
        })
    }
}

impl vui::Layoutable for TemplateButtonWidget {
    fn requirements(&self) -> vui::LayoutRequest {
        LayoutRequest {
            minimum: GridVector::new(10, 1, 2),
        }
    }
}
impl vui::Widget for TemplateButtonWidget {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn WidgetController> {
        Box::new(TemplateButtonController {
            definition: self,
            position: *position,
        })
    }
}

#[derive(Debug)]
struct TemplateButtonController {
    definition: Arc<TemplateButtonWidget>,
    position: LayoutGrant,
}
impl vui::WidgetController for TemplateButtonController {
    fn initialize(&mut self) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let mut txn = SpaceTransaction::default();

        // TODO: propagate error
        let bounds = &mut self.position.bounds;
        let background_bounds = bounds.abut(Face6::NZ, -1).unwrap();
        let text_bounds = bounds.abut(Face6::PZ, -1).unwrap();

        // Fill background
        // TODO: give SpaceTransaction a fill_uniform() analogue
        let background_block = &self.definition.background_block;
        for cube in background_bounds.interior_iter() {
            txn.set(cube, None, Some(background_block.clone())).unwrap();
        }

        // Fill text
        let text_dest_origin = GridVector::new(
            bounds.lower_bounds().x,
            bounds.lower_bounds().y,
            bounds.upper_bounds().z - 1,
        );
        txn = txn
            .merge(crate::space_to_transaction_copy(
                &self.definition.text_blocks,
                text_bounds.translate(-text_dest_origin),
                GridMatrix::from_translation(text_dest_origin),
            ))
            .unwrap();

        Ok(txn)
    }
}

pub(crate) fn template_menu(universe: &mut Universe) -> Result<Space, InGenError> {
    let template_iter = UniverseTemplate::iter().filter(UniverseTemplate::include_in_lists);

    let mut vertical_widgets: Vec<Arc<LayoutTree<Arc<dyn Widget>>>> = Vec::with_capacity(10);
    vertical_widgets.push(LayoutTree::leaf(Arc::new(LogoTextLarge)));
    for template in template_iter {
        vertical_widgets.push(LayoutTree::spacer(LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }));
        vertical_widgets.push(LayoutTree::leaf(Arc::new(TemplateButtonWidget::new(
            universe, template,
        )?)));
    }
    let tree: LayoutTree<Arc<dyn Widget>> = LayoutTree::Stack {
        direction: Face6::PZ,
        children: vec![
            LayoutTree::leaf(FrameWidget::new()),
            Arc::new(LayoutTree::Stack {
                direction: Face6::NY,
                children: vertical_widgets,
            }),
        ],
    };

    let size = tree.requirements().minimum;
    let bounds = GridAab::new([0, 0, 0], size);

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

    // TODO: These errors ought to autoconvert into InGenError
    tree.perform_layout(LayoutGrant::new(bounds))
        .unwrap()
        .installation()
        .map_err(|e| InGenError::Other(e.into()))?
        .execute(&mut space)
        .map_err(InGenError::Transaction)?; // TODO: shouldn't need to convert

    Ok(space)
}

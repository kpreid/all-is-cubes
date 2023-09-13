use std::sync::Arc;

use strum::IntoEnumIterator;

use all_is_cubes::{
    block::{
        Block, BlockAttributes,
        Resolution::{self, R16},
    },
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
    euclid::Vector3D,
    inv::Tool,
    linking::InGenError,
    math::{Face6, GridAab, GridVector, Gridgid},
    space::{Space, SpaceBuilder, SpacePhysics, SpaceTransaction},
    transaction::{self, Merge, Transaction as _},
    universe::Universe,
};
use all_is_cubes_ui::logo::logo_text;
use all_is_cubes_ui::vui::{
    self, install_widgets, widgets, Align, LayoutGrant, LayoutRequest, LayoutTree, Layoutable,
    WidgetController,
};

use crate::UniverseTemplate;

#[derive(Debug)]
struct TemplateButton {
    // template: UniverseTemplate,
    background_block: Block,
    /// TODO: Instead of pregenerated text we should be able to synthesize it as-needed,
    /// but that requires new facilities in the Universe (insert transactions and GC)
    text_blocks: Space,
}

impl TemplateButton {
    fn new(universe: &mut Universe, template: UniverseTemplate) -> Result<Self, InGenError> {
        let background_block = Block::builder()
            .display_name(template.to_string())
            .color(palette::MENU_FRAME)
            .build();

        // TODO: We should be doing this when the widget is instantiated, not now, but that's not yet possible.
        let text_blocks = draw_to_blocks(
            universe,
            R16,
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

impl vui::Layoutable for TemplateButton {
    fn requirements(&self) -> vui::LayoutRequest {
        LayoutRequest {
            minimum: GridVector::new(10, 1, 2),
        }
    }
}
impl vui::Widget for TemplateButton {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn WidgetController> {
        Box::new(TemplateButtonController {
            definition: self,
            position: *position,
        })
    }
}

#[derive(Debug)]
struct TemplateButtonController {
    definition: Arc<TemplateButton>,
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
                Gridgid::from_translation(text_dest_origin),
            ))
            .unwrap();

        Ok(txn)
    }
}

pub(crate) fn template_menu(universe: &mut Universe) -> Result<Space, InGenError> {
    let template_iter = UniverseTemplate::iter().filter(UniverseTemplate::include_in_lists);

    let logo_text_space = LayoutTree::leaf(logo_text()).to_space(
        SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
        Vector3D::new(Align::Center, Align::Center, Align::Low),
    )?;
    let logo_widget = widgets::Voxels::new(
        logo_text_space.bounds(),
        universe.insert_anonymous(logo_text_space),
        Resolution::R4,
        BlockAttributes::default(),
    );

    let mut vertical_widgets: Vec<vui::WidgetTree> = Vec::with_capacity(10);
    vertical_widgets.push(LayoutTree::leaf(Arc::new(logo_widget)));
    for template in template_iter {
        vertical_widgets.push(LayoutTree::spacer(LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }));
        vertical_widgets.push(LayoutTree::leaf(Arc::new(TemplateButton::new(
            universe, template,
        )?)));
    }
    let tree: vui::WidgetTree = Arc::new(LayoutTree::Stack {
        direction: Face6::PZ,
        children: vec![
            LayoutTree::leaf({
                // TODO: this should be the 'dialog box' background but we don't have access to that from this crate
                let background = Block::from(palette::MENU_BACK);
                let frame = Block::from(palette::MENU_FRAME);
                widgets::Frame::new(widgets::BoxStyle::from_geometric_categories(
                    None,
                    Some(background),
                    Some(frame.clone()),
                    Some(frame),
                ))
            }),
            Arc::new(LayoutTree::Stack {
                direction: Face6::NY,
                children: vertical_widgets,
            }),
        ],
    });

    let size = tree.requirements().minimum;
    let bounds = GridAab::from_lower_size([0, 0, 0], size);

    // TODO: can't use LayoutTree::to_space here because we want to use the bounds for looking_at_space
    let mut space = Space::builder(bounds)
        .physics({
            let mut p = SpacePhysics::default();
            p.sky_color = palette::MENU_BACK.to_rgb();
            p.gravity = Vector3D::zero();
            p
        })
        .spawn({
            let mut spawn = Spawn::looking_at_space(bounds, [0., 0., 1.]);
            spawn.set_inventory(vec![Tool::Activate.into()]);
            spawn
        })
        .build();

    install_widgets(LayoutGrant::new(bounds), &tree)?
        .execute(&mut space, &mut transaction::no_outputs)?;

    Ok(space)
}

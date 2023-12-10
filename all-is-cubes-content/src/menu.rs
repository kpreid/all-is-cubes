use all_is_cubes::arcstr;
use all_is_cubes::block::{text, Primitive};
use all_is_cubes::space::CubeTransaction;
use alloc::string::ToString as _;
use alloc::sync::Arc;
use alloc::{boxed::Box, vec::Vec};

use strum::IntoEnumIterator;

use all_is_cubes::{
    block::{
        Block, BlockAttributes,
        Resolution::{self},
    },
    character::Spawn,
    content::palette,
    euclid::Vector3D,
    inv::Tool,
    linking::InGenError,
    math::{Face6, GridAab, GridVector},
    space::{Space, SpaceBuilder, SpacePhysics, SpaceTransaction},
    transaction::{self, Merge, Transaction as _},
    universe::Universe,
};
use all_is_cubes_ui::logo::logo_text;
use all_is_cubes_ui::vui::{self, install_widgets, widgets, Align, LayoutTree, Layoutable as _};

use crate::UniverseTemplate;

#[derive(Debug)]
struct TemplateButton {
    // template: UniverseTemplate,
    background_block: Block,
    text: text::Text,
}

impl TemplateButton {
    fn new(_universe: &mut Universe, template: UniverseTemplate) -> Result<Self, InGenError> {
        let background_block = Block::builder()
            .display_name(template.to_string())
            .color(palette::MENU_FRAME)
            .build();

        let text = text::Text::builder()
            .string(arcstr::format!("{template}"))
            .positioning(text::Positioning {
                x: text::PositioningX::Left,
                line_y: text::PositioningY::BodyMiddle,
                z: text::PositioningZ::Back,
            })
            .build();

        Ok(Self {
            // template,
            background_block,
            text,
        })
    }
}

impl vui::Layoutable for TemplateButton {
    fn requirements(&self) -> vui::LayoutRequest {
        vui::LayoutRequest {
            minimum: GridVector::new(10, 1, 2),
        }
    }
}
impl vui::Widget for TemplateButton {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(TemplateButtonController {
            definition: self,
            position: *position,
        })
    }
}

#[derive(Debug)]
struct TemplateButtonController {
    definition: Arc<TemplateButton>,
    position: vui::LayoutGrant,
}
impl vui::WidgetController for TemplateButtonController {
    fn initialize(&mut self) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let text = &self.definition.text;
        // TODO: propagate error
        let bounds = &mut self.position.bounds;
        let background_bounds = bounds.abut(Face6::NZ, -1).unwrap();
        let text_bounds = bounds.abut(Face6::PZ, -1).unwrap();

        // Fill background
        let background_block = &self.definition.background_block;
        let mut txn = SpaceTransaction::filling(background_bounds, |_| {
            CubeTransaction::replacing(None, Some(background_block.clone()))
        });

        // Fill text
        txn.merge_from(SpaceTransaction::filling(text_bounds, |cube| {
            CubeTransaction::replacing(
                None,
                Some(Block::from_primitive(Primitive::Text {
                    text: text.clone(),
                    offset: cube.lower_bounds() - text_bounds.lower_bounds(),
                })),
            )
        }))
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
        vertical_widgets.push(LayoutTree::spacer(vui::LayoutRequest {
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

    install_widgets(vui::LayoutGrant::new(bounds), &tree)?
        .execute(&mut space, &mut transaction::no_outputs)?;

    Ok(space)
}

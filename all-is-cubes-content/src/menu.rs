use alloc::sync::Arc;
use alloc::vec::Vec;

use strum::IntoEnumIterator;

use all_is_cubes::arcstr;
use all_is_cubes::block::{BlockAttributes, Resolution};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::palette;
use all_is_cubes::euclid::Vector3D;
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{Face6, GridAab};
use all_is_cubes::space::{Space, SpaceBuilder, SpacePhysics};
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;
use all_is_cubes_ui::logo::logo_text;
use all_is_cubes_ui::vui::{self, install_widgets, widgets, Align, LayoutTree, Layoutable as _};

use crate::UniverseTemplate;

pub(crate) async fn template_menu(
    universe: &mut Universe,
    mut p: YieldProgress,
) -> Result<Space, InGenError> {
    let mut install_txn = UniverseTransaction::default();
    let widget_theme_progress = p.start_and_cut(0.05, "WidgetTheme").await;
    let widget_theme = widgets::WidgetTheme::new(&mut install_txn, widget_theme_progress).await?;
    install_txn.execute(universe, &mut transaction::no_outputs)?;

    let template_iter = UniverseTemplate::iter().filter(UniverseTemplate::include_in_lists);

    let logo_text_space = vui::leaf_widget(logo_text()).to_space(
        SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
        Vector3D::new(Align::Center, Align::Center, Align::Low),
    )?;
    let logo_widget = widgets::Voxels::new(
        logo_text_space.bounds(),
        universe.insert_anonymous(logo_text_space),
        Resolution::R8,
        BlockAttributes::default(),
    );

    let mut vertical_widgets: Vec<vui::WidgetTree> = Vec::with_capacity(10);
    vertical_widgets.push(vui::leaf_widget(logo_widget));
    for template in template_iter {
        vertical_widgets.push(vui::leaf_widget(widgets::ActionButton::new(
            arcstr::format!("{template}"),
            &widget_theme,
            move || {
                // todo!("actually load template")
            },
        )));
    }
    let tree: vui::WidgetTree =
        widget_theme
            .dialog_background()
            .as_background_of(Arc::new(LayoutTree::Stack {
                direction: Face6::NY,
                children: vertical_widgets,
            }));

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

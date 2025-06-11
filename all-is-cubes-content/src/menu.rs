use alloc::sync::Arc;
use alloc::vec::Vec;

use strum::IntoEnumIterator;

use all_is_cubes::arcstr;
use all_is_cubes::block::Resolution;
use all_is_cubes::character::Spawn;
use all_is_cubes::content::palette;
use all_is_cubes::euclid::Vector3D;
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{Face6, GridAab};
use all_is_cubes::space::{self, Space};
use all_is_cubes::transaction::{self, Merge, Transaction as _};
use all_is_cubes::universe::{Handle, Name, ReadTicket, Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;
use all_is_cubes_ui::logo::logo_text;
use all_is_cubes_ui::vui::{self, Align, LayoutTree, Layoutable as _, install_widgets, widgets};

use crate::UniverseTemplate;

#[doc(hidden)] // TODO: this being public is an awkward kludge for hooking up the buttons. Instead, `TemplateParameters` should have an extension mechanism by which we can pass the button hook.
pub async fn template_menu_space(
    universe: &mut Universe,
    mut progress: YieldProgress,
    action: Arc<dyn Fn(&UniverseTemplate) + Send + Sync>,
) -> Result<Space, InGenError> {
    let tree = {
        let mut install_txn = UniverseTransaction::default();

        let widget_theme_progress = progress.start_and_cut(0.05, "WidgetTheme").await;
        let widget_theme = widgets::WidgetTheme::new(
            universe.read_ticket(),
            &mut install_txn,
            widget_theme_progress,
        )
        .await?;

        // doesn't actually do anything (yet)
        let (tree, widget_txn) = template_menu_widget_tree(&widget_theme, action)?;
        install_txn.merge_from(widget_txn).unwrap();

        install_txn.execute(universe, (), &mut transaction::no_outputs)?;
        tree
    };

    let size = tree.requirements().minimum;
    let bounds = GridAab::from_lower_size([0, 0, 0], size);

    // TODO: can't use LayoutTree::to_space here because we want to use the bounds for looking_at_space
    let mut space = Space::builder(bounds)
        .physics({
            let mut p = space::SpacePhysics::default();
            p.sky = space::Sky::Uniform(palette::MENU_BACK.to_rgb());
            p.gravity = Vector3D::zero();
            p
        })
        .spawn({
            let mut spawn = Spawn::looking_at_space(bounds, [0., 0., 1.]);
            spawn.set_inventory(vec![Tool::Activate.into()]);
            spawn
        })
        .build();

    install_widgets(vui::LayoutGrant::new(bounds), &tree, universe.read_ticket())?.execute(
        &mut space,
        universe.read_ticket(),
        &mut transaction::no_outputs,
    )?;

    Ok(space)
}

#[expect(clippy::needless_pass_by_value)]
fn template_menu_widget_tree(
    widget_theme: &widgets::WidgetTheme,
    action: Arc<dyn Fn(&UniverseTemplate) + Send + Sync>,
) -> Result<(vui::WidgetTree, UniverseTransaction), InGenError> {
    let template_iter = UniverseTemplate::iter().filter(UniverseTemplate::include_in_lists);

    // TODO: the scaled-down logo should exist as a widget itself
    let logo_text_space = vui::leaf_widget(logo_text()).to_space(
        ReadTicket::stub(),
        space::Builder::default().physics(space::SpacePhysics::DEFAULT_FOR_BLOCK),
        Vector3D::new(Align::Center, Align::Center, Align::Low),
    )?;
    let logo_text_bounds = logo_text_space.bounds();
    let logo_text_space_handle = Handle::new_pending(Name::Pending, logo_text_space);
    let logo_widget = widgets::Voxels::new(
        logo_text_bounds,
        logo_text_space_handle.clone().into(),
        Resolution::R8,
        [],
    );

    let mut vertical_widgets: Vec<vui::WidgetTree> = Vec::with_capacity(10);
    vertical_widgets.push(vui::leaf_widget(logo_widget));
    for template in template_iter {
        let action = action.clone();
        vertical_widgets.push(vui::leaf_widget(widgets::ActionButton::new(
            arcstr::format!("{template}"),
            widget_theme,
            move || action(&template),
        )));
    }
    let tree: vui::WidgetTree =
        widget_theme
            .dialog_background()
            .as_background_of(Arc::new(LayoutTree::Stack {
                direction: Face6::NY,
                children: vertical_widgets,
            }));
    Ok((tree, UniverseTransaction::insert(logo_text_space_handle)))
}

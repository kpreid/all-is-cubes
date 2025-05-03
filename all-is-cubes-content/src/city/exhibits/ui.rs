use all_is_cubes::inv;
use all_is_cubes_ui::vui;
use all_is_cubes_ui::vui::blocks::UiBlocks;
use all_is_cubes_ui::vui::widgets::{self, ToolbarButtonState, WidgetBlocks};

use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "UI Blocks",
    subtitle:
        "Blocks from the UI system (inactive)",
    placement: Placement::Surface,
)]
fn UI_BLOCKS(ctx: Context<'_>) {
    // TODO: This was designed for a render test and is still shaped for that rather than
    // any-viewpoint examination.

    let icons = BlockProvider::<inv::Icons>::using(ctx.universe)?;
    let icons = icons.iter().map(|(_, block)| block.clone());

    let widget_blocks = BlockProvider::<WidgetBlocks>::using(ctx.universe)?;
    let widget_blocks = widget_blocks
        .iter()
        .filter(|&(key, _)| match key {
            // Filter out large number of pointer blocks
            WidgetBlocks::ToolbarPointer(
                [
                    ToolbarButtonState::Unmapped,
                    ToolbarButtonState::Mapped,
                    ToolbarButtonState::Pressed,
                ],
            ) => true,
            WidgetBlocks::ToolbarPointer(_) => false,
            _ => true,
        })
        .map(|(_, block)| block.clone());

    let ui_blocks = BlockProvider::<UiBlocks>::using(ctx.universe)?;
    let ui_blocks = ui_blocks.iter().map(|(_, block)| block.clone());

    let all_blocks: Vec<Block> = icons.chain(widget_blocks).chain(ui_blocks).collect();

    // Compute layout
    let count = all_blocks.len() as GridCoordinate;
    let row_length = 4;
    let bounds = GridAab::from_lower_upper(
        [0, 0, 0],
        [row_length, ((count + row_length - 1) / row_length), 2],
    );

    // Fill space with blocks
    let space = Space::builder(bounds)
        .spawn_position(Point3D::new(
            FreeCoordinate::from(bounds.size().width) / 2.,
            FreeCoordinate::from(bounds.size().height) / 2.,
            FreeCoordinate::from(bounds.size().height) * 1.5,
        ))
        .read_ticket(ctx.universe.read_ticket())
        .build_and_mutate(|m| {
            for (index, block) in (0i32..).zip(all_blocks) {
                m.set(
                    [
                        index.rem_euclid(row_length),
                        index.div_euclid(row_length),
                        0,
                    ],
                    block,
                )
                .unwrap();
            }
            Ok(())
        })?;

    Ok((space, ExhibitTransaction::default()))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "UI: Progress Bar",
    subtitle: "",
    placement: Placement::Surface,
)]
fn UI_PROGRESS_BAR(ctx: Context<'_>) {
    let pb = |fraction: f64| -> vui::WidgetTree {
        vui::LayoutTree::leaf(widgets::ProgressBar::new(
            ctx.widget_theme,
            Face6::PX,
            listen::constant(widgets::ProgressBarState::new(fraction)),
        ))
    };

    let tree: vui::WidgetTree = Arc::new(vui::LayoutTree::Stack {
        direction: Face6::PY,
        children: vec![
            Arc::new(vui::LayoutTree::Spacer(vui::LayoutRequest {
                minimum: size3(2, 0, 1),
            })),
            pb(0.00),
            pb(0.25),
            pb(0.50),
            pb(0.75),
            pb(1.00),
        ],
    });

    let space = tree.to_space(
        space::Builder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
        vui::Gravity::new(vui::Align::Center, vui::Align::Low, vui::Align::Center),
    )?;

    Ok((space, ExhibitTransaction::default()))
}

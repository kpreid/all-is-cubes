use std::sync::{mpsc, Arc, Mutex};

use cgmath::Vector2;
use embedded_graphics::geometry::Point;
use embedded_graphics::prelude::{Primitive as _, Transform as _};
use embedded_graphics::primitives::{Circle, PrimitiveStyleBuilder};
use embedded_graphics::Drawable as _;

use crate::apps::ControlMessage;
use crate::block::{space_to_blocks, Block, BlockAttributes, Resolution, AIR};
use crate::camera::{GraphicsOptions, Viewport};
use crate::character::Character;
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::linking::BlockProvider;
use crate::listen::ListenableSource;
use crate::math::{Face6, FreeCoordinate, GridAab, GridCoordinate, GridMatrix, GridRotation, Rgba};
use crate::space::{Space, SpacePhysics};
use crate::universe::{URef, Universe};
use crate::util::YieldProgress;
use crate::vui::options::{graphics_options_widgets, pause_toggle_button};
use crate::vui::widgets::{
    Crosshair, FrameWidget, ToggleButtonWidget, Toolbar, TooltipState, TooltipWidget,
};
use crate::vui::{CueNotifier, Icons, LayoutTree, UiBlocks, Widget, WidgetTree};

pub(crate) use embedded_graphics::mono_font::iso_8859_1::FONT_8X13_BOLD as HudFont;

/// Knows where and how to place graphics within the HUD space, but does not store
/// the space or any related state itself; depends only on the screen size and other
/// parameters not primarily dependent on user interaction. This split is intended to
/// simplify the problem of adapting to size changes.
///
/// TODO: Since introducing widgets, `HudLayout` does much less work. Think about whether it should exist.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct HudLayout {
    size: Vector2<GridCoordinate>,
    pub(crate) toolbar_positions: usize,
}

impl HudLayout {
    pub(crate) const DEPTH_BEHIND_VIEW_PLANE: GridCoordinate = 5;

    /// Construct `HudLayout` with a size that suits the given viewport
    /// (based on pixel resolution and aspect ratio)
    pub fn new(viewport: Viewport) -> Self {
        // Note: Dimensions are enforced to be odd so that the crosshair can work.
        // The toolbar is also designed to be odd width when it has an even number of positions.
        let width = 25;
        // we want to ceil() the height because the camera setup makes the height match
        // the viewport and ignores width, so we want to prefer too-narrow over too-wide
        let height = ((FreeCoordinate::from(width) / viewport.nominal_aspect_ratio()).ceil()
            as GridCoordinate)
            .max(8);
        let height = height / 2 * 2 + 1; // ensure odd
        Self {
            size: Vector2::new(width, height),
            toolbar_positions: 10,
        }
    }

    pub(crate) fn bounds(&self) -> GridAab {
        GridAab::from_lower_upper(
            (0, 0, -Self::DEPTH_BEHIND_VIEW_PLANE),
            (self.size.x, self.size.y, 5),
        )
    }

    /// Create a new space with the size controlled by this layout,
    /// and a standard lighting condition.
    // TODO: validate this doesn't crash on wonky sizes.
    pub(crate) fn new_space(&self) -> Space {
        let Vector2 { x: w, y: h } = self.size;
        let bounds = self.bounds();
        let mut space = Space::builder(bounds)
            .physics(SpacePhysics {
                sky_color: palette::HUD_SKY,
                ..SpacePhysics::default()
            })
            .build();

        if false {
            // Visualization of the bounds of the space we're drawing.
            let mut add_frame = |z, color| {
                let frame_block = Block::from(color);
                space
                    .fill_uniform(GridAab::from_lower_size([0, 0, z], [w, h, 1]), frame_block)
                    .unwrap();
                space
                    .fill_uniform(GridAab::from_lower_size([1, 1, z], [w - 2, h - 2, 1]), &AIR)
                    .unwrap();
            };
            add_frame(bounds.lower_bounds().z, Rgba::new(0.5, 0., 0., 1.));
            add_frame(-1, Rgba::new(0.5, 0.5, 0.5, 1.));
            add_frame(bounds.upper_bounds().z - 1, Rgba::new(0., 1., 1., 1.));
        }

        space
    }
}

/// Ad-hoc bundle of elements needed to construct HUD UI widgets.
///
/// TODO: Still looking for the right general abstraction here...
#[derive(Debug)]
pub(crate) struct HudInputs {
    pub hud_blocks: Arc<HudBlocks>,
    pub control_channel: mpsc::SyncSender<ControlMessage>,
    pub cue_channel: CueNotifier,
    pub graphics_options: ListenableSource<GraphicsOptions>,
    pub paused: ListenableSource<bool>,
    pub mouselook_mode: ListenableSource<bool>,
}

#[allow(clippy::too_many_arguments, clippy::redundant_clone)]
pub(super) fn new_hud_widget_tree(
    // TODO: terrible mess of tightly coupled parameters
    character_source: ListenableSource<Option<URef<Character>>>,
    hud_inputs: &HudInputs,
    hud_layout: &HudLayout,
    // TODO: stop mutating the universe in widget construction
    universe: &mut Universe,
    tooltip_state: Arc<Mutex<TooltipState>>,
) -> WidgetTree {
    // Miscellaneous control widgets drawn in the corner
    let control_bar_widgets: WidgetTree = Arc::new(LayoutTree::Stack {
        direction: Face6::NX,
        children: vec![
            Arc::new(LayoutTree::Stack {
                direction: Face6::NX,
                children: graphics_options_widgets(hud_inputs),
            }),
            LayoutTree::leaf(pause_toggle_button(hud_inputs)),
            LayoutTree::leaf(ToggleButtonWidget::new(
                hud_inputs.mouselook_mode.clone(),
                |&value| value,
                |state| hud_inputs.hud_blocks.blocks[UiBlocks::MouselookButton(state)].clone(),
                {
                    let cc = hud_inputs.control_channel.clone();
                    move || {
                        let _ignore_errors = cc.send(ControlMessage::ToggleMouselook);
                    }
                },
            )),
        ],
    });
    let control_bar_positioning: WidgetTree = if false {
        // reveal the bounds by adding a FrameWidget
        Arc::new(LayoutTree::Stack {
            direction: Face6::PZ,
            children: vec![LayoutTree::leaf(FrameWidget::new()), control_bar_widgets],
        })
    } else {
        control_bar_widgets
    };
    let toolbar: Arc<dyn Widget> = Toolbar::new(
        character_source,
        Arc::clone(&hud_inputs.hud_blocks),
        hud_layout.toolbar_positions,
        universe,
        hud_inputs.cue_channel.clone(),
    );
    let tooltip: Arc<dyn Widget> = TooltipWidget::new(
        Arc::clone(&tooltip_state),
        hud_inputs.hud_blocks.clone(),
        universe,
    );
    let hud_widget_tree: WidgetTree = Arc::new(LayoutTree::Hud {
        crosshair: LayoutTree::leaf(Crosshair::new(
            hud_inputs.hud_blocks.blocks[UiBlocks::Crosshair].clone(),
            hud_inputs.mouselook_mode.clone(),
        )),
        toolbar: Arc::new(LayoutTree::Stack {
            direction: Face6::PY,
            children: vec![LayoutTree::leaf(toolbar), LayoutTree::leaf(tooltip)],
        }),
        control_bar: control_bar_positioning,
    });
    hud_widget_tree
}

// TODO: Unclear if HudBlocks should exist; maybe it should be reworked into a BlockProvider for widget graphics instead.
#[derive(Debug, Clone)]
pub(crate) struct HudBlocks {
    pub(crate) blocks: BlockProvider<UiBlocks>,
    pub(crate) icons: BlockProvider<Icons>,
    pub(crate) text: VoxelBrush<'static>,
    pub(crate) toolbar_left_cap: VoxelBrush<'static>,
    pub(crate) toolbar_right_cap: VoxelBrush<'static>,
    pub(crate) toolbar_divider: VoxelBrush<'static>,
    pub(crate) toolbar_middle: VoxelBrush<'static>,
}

impl HudBlocks {
    pub(crate) async fn new(
        universe: &mut Universe,
        p: YieldProgress,
        resolution: Resolution,
    ) -> Self {
        let resolution_g = GridCoordinate::from(resolution);

        let [p1, p2] = p.split(0.5);
        let ui_blocks = UiBlocks::new(universe, p1).await.install(universe).unwrap();
        let icons = Icons::new(universe, p2).await.install(universe).unwrap();

        let text_brush = VoxelBrush::new::<_, Block>([
            ([0, 0, 1], palette::HUD_TEXT_FILL.into()),
            ([1, 0, 0], palette::HUD_TEXT_STROKE.into()),
            ([-1, 0, 0], palette::HUD_TEXT_STROKE.into()),
            ([0, 1, 0], palette::HUD_TEXT_STROKE.into()),
            ([0, -1, 0], palette::HUD_TEXT_STROKE.into()),
        ]);

        // TODO: This toolbar graphic is a "get the bugs in the drawing tools worked out"
        // placeholder for better art...
        // The frame is drawn multiple times, with different copies having different status
        // indicators present, so that we can have the graphics not rigidly aligned to blocks.
        let frame_count = 4;
        let frame_spacing_blocks = 2;

        let toolbar_frame_block_bounds =
            GridAab::from_lower_size([-1, -1, -1], [1 + frame_count * frame_spacing_blocks, 3, 3]);
        let toolbar_frame_voxel_bounds = toolbar_frame_block_bounds.multiply(resolution_g);
        let mut toolbar_drawing_space = Space::builder(toolbar_frame_voxel_bounds)
            .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
            .build();

        // Draw background for icons to “rest on”
        let horizontal_drawing = &mut toolbar_drawing_space.draw_target(
            GridMatrix::from_translation([0, -2, 0]) * GridRotation::RXZY.to_rotation_matrix(),
        );
        let padding = 3;
        let stroke_width = 1;
        let background_fill = VoxelBrush::single(palette::HUD_TOOLBAR_BACK);
        let background_stroke = VoxelBrush::single(palette::HUD_TOOLBAR_FRAME);
        let icon_background_rectangle = Circle::new(
            // TODO: confirm these offsets are exactly right
            Point::new(-padding, -padding),
            (resolution_g + padding * 2) as u32,
        )
        .into_styled(
            PrimitiveStyleBuilder::new()
                .fill_color(&background_fill)
                .stroke_color(&background_stroke)
                .stroke_width(stroke_width as u32)
                .build(),
        );
        for i in 0..frame_count {
            icon_background_rectangle
                .translate(Point::new(resolution_g * frame_spacing_blocks * i, 0))
                .draw(horizontal_drawing)
                .unwrap();
        }

        // TODO: use a name for the space
        let toolbar_blocks_space = space_to_blocks(
            resolution,
            BlockAttributes::default(),
            universe.insert_anonymous(toolbar_drawing_space),
        )
        .unwrap();

        // TODO: Make this a feature of VoxelBrush?
        let slice_drawing = |points: GridAab| {
            VoxelBrush::new(
                points
                    .interior_iter()
                    .map(|p| (p, toolbar_blocks_space[p].clone())),
            )
        };

        Self {
            blocks: ui_blocks,
            icons,
            text: text_brush,
            toolbar_middle: slice_drawing(GridAab::from_lower_upper((0, -1, -1), (1, 2, 2))),
            toolbar_divider: slice_drawing(GridAab::from_lower_upper((1, -1, -1), (2, 2, 2)))
                .translate((-1, 0, 0)),
            toolbar_left_cap: slice_drawing(GridAab::from_lower_upper((-1, -1, -1), (0, 2, 2)))
                .translate((1, 0, 0)),
            // Right cap comes from the right end of the frames
            toolbar_right_cap: slice_drawing(
                GridAab::from_lower_upper((1, -1, -1), (2, 2, 2)).translate([
                    frame_spacing_blocks * (frame_count - 1),
                    0,
                    0,
                ]),
            )
            .translate([-(frame_spacing_blocks * (frame_count - 1) + 1), 0, 0]),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hud_layout_sizes() {
        let cases: Vec<([u32; 2], [i32; 2])> =
            vec![([800, 600], [25, 19]), ([1000, 600], [25, 15])];
        let mut failed = 0;
        for (nominal_viewport, expected_size) in cases {
            let actual_size =
                HudLayout::new(Viewport::with_scale(1.0, nominal_viewport.into())).size;
            let actual_size: [i32; 2] = actual_size.into();
            if actual_size != expected_size {
                println!("{nominal_viewport:?} expected to produce {expected_size:?}; got {actual_size:?}");
                failed += 1;
            }
        }
        if failed > 0 {
            panic!("{failed} cases failed");
        }
    }
}

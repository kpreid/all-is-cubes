use alloc::sync::Arc;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::block::AIR;
use all_is_cubes::block::{Block, BlockAttributes, Resolution};
use all_is_cubes::camera;
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::{mono_font::iso_8859_1 as font, text::TextStyle};
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::euclid::{vec2, Vector2D};
use all_is_cubes::math::{Cube, Face6, FreeCoordinate, GridAab, GridCoordinate, GridVector, Rgba};
use all_is_cubes::space::{Space, SpaceBuilder, SpacePhysics};
use all_is_cubes::time;
use all_is_cubes::transaction;
use all_is_cubes::universe::{URef, Universe};

use crate::vui::{
    install_widgets, widgets, Align, Gravity, InstallVuiError, LayoutGrant, LayoutRequest,
    LayoutTree, Widget, WidgetTree,
};

/// Bounds for UI display; a choice of scale and aspect ratio based on the viewport size
/// and aspect ratio (and maybe in the future, preferences).
///
///
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct UiSize {
    /// Two-dimensional size; individual pages may have their own choices of depth.
    size: Vector2D<GridCoordinate, Cube>,
}

impl UiSize {
    pub(crate) const DEPTH_BEHIND_VIEW_PLANE: GridCoordinate = 5;

    /// Construct [`UiSize`] that suits the given viewport
    /// (based on pixel resolution and aspect ratio).
    pub fn new(viewport: camera::Viewport) -> Self {
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
            size: vec2(width, height),
        }
    }

    /// TODO: depth should be up to the choice of the individual pages.
    pub(crate) fn space_bounds(&self) -> GridAab {
        GridAab::from_lower_upper(
            [0, 0, -Self::DEPTH_BEHIND_VIEW_PLANE],
            [self.size.x, self.size.y, 5],
        )
    }

    /// Create a new space with the specified bounds and a standard lighting condition for UI.
    // TODO: validate this doesn't crash on wonky sizes.
    pub(crate) fn create_space(self) -> Space {
        let bounds = self.space_bounds();
        let Vector2D { x: w, y: h, .. } = self.size;
        let mut space = Space::builder(bounds)
            .physics({
                let mut physics = SpacePhysics::default();
                physics.sky_color = palette::HUD_SKY;
                physics
            })
            .build();

        if false {
            // Visualization of the bounds of the space we're drawing.
            // TODO: Use `BoxStyle` to draw this instead
            let mut add_frame = |z, color| {
                let frame_block = Block::from(color);
                space
                    .fill_uniform(GridAab::from_lower_size([0, 0, z], [w, h, 1]), &frame_block)
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

/// Pair of a widget tree and a cached space it is instantiated in with a particular size,
/// which can be recreated with a different size as needed.
///
/// TODO: Give this a better name.
#[derive(Clone, Debug)]
pub(crate) struct PageInst {
    tree: WidgetTree,
    space: Option<URef<Space>>,
}

impl PageInst {
    pub fn new(tree: WidgetTree) -> Self {
        Self { tree, space: None }
    }

    pub fn get_or_create_space(&mut self, size: UiSize, universe: &mut Universe) -> URef<Space> {
        if let Some(space) = self.space.as_ref() {
            // TODO: We will need to be comparing the entire size if it gains other fields
            if space.read().unwrap().bounds() == size.space_bounds() {
                return space.clone();
            }
        }

        // Size didn't match, so recreate the space.
        // TODO: Resize in-place instead, once `Space` supports that.
        let space = universe.insert_anonymous(size.create_space());
        // TODO: error handling for layout
        space
            .execute(
                &install_widgets(LayoutGrant::new(size.space_bounds()), &self.tree)
                    .expect("layout/widget error"),
                &mut transaction::no_outputs,
            )
            .expect("transaction error");

        // Initialize lighting
        space
            .try_modify(|space| {
                space.fast_evaluate_light();
                space.evaluate_light::<time::NoTime>(10, |_| {});
            })
            .unwrap();

        self.space = Some(space.clone());
        space
    }
}

/// Wrap the given widget tree in a transparent screen-filling background.
pub(crate) fn page_modal_backdrop(foreground: WidgetTree) -> WidgetTree {
    Arc::new(LayoutTree::Stack {
        direction: Face6::PZ,
        children: vec![
            // TODO: have a better way to communicate our choice of "baseline" alignment
            Arc::new(LayoutTree::Spacer(LayoutRequest {
                // magic number 2 allows us to fill the edges of the viewport, ish
                // TODO: VUI camera positioning should give us the option of "overscan",
                // where all edges of the space spill off the window.
                minimum: GridVector::new(0, 0, UiSize::DEPTH_BEHIND_VIEW_PLANE + 2),
            })),
            LayoutTree::leaf(
                widgets::Frame::with_block(Block::from(Rgba::new(0., 0., 0., 0.7)))
                    as Arc<dyn Widget>,
            ),
            foreground,
        ],
    })
}

/// Helpers for assembling widget trees into dialog stuff.
pub(crate) mod parts {
    use super::*;

    /// Construct a [`Voxels`] widget around a widget tree containing [`LargeText`] or similar.
    pub fn shrink(
        universe: &mut Universe,
        resolution: Resolution,
        large: WidgetTree,
    ) -> Result<Arc<dyn Widget>, InstallVuiError> {
        let space = large.to_space(
            SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
            Gravity::new(Align::Center, Align::Center, Align::Low),
        )?;
        Ok(Arc::new(widgets::Voxels::new(
            space.bounds(),
            universe.insert_anonymous(space),
            resolution,
            BlockAttributes::default(),
        )))
    }

    pub fn heading(text: impl Into<ArcStr>) -> WidgetTree {
        LayoutTree::leaf(Arc::new(widgets::LargeText {
            text: text.into(),
            font: || &font::FONT_9X15_BOLD,
            brush: VoxelBrush::single(Block::from(palette::ALMOST_BLACK)),
            text_style: TextStyle::default(),
        }))
    }

    pub fn paragraph(text: impl Into<ArcStr>) -> WidgetTree {
        LayoutTree::leaf(Arc::new(widgets::LargeText {
            text: text.into(),
            font: || &font::FONT_6X10,
            brush: VoxelBrush::single(Block::from(palette::ALMOST_BLACK)),
            text_style: TextStyle::default(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ui_size() {
        let cases: Vec<([u32; 2], [i32; 2])> =
            vec![([800, 600], [25, 19]), ([1000, 600], [25, 15])];
        let mut failed = 0;
        for (nominal_viewport, expected_size) in cases {
            let actual_size = UiSize::new(camera::Viewport::with_scale(1.0, nominal_viewport)).size;
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

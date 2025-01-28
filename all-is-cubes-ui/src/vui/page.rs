use alloc::sync::Arc;
use alloc::vec;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::block::{AIR, text};
use all_is_cubes::block::{Block, Resolution};
use all_is_cubes::color_block;
use all_is_cubes::content::palette;
use all_is_cubes::euclid::{Size2D, size2};
use all_is_cubes::math::{
    Cube, Face6, FreeCoordinate, FreeVector, GridAab, GridCoordinate, GridSize, GridSizeCoord, Rgba,
};
use all_is_cubes::space::{self, Space, SpacePhysics};
use all_is_cubes::time;
use all_is_cubes::universe::{Handle, Universe};
use all_is_cubes_render::camera::{self, ViewTransform};

use crate::vui::{
    self, Align, Gravity, InstallVuiError, LayoutGrant, LayoutRequest, LayoutTree, Layoutable,
    Widget, WidgetTree, install_widgets, widgets,
};

/// Bounds for UI display; a choice of scale and aspect ratio based on the viewport size
/// and aspect ratio (and maybe in the future, preferences).
///
///
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct UiSize {
    /// Two-dimensional size; individual pages may have their own choices of depth.
    size: Size2D<u32, Cube>,
}

impl UiSize {
    pub(crate) const DEPTH_BEHIND_VIEW_PLANE: u8 = 5;

    /// Construct [`UiSize`] that suits the given viewport
    /// (based on pixel resolution and aspect ratio).
    pub fn new(viewport: camera::Viewport) -> Self {
        // Note: Dimensions are enforced to be odd so that the crosshair can work.
        // The toolbar is also designed to be odd width when it has an even number of positions.
        let width = 25;
        // we want to ceil() the height because the camera setup makes the height match
        // the viewport and ignores width, so we want to prefer too-narrow over too-wide
        let height =
            ((FreeCoordinate::from(width) / viewport.nominal_aspect_ratio()).ceil() as u32).max(8);
        let height = height / 2 * 2 + 1; // ensure odd
        Self {
            size: size2(width, height),
        }
    }

    /// TODO: depth should be up to the choice of the individual pages.
    pub(crate) fn space_bounds(self) -> GridAab {
        let size_c = self.size.to_i32();
        GridAab::from_lower_upper(
            [0, 0, -GridCoordinate::from(Self::DEPTH_BEHIND_VIEW_PLANE)],
            [size_c.width, size_c.height, 5],
        )
    }

    /// Create a new space with the specified bounds and a standard lighting condition for UI.
    // TODO: validate this doesn't crash on wonky sizes.
    pub(crate) fn create_space(self) -> Space {
        let bounds = self.space_bounds();
        let Size2D {
            width: w,
            height: h,
            ..
        } = self.size;
        let mut space = Space::builder(bounds)
            .physics({
                let mut physics = SpacePhysics::default();
                physics.sky = space::Sky::Uniform(palette::HUD_SKY);
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

/// A widget tree combined with instructions for how to present it to the user.
#[derive(Clone, Debug)]
pub(crate) struct Page {
    /// The widgets that this page displays.
    pub tree: WidgetTree,

    /// How the tree should be presented on screen.
    pub layout: PageLayout,

    /// Whether, when this page is displayed,
    ///
    /// * mouselook is cancelled
    /// * TODO: keyboard focus should go to UI elements and not to gameplay controls
    pub focus_on_ui: bool,
}

impl Page {
    pub fn empty() -> Self {
        Self {
            tree: LayoutTree::empty(),
            layout: PageLayout::Hud,
            focus_on_ui: false,
        }
    }

    /// Wrap the given widget tree in a dialog box and a transparent screen-filling background.
    pub fn new_modal_dialog(
        theme: &widgets::WidgetTheme,
        title: ArcStr,
        corner_button: Option<WidgetTree>,
        contents: WidgetTree,
    ) -> Self {
        Self::new_modal_dialog_with_title_widget(
            theme,
            vui::leaf_widget(widgets::Label::new(title)),
            corner_button,
            contents,
        )
    }

    pub fn new_modal_dialog_with_title_widget(
        theme: &widgets::WidgetTheme,
        title_widget: WidgetTree,
        corner_button: Option<WidgetTree>,
        contents: WidgetTree,
    ) -> Self {
        let tree = Arc::new(LayoutTree::Stack {
            direction: Face6::PZ,
            children: vec![
                Arc::new(LayoutTree::Spacer(LayoutRequest {
                    // magic number 2 allows us to fill the edges of the viewport, ish
                    // TODO: PageLayout should give us the option of "overscan",
                    // where all edges of the space spill off the window.
                    minimum: GridSize::new(
                        0,
                        0,
                        GridSizeCoord::from(UiSize::DEPTH_BEHIND_VIEW_PLANE) + 2,
                    ),
                })),
                vui::leaf_widget(widgets::Frame::with_block(color_block!(0., 0., 0., 0.7))),
                Arc::new(LayoutTree::Shrink(
                    theme
                        .dialog_background()
                        .as_background_of(Arc::new(LayoutTree::Stack {
                            direction: Face6::NY,
                            children: vec![
                                Arc::new(LayoutTree::Stack {
                                    direction: Face6::PX,
                                    children: if let Some(corner_button) = corner_button {
                                        // TODO: arrange so that title text is centered if possible
                                        // (need a new LayoutTree variant or to generalize Stack, but it might help with our HUD too)
                                        vec![corner_button, title_widget]
                                    } else {
                                        vec![title_widget]
                                    },
                                }),
                                contents,
                            ],
                        })),
                )),
            ],
        });

        Page {
            tree,
            layout: PageLayout::Dialog,
            focus_on_ui: true,
        }
    }
}

/// How the camera should be positioned to look at a [`Page`]’s content.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum PageLayout {
    /// The layout designed for the HUD.
    ///
    /// * The root [`LayoutGrant`] is always an odd width and height.
    /// * Z = 0 meets the top and bottom edges of the viewport (if feasible).
    Hud,

    /// Layout designed for displaying dialog boxes.
    ///
    /// TODO: Give it appropriate characteristics and document them.
    Dialog,
}

impl PageLayout {
    /// Calculate the camera view transform that should be used for a page with this layout.
    pub(crate) fn view_transform(
        self,
        space: &Space,
        fov_y_degrees: FreeCoordinate,
    ) -> ViewTransform {
        let bounds = space.bounds();
        let mut ui_center = bounds.center();

        match self {
            PageLayout::Hud | PageLayout::Dialog => {
                // Arrange a view distance which will place the Z=0 plane sized to fill the viewport
                // vertically.
                ui_center.z = 0.0;

                let view_distance = FreeCoordinate::from(bounds.size().height)
                    / (fov_y_degrees / 2.).to_radians().tan()
                    / 2.;
                ViewTransform::from_translation(
                    ui_center.to_vector() + FreeVector::new(0., 0., view_distance),
                )
            }
        }
    }
}

/// Pair of a [`Page`] and a cached space that is an instantiation of its widgets,
/// which can be recreated with a different size as needed by viewport size changes.
///
/// TODO: Give this a better name.
#[derive(Clone, Debug)]
pub(crate) struct PageInst {
    page: Page,
    space: Option<Handle<Space>>,
}

impl PageInst {
    pub fn new(page: Page) -> Self {
        Self { page, space: None }
    }

    pub fn get_or_create_space(
        &mut self,
        mut size: UiSize,
        universe: &mut Universe,
    ) -> Handle<Space> {
        if let Some(space) = self.space.as_ref() {
            // TODO: We will need to be comparing the entire size if it gains other fields
            if space.read().unwrap().bounds() == size.space_bounds() {
                return space.clone();
            }
        }

        // TODO: there will be multiple layouts and the size calculation will depend on the layout
        match self.page.layout {
            PageLayout::Hud | PageLayout::Dialog => {}
        }

        // If necessary, enlarge the proposed dimensions.
        // The camera may be bad but at least the widgets won't be missing parts.
        // TODO: We need overall better handling of this; for example, we should be able to
        // pan ("scroll") the camera over a tall dialog box.
        // Also, this doesn't handle Z size.
        let fitting_size = size
            .size
            .max(drop_depth(self.page.tree.requirements().minimum));
        if fitting_size != size.size {
            log::debug!("VUI page had to enlarge proposed size {size:?} to {fitting_size:?}");
            size.size = fitting_size;
        }

        // Size didn't match, so recreate the space.
        // TODO: Resize in-place instead, once `Space` supports that.
        match self.create_space(size, universe) {
            Ok(space) => {
                self.space = Some(space.clone());
                space
            }
            Err(error) => {
                // TODO: Recover by using old space or by popping the page state stack if possible
                panic!(
                    "VUI page construction failure:\n{error}",
                    error = all_is_cubes::util::ErrorChain(&error),
                );
            }
        }
    }

    fn create_space(
        &self,
        size: UiSize,
        universe: &mut Universe,
    ) -> Result<Handle<Space>, InstallVuiError> {
        let space = universe.insert_anonymous(size.create_space());
        let txn = install_widgets(LayoutGrant::new(size.space_bounds()), &self.page.tree)?;
        space
            .execute(&txn)
            .map_err(|error| InstallVuiError::ExecuteInstallation { error })?;

        // Initialize lighting
        space
            .try_modify(|space| {
                space.fast_evaluate_light();
                space.evaluate_light::<time::NoTime>(10, |_| {});
            })
            .unwrap();

        Ok(space)
    }

    pub fn page(&self) -> &Page {
        &self.page
    }
}

impl From<Page> for PageInst {
    fn from(page: Page) -> Self {
        Self::new(page)
    }
}

// TODO: contribute this to euclid
fn drop_depth(size: GridSize) -> Size2D<u32, Cube> {
    Size2D::new(size.width, size.height)
}

/// Helpers for assembling widget trees into dialog stuff.
pub(crate) mod parts {
    use super::*;

    /// Construct a [`Voxels`] widget around a widget tree containing [`LargeText`] or similar.
    ///
    /// TODO: Replace all uses of this with the new block-based text rendering.
    pub fn shrink(
        universe: &mut Universe,
        resolution: Resolution,
        large: &WidgetTree,
    ) -> Result<Arc<dyn Widget>, InstallVuiError> {
        let space = large.to_space(
            space::Builder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
            Gravity::new(Align::Center, Align::Center, Align::Low),
        )?;
        Ok(Arc::new(widgets::Voxels::new(
            space.bounds(),
            universe.insert_anonymous(space),
            resolution,
            [],
        )))
    }

    pub fn heading(text: impl Into<ArcStr>) -> WidgetTree {
        vui::leaf_widget(widgets::Label::new(text.into()))
    }

    pub fn paragraph(text: impl Into<ArcStr>) -> WidgetTree {
        vui::leaf_widget(widgets::Label::with_font(
            text.into(),
            text::Font::SmallerBodyText,
            text::Positioning {
                x: text::PositioningX::Left,
                line_y: text::PositioningY::BodyTop,
                z: text::PositioningZ::Back,
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;

    #[test]
    fn ui_size() {
        let cases: Vec<([u32; 2], [u32; 2])> =
            vec![([800, 600], [25, 19]), ([1000, 600], [25, 15])];
        let mut failed = 0;
        for (nominal_viewport, expected_size) in cases {
            let actual_size = UiSize::new(camera::Viewport::with_scale(1.0, nominal_viewport)).size;
            let actual_size: [u32; 2] = actual_size.into();
            if actual_size != expected_size {
                println!(
                    "{nominal_viewport:?} expected to produce {expected_size:?}; got {actual_size:?}"
                );
                failed += 1;
            }
        }
        if failed > 0 {
            panic!("{failed} cases failed");
        }
    }
}

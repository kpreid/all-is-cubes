//! Specific UI widgets.

use std::error::Error;
use std::fmt::{self, Debug};
use std::sync::Arc;

use cgmath::EuclideanSpace;
use embedded_graphics::prelude::Point;
use embedded_graphics::primitives::{Primitive as _, PrimitiveStyleBuilder, Rectangle};
use embedded_graphics::Drawable;
use exhaust::Exhaust;

use crate::behavior::BehaviorSetTransaction;
use crate::block::{Block, BlockAttributes, Primitive, Resolution, AIR};
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::inv::EphemeralOpaque;
use crate::listen::{DirtyFlag, ListenableSource};
use crate::math::{GridAab, GridCoordinate, GridMatrix, GridPoint, GridVector};
use crate::space::{Space, SpaceTransaction};
use crate::time::Tick;
use crate::universe::URef;
use crate::vui::{
    ActivatableRegion, InstallVuiError, LayoutGrant, LayoutRequest, Layoutable, Widget,
    WidgetController, WidgetTransaction,
};

mod text;
pub use text::*;
mod toolbar;
pub(crate) use toolbar::*;
mod tooltip;
pub(crate) use tooltip::*;

/// Generic widget controller that only does something on `initialize()`.
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
#[doc(hidden)] // TODO: widget API still in development and this is particularly dubious
pub struct OneshotController(pub Option<WidgetTransaction>);

impl OneshotController {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(transaction: WidgetTransaction) -> Box<dyn WidgetController> {
        Box::new(Self(Some(transaction)))
    }
}

impl WidgetController for OneshotController {
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        Ok(self.0.take().unwrap_or_default())
    }

    // TODO: Arrange somehow for this controller to be deleted since it doesn't need to be step()ped
}

/// Widget that fills a flat XY surface with a "dialog box" sort of background.
/// Also useful for identifying the bounds of a layout region.
///
/// TODO: Define what it does in 3D.
#[derive(Debug)]
pub struct Frame {
    background: VoxelBrush<'static>,
    frame: VoxelBrush<'static>,
}

impl Frame {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            background: VoxelBrush::single(Block::from(palette::MENU_BACK)),
            frame: VoxelBrush::single(Block::from(palette::MENU_FRAME)),
        })
    }

    /// experimental
    #[doc(hidden)]
    pub fn with_block(block: Block) -> Arc<Self> {
        Arc::new(Self {
            background: VoxelBrush::single(block.clone()),
            frame: VoxelBrush::single(block),
        })
    }
}

// Frame can be any size with at least 1 depth.
impl Layoutable for Frame {
    fn requirements(&self) -> LayoutRequest {
        // TODO: account for size of the chosen VoxelBrushes (currently not possible to change)
        LayoutRequest {
            minimum: GridVector::new(0, 0, 1),
        }
    }
}

impl Widget for Frame {
    fn controller(self: Arc<Self>, position: &LayoutGrant) -> Box<dyn WidgetController> {
        let bounds = position.bounds;
        let mut txn = SpaceTransaction::default();

        if !bounds.is_empty() {
            let background_rect = Rectangle::with_corners(
                Point::new(bounds.lower_bounds().x, bounds.lower_bounds().y),
                Point::new(bounds.upper_bounds().x - 1, bounds.upper_bounds().y - 1),
            );

            let dt = &mut txn.draw_target(GridMatrix::from_translation([
                0,
                0,
                bounds.lower_bounds().z,
            ]));

            background_rect
                .into_styled(
                    PrimitiveStyleBuilder::new()
                        .stroke_width(1)
                        .stroke_color(&self.frame)
                        .fill_color(&self.background)
                        .build(),
                )
                .draw(dt)
                .unwrap();
        }

        OneshotController::new(txn)
    }
}

/// Widget that takes a [`Space`] and displays it shrunken, such as for a text label,
/// with no interactive behavior.
///
/// TODO: add a click response feature
#[derive(Clone, Debug)]
pub struct Voxels {
    space: URef<Space>,
    region: GridAab,
    scale: Resolution,
    block_attributes: BlockAttributes,
}

impl Voxels {
    /// Create [`Voxels`] that displays the specified `region` of the `space`.
    ///
    /// When the `region`'s dimensions are not multiples of `scale`, their alignment within the
    /// block grid will be determined by the layout gravity. Note that areas of `space` outside of
    /// `region` may be displayed in that case.
    pub const fn new(
        region: GridAab,
        space: URef<Space>,
        scale: Resolution,
        block_attributes: BlockAttributes,
    ) -> Self {
        // Design note: We could take `region` from `space` but that'd require locking it,
        // and the caller is very likely to already have that information.
        Self {
            region,
            space,
            scale,
            block_attributes,
        }
    }
}

impl Layoutable for Voxels {
    fn requirements(&self) -> LayoutRequest {
        let scale = GridCoordinate::from(self.scale);

        LayoutRequest {
            // Divide rounding up.
            // Note: Not using GridAab::divide because that would take into account the
            // absolute position, which we *don't* want.
            minimum: self.region.size().map(|size| (size + scale - 1) / scale),
        }
    }
}

impl Widget for Voxels {
    fn controller(self: Arc<Self>, position: &LayoutGrant) -> Box<dyn WidgetController> {
        // This is similar but not identical to block::space_to_blocks().

        let scale_g = GridCoordinate::from(self.scale);
        let position = position.shrink_to(self.requirements().minimum);

        // Calculate where the voxels should land in the blocks, respecting layout gravity,
        // by using the shrink_to algorithm again.
        let grant_in_voxels =
            GridAab::from_lower_size(GridPoint::origin(), position.bounds.size() * scale_g);
        let gravity_offset_in_voxels: GridVector = LayoutGrant {
            bounds: grant_in_voxels,
            gravity: position.gravity,
        }
        .shrink_to(self.region.size())
        .bounds
        .lower_bounds()
        .to_vec();
        // gravity_offset_in_voxels is now the offset within the low-corner block at
        // which the low-corner voxels should start.

        let block_to_voxels_transform = {
            // Apply gravity and the translation that was requested
            GridMatrix::from_translation(
                self.region.lower_bounds().to_vec() - gravity_offset_in_voxels)
            // Scale up from blocks to voxels
            * GridMatrix::from_scale(scale_g)
            // Subtract the absolute position to get relative position
            * GridMatrix::from_translation(-position.bounds.lower_bounds().to_vec())
        };

        let mut txn = SpaceTransaction::default();
        for cube in position.bounds.interior_iter() {
            txn.set_overwrite(
                cube,
                Block::from_primitive(Primitive::Recur {
                    attributes: self.block_attributes.clone(),
                    offset: block_to_voxels_transform.transform_cube(cube),
                    resolution: self.scale,
                    space: self.space.clone(),
                }),
            )
        }
        OneshotController::new(txn)
    }
}

/// A single-block button that displays a boolean state derived from a
/// [`ListenableSource`].
#[derive(Clone)]
pub(crate) struct ToggleButton<D> {
    states: [Block; 2],
    data_source: ListenableSource<D>,
    projection: Arc<dyn Fn(&D) -> bool + Send + Sync>,
    action: EphemeralOpaque<dyn Fn() + Send + Sync>,
}

impl<D: Clone + Sync + Debug> Debug for ToggleButton<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ToggleButton")
            .field("states", &self.states)
            .field("data_source", &self.data_source)
            .field(
                "projection(data_source)",
                &(self.projection)(&self.data_source.snapshot()),
            )
            .field("action", &self.action)
            .finish()
    }
}

impl<D> ToggleButton<D> {
    pub(crate) fn new(
        data_source: ListenableSource<D>,
        projection: impl Fn(&D) -> bool + Send + Sync + 'static,
        mut blocks: impl FnMut(ToggleButtonVisualState) -> Block,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            data_source,
            projection: Arc::new(projection),
            states: [
                blocks(ToggleButtonVisualState { value: false }),
                blocks(ToggleButtonVisualState { value: true }),
            ],
            action: EphemeralOpaque::from(Arc::new(action) as Arc<dyn Fn() + Send + Sync>),
        })
    }
}

impl<D> Layoutable for ToggleButton<D> {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }
    }
}

// TODO: Mess of generic bounds due to the combination of Widget and ListenableSource
// requirements -- should we make a trait alias for these?
impl<D: Clone + Debug + Send + Sync + 'static> Widget for ToggleButton<D> {
    fn controller(self: Arc<Self>, position: &LayoutGrant) -> Box<dyn WidgetController> {
        Box::new(ToggleButtonController::new(
            position
                .shrink_to(self.requirements().minimum)
                .bounds
                .lower_bounds(),
            self,
        ))
    }
}

/// Possible visual states of a `ToggleButton`.
///
/// The [`fmt::Display`] implementation of this type produces a string form suitable for
/// naming blocks depicting this state; the [`Exhaust`] implementation allows iterating
/// over all possible states.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
pub struct ToggleButtonVisualState {
    /// The on/off value depicted.
    pub value: bool,
    // TODO: add hover/press states
}

/// Represents this value as a string suitable for naming blocks depicting this state.
impl fmt::Display for ToggleButtonVisualState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self { value: true } => write!(f, "on"),
            Self { value: false } => write!(f, "off"),
        }
    }
}

/// [`WidgetController`] for [`ToggleButton`].
#[derive(Debug)]
pub(crate) struct ToggleButtonController<D: Clone + Send + Sync> {
    definition: Arc<ToggleButton<D>>,
    position: GridPoint,
    todo: DirtyFlag,
}

impl<D: Clone + Debug + Send + Sync + 'static> ToggleButtonController<D> {
    pub(crate) fn new(position: GridPoint, definition: Arc<ToggleButton<D>>) -> Self {
        Self {
            todo: DirtyFlag::listening(true, |l| definition.data_source.listen(l)),
            position,
            definition,
        }
    }
}

impl<D: Clone + Debug + Send + Sync + 'static> WidgetController for ToggleButtonController<D> {
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        Ok(SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            Arc::new(ActivatableRegion {
                region: GridAab::single_cube(self.position),
                effect: self.definition.action.clone(),
            }),
        )))
    }

    fn step(&mut self, _: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        Ok(if self.todo.get_and_clear() {
            let value = (self.definition.projection)(&self.definition.data_source.get());
            SpaceTransaction::set_cube(
                self.position,
                None,
                Some(self.definition.states[usize::from(value)].clone()),
            )
        } else {
            SpaceTransaction::default()
        })
    }
}

#[derive(Debug)]
pub(crate) struct Crosshair {
    icon: Block,
    mouselook_mode: ListenableSource<bool>,
}

impl Crosshair {
    pub fn new(icon: Block, mouselook_mode: ListenableSource<bool>) -> Arc<Self> {
        Arc::new(Self {
            icon,
            mouselook_mode,
        })
    }
}

impl Layoutable for Crosshair {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }
    }
}

impl Widget for Crosshair {
    fn controller(self: Arc<Self>, position: &LayoutGrant) -> Box<dyn WidgetController> {
        assert!(position
            .bounds
            .contains_cube(position.bounds.lower_bounds()));
        Box::new(CrosshairController {
            position: position.bounds.lower_bounds(),
            todo: DirtyFlag::listening(false, |l| self.mouselook_mode.listen(l)),
            definition: self,
        })
    }
}

/// Shows/hides the crosshair depending on mouselook mode.
#[derive(Debug)]
pub(crate) struct CrosshairController {
    definition: Arc<Crosshair>,
    position: GridPoint,
    todo: DirtyFlag,
}

impl WidgetController for CrosshairController {
    fn step(&mut self, _tick: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        Ok(if self.todo.get_and_clear() {
            let d = &*self.definition;
            SpaceTransaction::set_cube(
                self.position,
                None,
                Some(if *d.mouselook_mode.get() {
                    d.icon.clone()
                } else {
                    AIR
                }),
            )
        } else {
            SpaceTransaction::default()
        })
    }
}

#[cfg(test)]
mod tests {
    use cgmath::{Point3, Vector3};

    use crate::block::Resolution::*;
    use crate::transaction::Transaction;
    use crate::universe::Universe;
    use crate::vui::{install_widgets, Align, LayoutTree};

    use super::*;

    /// Create a Space to put a widget in.
    /// TODO: if this is a useful test helper it should live somewhere else
    #[track_caller]
    fn instantiate_widget<W: Widget + 'static>(grant: LayoutGrant, widget: W) -> (GridAab, Space) {
        let mut space = Space::builder(grant.bounds).build();
        let txn = install_widgets(grant, &LayoutTree::leaf(Arc::new(widget)))
            .expect("widget instantiation");
        txn.execute(&mut space).expect("widget transaction");
        (txn.bounds().unwrap(), space)
    }

    fn test_voxels_widget(
        voxel_space_bounds: GridAab,
        grant: LayoutGrant,
        attributes: BlockAttributes,
    ) -> (GridAab, Space) {
        let mut universe = Universe::new();
        instantiate_widget(
            grant,
            Voxels::new(
                voxel_space_bounds,
                universe.insert_anonymous(Space::builder(voxel_space_bounds).build()),
                R8,
                attributes,
            ),
        )
    }

    // TODO: not sure if this is actually something the widget system wants to support or not
    #[test]
    fn voxels_in_too_small_grant_succeeds() {
        let v_space_bounds = GridAab::from_lower_upper([0, 0, 0], [7, 8, 9]);
        let output_origin = GridPoint::new(100, 100, 100);
        let _output = test_voxels_widget(
            v_space_bounds,
            LayoutGrant::new(GridAab::single_cube(output_origin)),
            BlockAttributes::default(),
        );
        // TODO assertions about resulting blocks
    }

    #[test]
    fn voxels_alignment() {
        // Space bounds such that X is smaller, Y is exact, and Z is bigger than one block
        // TODO: test voxel space lower bounds being nonzero
        let v_space_bounds = GridAab::from_lower_upper([0, 0, 0], [7, 8, 9]);
        // (TODO: put something in v_space so we can visualize)
        // Arbitrary nonzero origin point
        let output_origin = GridPoint::new(100, 100, 100);
        let grant = LayoutGrant {
            bounds: GridAab::from_lower_size(output_origin, [3, 3, 3]),
            gravity: Vector3::new(Align::Low, Align::Center, Align::High),
        };
        let (output_bounds, output) =
            test_voxels_widget(v_space_bounds, grant, BlockAttributes::default());
        assert_eq!(
            output_bounds,
            GridAab::from_lower_size([100, 101, 101], [1, 1, 2])
        );
        // Expect two adjacent recursive blocks
        match *output[[100, 101, 101]].primitive() {
            Primitive::Recur {
                attributes: _,
                offset,
                resolution,
                space: _,
            } => {
                assert_eq!(resolution, R8);
                assert_eq!(offset, Point3::new(0, 0, -7));
            }
            ref p => panic!("unexpected primitive {p:?}"),
        }
        match *output[[100, 101, 102]].primitive() {
            Primitive::Recur {
                attributes: _,
                offset,
                resolution,
                space: _,
            } => {
                assert_eq!(resolution, R8);
                assert_eq!(offset, Point3::new(0, 0, 1));
            }
            ref p => panic!("unexpected primitive {p:?}"),
        }
    }
}

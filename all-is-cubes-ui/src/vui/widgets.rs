//! Specific UI widgets.

use std::sync::Arc;

use all_is_cubes::block::Block;
use all_is_cubes::math::GridVector;
use all_is_cubes::space::SpaceTransaction;

use crate::vui;

mod crosshair;
pub(crate) use crosshair::*;
mod frame;
pub use frame::*;
mod text;
pub use text::*;
mod button;
pub use button::*;
mod toolbar;
pub(crate) use toolbar::*;
mod tooltip;
pub(crate) use tooltip::*;
mod voxels;
pub use voxels::*;

/// Generic widget controller that only does something on `initialize()`.
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct OneshotController(pub Option<vui::WidgetTransaction>);

impl OneshotController {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(transaction: vui::WidgetTransaction) -> Box<dyn vui::WidgetController> {
        Box::new(Self(Some(transaction)))
    }
}

impl vui::WidgetController for OneshotController {
    fn initialize(&mut self) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        Ok(self.0.take().unwrap_or_default())
    }

    // TODO: Arrange somehow for this controller to be deleted since it doesn't need to be step()ped
}

/// A block may act as a 1×1×1 non-interactive widget.
impl vui::Widget for Block {
    fn controller(self: Arc<Self>, grant: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        let cube = grant
            .shrink_to(GridVector::new(1, 1, 1), false)
            .bounds
            .lower_bounds();
        OneshotController::new(if grant.bounds.contains_cube(cube) {
            SpaceTransaction::set_cube(cube, None, Some(Block::clone(&self)))
        } else {
            SpaceTransaction::default()
        })
    }
}

impl vui::Layoutable for Block {
    fn requirements(&self) -> vui::LayoutRequest {
        vui::LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vui::Align;
    use all_is_cubes::cgmath::Vector3;
    use all_is_cubes::content::make_some_blocks;
    use all_is_cubes::math::{GridAab, GridPoint};

    #[test]
    fn block_widget_in_position() {
        let [block] = make_some_blocks();
        let origin = GridPoint::new(1, 2, 3);
        let grant = vui::LayoutGrant {
            bounds: GridAab::from_lower_size(origin, [1, 1, 1]),
            gravity: Vector3::new(Align::Low, Align::Center, Align::High),
        };
        let (bounds, space) = vui::instantiate_widget(grant, block.clone());
        assert_eq!(bounds, Some(grant.bounds));
        assert_eq!(&space[origin], &block);
    }

    #[test]
    fn block_widget_too_small() {
        let [block] = make_some_blocks();
        let origin = GridPoint::new(1, 2, 3);
        let grant = vui::LayoutGrant {
            bounds: GridAab::from_lower_size(origin, [0, 1, 1]),
            gravity: Vector3::new(Align::Low, Align::Center, Align::High),
        };
        let (bounds, _space) = vui::instantiate_widget(grant, block.clone());
        assert_eq!(bounds, None);
    }
}

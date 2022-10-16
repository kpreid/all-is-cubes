//! Specific UI widgets.

use crate::vui::{InstallVuiError, WidgetController, WidgetTransaction};

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

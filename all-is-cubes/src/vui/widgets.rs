//! Specific UI widgets.

use crate::vui::{self, InstallVuiError, WidgetController, WidgetTransaction};

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

/// A one-cube action button which performs the VUI 'back' action.
pub(crate) fn back_button(hud_inputs: &vui::HudInputs) -> vui::WidgetTree {
    // TODO: define a narrower set of inputs than HudInputs
    // TODO: this function should maybe live in a 'UI mid-level components' module?
    vui::LayoutTree::leaf(ActionButton::new(
        |state| hud_inputs.hud_blocks.blocks[vui::UiBlocks::BackButton(state)].clone(),
        {
            let cc = hud_inputs.vui_control_channel.clone();
            move || {
                let _ignore_errors = cc.send(vui::VuiMessage::Back);
            }
        },
    ))
}

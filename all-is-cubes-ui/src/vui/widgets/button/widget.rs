//! Public API types and functions for button widgets.

use all_is_cubes::universe;
use alloc::sync::Arc;
use core::fmt;
use core::hash::Hash;

use exhaust::Exhaust;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::block::Block;
use all_is_cubes::inv::EphemeralOpaque;
use all_is_cubes::linking;
use all_is_cubes::listen;

use crate::vui;
use crate::vui::widgets::{BoxStyle, WidgetBlocks, WidgetTheme};

use super::Action;

// -------------------------------------------------------------------------------------------------

/// Common elements of button widgets.
/// A button widget is a widget that displays a single clickable shape that can have a finite set
/// of possible appearances (on/off, pressed, etc).
#[derive(Clone, Debug)]
pub(in crate::vui::widgets::button) struct ButtonCommon<St> {
    /// Button shape, indicating what kind of button it is.
    pub(in crate::vui::widgets::button) shape: linking::Provider<St, BoxStyle>,

    /// Label to be put on top of the shape.
    pub(in crate::vui::widgets::button) label: ButtonLabel,
}

impl<St: Clone + Eq + Hash + Exhaust + fmt::Debug> ButtonCommon<St> {
    fn new(shape: &linking::Provider<St, Block>, label: ButtonLabel) -> Self {
        let shape = shape.map(|_, base_multiblock| BoxStyle::from_nine_and_thin(base_multiblock));
        Self { shape, label }
    }
}

impl<St: Eq + Hash + universe::VisitHandles> universe::VisitHandles for ButtonCommon<St> {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self { shape, label } = self;
        shape.visit_handles(visitor);
        label.visit_handles(visitor);
    }
}

// -------------------------------------------------------------------------------------------------

/// What is displayed on the face of a button widget.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct ButtonLabel {
    /// Picture stuck to the front face of the button. It should be flat against the -Z face and
    /// only a few voxels thick.
    /// Specific button types have more specific requirements for margin.
    pub icon: Option<Block>,

    /// Text to display.
    pub text: Option<vui::widgets::Label>,
}

impl From<Block> for ButtonLabel {
    fn from(icon: Block) -> Self {
        ButtonLabel {
            icon: Some(icon),
            text: None,
        }
    }
}
impl From<vui::widgets::Label> for ButtonLabel {
    fn from(text: vui::widgets::Label) -> Self {
        ButtonLabel {
            icon: None,
            text: Some(text),
        }
    }
}
impl From<ArcStr> for ButtonLabel {
    fn from(string: ArcStr) -> Self {
        ButtonLabel {
            icon: None,
            text: Some(vui::widgets::Label::new(string)),
        }
    }
}

impl universe::VisitHandles for ButtonLabel {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self { icon, text } = self;
        icon.visit_handles(visitor);
        text.visit_handles(visitor);
    }
}

// -------------------------------------------------------------------------------------------------
// The button widget types themselves.

/// A single-block button that reacts to activations (clicks) but does not change
/// otherwise.
#[derive(Clone, Debug)]
pub struct ActionButton {
    pub(in crate::vui::widgets::button) common: ButtonCommon<ButtonVisualState>,
    pub(in crate::vui::widgets::button) action: Action,
}

impl ActionButton {
    #[allow(missing_docs)] // TODO
    pub fn new(
        label: impl Into<ButtonLabel>,
        theme: &WidgetTheme,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            common: ButtonCommon::new(
                &theme.widget_blocks.subset(WidgetBlocks::ActionButton),
                label.into(),
            ),
            action: EphemeralOpaque::new(Arc::new(action)),
        })
    }
}

impl vui::Layoutable for ActionButton {
    fn requirements(&self) -> vui::LayoutRequest {
        self.common.requirements()
    }
}

/// A single-block button that displays a boolean state derived from a
/// [`listen::DynSource`] and can be clicked.
#[derive(Clone)]
pub struct ToggleButton<D> {
    pub(in crate::vui::widgets::button) common: ButtonCommon<ToggleButtonVisualState>,
    pub(in crate::vui::widgets::button) data_source: listen::DynSource<D>,
    pub(in crate::vui::widgets::button) projection: Arc<dyn Fn(&D) -> bool + Send + Sync>,
    pub(in crate::vui::widgets::button) action: Action,
}

impl<D: Clone + Sync + fmt::Debug> fmt::Debug for ToggleButton<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            common,
            data_source,
            projection,
            action,
        } = self;
        f.debug_struct("ToggleButton")
            .field("common", common)
            .field("data_source", data_source)
            .field("projection(data_source)", &projection(&data_source.get()))
            .field("action", action)
            .finish()
    }
}

impl<D> ToggleButton<D> {
    #[allow(missing_docs)]
    pub fn new(
        data_source: listen::DynSource<D>,
        projection: impl Fn(&D) -> bool + Send + Sync + 'static,
        label: impl Into<ButtonLabel>,
        theme: &WidgetTheme,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            common: ButtonCommon::new(
                &theme.widget_blocks.subset(WidgetBlocks::ToggleButton),
                label.into(),
            ),
            data_source,
            projection: Arc::new(projection),
            action: EphemeralOpaque::new(Arc::new(action)),
        })
    }
}

impl<D> vui::Layoutable for ToggleButton<D> {
    fn requirements(&self) -> vui::LayoutRequest {
        self.common.requirements()
    }
}

impl universe::VisitHandles for ActionButton {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self { common, action } = self;
        common.visit_handles(visitor);
        action.visit_handles(visitor);
    }
}
impl<D> universe::VisitHandles for ToggleButton<D> {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            common,
            action,
            data_source: _,
            projection: _,
        } = self;
        common.visit_handles(visitor);
        action.visit_handles(visitor);
    }
}

// -------------------------------------------------------------------------------------------------

/// Possible visual states of a button.
///
///
/// [`ActionButton`] uses this directly, and other buttons' state may incorporate it.
///
/// The [`fmt::Display`] implementation of this type produces a string form suitable for
/// naming blocks depicting this state; the [`Exhaust`] implementation allows iterating
/// over all possible states.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
pub struct ButtonVisualState {
    // TODO: Add hover, disabled
    /// The button looks pushed in.
    pub(crate) pressed: bool,
}

/// Represents this value as a string suitable for naming blocks depicting this state.
impl fmt::Display for ButtonVisualState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.pressed { "pressed" } else { "idle" }.fmt(f)
    }
}

impl universe::VisitHandles for ButtonVisualState {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {}
}

/// Possible visual states of a [`ToggleButton`].
///
/// The [`fmt::Display`] implementation of this type produces a string form suitable for
/// naming blocks depicting this state; the [`Exhaust`] implementation allows iterating
/// over all possible states.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
pub struct ToggleButtonVisualState {
    pub(crate) common: ButtonVisualState,
    /// The on/off value depicted.
    pub value: bool,
}

/// Represents this value as a string suitable for naming blocks depicting this state.
impl fmt::Display for ToggleButtonVisualState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self { value, common } = self;
        write!(
            f,
            "{value}-{common}",
            value = match value {
                false => "off",
                true => "on",
            },
        )
    }
}

impl ToggleButtonVisualState {
    /// Returns the “off” (false) or “on” (true) state.
    pub const fn new(value: bool) -> Self {
        Self {
            value,
            common: ButtonVisualState { pressed: false },
        }
    }
}

impl universe::VisitHandles for ToggleButtonVisualState {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {}
}

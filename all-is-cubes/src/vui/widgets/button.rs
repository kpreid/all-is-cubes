use std::error::Error;
use std::fmt;
use std::sync::Arc;

use exhaust::Exhaust;

use crate::behavior::BehaviorSetTransaction;
use crate::block::Block;
use crate::inv::EphemeralOpaque;
use crate::listen::{DirtyFlag, ListenableSource};
use crate::math::{GridAab, GridPoint, GridVector};
use crate::space::SpaceTransaction;
use crate::time::Tick;
use crate::vui::{self, Layoutable};

/// A single-block button that displays a boolean state derived from a
/// [`ListenableSource`].
#[derive(Clone)]
pub struct ToggleButton<D> {
    states: [Block; 2],
    data_source: ListenableSource<D>,
    projection: Arc<dyn Fn(&D) -> bool + Send + Sync>,
    action: EphemeralOpaque<dyn Fn() + Send + Sync>,
}

impl<D: Clone + Sync + fmt::Debug> fmt::Debug for ToggleButton<D> {
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
    pub fn new(
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

impl<D> vui::Layoutable for ToggleButton<D> {
    fn requirements(&self) -> vui::LayoutRequest {
        vui::LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }
    }
}

// TODO: Mess of generic bounds due to the combination of Widget and ListenableSource
// requirements -- should we make a trait alias for these?
impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::Widget for ToggleButton<D> {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ToggleButtonController::new(
            position
                .shrink_to(self.requirements().minimum)
                .bounds
                .lower_bounds(),
            self,
        ))
    }
}

/// Possible visual states of a [`ToggleButton`].
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
struct ToggleButtonController<D: Clone + Send + Sync> {
    definition: Arc<ToggleButton<D>>,
    position: GridPoint,
    todo: DirtyFlag,
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> ToggleButtonController<D> {
    fn new(position: GridPoint, definition: Arc<ToggleButton<D>>) -> Self {
        Self {
            todo: DirtyFlag::listening(true, |l| definition.data_source.listen(l)),
            position,
            definition,
        }
    }
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::WidgetController
    for ToggleButtonController<D>
{
    fn initialize(&mut self) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        Ok(SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            Arc::new(vui::ActivatableRegion {
                region: GridAab::single_cube(self.position),
                effect: self.definition.action.clone(),
            }),
        )))
    }

    fn step(&mut self, _: Tick) -> Result<vui::WidgetTransaction, Box<dyn Error + Send + Sync>> {
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

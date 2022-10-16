use std::error::Error;
use std::sync::Arc;

use crate::block::{Block, AIR};
use crate::listen::{DirtyFlag, ListenableSource};
use crate::math::{GridPoint, GridVector};
use crate::space::SpaceTransaction;
use crate::time::Tick;
use crate::vui;

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

impl vui::Layoutable for Crosshair {
    fn requirements(&self) -> vui::LayoutRequest {
        vui::LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }
    }
}

impl vui::Widget for Crosshair {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
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

impl vui::WidgetController for CrosshairController {
    fn step(
        &mut self,
        _tick: Tick,
    ) -> Result<vui::WidgetTransaction, Box<dyn Error + Send + Sync>> {
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

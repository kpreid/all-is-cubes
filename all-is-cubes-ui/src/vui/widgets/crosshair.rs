use alloc::sync::Arc;
use std::error::Error;

use all_is_cubes::block::{Block, AIR};
use all_is_cubes::listen::{DirtyFlag, ListenableSource};
use all_is_cubes::math::{Cube, GridVector};
use all_is_cubes::space::SpaceTransaction;

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
        let position = position.shrink_to_cube().unwrap();
        Box::new(CrosshairController {
            position,
            todo: DirtyFlag::listening(false, &self.mouselook_mode),
            definition: self,
        })
    }
}

/// Shows/hides the crosshair depending on mouselook mode.
#[derive(Debug)]
pub(crate) struct CrosshairController {
    definition: Arc<Crosshair>,
    position: Cube,
    todo: DirtyFlag,
}

impl vui::WidgetController for CrosshairController {
    fn step(
        &mut self,
        _: &vui::WidgetContext<'_>,
    ) -> Result<(vui::WidgetTransaction, vui::Then), Box<dyn Error + Send + Sync>> {
        let txn = if self.todo.get_and_clear() {
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
        };

        Ok((txn, vui::Then::Step))
    }
}

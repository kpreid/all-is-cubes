use alloc::boxed::Box;
use alloc::sync::Arc;

use all_is_cubes::block::{AIR, Block};
use all_is_cubes::euclid::size3;
use all_is_cubes::space::SpaceTransaction;
use all_is_cubes::{listen, universe};

use crate::vui;

#[derive(Debug)]
pub(crate) struct Crosshair {
    icon: Block,
    mouselook_mode: listen::DynSource<bool>,
}

impl Crosshair {
    pub fn new(icon: Block, mouselook_mode: listen::DynSource<bool>) -> Arc<Self> {
        Arc::new(Self {
            icon,
            mouselook_mode,
        })
    }
}

impl vui::Layoutable for Crosshair {
    fn requirements(&self) -> vui::LayoutRequest {
        vui::LayoutRequest {
            minimum: size3(1, 1, 1),
        }
    }
}

impl vui::Widget for Crosshair {
    fn controller(self: Arc<Self>, _: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(CrosshairController {
            todo: listen::Flag::listening(false, &self.mouselook_mode),
            definition: self,
        })
    }
}

/// Shows/hides the crosshair depending on mouselook mode.
#[derive(Debug)]
pub(crate) struct CrosshairController {
    definition: Arc<Crosshair>,
    todo: listen::Flag,
}

impl vui::WidgetController for CrosshairController {
    fn step(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<vui::StepSuccess, vui::StepError> {
        let Some(position) = context.grant().shrink_to_cube() else {
            return Ok((vui::WidgetTransaction::default(), vui::Then::Drop));
        };

        let txn = if self.todo.get_and_clear() {
            let d = &*self.definition;
            SpaceTransaction::set_cube(
                position,
                None,
                Some(if d.mouselook_mode.get() {
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

impl universe::VisitHandles for CrosshairController {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            definition,
            todo: _,
        } = self;
        definition.visit_handles(visitor);
    }
}

impl universe::VisitHandles for Crosshair {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            icon,
            mouselook_mode: _,
        } = self;
        icon.visit_handles(visitor);
    }
}

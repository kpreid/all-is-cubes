#[cfg(test)]
use alloc::vec::Vec;

use bevy_ecs::prelude as ecs;
use bevy_ecs::ptr::Ptr;

#[cfg(doc)]
use crate::universe::Universe;
use crate::universe::{self, ErasedHandle, Handle};

// -------------------------------------------------------------------------------------------------

/// Allows finding all of the [`Handle`]s inside a data structure.
///
/// Correct implementations of this trait are necessary for many functions of a
/// [`Universe`] to work correctly; failing to report a handle may result in it
/// being disconnected when it should not be.
pub trait VisitHandles {
    /// For each [`Handle`] contained within `self` that is reachable without traversing
    /// another [`Handle`], call `visitor` with a reference to it.
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor);
}

/// Callback used by [`VisitHandles::visit_handles`].
///
/// Note that this is automatically implemented for functions.
pub trait HandleVisitor {
    /// Called by a value which is responding to a [`VisitHandles::visit_handles()`] operation
    /// to report one of the handles it contains.
    fn visit(&mut self, r: &dyn ErasedHandle);
}

impl<F> HandleVisitor for F
where
    F: FnMut(&dyn ErasedHandle),
{
    fn visit(&mut self, h: &dyn ErasedHandle) {
        (*self)(h)
    }
}

impl<T: universe::UniverseMember> VisitHandles for Handle<T> {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        visitor.visit(self)
    }
}

impl<T: ?Sized + VisitHandles> VisitHandles for alloc::boxed::Box<T> {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        T::visit_handles(&**self, visitor);
    }
}
impl<T: ?Sized + VisitHandles> VisitHandles for alloc::sync::Arc<T> {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        T::visit_handles(&**self, visitor);
    }
}

impl<T: VisitHandles> VisitHandles for Option<T> {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        if let Some(element) = self {
            element.visit_handles(visitor);
        }
    }
}

impl<T: VisitHandles> VisitHandles for [T] {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        for element in self {
            element.visit_handles(visitor);
        }
    }
}

impl<T: VisitHandles, const N: usize> VisitHandles for [T; N] {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        for element in self {
            element.visit_handles(visitor);
        }
    }
}

// The following no-op VisitHandles implementations allow simpler code in BlockAttributes.
impl VisitHandles for arcstr::ArcStr {
    fn visit_handles(&self, _: &mut dyn HandleVisitor) {}
}
impl VisitHandles for bool {
    fn visit_handles(&self, _: &mut dyn HandleVisitor) {}
}

#[cfg(test)]
pub(crate) fn list_handles<T: VisitHandles + 'static>(target: &T) -> Vec<super::Name> {
    let mut names: Vec<super::Name> = Vec::new();
    target.visit_handles(&mut |r: &dyn ErasedHandle| names.push(r.name().clone()));
    names
}

// -------------------------------------------------------------------------------------------------

/// Registry of [`ecs::Component`] types that implement [`VisitHandles`].
/// Allows obtaining `&dyn VisitHandles` from each one.
///
/// Used by the garbage collector.
/// If components containing handles are not registered, they will not keep their referents alive.
//---
// Note: If we find ourselves wanting this for more traits, or with less unsafe code,
// try <https://docs.rs/bevy-trait-query/>.
#[derive(Default, ecs::Resource)]
pub(crate) struct VisitableComponents(
    hashbrown::HashMap<bevy_ecs::component::ComponentId, unsafe fn(Ptr<'_>) -> &dyn VisitHandles>,
);

impl VisitableComponents {
    pub(crate) fn register<C: ecs::Component + VisitHandles>(world: &mut ecs::World) {
        let component_id = world.register_component::<C>();
        world.get_resource_or_init::<Self>().0.insert(
            component_id,
            |ptr: Ptr<'_>| -> &dyn VisitHandles {
                // SAFETY: This function will only be called with pointers to `C`.
                unsafe { ptr.deref::<C>() }
            },
        );
    }

    pub(in crate::universe) fn visit_handles_in_entity<'w>(
        &self,
        entity: bevy_ecs::world::EntityRefExcept<'w, impl ecs::Bundle>,
    ) -> impl Iterator<Item = &'w dyn VisitHandles> {
        // TODO: should this accept a visitor instead as its name suggest,
        // or is this version with less code near the unsafe{} preferable?
        self.0.iter().filter_map(move |(&component_id, &downcaster)| {
            entity
                .get_by_id(component_id)
                // SAFETY: the component id ensures the component is of the correct type
                .map(|ptr| unsafe { downcaster(ptr) })
        })
    }
}

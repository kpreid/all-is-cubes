#[cfg(test)]
use alloc::vec::Vec;

#[cfg(doc)]
use crate::universe::Universe;
use crate::universe::{self, URef, URefErased};

/// Allows finding all of the [`URef`]s inside a data structure.
///
/// Correct implementations of this trait are necessary for many functions of a
/// [`Universe`] to work correctly; failing to report a reference may result in it
/// breaking.
pub trait VisitRefs {
    /// For each [`URef`] contained within `self` that is reachable without traversing
    /// another [`URef`], call `visitor` with a reference to it.
    fn visit_refs(&self, visitor: &mut dyn RefVisitor);
}

/// Callback used by [`VisitRefs::visit_refs`].
///
/// Note that this is automatically implemented for functions.
pub trait RefVisitor {
    /// Called by a value which is responding to a [`VisitRefs::visit_refs()`] operation
    /// to report one of the refs it contains.
    fn visit(&mut self, r: &dyn URefErased);
}

impl<F> RefVisitor for F
where
    F: FnMut(&dyn URefErased),
{
    fn visit(&mut self, r: &dyn URefErased) {
        (*self)(r)
    }
}

impl<T: universe::UniverseMember> VisitRefs for URef<T> {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        visitor.visit(self)
    }
}

impl<T: VisitRefs> VisitRefs for Option<T> {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        if let Some(element) = self {
            element.visit_refs(visitor);
        }
    }
}

impl<T: VisitRefs> VisitRefs for [T] {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        for element in self.iter() {
            element.visit_refs(visitor);
        }
    }
}

impl<T: VisitRefs, const N: usize> VisitRefs for [T; N] {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        for element in self {
            element.visit_refs(visitor);
        }
    }
}

#[cfg(test)]
pub(crate) fn list_refs<T: VisitRefs + 'static>(target: &T) -> Vec<super::Name> {
    let mut names: Vec<super::Name> = Vec::new();
    target.visit_refs(&mut |r: &dyn URefErased| names.push(r.name().clone()));
    names
}

use crate::universe::URefErased;
#[cfg(doc)]
use crate::universe::{URef, Universe};

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

impl<T: VisitRefs> VisitRefs for Vec<T> {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        for element in self {
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

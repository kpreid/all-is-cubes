// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

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
/// This is a trait rather than a function type so that it can be generic over `T` in
/// URef<T>`.
pub trait RefVisitor {
    fn visit(&mut self, r: &dyn URefErased);
}

/// A mutable reference to any [`RefVisitor`] may be used as one itself.
impl<V: RefVisitor> RefVisitor for &mut V {
    fn visit(&mut self, r: &dyn URefErased) {
        (*self).visit(r);
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
mod testers {
    use super::*;
    use crate::universe::{Name, URef};

    /// An implementation of `RefVisitor` for testing implementations of `VisitRefs`.
    /// It reports names and types, but also checks for consistency between [`RefVisitor`] and
    /// [`RefVisitorMut`].
    #[derive(Clone, Debug, Default, Eq, PartialEq)]
    pub(crate) struct ListRefs {
        // In principle we'd like to store the whole `URef`, but the `Name` is conveniently
        // monomorphic.
        names: Vec<Name>,
        names_as_mut: Vec<Name>,
    }

    impl ListRefs {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn list<T: VisitRefs + 'static>(target: &URef<T>) -> Vec<Name> {
            let mut visitor = Self::new();
            target.borrow().visit_refs(&mut visitor);
            visitor.names
        }
    }

    impl RefVisitor for ListRefs {
        fn visit(&mut self, r: &dyn URefErased) {
            self.names.push(r.name().clone());
        }
    }
}
#[cfg(test)]
pub(crate) use testers::*;

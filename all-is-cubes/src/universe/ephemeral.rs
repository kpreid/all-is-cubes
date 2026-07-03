use alloc::sync::Arc;
use core::fmt;
use core::hash;

use crate::universe;

// -------------------------------------------------------------------------------------------------

/// A wrapper around a value which cannot be printed, serialized, or compared.
///
/// This is used for special situations where extensibility is needed within a data structure that
/// is otherwise a closed set of possibilities.
///
/// [`EphemeralOpaque`]s are always compared by pointer identity.
//---
// TODO: Consider moving this to be a new kind of `UniverseMember`, so that they can
// be audited and maybe even reattached after deserialization?
pub struct EphemeralOpaque<T: ?Sized>(Option<Arc<T>>);

impl<T: ?Sized> EphemeralOpaque<T> {
    /// Constructs an [`EphemeralOpaque`] that holds the given value.
    //---
    // Note: This must accept the `Arc<T>` rather than the `T` to allow unsizing coercion.
    pub fn new(contents: Arc<T>) -> Self {
        Self(Some(contents))
    }

    /// Constructs an [`EphemeralOpaque`] that is already defunct (holds no value).
    pub fn defunct() -> Self {
        Self(None)
    }

    /// Get a reference to the value if it still exists.
    pub fn try_ref(&self) -> Option<&T> {
        self.0.as_deref()
    }

    /// Data used to implement `Eq` and `Hash`.
    fn identity(&self) -> *const () {
        match self.0 {
            // The cast discards the metadata, so equality is strictly based on distinct `Arc`
            // allocations and not on the pointer metadata, which might vary due to coercions.
            Some(ref arc) => Arc::as_ptr(arc).cast(),
            None => core::ptr::null(),
        }
    }
}

impl<T: ?Sized> From<Arc<T>> for EphemeralOpaque<T> {
    fn from(contents: Arc<T>) -> Self {
        Self(Some(contents))
    }
}
impl<T> From<T> for EphemeralOpaque<T> {
    fn from(contents: T) -> Self {
        Self(Some(Arc::new(contents)))
    }
}

impl<T: ?Sized> fmt::Debug for EphemeralOpaque<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EphemeralOpaque(..)")
    }
}
impl<T: ?Sized> PartialEq for EphemeralOpaque<T> {
    fn eq(&self, other: &Self) -> bool {
        self.identity() == other.identity()
    }
}
impl<T: ?Sized> Eq for EphemeralOpaque<T> {}

impl<T: ?Sized> Clone for EphemeralOpaque<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<T: ?Sized> hash::Hash for EphemeralOpaque<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.identity().hash(state)
    }
}

impl<T: ?Sized> universe::VisitHandles for EphemeralOpaque<T> {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        // Being opaque, an `EphemeralOpaque` doesn’t count as containing any handles.
        // In the future, we might replace it with something that *does* constitute a handle
        // to a special “external connection” entity, and if we do that, this will change.
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a, T: arbitrary::Arbitrary<'a>> arbitrary::Arbitrary<'a> for EphemeralOpaque<T> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(EphemeralOpaque(if u.arbitrary()? {
            Some(Arc::new(u.arbitrary()?))
        } else {
            None
        }))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        Self::try_size_hint(depth).unwrap_or_default()
    }
    fn try_size_hint(
        depth: usize,
    ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        use arbitrary::{Arbitrary, size_hint};
        size_hint::try_recursion_guard(depth, |depth| {
            Ok(size_hint::and(
                <bool as Arbitrary>::size_hint(depth),
                <T as Arbitrary>::try_size_hint(depth)?,
            ))
        })
    }
}

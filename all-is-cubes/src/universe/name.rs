use alloc::string::String;
use core::fmt;

use arcstr::ArcStr;

use crate::universe::Builtin;

#[cfg(doc)]
use crate::universe::{Handle, Universe};

// -------------------------------------------------------------------------------------------------

/// Name of an object in a [`Universe`].
///
/// Unlike a [`Handle`], a [`Name`] is plain data and the same name could refer to different members
/// of different universes; or to different members of the same universe if one was deleted, freeing
/// up the name for reuse.
#[doc = include_str!("../save/serde-warning.md")]
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub enum Name {
    /// An explicitly set user-defined name.
    Specific(ArcStr),

    /// An automatically assigned name.
    ///
    /// This numbering is unique within a given [`Universe`].
    Anonym(usize),

    /// Not yet been assigned a name; this may later be replaced with `Anonym` (but not `Specific`).
    ///
    /// This name is always replaced at the moment of insertion in the [`Universe`].
    Pending,

    /// Refers to an immutable object defined in code which acts as if it is a member of every
    /// [`Universe`].
    Builtin(Builtin),
}

impl Name {
    /// Returns whether universe members with this name should be counted as GC roots
    /// (not deleted even if unreferenced).
    pub(in crate::universe) fn is_gc_root(&self) -> bool {
        match self {
            Name::Specific(_) => true,
            Name::Anonym(_) => false,
            Name::Builtin(_) | Name::Pending => {
                unreachable!("inconsistency: name of type {self} should not be subject to GC")
            }
        }
    }
}

impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self::Specific(value.into())
    }
}

impl From<String> for Name {
    fn from(value: String) -> Self {
        Self::Specific(value.into())
    }
}

impl From<ArcStr> for Name {
    fn from(value: ArcStr) -> Self {
        Self::Specific(value)
    }
}

impl From<Builtin> for Name {
    fn from(value: Builtin) -> Self {
        Name::Builtin(value)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Specific(name) => write!(f, "'{name}'"),
            Name::Anonym(index) => write!(f, "[anonymous #{index}]"),
            Name::Pending => write!(f, "[pending anonymous]"),
            Name::Builtin(name) => write!(f, "builtin '{name}'"),
        }
    }
}

// Manual impl because `ArcStr` doesn't impl Arbitrary.
#[cfg(feature = "arbitrary")]
mod impl_arbitrary {
    use super::*;

    #[derive(arbitrary::Arbitrary)]
    enum ArbName {
        Specific(String),
        Anonym(usize),
        Pending,
        Builtin(Builtin),
    }

    impl<'a> arbitrary::Arbitrary<'a> for Name {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            let input = ArbName::arbitrary(u)?;
            let value = match input {
                ArbName::Specific(name) => Name::Specific(name.into()),
                ArbName::Anonym(index) => Name::Anonym(index),
                ArbName::Pending => Name::Pending,
                ArbName::Builtin(builtin) => Name::Builtin(builtin),
            };
            if false {
                // This non-executed code proves ArbName has as many variants as Name
                let _ = match value {
                    Name::Specific(name) => ArbName::Specific(name.to_string()),
                    Name::Anonym(index) => ArbName::Anonym(index),
                    Name::Pending => ArbName::Pending,
                    Name::Builtin(name) => ArbName::Builtin(name),
                };
                unreachable!()
            } else {
                Ok(value)
            }
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            ArbName::size_hint(depth)
        }
        fn try_size_hint(
            depth: usize,
        ) -> arbitrary::Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
            ArbName::try_size_hint(depth)
        }
    }
}

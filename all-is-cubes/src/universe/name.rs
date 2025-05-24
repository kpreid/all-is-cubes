#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
use alloc::string::String;
use core::fmt;

use arcstr::ArcStr;

#[cfg(doc)]
use crate::universe::{Handle, Universe};

// -------------------------------------------------------------------------------------------------

/// Name/key of an object in a [`Universe`].
///
/// Unlike a [`Handle`], a [`Name`] is plain data and the same name could refer to different members
/// of different universes.
#[doc = include_str!("../save/serde-warning.md")]
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub enum Name {
    /// An explicitly set name.
    Specific(ArcStr),

    /// An automatically assigned name.
    Anonym(usize),

    /// Not yet been assigned a name; this may be replaced with `Anonym` but not `Specific`.
    ///
    /// This name is always replaced at the moment of insertion in the [`Universe`].
    Pending,
}

impl Name {
    /// Returns whether universe members with this name should be counted as GC roots
    /// (not deleted even if unreferenced).
    pub(in crate::universe) fn is_gc_root(&self) -> bool {
        match self {
            Name::Specific(_) => true,
            Name::Anonym(_) => false,
            Name::Pending => unreachable!("inconsistency: Pending should not occur here"),
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

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Specific(name) => write!(f, "'{name}'"),
            Name::Anonym(index) => write!(f, "[anonymous #{index}]"),
            Name::Pending => write!(f, "[pending anonymous]"),
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
    }

    impl<'a> arbitrary::Arbitrary<'a> for Name {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            let input = ArbName::arbitrary(u)?;
            let value = match input {
                ArbName::Specific(name) => Name::Specific(name.into()),
                ArbName::Anonym(index) => Name::Anonym(index),
                ArbName::Pending => Name::Pending,
            };
            if false {
                // This non-executed code proves ArbName has as many variants as Name
                let _ = match value {
                    Name::Specific(name) => ArbName::Specific(name.to_string()),
                    Name::Anonym(index) => ArbName::Anonym(index),
                    Name::Pending => ArbName::Pending,
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

// -------------------------------------------------------------------------------------------------

pub(in crate::universe) type OnceName = MbOnceLock<Name>;

/// As `std::sync::OnceLock`, but implemented using `once_cell::race::OnceBox` if necessary.
/// Implemented generically because it might be useful for more things later.
#[derive(Debug)]
pub(in crate::universe) struct MbOnceLock<T>(
    #[cfg(feature = "std")] std::sync::OnceLock<T>,
    #[cfg(not(feature = "std"))] once_cell::race::OnceBox<T>, // OnceBox has extra boxing, so only use it if necessary
);

impl<T> MbOnceLock<T> {
    pub fn new() -> Self {
        Self(Default::default())
    }

    pub fn get(&self) -> Option<&T> {
        self.0.get()
    }

    pub fn set(&self, value: T) -> Result<(), T> {
        #[cfg(feature = "std")]
        self.0.set(value)?;
        #[cfg(not(feature = "std"))]
        self.0.set(Box::new(value)).map_err(|boxed| *boxed)?;

        Ok(())
    }

    pub fn from_optional_value(option: Option<T>) -> Self {
        match option {
            #[cfg(feature = "std")]
            Some(value) => Self(std::sync::OnceLock::from(value)),
            #[cfg(not(feature = "std"))]
            Some(value) => Self(once_cell::race::OnceBox::with_value(Box::new(value))),

            None => Self::new(),
        }
    }
}

use alloc::boxed::Box;
use alloc::string::String;
use core::fmt;

use bevy_platform::sync::LazyLock;

use crate::block::{self, BlockDef};
use crate::universe::{self, AnyPending, Handle, ReadTicket};

// -------------------------------------------------------------------------------------------------

macro_rules! derive_for_builtin_name {
    (
        $(#[$ignored_attr:meta])*
        pub enum Builtin {
            $(
                $( #[doc = $($doc:tt)*] )*
                $( #[doc($($doc_modifier:tt)*)] )*
                #[custom(
                    string = $string:literal,
                    value: $member_ty:ident = $value_expr:expr,
                )]
                $variant:ident,
            )*
        }
    ) => {

        impl Builtin {
            // Used for serialization.
            // If we decide to make this public, it should be `Into<&'static str>` and `AsRef<str>`.
            #[cfg_attr(not(feature = "save"), allow(dead_code))]
            pub(crate) fn static_string(self) -> &'static str {
                match self {
                    $( Self::$variant => $string, )*
                }
            }

            /// Obtains the type-erased handle and value for this builtin,
            /// initializing them if not already done.
            pub(in crate::universe) fn get(self) -> &'static AnyPending {
                match self {
                    $( Self::$variant => {
                        // TODO: `AnyPending` is not ideal here because it has the `Option`ality.
                        // Also, we could store this in a non-type-erased form.
                        static DATA: LazyLock<AnyPending> = LazyLock::new(|| {
                            AnyPending::$member_ty {
                                handle: Handle::new_for_builtin_initialization(Builtin::$variant),
                                value: Some(Box::new($value_expr)),
                            }
                        });
                        &*DATA
                    } )*
                }
            }
        }

        /// # Builtin handle getters
        ///
        /// Each of the following associated functions returns the [`Handle`] for a specific
        /// [`Builtin`]. It returns the same handle as [`Builtin::erased_handle()`],
        /// but with the exact type statically defined.
        impl Builtin {
            paste::paste! {
                $(
                    #[doc = concat!(
                        "Returns the handle for [`Builtin::", stringify!($variant), "`]."
                    )]
                    $( #[doc($($doc_modifier)*)] )*
                    pub fn [< $variant:snake:lower >]() -> &'static Handle<$member_ty> {
                        (Builtin::$variant).erased_handle().try_into().unwrap()
                    }
                )*
            }
        }

        impl fmt::Display for Builtin {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.pad(match *self {
                    $(
                        Self::$variant => $string,
                    )*
                })
            }
        }

        impl core::str::FromStr for Builtin {
            type Err = UnknownBuiltin;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $(
                        $string => Ok(Self::$variant),
                    )*
                    _ => Err(UnknownBuiltin(String::from(s))),
                }
            }
        }

    }
}

// Putting this impl block before the macro invocation, even though this puts it above the
// struct declaration, is done in order to adjust the order in rustdoc,
// putting these above the numerous macro-generated functions.
impl Builtin {
    // /// Returns the [`Handle`] to this builtin, assuming that it is of type `Handle<T>`.
    // ///
    // /// # Panics
    // ///
    // /// Panics if the builtin’s type is not `T`.
    // ///
    // /// Note that each builtin has a corresponding function to retrieve its handle.
    // /// Consider using those instead of this, as they cannot panic.
    // pub fn handle<T: universe::UniverseMember>(self) -> &'static Handle<T> {
    //     self.erased_handle()
    //         .try_into()
    //         .unwrap_or_else(|error| panic!("type mismatch in Builtin::handle(): {error}"))
    // }

    /// Returns the type-erased [`Handle`] to this builtin.
    pub fn erased_handle(self) -> &'static dyn universe::ErasedHandle {
        self.get().handle()
    }
}

#[macro_rules_attribute::derive(derive_for_builtin_name!)]
/// Name of an immutable object defined by All is Cubes.
///
/// [`Handle`]s to builtins act as if they exist in all universes.
/// To obtain such a handle, call the [function for the specific builtin](#builtin-handle-getters),
/// or [`Builtin::erased_handle()`] for dynamic lookups.
#[derive(Clone, Copy, Debug, Hash, Eq, Ord, PartialEq, PartialOrd, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Builtin {
    /// A copy of `Primitive::Air`.
    ///
    /// This hidden variant exists to prototype the builtins system.
    /// One of the following should occur:
    ///
    /// * Replace this with other, more valuable builtins.
    /// * `Primitive::Air` should be entirely replaced by a `Primitive::Indirect` builtin
    ///   pointing to a `Primitive::Atom`.
    /// * Delete the builtins system.
    #[doc(hidden)]
    #[custom(
        string = "air",
        value: BlockDef = BlockDef::new(ReadTicket::stub(), block::AIR),
    )]
    Air,
}

impl<T: universe::UniverseMember> TryFrom<Builtin> for &Handle<T> {
    type Error = universe::TypeError;

    /// Returns the builtin’s handle, if `T` is its type.
    ///
    /// This function is identical to calling [`Builtin::erased_handle()`] and
    /// converting the result.
    fn try_from(builtin: Builtin) -> Result<Self, Self::Error> {
        builtin.erased_handle().try_into()
    }
}

impl<T: universe::UniverseMember> TryFrom<Builtin> for Handle<T> {
    type Error = universe::TypeError;

    /// Returns the builtin’s handle, if `T` is its type.
    ///
    /// This function is identical to calling [`Builtin::erased_handle()`] and
    /// converting the result.
    fn try_from(builtin: Builtin) -> Result<Self, Self::Error> {
        builtin.erased_handle().try_into()
    }
}

impl From<Builtin> for universe::AnyHandle {
    /// Returns a type-erased, owned [`Handle`] to this builtin.
    ///
    /// This function is identical to calling [`Builtin::erased_handle()`] and
    /// converting the result.
    fn from(builtin: Builtin) -> Self {
        builtin.erased_handle().into()
    }
}

impl From<Builtin> for &dyn universe::ErasedHandle {
    /// Returns a type-erased, borrowed [`Handle`] to this builtin.
    ///
    /// This function is identical to calling [`Builtin::erased_handle()`].
    fn from(builtin: Builtin) -> Self {
        builtin.erased_handle()
    }
}

// -------------------------------------------------------------------------------------------------

/// Error from parsing a string as a [`Builtin`].
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct UnknownBuiltin(pub String);

impl fmt::Display for UnknownBuiltin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unknown builtin '{}'", self.0)
    }
}

impl core::error::Error for UnknownBuiltin {}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::ToString as _;
    use exhaust::Exhaust as _;

    #[test]
    fn basic_usage() {
        let handle: &Handle<BlockDef> = Builtin::air();
        assert_eq!(
            *handle.read(ReadTicket::stub()).unwrap().block(),
            block::AIR
        );
    }

    #[test]
    fn handle_equality() {
        assert_eq!(Builtin::air(), Builtin::Air.erased_handle());
        assert_eq!(Builtin::Air.erased_handle(), Builtin::Air.erased_handle());
        assert_eq!(Builtin::air(), Builtin::air());

        // Test non-equality
        assert_ne!(Builtin::Air.erased_handle(), Builtin::Beep.erased_handle());
    }

    #[test]
    fn string_conversions() {
        let name = Builtin::Air;
        let string = "air";

        assert_eq!(name.static_string(), string);
        assert_eq!(name.to_string(), string);
        assert_eq!(string.parse::<Builtin>().unwrap(), name);
    }

    #[test]
    fn names_do_not_overlap() {
        let mut table = hashbrown::HashMap::new();
        for name in Builtin::exhaust() {
            let string = name.static_string();
            if let Some(other_name) = table.insert(string, name) {
                panic!(
                    "\
name collision between {name:?} ({string:?})
                   and {other_name:?} ({other_string:?})\
                    ",
                    other_string = other_name.static_string()
                );
            }
        }
    }

    #[test]
    fn unknown_builtin_error() {
        let error = "nonexistent".parse::<Builtin>().unwrap_err();

        assert_eq!(error, UnknownBuiltin(String::from("nonexistent")));
        assert_eq!(error.to_string(), "unknown builtin 'nonexistent'");
    }
}

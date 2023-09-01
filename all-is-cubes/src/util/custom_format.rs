use core::fmt;
use core::marker::PhantomData;
use core::time::Duration;

/// Generic extension to [`core::fmt`'s set of formatting traits](core::fmt#formatting-traits).
///
/// This can be thought of as a mechanism to easily create a new special-purpose
/// formatting trait, analogous to [`fmt::LowerHex`] or [`fmt::Pointer`].
/// Instead of implementing the entire necessary wrapper and setup code, implementations
/// need only be types representing the choice of formatting (e.g. [`ConciseDebug`]),
/// and [`CustomFormat::custom_format`] provides a wrapper which may be used to cause
/// a value implementing `CustomFormat<T>` to be formatted using
/// [`CustomFormat<T>::fmt`](Self::fmt).
pub trait CustomFormat<F: Copy> {
    /// Wrap this value so that when formatted with [`fmt::Debug`] or [`fmt::Display`], it uses
    /// the given custom format instead.
    fn custom_format(&self, format_type: F) -> CustomFormatWrapper<'_, F, Self> {
        CustomFormatWrapper(format_type, self)
    }

    /// Implement this to provide custom formatting for this type.
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: F) -> fmt::Result;
}

/// You can use [`CustomFormat::custom_format`] to construct this.
/// See its documentation.
///
/// To enable using the wrapper inside [`assert_eq`], it implements [`PartialEq`]
/// (comparing both value and format).
#[derive(Eq, PartialEq)]
pub struct CustomFormatWrapper<'a, F: Copy, T: CustomFormat<F> + ?Sized>(F, &'a T);
impl<'a, F: Copy, T: CustomFormat<F>> fmt::Debug for CustomFormatWrapper<'a, F, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as CustomFormat<F>>::fmt(self.1, fmt, self.0)
    }
}
impl<'a, F: Copy, T: CustomFormat<F>> fmt::Display for CustomFormatWrapper<'a, F, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as CustomFormat<F>>::fmt(self.1, fmt, self.0)
    }
}

impl<F: Copy, T: CustomFormat<F>> CustomFormat<F> for &'_ T {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: F) -> fmt::Result {
        <T as CustomFormat<F>>::fmt(&**self, fmt, format_type)
    }
}

/// Format type for [`CustomFormat`] which forces a string to be unquoted when [`fmt::Debug`]ged.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub(crate) struct Unquote;
impl CustomFormat<Unquote> for String {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: Unquote) -> fmt::Result {
        write!(fmt, "{self}")
    }
}
impl CustomFormat<Unquote> for &'_ str {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: Unquote) -> fmt::Result {
        write!(fmt, "{self}")
    }
}

/// Format type for [`CustomFormat`] which prints the name of a type.
/// The value is a `PhantomData` to avoid requiring an actual instance of the type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct TypeName;
impl<T> CustomFormat<TypeName> for PhantomData<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: TypeName) -> fmt::Result {
        write!(fmt, "{}", core::any::type_name::<T>())
    }
}

/// Format type for [`CustomFormat`] which is similar to [`fmt::Debug`], but uses an
/// alternate concise format.
///
/// This format may be on one line despite the pretty-printing option, and may lose
/// precision or Rust syntax in favor of a short at-a-glance representation.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ConciseDebug;

impl<T: CustomFormat<ConciseDebug>, const N: usize> CustomFormat<ConciseDebug> for [T; N] {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: ConciseDebug) -> fmt::Result {
        fmt.debug_list()
            .entries(self.iter().map(|item| item.custom_format(format_type)))
            .finish()
    }
}

impl<T: fmt::Debug, U> CustomFormat<ConciseDebug> for euclid::Point2D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?})", self.x, self.y)
    }
}
impl<T: fmt::Debug, U> CustomFormat<ConciseDebug> for euclid::Point3D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}
impl<T: fmt::Debug, U> CustomFormat<ConciseDebug> for euclid::Vector2D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?})", self.x, self.y)
    }
}
impl<T: fmt::Debug, U> CustomFormat<ConciseDebug> for euclid::Vector3D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}

/// Format type for [`CustomFormat`] which provides an highly condensed, ideally
/// constant-size, user-facing format for live-updating textual status messages.
/// This format does not follow Rust [`fmt::Debug`] syntax, and when implemented
/// for standard Rust types may have quirks. Values may have multiple lines.
#[allow(clippy::exhaustive_structs)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct StatusText;

/// Makes the assumption that [`Duration`]s are per-frame timings and hence the
/// interesting precision is in the millisecond-to-microsecond range.
impl CustomFormat<StatusText> for Duration {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(fmt, "{:5.2?} ms", (self.as_micros() as f32) / 1000.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_concise_debug() {
        #[derive(Debug)]
        struct Foo;
        impl CustomFormat<ConciseDebug> for Foo {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
                write!(fmt, "<Foo>")
            }
        }
        assert_eq!("Foo", format!("{Foo:?}"));
        assert_eq!("<Foo>", format!("{:?}", Foo.custom_format(ConciseDebug)));
    }
}

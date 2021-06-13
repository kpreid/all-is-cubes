// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Tools that we could imagine being in the Rust standard library, but aren't.

use cgmath::{Matrix4, Point3, Vector2, Vector3, Vector4};
use std::fmt::{self, Debug, Display};
use std::marker::PhantomData;
use std::time::Duration;

/// Objects for which alternate textual representations can be generated.
/// These are analogous to [`Display`] and [`Debug`], but have additional options.
pub trait CustomFormat<F: Copy> {
    /// Wrap this value so that when formatted with [`Debug`] or [`Display`] it uses
    /// the given custom format instead.
    fn custom_format(&self, format_type: F) -> CustomFormatWrapper<'_, F, Self> {
        CustomFormatWrapper(format_type, self)
    }

    /// Implement this to provide custom formatting for this type.
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: F) -> fmt::Result;
}

/// You can use [`CustomFormat::custom_format`] to construct this.
/// See its documentation.
pub struct CustomFormatWrapper<'a, F: Copy, T: CustomFormat<F> + ?Sized>(F, &'a T);
impl<'a, F: Copy, T: CustomFormat<F>> Debug for CustomFormatWrapper<'a, F, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as CustomFormat<F>>::fmt(self.1, fmt, self.0)
    }
}
impl<'a, F: Copy, T: CustomFormat<F>> Display for CustomFormatWrapper<'a, F, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as CustomFormat<F>>::fmt(self.1, fmt, self.0)
    }
}

/// Format type for [`CustomFormat`] which prints the name of a type.
/// The value is a `PhantomData` to avoid requiring an actual instance of the type.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub(crate) struct TypeName;
impl<T> CustomFormat<TypeName> for PhantomData<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: TypeName) -> fmt::Result {
        write!(fmt, "{}", std::any::type_name::<T>())
    }
}

/// Format type for [`CustomFormat`] which is similar to [`Debug`], but uses an
/// alternate concise format.
///
/// This format may be on one line despite the pretty-printing option, and may lose
/// precision or Rust syntax in favor of a short at-a-glance representation.
#[allow(clippy::exhaustive_structs)]
#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub struct ConciseDebug;

// TODO: Macro time?
impl<S: Debug> CustomFormat<ConciseDebug> for Point3<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}

impl<S: Debug> CustomFormat<ConciseDebug> for Matrix4<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(
            fmt,
            "\n[{:?},\n {:?},\n {:?},\n {:?}]",
            self.x.custom_format(ConciseDebug),
            self.y.custom_format(ConciseDebug),
            self.z.custom_format(ConciseDebug),
            self.w.custom_format(ConciseDebug)
        )
    }
}

impl<S: Debug> CustomFormat<ConciseDebug> for Vector2<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?})", self.x, self.y)
    }
}
impl<S: Debug> CustomFormat<ConciseDebug> for Vector3<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}
impl<S: Debug> CustomFormat<ConciseDebug> for Vector4<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(
            fmt,
            "({:+.3?}, {:+.3?}, {:+.3?}, {:+.3?})",
            self.x, self.y, self.z, self.w
        )
    }
}

/// Format type for [`CustomFormat`] which provides an highly condensed, ideally
/// constant-size, user-facing format for live-updating textual status messages.
/// This format does not follow Rust [`Debug`](fmt::Debug) syntax, and when implemented
/// for standard Rust types may have quirks. Values may have multiple lines.
#[allow(clippy::exhaustive_structs)]
#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub struct StatusText;

/// Makes the assumption that [`Duration`]s are per-frame timings and hence the
/// interesting precision is in the millisecond-to-microsecond range.
impl CustomFormat<StatusText> for Duration {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(fmt, "{:5.2?} ms", (self.as_micros() as f32) / 1000.0)
    }
}

/// Equivalent of [`Iterator::map`] but applied to an [`Extend`] instead, transforming
/// the incoming elements.
pub(crate) struct MapExtend<'a, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
    target: &'a mut T,
    function: F,
    _input: PhantomData<fn(A)>,
}

impl<'a, A, B, T, F> MapExtend<'a, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
    pub(crate) fn new(target: &'a mut T, function: F) -> Self {
        Self {
            target,
            function,
            _input: PhantomData,
        }
    }
}

impl<'a, A, B, T, F> Extend<A> for MapExtend<'a, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = A>,
    {
        self.target.extend(iter.into_iter().map(&self.function));
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
        assert_eq!("Foo", format!("{:?}", Foo));
        assert_eq!("<Foo>", format!("{:?}", Foo.custom_format(ConciseDebug)));
    }
}

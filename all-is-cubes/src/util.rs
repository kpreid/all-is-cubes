// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Dumping ground for stuff that hasn't gotten big enough to put in a well-named module yet.
//!
//! also somewhat themed around “things that we could imagine being in the standard library”.

use cgmath::{Matrix4, Point3, Vector2, Vector3, Vector4};
use std::fmt;

/// Objects for which alternate `Debug` representations can be generated.
pub trait ConciseDebug: Sized {
    // TODO: Can we not require Sized?

    /// Wrap this value to provide an alternate concise format. This format
    /// may be on one line despite the pretty-printing option, and may lose
    /// precision or Rust syntax in favor of a short at-a-glance representation.
    fn as_concise_debug(&self) -> ConciseDebugWrapper<'_, Self> {
        ConciseDebugWrapper(self)
    }

    /// Implement this to provide ConciseDebug formatting for this type.
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result;
}

/// You can use `ConciseDebug::as_concise_debug` to construct this. See its documentation.
pub struct ConciseDebugWrapper<'a, T: ConciseDebug>(&'a T);

impl<'a, T: ConciseDebug> fmt::Debug for ConciseDebugWrapper<'a, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        <T as ConciseDebug>::fmt(self.0, fmt)
    }
}

// TODO: Macro time?
impl<S: fmt::Debug> ConciseDebug for Point3<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}

impl<S: fmt::Debug> ConciseDebug for Matrix4<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "\n[{:?},\n {:?},\n {:?},\n {:?}]",
            self.x.as_concise_debug(),
            self.y.as_concise_debug(),
            self.z.as_concise_debug(),
            self.w.as_concise_debug()
        )
    }
}

impl<S: fmt::Debug> ConciseDebug for Vector2<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?})", self.x, self.y)
    }
}
impl<S: fmt::Debug> ConciseDebug for Vector3<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}
impl<S: fmt::Debug> ConciseDebug for Vector4<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "({:+.3?}, {:+.3?}, {:+.3?}, {:+.3?})",
            self.x, self.y, self.z, self.w
        )
    }
}

/// A type for "return a result or errors, with possible warnings".
///
/// In order to preserve compatibility with the `?` operator and other idioms,
/// it is an alias for `Result` instead of a new type.
///
/// Use the trait `Warnings` for helpful methods.
pub type WarningsResult<T, E, W> = Result<(T, Vec<W>), (E, Vec<W>)>;

/// Helper trait to provide methods for the `WarningsResult` type.
pub trait Warnings: Sized {
    type T;
    type E;
    type W;
    fn split_warnings(self) -> (Vec<Self::W>, Result<Self::T, Self::E>);

    /// Call the `handler` with all warnings and return a `Result` stripped of the warnings.
    fn handle_warnings<H>(self, handler: H) -> Result<Self::T, Self::E>
    where
        H: Fn(Self::W),
    {
        let (warnings, r) = self.split_warnings();
        for warning in warnings {
            handler(warning);
        }
        r
    }
}

impl<T, E, W> Warnings for WarningsResult<T, E, W> {
    type T = T;
    type E = E;
    type W = W;
    fn split_warnings(self) -> (Vec<W>, Result<T, E>) {
        match self {
            Ok((v, w)) => (w, Ok(v)),
            Err((v, w)) => (w, Err(v)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_concise_debug() {
        #[derive(Debug)]
        struct Foo;
        impl ConciseDebug for Foo {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                write!(fmt, "<Foo>")
            }
        }
        assert_eq!("Foo", format!("{:?}", Foo));
        assert_eq!("<Foo>", format!("{:?}", Foo.as_concise_debug()));
    }
}

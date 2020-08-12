// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Dumping ground for stuff that hasn't gotten big enough to put in a well-named module yet.

use cgmath::{Point3, Matrix4, Vector3, Vector4};
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
        write!(fmt, "\n[{:?},\n {:?},\n {:?},\n {:?}]",
            self.x.as_concise_debug(),
            self.y.as_concise_debug(),
            self.z.as_concise_debug(),
            self.w.as_concise_debug())
    }
}

impl<S: fmt::Debug> ConciseDebug for Vector3<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}

impl<S: fmt::Debug> ConciseDebug for Vector4<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z, self.w)
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

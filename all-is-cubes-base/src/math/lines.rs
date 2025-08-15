//! Wireframe representations of shapes for debugging.

use crate::math::{FreePoint, Rgba};
use crate::util::MapExtend;

/// Represent objects as line drawings, or wireframes.
pub trait Wireframe {
    /// Represent this object as a line drawing, or wireframe, by producing lines to be drawn.
    ///
    /// The generated points should be in pairs, each pair defining a line segment.
    /// If there are an odd number of vertices generated, the caller should ignore the last.
    ///
    /// Design note: This method accepts a destination to write to, rather than returning an
    /// iterator, because if it did return an iterator, it would be difficult to compose in
    /// ways like allocating a temporary `Wireframe` and delegating to that, if it borrowed
    /// its input, and would risk composing a very large yet unnecessary iterator struct
    /// if it owned its input.
    /// This way, composition is simply calling further functions.
    ///
    /// (If Rust gains stable [generator coroutines], we might be able to revisit that decision.)
    ///
    /// [generator coroutines]: https://doc.rust-lang.org/std/iter/macro.iter.html
    fn wireframe_points<E: Extend<Vertex>>(&self, output: &mut E);
}

impl<T: Wireframe> Wireframe for Option<T> {
    #[allow(clippy::missing_inline_in_public_items)]
    fn wireframe_points<E: Extend<Vertex>>(&self, output: &mut E) {
        if let Some(value) = self {
            value.wireframe_points(output)
        }
    }
}

/// One end of a line to be drawn.
///
/// These are the output of [`Wireframe::wireframe_points()`].
#[derive(Clone, Copy, Debug, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct Vertex {
    /// Position of the vertex.
    pub position: FreePoint,

    /// Color in which to draw the line.
    ///
    /// If [`None`], a color set by the context/parent should be used instead.
    ///
    /// If the ends of a line are different colors, color should be interpolated along
    /// the line.
    pub color: Option<Rgba>,
}

impl From<FreePoint> for Vertex {
    #[inline]
    fn from(position: FreePoint) -> Self {
        Self {
            position,
            color: None,
        }
    }
}

/// Add color to all vertices that don't have it.
#[inline]
pub fn colorize(output: &mut impl Extend<Vertex>, color: Rgba) -> impl Extend<Vertex> {
    MapExtend::new(output, move |mut vertex: Vertex| {
        vertex.color.get_or_insert(color);
        vertex
    })
}

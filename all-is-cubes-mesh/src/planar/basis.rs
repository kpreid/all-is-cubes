use core::cmp::Ordering;
use core::fmt;
use core::num::Wrapping;

use all_is_cubes::math::{Cube, Face, GridCoordinate, GridRotation};

use crate::planar::Vertex;

#[cfg(doc)]
use crate::{
    Analysis,
    planar::{Outliner, Triangulator},
};

// -------------------------------------------------------------------------------------------------

/// Wrapping arithmetic helps `compare_perp()` be simple
type WrappingVector3D = all_is_cubes::euclid::Vector3D<Wrapping<GridCoordinate>, Cube>;

/// Defines the coordinate system of the input to a [`Triangulator`] or [`Outliner`].
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Basis {
    /// Orientation of the face/plane being processed.
    pub(in crate::planar) face: Face,

    /// Direction along the plane in which we are receiving input vertices.
    /// Input vertices must be sorted by `sweep_direction.dot(vertex.position)`.
    pub(in crate::planar) sweep_direction: Face,

    /// A direction perpendicular to `self.face` and `self.sweep_direction`.
    ///
    /// Input vertices must be sorted by `perpendicular_direction.dot(vertex.position)`
    /// as a secondary key after `sweep_direction`.
    pub(in crate::planar) perpendicular_direction: Face,

    /// `perpendicular_direction` as a unit vector.
    /// Wrapping arithmetic helps `compare_perp()` compile to simple code.
    /// (We do not need to worry about actual wrapping because vertices are always in u8 range
    /// anyway.)
    pub(in crate::planar) perpendicular_vector: WrappingVector3D,

    /// Our normal coordinate system is understood as right-handed and in that system we build
    /// meshes that have counterclockwise triangle winding.
    ///
    /// If the coordinate system established by the sweep is mirrored (which it is, half the time),
    /// then this is true to tell us to flip the winding order.
    pub(in crate::planar) left_handed: bool,
}

impl fmt::Debug for Basis {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            face,
            sweep_direction,
            perpendicular_direction,
            perpendicular_vector: _, // redundant with perpendicular_direction
            left_handed,
        } = self;
        f.debug_struct("Basis")
            .field("face", &face)
            .field("sweep_direction", &sweep_direction)
            .field("perpendicular_direction", &perpendicular_direction)
            .field("left_handed", &left_handed)
            .finish()
    }
}

impl Basis {
    /// Value used as a placeholder in [`Triangulator`]s that are not currently in use.
    /// Its data is nonsense and it is never actually used.
    pub(crate) const DUMMY: Self = Self {
        face: Face::PX,
        sweep_direction: Face::PX,
        perpendicular_direction: Face::PX,
        perpendicular_vector: WrappingVector3D::new(Wrapping(0), Wrapping(0), Wrapping(0)),
        left_handed: false,
    };

    /// Constructs a [`Basis`].
    ///
    /// * `face` is the normal of the plane in which the polygon to be processed lies.
    ///   It affects the winding order of the output, not the interpretation of the input.
    /// * `sweep_direction` is a direction which must be the primary sort key of
    ///   the input vertices, and must be perpendicular to `face`.
    /// * `perpendicular_direction` is a direction which must be the secondary sort key of
    ///   the input vertices, and must be perpendicular to both `face` and `sweep_direction`.
    ///
    /// # Panics
    ///
    /// Panics if the three provided directions are not perpendicular.
    #[track_caller]
    pub const fn new(face: Face, sweep_direction: Face, perpendicular_direction: Face) -> Self {
        let left_handed = match GridRotation::try_from_basis_const([
            face,
            sweep_direction,
            perpendicular_direction,
        ]) {
            Some(rot) => rot.is_reflection(),
            None => panic!("directions provided to Basis must be orthogonal"),
        };

        Self {
            face,
            sweep_direction,
            perpendicular_direction,
            perpendicular_vector: perpendicular_direction.vector_const(
                Wrapping(-1),
                Wrapping(0),
                Wrapping(1),
            ),
            left_handed,
        }
    }

    /// Constructs a [`Basis`] from the ordering of vertex data.
    ///
    /// If using filtered vertex data from an [`Analysis`], pass [`Analysis::vertex_ordering()`]
    /// to this function to get the appropriate basis.
    ///
    /// (Currently, only one ordering of voxel data is supported across All is Cubes, and this
    /// function’s signature serves more to formally document that fact than to provide
    /// flexibility.)
    pub const fn from_ordering(face: Face, _ordering: all_is_cubes::math::ZMaj) -> Self {
        let (sweep_direction, perpendicular_direction) = match face {
            Face::NX => (Face::PY, Face::PZ),
            Face::NY => (Face::PX, Face::PZ),
            Face::NZ => (Face::PX, Face::PY),
            Face::PX => (Face::PY, Face::PZ),
            Face::PY => (Face::PX, Face::PZ),
            Face::PZ => (Face::PX, Face::PY),
        };
        Basis::new(face, sweep_direction, perpendicular_direction)
    }

    /// Returns the `face` direction this was constructed with.
    #[inline(always)]
    pub fn face(&self) -> Face {
        self.face
    }

    /// Returns the `sweep_direction` this was constructed with.
    #[inline(always)]
    pub fn sweep_direction(&self) -> Face {
        self.sweep_direction
    }

    /// Returns the `perpendicular_direction` this was constructed with.
    #[inline(always)]
    pub fn perpendicular_direction(&self) -> Face {
        self.perpendicular_direction
    }

    /// Compare two vertices’ positions along the direction perpendicular to the sweep.
    #[inline(always)]
    pub(crate) fn compare_perp(self, v1: &Vertex, v2: &Vertex) -> Ordering {
        // Wrapping arithmetic helps `compare_perp()` compile to simple code.
        // (We do not need to worry about actual wrapping because vertices are always in u8 range
        // anyway.)
        self.perpendicular_vector
            .dot(v1.position.to_vector().map(Wrapping))
            .cmp(&self.perpendicular_vector.dot(v2.position.to_vector().map(Wrapping)))
    }

    /// Returns the component of `vertex.position` on the `perpendicular_direction` axis.
    ///
    /// Note that this is not a dot product and never performs any arithmetic, unlike
    /// [`Self::compare_perp()`].
    #[inline(always)]
    pub(crate) fn perpendicular_coordinate_of(&self, vertex: &Vertex) -> GridCoordinate {
        vertex.position[self.perpendicular_direction.axis()]
    }

    /// Returns whether the winding order of the triangle is as it should be, *before* the
    /// [`Basis::emit()`] stage.
    ///
    /// Always returns `false` for degenerate triangles (ones where all vertices lie on one line
    /// and thus cover no area).
    ///
    /// # Explanation
    ///
    /// “Correct” always means counterclockwise wound in the (sweep right, perpendicular up)
    /// right-handed 2D coordinate system, regardless of the 3D handedness that results when the
    /// third `face` axis is included.
    /// For example, the triangle ABC in this diagram is correctly wound:
    ///
    /// ```text
    /// C
    /// |\     ↑ perpendicular ↑
    /// | \
    /// A--B   → sweep →
    /// ```
    ///
    /// Doing things this way allows us to avoid making each case of triangle emission in the
    /// algorithm handedness-aware; instead, if the desired output is left-handed, [`Basis::emit()`]
    /// reverses *all* triangles.
    ///
    /// Within the algorithm, `is_correct_winding()` is not used to make windings consistent, but
    /// rather to test for triangles that are inside-out because they are covering areas they should
    /// be avoiding.
    #[inline(always)]
    pub(super) fn is_correct_winding(self, triangle: [&Vertex; 3]) -> bool {
        // depending on handedness this might be negated
        let triangle_normal_by_cross_product = (triangle[1].position - triangle[0].position)
            .cross(triangle[2].position - triangle[0].position);

        let normal_dot_face = self.face.dot(triangle_normal_by_cross_product);

        // This is not a single case because we want to always return false for degenerate
        // triangles.
        //
        // (Note that because our coordinates are integers, there will be no rounding error.)
        if self.left_handed {
            normal_dot_face < 0
        } else {
            normal_dot_face > 0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::planar::testing::vertices_from_ascii_art;
    use std::dbg;

    #[test]
    fn winding() {
        // See the doc comment on `is_correct_winding()` for details on what makes these LH and RH
        let xy_rh_basis = Basis::new(Face::PZ, Face::PX, Face::PY);
        let xy_lh_basis = Basis::new(Face::NZ, Face::PX, Face::PY);
        let yx_rh_basis = Basis::new(Face::NZ, Face::PY, Face::PX);
        let yx_lh_basis = Basis::new(Face::PZ, Face::PY, Face::PX);
        assert_eq!(xy_rh_basis.left_handed, false, "xy_rh_basis");
        assert_eq!(xy_lh_basis.left_handed, true, "xy_lh_basis");
        assert_eq!(yx_rh_basis.left_handed, false, "yx_rh_basis");
        assert_eq!(yx_lh_basis.left_handed, true, "yx_lh_basis");

        let try_all = |triangle: [&Vertex; 3]| -> [bool; 4] {
            [
                xy_rh_basis.is_correct_winding(triangle),
                xy_lh_basis.is_correct_winding(triangle),
                yx_rh_basis.is_correct_winding(triangle),
                yx_lh_basis.is_correct_winding(triangle),
            ]
        };

        // Some vertices to make both degenerate and non-degenerate triangles
        let vertices = vertices_from_ascii_art([
            b"b  ", //
            b"   ", //
            b" c ", //
            b"   ", //
            b"a d", //
        ]);
        dbg!(&vertices);
        let [a, b, c, d] = &*vertices else {
            unreachable!()
        };

        assert_eq!(
            try_all([a, b, c]),
            [false, false, true, true],
            "clockwise in +x+y"
        );
        assert_eq!(
            try_all([a, c, b]),
            [true, true, false, false],
            "counterclockwise in +x+y"
        );
        assert_eq!(
            try_all([b, c, d]),
            [false, false, false, false],
            "degenerate"
        );
    }
}

use core::fmt;

use crate::math::{Face6, Rgb01};

/// Enumeration of the axes of three-dimensional space.
///
/// Can be used to infallibly index 3-component arrays and vectors.
///
/// See also:
///
/// * [`Face6`] specifies an axis and a direction on the axis.
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
// do after tests:#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
#[allow(missing_docs)]
pub enum Axis {
    X = 0,
    Y = 1,
    Z = 2,
}

impl Axis {
    /// All three axes in the standard order, [X, Y, Z].
    pub const ALL: [Self; 3] = [Self::X, Self::Y, Self::Z];

    /// Returns a standard color to denote this axis among the three axes.
    /// All colors have equal luminance.
    ///
    /// * X = red
    /// * Y = green
    /// * Z = blue
    #[mutants::skip]
    #[inline]
    pub fn color(self) -> Rgb01 {
        match self {
            Axis::X => Rgb01::UNIFORM_LUMINANCE_RED,
            Axis::Y => Rgb01::UNIFORM_LUMINANCE_GREEN,
            Axis::Z => Rgb01::UNIFORM_LUMINANCE_BLUE,
        }
    }

    /// Returns the [`Face6`] value which corresponds to the positive direction on this axis.
    #[inline]
    pub fn positive_face(self) -> Face6 {
        match self {
            Axis::X => Face6::PX,
            Axis::Y => Face6::PY,
            Axis::Z => Face6::PZ,
        }
    }

    /// Returns the [`Face6`] value which corresponds to the negative direction on this axis.
    #[inline]
    pub fn negative_face(self) -> Face6 {
        match self {
            Axis::X => Face6::NX,
            Axis::Y => Face6::NY,
            Axis::Z => Face6::NZ,
        }
    }

    /// Convert the axis to a number for indexing 3-element arrays.
    #[inline]
    pub const fn index(self) -> usize {
        self as usize
    }

    /// Maps X to Y, Y to Z, and Z to X.
    #[inline]
    #[must_use]
    pub const fn increment(self) -> Self {
        match self {
            Axis::X => Axis::Y,
            Axis::Y => Axis::Z,
            Axis::Z => Axis::X,
        }
    }

    /// Maps X to Z, Y to X, and Z to Y.
    #[inline]
    #[must_use]
    pub const fn decrement(self) -> Self {
        match self {
            Axis::X => Axis::Z,
            Axis::Y => Axis::X,
            Axis::Z => Axis::Y,
        }
    }
}

/// Format the axis as one of the strings "x", "y", or "z" (lowercase).
impl fmt::LowerHex for Axis {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Axis::X => "x",
            Axis::Y => "y",
            Axis::Z => "z",
        })
    }
}
/// Format the axis as one of the strings "X", "Y", or "Z" (uppercase).
impl fmt::UpperHex for Axis {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Axis::X => "X",
            Axis::Y => "Y",
            Axis::Z => "Z",
        })
    }
}

impl From<Axis> for u8 {
    #[inline]
    fn from(value: Axis) -> Self {
        value as u8
    }
}
impl From<Axis> for usize {
    #[inline]
    fn from(value: Axis) -> Self {
        value as usize
    }
}

mod impl_index_axis {
    use super::Axis;
    use core::ops;

    impl<T> ops::Index<Axis> for [T; 3] {
        type Output = T;

        #[inline]
        fn index(&self, index: Axis) -> &Self::Output {
            &self[index as usize]
        }
    }
    impl<T> ops::IndexMut<Axis> for [T; 3] {
        #[inline]
        fn index_mut(&mut self, index: Axis) -> &mut Self::Output {
            &mut self[index as usize]
        }
    }

    macro_rules! impl_xyz_e {
        ($x:ident $y:ident $z:ident, $($type:tt)*) => {
            impl<T, U> ops::Index<Axis> for $($type)*<T, U> {
                type Output = T;

                #[inline]
                fn index(&self, index: Axis) -> &Self::Output {
                    match index {
                        Axis::X => &self.$x,
                        Axis::Y => &self.$y,
                        Axis::Z => &self.$z,
                    }
                }
            }
            impl<T, U> ops::IndexMut<Axis> for $($type)*<T, U> {
                #[inline]
                fn index_mut(&mut self, index: Axis) -> &mut Self::Output {
                    match index {
                        Axis::X => &mut self.$x,
                        Axis::Y => &mut self.$y,
                        Axis::Z => &mut self.$z,
                    }
                }
            }
        };
    }
    impl_xyz_e!(x y z, euclid::Vector3D);
    impl_xyz_e!(x y z, euclid::Point3D);
    impl_xyz_e!(width height depth, euclid::Size3D);

    // `Cube` also has implementations like this, in its own module.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn axis_conversion() {
        assert_eq!(u8::from(Axis::X), 0);
        assert_eq!(u8::from(Axis::Y), 1);
        assert_eq!(u8::from(Axis::Z), 2);

        for axis in Axis::ALL {
            assert_eq!(usize::from(axis), usize::from(u8::from(axis)));
            assert_eq!(usize::from(axis), axis.index());
        }
    }

    #[test]
    fn axis_fmt() {
        use Axis::*;
        assert_eq!(
            format!("{X:x} {Y:x} {Z:x} {X:X} {Y:X} {Z:X}"),
            "x y z X Y Z"
        );
    }

    #[test]
    fn inc_dec_properties() {
        for axis in Axis::ALL {
            assert_ne!(axis, axis.increment());
            assert_ne!(axis, axis.decrement());
            assert_eq!(axis, axis.increment().decrement());
            assert_eq!(axis, axis.decrement().increment());
        }
    }
}

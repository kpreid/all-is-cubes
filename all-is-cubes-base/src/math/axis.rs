use core::fmt;

use crate::math::{Face, Rgb01};

/// Enumeration of the axes of three-dimensional space.
///
/// Can be used to infallibly index 3-component arrays and vectors.
///
/// See also:
///
/// * [`Face`] specifies an axis and a direction on the axis.
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, exhaust::Exhaust)]
#[exhaust(factory_is_self)]
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
    pub const fn color(self) -> Rgb01 {
        match self {
            Axis::X => Rgb01::UNIFORM_LUMINANCE_RED,
            Axis::Y => Rgb01::UNIFORM_LUMINANCE_GREEN,
            Axis::Z => Rgb01::UNIFORM_LUMINANCE_BLUE,
        }
    }

    /// Returns the [`Face`] value which corresponds to the positive direction on this axis.
    #[inline]
    pub const fn positive_face(self) -> Face {
        match self {
            Axis::X => Face::PX,
            Axis::Y => Face::PY,
            Axis::Z => Face::PZ,
        }
    }

    /// Returns the [`Face`] value which corresponds to the negative direction on this axis.
    #[inline]
    pub const fn negative_face(self) -> Face {
        match self {
            Axis::X => Face::NX,
            Axis::Y => Face::NY,
            Axis::Z => Face::NZ,
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

const impl From<Axis> for u8 {
    #[inline]
    fn from(value: Axis) -> Self {
        value as u8
    }
}
const impl From<Axis> for usize {
    #[inline]
    fn from(value: Axis) -> Self {
        value as usize
    }
}

mod impl_index_axis {
    use super::Axis;
    use crate::math;
    use core::ops;

    const impl<T> ops::Index<Axis> for [T; 3] {
        type Output = T;

        #[inline]
        fn index(&self, index: Axis) -> &Self::Output {
            &self[index as usize]
        }
    }
    const impl<T> ops::IndexMut<Axis> for [T; 3] {
        #[inline]
        fn index_mut(&mut self, index: Axis) -> &mut Self::Output {
            &mut self[index as usize]
        }
    }

    // TODO: Changing the below implementations to reinterpret the vector as an array instead of
    // `match`ing has notable positive and negative effects on performance. We should find a way
    // to get only the positive ones (in the worst case, by providing a wrapper type to choose
    // which style to use).

    macro_rules! impl_euclid_generic {
        ($x:ident $y:ident $z:ident, $($container_type:tt)*) => {
            const impl<T, U> ops::Index<Axis> for $($container_type)*<T, U> {
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
            const impl<T, U> ops::IndexMut<Axis> for $($container_type)*<T, U> {
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
    impl_euclid_generic!(x y z, euclid::Vector3D);
    impl_euclid_generic!(x y z, euclid::Point3D);
    impl_euclid_generic!(width height depth, euclid::Size3D);

    macro_rules! impl_concrete {
        ($x:ident $y:ident $z:ident, $container_type:ty, $field_type:ty) => {
            const impl ops::Index<Axis> for $container_type {
                type Output = $field_type;

                #[inline]
                fn index(&self, index: Axis) -> &Self::Output {
                    match index {
                        Axis::X => &self.$x,
                        Axis::Y => &self.$y,
                        Axis::Z => &self.$z,
                    }
                }
            }
            const impl ops::IndexMut<Axis> for $container_type {
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

    impl_concrete!(x y z, euclid::BoolVector3D, bool);
    impl_concrete!(x y z, math::Cube, math::GridCoordinate);
}

#[cfg(test)]
mod tests {
    use crate::math::Cube;

    use super::*;
    use alloc::format;
    use euclid::{UnknownUnit as Uu, bvec3, point3, size3, vec3};

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

    #[test]
    fn indexing_array() {
        let mut array = ['a', 'b', 'c'];
        assert_eq!(
            [array[Axis::Z], array[Axis::Y], array[Axis::X]],
            ['c', 'b', 'a']
        );

        array[Axis::X] = 'd';
        array[Axis::Y] = 'e';
        array[Axis::Z] = 'f';
        assert_eq!(array, ['d', 'e', 'f']);
    }

    #[test]
    fn indexing_vector() {
        let mut vector = vec3::<char, Uu>('a', 'b', 'c');
        assert_eq!(
            [vector[Axis::Z], vector[Axis::Y], vector[Axis::X]],
            ['c', 'b', 'a']
        );

        vector[Axis::X] = 'd';
        vector[Axis::Y] = 'e';
        vector[Axis::Z] = 'f';
        assert_eq!(vector, vec3('d', 'e', 'f'));
    }

    #[test]
    fn indexing_point() {
        let mut point = point3::<char, Uu>('a', 'b', 'c');
        assert_eq!(
            [point[Axis::Z], point[Axis::Y], point[Axis::X]],
            ['c', 'b', 'a']
        );

        point[Axis::X] = 'd';
        point[Axis::Y] = 'e';
        point[Axis::Z] = 'f';
        assert_eq!(point, point3('d', 'e', 'f'));
    }

    #[test]
    fn indexing_size() {
        let mut size = size3::<char, Uu>('a', 'b', 'c');
        assert_eq!(
            [size[Axis::Z], size[Axis::Y], size[Axis::X]],
            ['c', 'b', 'a']
        );

        size[Axis::X] = 'd';
        size[Axis::Y] = 'e';
        size[Axis::Z] = 'f';
        assert_eq!(size, size3('d', 'e', 'f'));
    }

    #[test]
    fn indexing_bool_vector() {
        let mut vector = bvec3(true, false, false);
        assert_eq!(
            [vector[Axis::Z], vector[Axis::Y], vector[Axis::X]],
            [false, false, true]
        );

        vector[Axis::X] = false;
        vector[Axis::Y] = true;
        assert_eq!(vector, bvec3(false, true, false));
        vector[Axis::Z] = true;
        assert_eq!(vector, bvec3(false, true, true));
    }

    #[test]
    fn indexing_cube() {
        let mut size = Cube::new(10, 20, 30);
        assert_eq!([size[Axis::Z], size[Axis::Y], size[Axis::X]], [30, 20, 10]);

        size[Axis::X] = 40;
        size[Axis::Y] = 50;
        size[Axis::Z] = 60;
        assert_eq!(size, Cube::new(40, 50, 60));
    }
}

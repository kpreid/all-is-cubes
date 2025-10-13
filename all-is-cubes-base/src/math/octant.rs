use core::{fmt, ops};

use euclid::{Vector3D, vec3};

use crate::math::{Cube, Face6, FreeVector, GridCoordinate, GridPoint};

// -------------------------------------------------------------------------------------------------

/// Identifies one of eight octants, or elements of a 2×2×2 cube.
///
/// Used with [`OctantMask`] and [`OctantMap`].
///
/// This enum's discriminants are not currently to be considered stable; they may be reordered.
//---
// TODO: Replace this with an enum.
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Eq, Hash, PartialEq, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[repr(u8)]
pub enum Octant {
    /// The -X, -Y, -Z octant.
    Nnn = 0,
    /// The -X, -Y, +Z octant.
    Nnp = 1,
    /// The -X, +Y, -Z octant.
    Npn = 2,
    /// The -X, +Y, +Z octant.
    Npp = 3,
    /// The +X, -Y, -Z octant.
    Pnn = 4,
    /// The +X, -Y, +Z octant.
    Pnp = 5,
    /// The +X, +Y, -Z octant.
    Ppn = 6,
    /// The +X, +Y, +Z octant.
    Ppp = 7,
}

impl Octant {
    /// All values of the enum.
    //---
    // Note: `OctantMap::iter()` depends on the ordering of this.
    pub const ALL: [Self; 8] = [
        Self::Nnn,
        Self::Nnp,
        Self::Npn,
        Self::Npp,
        Self::Pnn,
        Self::Pnp,
        Self::Ppn,
        Self::Ppp,
    ];

    #[inline]
    fn from_zmaj_index(index: u8) -> Self {
        match index {
            0 => Self::Nnn,
            1 => Self::Nnp,
            2 => Self::Npn,
            3 => Self::Npp,
            4 => Self::Pnn,
            5 => Self::Pnp,
            6 => Self::Ppn,
            7 => Self::Ppp,
            _ => panic!("Octant::from_zmaj_index({index}) is out of bounds"),
        }
    }

    /// Given a cube in the volume (0..2)³, return which octant of that volume it is.
    #[inline]
    pub fn try_from_positive_cube(cube: Cube) -> Option<Self> {
        match <[i32; 3]>::from(cube) {
            [0, 0, 0] => Some(Self::Nnn),
            [0, 0, 1] => Some(Self::Nnp),
            [0, 1, 0] => Some(Self::Npn),
            [0, 1, 1] => Some(Self::Npp),
            [1, 0, 0] => Some(Self::Pnn),
            [1, 0, 1] => Some(Self::Pnp),
            [1, 1, 0] => Some(Self::Ppn),
            [1, 1, 1] => Some(Self::Ppp),
            _ => None,
        }
    }

    /// Given the low corner of an octant in the volume (0..2)³,
    /// return which octant it is.
    ///
    /// That is, each coordinate of the returned [`Vector3D`] is either 0 or 1.
    /// This is equivalent to [`Self::try_from_positive_cube()`] but with more flexible input.
    ///
    /// TODO: better trait bounds would be `Zero + One`
    #[inline]
    pub fn try_from_01<T: Copy + num_traits::NumCast, U>(
        corner_or_translation: Vector3D<T, U>,
    ) -> Option<Self> {
        let low_corner: GridPoint = corner_or_translation
            .try_cast::<GridCoordinate>()?
            .cast_unit()
            .to_point();
        Self::try_from_positive_cube(Cube::from(low_corner))
    }

    const fn to_zmaj_index(self) -> u8 {
        self as u8
    }

    /// Returns the octant the given vector points toward, when interpreted as pointing away
    /// from the center point where all octants meet.
    ///
    /// Ties due to zero components are broken in the positive direction.
    #[inline]
    pub fn from_vector(vector: FreeVector) -> Self {
        let index = (u8::from(vector.x >= 0.) << 2)
            | (u8::from(vector.y >= 0.) << 1)
            | u8::from(vector.z >= 0.);
        Self::from_zmaj_index(index)
    }

    // TODO: decide whether to make these public
    #[inline(always)]
    pub(crate) fn negative_on_x(self) -> bool {
        self.to_zmaj_index() & 0b100 == 0
    }
    #[inline(always)]
    pub(crate) fn negative_on_y(self) -> bool {
        self.to_zmaj_index() & 0b010 == 0
    }
    #[inline(always)]
    pub(crate) fn negative_on_z(self) -> bool {
        self.to_zmaj_index() & 0b001 == 0
    }

    /// Returns this octant of the volume (0..2)³.
    ///
    /// That is, each coordinate of the returned [`Cube`] is either 0 or 1.
    #[inline]
    #[must_use]
    pub fn to_positive_cube(self) -> Cube {
        Cube::from(self.to_01::<Cube>().map(GridCoordinate::from).to_point())
    }

    /// Returns this octant of the volume (0..2)³ expressed as a translation vector
    /// from the origin.
    ///
    /// That is, each coordinate of the returned [`Vector3D`] is either 0 or 1.
    #[inline]
    #[must_use]
    pub fn to_01<U>(self) -> Vector3D<u8, U> {
        let i = self.to_zmaj_index();
        vec3((i >> 2) & 1, (i >> 1) & 1, i & 1)
    }

    /// For each component of `vector`, negate it if `self` is on the negative side of that axis.
    ///
    /// This may be used to transform between positive-octant-only data and data mirrored into
    /// arbitrary octants.
    #[inline]
    pub fn reflect<T, U>(self, mut vector: Vector3D<T, U>) -> Vector3D<T, U>
    where
        T: ops::Neg<Output = T>,
    {
        if self.negative_on_x() {
            vector.x = -vector.x;
        }
        if self.negative_on_y() {
            vector.y = -vector.y;
        }
        if self.negative_on_z() {
            vector.z = -vector.z;
        }
        vector
    }
}

impl fmt::Debug for Octant {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Nnn => "−X−Y−Z",
            Self::Nnp => "−X−Y+Z",
            Self::Npn => "−X+Y−Z",
            Self::Npp => "−X+Y+Z",
            Self::Pnn => "+X−Y−Z",
            Self::Pnp => "+X−Y+Z",
            Self::Ppn => "+X+Y−Z",
            Self::Ppp => "+X+Y+Z",
        })
    }
}

// -------------------------------------------------------------------------------------------------

/// A set of [`Octant`]s (or equivalently, one [`bool`] for each cube of a 2×2×2 volume).
///
/// Internally represented as a `u8` bit-set.
///
/// This type may be used for culling or other calculations relating to the axis-aligned volumes
/// surrounding a point. If more data than `bool` is needed, use [`OctantMap`].
#[derive(Clone, Copy, Eq, Hash, PartialEq, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct OctantMask {
    /// A bit-mask of octants, where the bit positions are, LSB first, [-X-Y-Z, -X-Y+Z, ..., +X+Y+Z]
    ///
    /// TODO: It would probably make more sense in many cases to use the opposite bit ordering.
    flags: u8,
}

impl OctantMask {
    /// The mask including all octants (all bits set).
    pub const ALL: Self = Self { flags: 0xFF };
    /// The mask including no octants (no bits set).
    pub const NONE: Self = Self { flags: 0x00 };

    #[inline]
    #[doc(hidden)]
    pub const fn from_zmaj_bits(flags: u8) -> Self {
        Self { flags }
    }

    /// Get the flag for the given octant.
    #[inline]
    pub fn get(self, octant: Octant) -> bool {
        self.flags & (1 << octant.to_zmaj_index()) != 0
    }

    /// Set the flag for the given octant.
    #[inline]
    pub fn set(&mut self, octant: Octant) {
        self.flags |= 1 << octant.to_zmaj_index();
    }

    /// Clear the flat for the given octant.
    #[inline]
    pub fn clear(&mut self, octant: Octant) {
        self.flags &= !(1 << octant.to_zmaj_index());
    }

    /// Shift the data in the specified direction, discarding overflows and filling with
    /// [`false`]/clear.
    #[inline]
    #[must_use]
    pub fn shift(self, direction: Face6) -> Self {
        let flags = self.flags;
        Self {
            flags: match direction {
                Face6::NX => flags >> 4,
                Face6::PX => flags << 4,
                Face6::NY => (flags & 0b11001100) >> 2,
                Face6::PY => (flags & 0b00110011) << 2,
                Face6::NZ => (flags & 0b10101010) >> 1,
                Face6::PZ => (flags & 0b01010101) << 1,
            },
        }
    }

    /// Returns the first octant included in the mask.
    ///
    /// “First” is in an arbitrary ordering which corresponds to the binary-counting ordering
    /// which has X as most significant bit and Z as least significant bit:
    ///
    /// ```text
    /// -X -Y -Z
    /// -X -Y +Z
    /// -X +Y -Z
    /// -X +Y +Z
    /// +X -Y -Z
    /// +X -Y +Z
    /// +X +Y -Z
    /// +X +Y +Z
    /// ```
    #[inline(always)]
    #[doc(hidden)] // TODO: Decide what bit ordering we really want before locking it in
    pub fn first(self) -> Option<Octant> {
        let tz = self.flags.trailing_zeros();
        if tz >= u8::BITS {
            None
        } else {
            Some(Octant::from_zmaj_index(tz as u8))
        }
    }

    /// As [`Self::first()`], but the opposite ordering.
    #[inline(always)]
    #[doc(hidden)] // TODO: Decide what bit ordering we really want before locking it in
    pub fn last(self) -> Option<Octant> {
        let lz = self.flags.leading_zeros();
        if lz >= u8::BITS {
            None
        } else {
            Some(Octant::from_zmaj_index((7 - lz) as u8))
        }
    }

    /// Along each of the specified axes, move set bits on the negative side to the positive side,
    /// combining with logical or.
    ///
    /// TODO: `collapse_to_positive()` would make more sense as an operation matching
    /// `Octant::reflect()`; this is just what we happened to have before `OctantMask`
    /// was a thing in itself.
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub fn collapse_to_negative(mut self, x: bool, y: bool, z: bool) -> Self {
        if x {
            self.flags = (self.flags & 0b00001111) | ((self.flags & 0b11110000) >> 4);
        }
        if y {
            self.flags = (self.flags & 0b00110011) | ((self.flags & 0b11001100) >> 2);
        }
        if z {
            self.flags = (self.flags & 0b01010101) | ((self.flags & 0b10101010) >> 1);
        }
        self
    }
}

impl fmt::Debug for OctantMask {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Not using f.debug_set() because we want never-multiline output.
        write!(f, "{{")?;
        let mut first = true;
        for octant in Octant::ALL {
            if self.get(octant) {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{octant:?}")?;
            }
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl ops::BitOr for OctantMask {
    type Output = Self;
    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            flags: self.flags | rhs.flags,
        }
    }
}
impl ops::BitAnd for OctantMask {
    type Output = Self;
    #[inline]
    fn bitand(self, rhs: Self) -> Self::Output {
        Self {
            flags: self.flags & rhs.flags,
        }
    }
}
impl ops::Not for OctantMask {
    type Output = Self;
    #[inline]
    fn not(self) -> Self::Output {
        Self { flags: !self.flags }
    }
}

impl FromIterator<Octant> for OctantMask {
    #[inline]
    fn from_iter<T: IntoIterator<Item = Octant>>(iter: T) -> Self {
        let mut new_self = OctantMask::NONE;
        iter.into_iter().for_each(|octant| {
            new_self.set(octant);
        });
        new_self
    }
}

impl From<Octant> for OctantMask {
    #[inline]
    fn from(octant: Octant) -> Self {
        let mut new_self = OctantMask::NONE;
        new_self.set(octant);
        new_self
    }
}

// -------------------------------------------------------------------------------------------------

/// Collection of 8 values keyed by [`Octant`]s.
///
/// If the values are [`bool`], use [`OctantMask`] instead for a more efficient representation.
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct OctantMap<T>([T; 8]);

impl<T> OctantMap<T> {
    /// Constructs an [`OctantMap`] by using the provided function to compute
    /// a value for each octant.
    #[inline]
    #[must_use]
    pub fn from_fn(mut function: impl FnMut(Octant) -> T) -> Self {
        Self(core::array::from_fn(|i| {
            function(Octant::from_zmaj_index(i as u8))
        }))
    }

    /// Constructs an [`OctantMap`] by cloning the provided value.
    #[inline]
    #[must_use]
    pub fn repeat(value: T) -> Self
    where
        T: Clone,
    {
        Self([
            value.clone(),
            value.clone(),
            value.clone(),
            value.clone(),
            value.clone(),
            value.clone(),
            value.clone(),
            value,
        ])
    }
    /// Returns an [`OctantMask`] constructed by applying the given `predicate` to each octant
    /// of the data in `self`.
    #[inline]
    #[must_use]
    pub fn to_mask(&self, mut predicate: impl FnMut(&T) -> bool) -> OctantMask {
        let mut output = 0;
        for (i, value) in self.0.iter().enumerate() {
            output |= u8::from(predicate(value)) << i;
        }
        OctantMask::from_zmaj_bits(output)
    }

    // /// Moves the data into a [`Vol`] that covers the same volume as
    // /// [`Octant::to_positive_cube()`].
    // #[inline]
    // #[must_use]
    // pub fn into_positive_vol(self) -> Vol<Box<[T]>> {
    //     Vol::from_elements(GridAab::for_block(Resolution::R2), self.0).unwrap()
    // }

    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub fn into_zmaj_array(self) -> [T; 8] {
        self.0
    }

    /// Returns an iterator over all elements by reference, and their octants.
    ///
    /// The order of iteration is not currently guarateed.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (Octant, &T)> + '_ {
        Octant::ALL.into_iter().zip(self.0.iter())
    }

    /// Returns an iterator over all elements by reference.
    ///
    /// The order of iteration is not currently guarateed.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &T> + '_ {
        self.0.iter()
    }

    /// Returns an iterator over all elements by mutable reference.
    ///
    /// The order of iteration is not currently guarateed.
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Octant, &mut T)> + '_ {
        Octant::ALL.into_iter().zip(self.0.iter_mut())
    }
}

impl<T: fmt::Debug> fmt::Debug for OctantMap<T> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("OctantMap");
        for octant in Octant::ALL {
            s.field(&format!("{octant:?}"), &self[octant]);
        }
        s.finish()
    }
}

impl<T> ops::Index<Octant> for OctantMap<T> {
    type Output = T;
    #[inline]
    fn index(&self, octant: Octant) -> &Self::Output {
        &self.0[octant.to_zmaj_index() as usize]
    }
}
impl<T> ops::IndexMut<Octant> for OctantMap<T> {
    #[inline]
    fn index_mut(&mut self, octant: Octant) -> &mut Self::Output {
        &mut self.0[octant.to_zmaj_index() as usize]
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use Face6::*;
    use euclid::vec3;

    #[test]
    fn reflect() {
        let vec3 = vec3::<i32, ()>; // avoid ambiguous type

        assert_eq!(Octant::Ppp.reflect(vec3(1, 2, 3)), vec3(1, 2, 3));
        assert_eq!(Octant::Ppn.reflect(vec3(1, 2, 3)), vec3(1, 2, -3));
        assert_eq!(Octant::Pnp.reflect(vec3(1, 2, 3)), vec3(1, -2, 3));
        assert_eq!(Octant::Npp.reflect(vec3(1, 2, 3)), vec3(-1, 2, 3));
        assert_eq!(Octant::Nnn.reflect(vec3(1, 2, 3)), vec3(-1, -2, -3));
    }

    #[test]
    fn mask_debug() {
        let none = OctantMask::NONE;

        assert_eq!(format!("{none:?}"), "{}");
        assert_eq!(format!("{none:#?}"), "{}");

        let mut some = OctantMask::NONE;
        some.set(Octant::Pnn);
        some.set(Octant::Ppn);

        assert_eq!(format!("{some:?}"), "{+X−Y−Z, +X+Y−Z}");
        assert_eq!(format!("{some:#?}"), "{+X−Y−Z, +X+Y−Z}");
    }

    #[test]
    fn shifts() {
        let all = OctantMask::ALL;
        assert_eq!(all.shift(NX), OctantMask::from_zmaj_bits(0b00001111));
        assert_eq!(all.shift(PX), OctantMask::from_zmaj_bits(0b11110000));
        assert_eq!(all.shift(NY), OctantMask::from_zmaj_bits(0b00110011));
        assert_eq!(all.shift(PY), OctantMask::from_zmaj_bits(0b11001100));
        assert_eq!(all.shift(NZ), OctantMask::from_zmaj_bits(0b01010101));
        assert_eq!(all.shift(PZ), OctantMask::from_zmaj_bits(0b10101010));
    }

    #[test]
    fn collapse_to_negative() {
        // TODO: more test coverage
        assert_eq!(
            OctantMask::ALL.collapse_to_negative(true, true, false),
            OctantMask::from_iter([Octant::Nnn, Octant::Nnp])
        );
        for octant in Octant::ALL {
            let mask = OctantMask::from_iter([octant]);
            assert_eq!(
                mask.collapse_to_negative(true, true, true),
                OctantMask::from_iter([Octant::Nnn])
            );
        }
    }

    #[test]
    fn octant_mask_smoke_test() {
        let mut mask = OctantMask::NONE;
        assert_eq!(mask, OctantMask::NONE);
        mask.set(Octant::from_vector(vec3(1., 1., 1.)));
        assert_eq!(mask, OctantMask { flags: 0b1000_0000 });
        mask.set(Octant::from_vector(vec3(-1., -1., -1.)));
        assert_eq!(mask, OctantMask { flags: 0b1000_0001 });
        assert_eq!(mask.first(), Some(Octant::from_zmaj_index(0)));
        assert_eq!(mask.last(), Some(Octant::from_zmaj_index(7)));
    }
}

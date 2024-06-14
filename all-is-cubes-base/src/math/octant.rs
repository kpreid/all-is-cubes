use core::{fmt, ops};

use euclid::Vector3D;

use crate::math::FreeVector;

/// Identifies one of eight octants, or elements of a 2×2×2 cube.
///
/// Used with [`OctantMask`] and [`OctantSet`].
///
/// This enum's discriminants are not currently to be considered stable; they may be reordered.
//---
// TODO: Replace this with an enum.
#[allow(clippy::exhaustive_enums)]
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

    fn to_zmaj_index(self) -> u8 {
        self as u8
    }

    /// Returns the octant the given vector points toward, when interpreted as pointing away
    /// from the center point where all octants meet.
    ///
    /// Ties due to zero components are broken in the positive direction.
    #[inline]
    pub fn from_vector(vector: FreeVector) -> Self {
        let index = u8::from(vector.x >= 0.) << 2
            | u8::from(vector.y >= 0.) << 1
            | u8::from(vector.z >= 0.);
        Self::from_zmaj_index(index)
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
        if self.to_zmaj_index() & 0b100 == 0 {
            vector.x = -vector.x;
        }
        if self.to_zmaj_index() & 0b10 == 0 {
            vector.y = -vector.y;
        }
        if self.to_zmaj_index() & 0b1 == 0 {
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

    /// Returns the first octant included in the mask.
    ///
    /// Here “first” means the arbitrary ordering [`ChunkChart`] uses, which corresponds
    /// to the binary-counting ordering with X as MSB and Z as LSB:
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
        write!(f, "OctantMask[")?;
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
        write!(f, "]")?;
        Ok(())
    }
}

impl FromIterator<Octant> for OctantMask {
    #[inline]
    fn from_iter<T: IntoIterator<Item = Octant>>(iter: T) -> Self {
        let mut this = OctantMask::NONE;
        iter.into_iter().for_each(|octant| {
            this.set(octant);
        });
        this
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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

        assert_eq!(format!("{none:?}"), "OctantMask[]");
        assert_eq!(format!("{none:#?}"), "OctantMask[]");

        let mut some = OctantMask::NONE;
        some.set(Octant::Pnn);
        some.set(Octant::Ppn);

        assert_eq!(format!("{some:?}"), "OctantMask[+X−Y−Z, +X+Y−Z]");
        assert_eq!(format!("{some:#?}"), "OctantMask[+X−Y−Z, +X+Y−Z]");
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

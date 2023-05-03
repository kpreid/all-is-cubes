use crate::content::palette;
use crate::math::{Cube, Face6, FaceMap, FreeVector, GridAab, Rgb};
use crate::space::PackedLight;

#[cfg(doc)]
use crate::space::{Space, SpacePhysics};

/// The infinitely-distant background/lighting environment outside a [`Space`].
///
/// When used to configure a [`Space`], this is stored in the [`SpacePhysics`] struct.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Sky {
    /// Uniform illumination in all directions.
    Uniform(Rgb),
}

impl Sky {
    pub(crate) const DEFAULT: Self = Self::Uniform(palette::DAY_SKY_COLOR);

    /// Return the incoming light from the specified direction.
    ///
    /// The result is a [luminance]; see [voxel light emission] for information about the units.
    ///
    /// [luminance]: https://en.wikipedia.org/wiki/Luminance
    /// [voxel light emission]: crate::block::Atom::emission
    pub fn sample(&self, _direction: FreeVector) -> Rgb {
        match *self {
            Sky::Uniform(color) => color,
        }
    }

    /// Returns the light which would be obtained by integrating [`Sky::sample()`] over all
    /// directions.
    pub fn mean(&self) -> Rgb {
        match *self {
            Sky::Uniform(color) => color,
        }
    }

    pub(crate) fn for_blocks(&self) -> BlockSky {
        BlockSky {
            faces: match *self {
                Sky::Uniform(color) => FaceMap::repeat(PackedLight::from(color)),
            },
            mean: PackedLight::from(self.mean()),
        }
    }
}

impl Default for Sky {
    fn default() -> Self {
        Self::DEFAULT
    }
}

/// Derived from a [`Sky`] and represents the light that will fall on each face of a block
/// in the absence of any other blocks obstructing light from the sky.
///
/// This should produce the same results as the light calculation algorithm computing it
/// by summing individual rays. TODO: Write a test validating that.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct BlockSky {
    faces: FaceMap<PackedLight>,
    mean: PackedLight,
}

impl BlockSky {
    /// Returns the light that should be considered to occupy `cube`, which is a cube
    /// on the outside surface of the [`Space`] bounds `bounds`.
    ///
    /// For example, if `bounds` is from [0, 0, 0] to [5, 5, 5] and `cube` is [-1, 2, 2],
    /// then `cube` is on the -X face of `bounds`.
    ///
    /// Panics if `cube` is not on the surface of `bounds`.
    #[track_caller]
    pub fn light_outside(&self, bounds: GridAab, cube: Cube) -> PackedLight {
        let lb = bounds.lower_bounds();
        let ub = bounds.upper_bounds();
        let nx = cube.x == lb.x - 1;
        let ny = cube.y == lb.y - 1;
        let nz = cube.z == lb.z - 1;
        let px = cube.x == ub.x;
        let py = cube.y == ub.y;
        let pz = cube.z == ub.z;
        match (nx, ny, nz, px, py, pz) {
            (true, false, false, false, false, false) => self.faces.nx,
            (false, true, false, false, false, false) => self.faces.ny,
            (false, false, true, false, false, false) => self.faces.nz,
            (false, false, false, true, false, false) => self.faces.px,
            (false, false, false, false, true, false) => self.faces.py,
            (false, false, false, false, false, true) => self.faces.pz,

            // Cannot be inside the bounds or beyond the surface
            (false, false, false, false, false, false) => {
                panic!("invalid cube position in BlockSky::light_outside({bounds:?}, {cube:?})");
            }

            // If it's at a corner or edge, then make it NO_RAYS, just like the block lighting
            // algorithm would do.
            _ => PackedLight::NO_RAYS,
        }
    }

    #[inline]
    pub(crate) fn in_direction(&self, face: Face6) -> PackedLight {
        self.faces[face]
    }

    /// Returns the same light value as [`Sky::mean()`].
    /// This should also be approximately equal to the mean of all six [`BlockSky::in_direction()`]
    /// outputs.
    #[inline]
    pub(crate) fn mean(&self) -> PackedLight {
        self.mean
    }
}

use euclid::vec3;

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
    /// Each octant a different color.
    Octants([Rgb; 8]),
}

impl Sky {
    /// Return the incoming light from the specified direction.
    ///
    /// The result is a [luminance]; see [voxel light emission] for information about the units.
    ///
    /// [luminance]: https://en.wikipedia.org/wiki/Luminance
    /// [voxel light emission]: crate::block::Atom::emission
    pub fn sample(&self, direction: FreeVector) -> Rgb {
        match *self {
            Sky::Uniform(color) => color,
            Sky::Octants(colors) => {
                colors[(usize::from(direction.x >= 0.0) << 2)
                    + (usize::from(direction.y >= 0.0) << 1)
                    + usize::from(direction.z >= 0.0)]
            }
        }
    }

    /// Returns the light which would be obtained by integrating [`Sky::sample()`] over all
    /// directions.
    pub fn mean(&self) -> Rgb {
        match *self {
            Sky::Uniform(color) => color,
            Sky::Octants(colors) => colors.iter().copied().sum::<Rgb>() * (1.0 / 8.0),
        }
    }

    /// Resample this sky to present the colors of block faces.
    #[doc(hidden)] // TODO: used by raytracer but not sure if we want to expose this
    pub fn for_blocks(&self) -> BlockSky {
        BlockSky {
            faces: match *self {
                Sky::Uniform(color) => FaceMap::splat(PackedLight::from(color)),
                Sky::Octants(_) => FaceMap::from_fn(|face| {
                    let transform = face.rotation_from_nz();
                    // Take four samples from rays into the correct octants.
                    // The rays start out exiting the NZ face and are transformed.
                    // We could calculate which octants to use directly, but that would be more
                    // error-prone.
                    PackedLight::from(
                        [
                            vec3(-1, -1, -1),
                            vec3(-1, 1, -1),
                            vec3(1, -1, -1),
                            vec3(1, 1, -1),
                        ]
                        .map(|p| self.sample(transform.transform_vector(p).to_f64()))
                        .into_iter()
                        .sum::<Rgb>()
                            * 0.25_f32,
                    )
                }),
            },
            mean: PackedLight::from(self.mean()),
        }
    }
}

impl const Default for Sky {
    fn default() -> Self {
        Self::Uniform(palette::DAY_SKY_COLOR)
    }
}

/// Derived from a [`Sky`] and represents the light that will fall on each face of a block
/// in the absence of any other blocks obstructing light from the sky.
///
/// This should produce the same results as the light calculation algorithm computing it
/// by summing individual rays. TODO: Write a test validating that.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[doc(hidden)] // TODO: used by raytracer but not sure if we want to expose this
pub struct BlockSky {
    faces: FaceMap<PackedLight>,
    mean: PackedLight,
}

impl BlockSky {
    /// Returns the light that should be considered to occupy `cube` when it is outside of
    /// the [`Space`] bounds `bounds`.
    ///
    /// For example, if `bounds` is from [0, 0, 0] to [5, 5, 5] and `cube` is [-1, 2, 2],
    /// then `cube` is on the -X face of `bounds`.
    ///
    /// # Edge cases
    ///
    /// * Returns [`PackedLight::UNINITIALIZED_AND_BLACK`] if `cube` is inside of `bounds`
    ///   (which is not the intended use of this function).
    /// * Returns [`PackedLight::NO_RAYS`] if `cube` is not touching one face of `bounds`.
    pub fn light_outside(&self, bounds: GridAab, cube: Cube) -> PackedLight {
        use core::cmp::Ordering::{Equal, Less};

        // For each face of the bounds, compute whether the cube is inside the bounds (`Less`),
        // touching the bounds from outside (`Equal`), or further outside (`Greater`).
        // Note: cube.upper_bounds() is defined to be maybe panicking, so we can't use it.
        let lower = bounds.lower_bounds().zip(cube.lower_bounds(), |bl, cl| {
            if let Some(beyond) = bl.checked_sub(1) {
                beyond.cmp(&cl)
            } else {
                // Cube can't possibly be outside the bounds
                Less
            }
        });
        let upper = bounds.upper_bounds().zip(cube.lower_bounds(), |bu, cl| cl.cmp(&bu));

        match (lower.x, lower.y, lower.z, upper.x, upper.y, upper.z) {
            (Equal, Less, Less, Less, Less, Less) => self.faces.nx,
            (Less, Equal, Less, Less, Less, Less) => self.faces.ny,
            (Less, Less, Equal, Less, Less, Less) => self.faces.nz,
            (Less, Less, Less, Equal, Less, Less) => self.faces.px,
            (Less, Less, Less, Less, Equal, Less) => self.faces.py,
            (Less, Less, Less, Less, Less, Equal) => self.faces.pz,

            // Undefined if inside the bounds
            (Less, Less, Less, Less, Less, Less) => {
                // panic!("invalid cube position in BlockSky::light_outside({bounds:?}, {cube:?})");
                PackedLight::UNINITIALIZED_AND_BLACK
            }

            // If it's at a corner or edge, or beyond the surface, then make it NO_RAYS,
            // just like the light algorithm would calculate if this cube were inside the bounds.
            _ => PackedLight::NO_RAYS,
        }
    }

    #[inline]
    pub fn in_direction(&self, face: Face6) -> PackedLight {
        self.faces[face]
    }

    /// Returns the same light value as [`Sky::mean()`].
    /// This should also be approximately equal to the mean of all six [`BlockSky::in_direction()`]
    /// outputs.
    #[inline]
    pub fn mean(&self) -> PackedLight {
        self.mean
    }
}

use core::fmt;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::block::{Resolution, Resolution::R1};
use crate::math::{Aab, Cube, CubeFace, Face7, FreeCoordinate, lines};
use crate::util::{ConciseDebug, MapExtend, Refmt as _};

#[cfg(doc)]
use crate::space::Space;

// -------------------------------------------------------------------------------------------------

/// Set of [`Contact`]s produced by a collision.
#[allow(
    exported_private_dependencies,
    reason = "transitive dependency false positive"
)]
pub type ContactSet = hashbrown::HashSet<Contact>;

// -------------------------------------------------------------------------------------------------

/// An individual collision contact; something in a [`Space`] that a moving [`Aab`]
/// collided with.
///
/// This type is designed to be comparable/hashable to deduplicate contacts.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[expect(
    clippy::exhaustive_enums,
    reason = "any change will probably be breaking anyway"
)]
pub enum Contact {
    /// Contact with a fully solid block; the [`CubeFace`] specifies the block position
    /// and the side of it that was collided with (hence also the contact normal).
    Block(CubeFace),
    /// Contact with one voxel of a block with a potentially complex shape.
    Voxel {
        /// The “outer” cube in the [`Space`].
        cube: Cube,
        /// The voxel resolution of the block; that is, the factor by which voxel
        /// coordinates are smaller than `cube` coordinates.
        resolution: Resolution,
        /// The voxel position in the block (each coordinate lies between `0` and
        /// `resolution - 1`) and the face of that voxel collided with, which is also
        /// the contact normal.
        voxel: CubeFace,
    },
}

impl Contact {
    /// Returns the cube that was collided with, or which contains the voxel collided with.
    pub fn cube(&self) -> Cube {
        match *self {
            Contact::Block(CubeFace { cube, .. }) => cube,
            Contact::Voxel { cube, .. } => cube,
        }
    }

    /// Returns the bounding box of the cube or voxel that was collided with.
    pub fn aab(self) -> Aab {
        match self {
            Contact::Block(cube_face) => cube_face.cube.aab(),
            Contact::Voxel {
                cube,
                resolution,
                voxel,
            } => {
                // Note: We're always scaling by a power of 2, so this should not introduce any
                // rounding error that isn't necessarily implied by the distance from the origin.
                voxel
                    .cube
                    .aab()
                    .scale(resolution.recip_f64())
                    .translate(cube.lower_bounds().to_vector().to_f64())
            }
        }
    }

    /// Returns the contact normal: the direction in which the colliding box should be
    /// pushed back.
    ///
    /// Note that this may be equal to [`Face7::Within`] in case the box was already
    /// intersecting before any movement.
    pub fn normal(&self) -> Face7 {
        match *self {
            Contact::Block(CubeFace { face, .. }) => face,
            Contact::Voxel {
                voxel: CubeFace { face, .. },
                ..
            } => face,
        }
    }

    /// Returns the scale of the voxel collided with.
    pub fn resolution(&self) -> Resolution {
        match *self {
            Contact::Block(_) => R1,
            Contact::Voxel { resolution, .. } => resolution,
        }
    }

    /// Return a copy where the contact normal is replaced with [`Face7::Within`].
    pub(crate) fn without_normal(&self) -> Self {
        let mut result = *self;
        match result {
            Contact::Block(CubeFace { ref mut face, .. }) => *face = Face7::Within,
            Contact::Voxel {
                voxel: CubeFace { ref mut face, .. },
                ..
            } => *face = Face7::Within,
        }
        result
    }
}

impl fmt::Debug for Contact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Block(CubeFace { cube, face }) => {
                write!(f, "{face:?} of {}", cube.refmt(&ConciseDebug))
            }
            Self::Voxel {
                cube,
                resolution,
                voxel: CubeFace { cube: voxel, face },
            } => write!(
                f,
                "{:?} of {} {}/{}",
                face,
                cube.refmt(&ConciseDebug),
                voxel.refmt(&ConciseDebug),
                resolution
            ),
        }
    }
}

impl lines::Wireframe for Contact {
    fn wireframe_points<E: Extend<[lines::Vertex; 2]>>(&self, output: &mut E) {
        match self {
            Contact::Block(cube_face) => cube_face.wireframe_points(output),
            Contact::Voxel {
                cube,
                resolution,
                voxel,
            } => {
                let resolution: FreeCoordinate = (*resolution).into();
                voxel.wireframe_points(&mut MapExtend::new(output, |line: [lines::Vertex; 2]| {
                    line.map(|mut vert| {
                        vert.position = vert.position / resolution + cube.aab().lower_bounds_v();
                        vert
                    })
                }))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::R4;

    #[test]
    fn contact_block_aab() {
        assert_eq!(
            Contact::Block(CubeFace {
                cube: Cube::new(1, 2, 3),
                face: Face7::PX
            })
            .aab(),
            Aab::from_lower_upper([1., 2., 3.], [2., 3., 4.]),
        );
    }

    #[test]
    fn contact_voxel_aab() {
        assert_eq!(
            Contact::Voxel {
                cube: Cube::new(1, 2, 3),
                resolution: R4,
                voxel: CubeFace {
                    cube: Cube::new(2, 1, 0),
                    face: Face7::NZ
                },
            }
            .aab(),
            Aab::from_lower_upper([1.5, 2.25, 3.0], [1.75, 2.5, 3.25]),
        );
    }
}

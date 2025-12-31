use all_is_cubes::block::Resolution;
use all_is_cubes::math::{Cube, Face7};

#[cfg(doc)]
use all_is_cubes::space::Space;

use crate::raytracer::ColorBuf;

#[cfg(doc)]
use crate::raytracer::{Accumulate, RtBlockData};

// -------------------------------------------------------------------------------------------------

/// Data passed by the raytracer to [`Accumulate::add()`] implementations.
//--
// Design note: Not every accumulator uses every part of this struct.
// We hope that optimization will drop the code to compute the unused parts.
// However, further work might be wanted, in particular to explicitly skip color.
//
// Also, a name other than `Hit` might be better but I haven't thought of one.
#[derive(Debug)]
#[non_exhaustive]
pub struct Hit<'d, D> {
    /// If [`Some`], indicates that this hit denotes an exceptional situation rather than
    /// an ordinary interaction of the ray with a block in the space.
    pub exception: Option<Exception>,

    /// Contains the opacity/transmittance and the light output of the encountered surface,
    /// in the direction towards the camera.
    ///
    /// This may also represent the ray traversing a thickness of transparent material
    /// when volume rendering is enabled, but it should be treated the same regardless.
    pub surface: ColorBuf,

    /// Distance along the ray at which the encountered surface was intersected.
    ///
    /// This distance is in units of the length of the original ray’s direction vector.
    /// Therefore, when forming an image, whether this value is Euclidean distance from
    /// the camera, or distance along the Z axis, depends on whether the rays are constructed
    /// with a constant length or a constant Z component.
    ///
    /// It is [`None`] when the event creating this hit does not correspond to a single
    /// point in physical space, such as error indicators or 2D overlays.
    pub t_distance: Option<f64>,

    /// [`RtBlockData`] value for the block this surface or volume is part of.
    pub block: &'d D,

    /// Which cube and voxel of the space the encountered surface belongs to,
    /// or [`None`] if this is a special case without a definite position such as the sky or
    /// an error report.
    ///
    /// If a position in continuous space is desired, use `t_distance` and the original ray
    /// to reconstruct it instead.
    // TODO: Use Point3D<u8> instead or not?
    pub position: Option<Position>,
}

/// Component of [`Hit`] indicating a situation other than an ordinary interaction of the ray
/// with a block in the space.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Exception {
    /// The ray entered the bounds of the space (or started inside it).
    /// This may be used to notice whether or not a ray intersected the space at all,
    /// even if it hit no blocks.
    EnterSpace,

    /// The ray exited the space and is considered to have hit the sky.
    Sky,

    /// For external reasons such as computational limits,
    /// the trace is being terminated and the the result will be incomplete.
    Incomplete,

    /// Result of calling [`Accumulate::paint()`].
    Paint,

    /// The hit color contains debug information.
    /// Its red and green channels should override previous transparent content.
    #[doc(hidden)] // used for debug purposes and only ColorBuf needs to consult it
    DebugOverrideRg,
}

/// Data about where a ray struck a voxel; part of [`Hit`].
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct Position {
    /// The cube in the [`Space`] containing the block whose surface was hit.
    pub cube: Cube,
    /// The voxel resolution of the block.
    pub resolution: Resolution,
    /// The voxel within the block.
    // TODO: Use Point3D<u8> instead or not?
    pub voxel: Cube,
    /// The surface normal of the hit surface, or [`Face7::Within`] if the ray started inside this
    /// voxel.
    pub face: Face7,
}

impl<'d, D> Hit<'d, D> {
    /// Apply the function `f` to `self.block`, producing a [`Hit`] of a different type.
    pub fn map_block_data<D2>(self, f: impl Fn(&D) -> &D2) -> Hit<'d, D2> {
        Hit {
            exception: self.exception,
            surface: self.surface,
            t_distance: self.t_distance,
            block: f(self.block),
            position: self.position,
        }
    }
}

impl<D> Clone for Hit<'_, D> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<D> Copy for Hit<'_, D> {}

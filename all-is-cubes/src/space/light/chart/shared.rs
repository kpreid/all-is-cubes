//! The single purpose of this file is to be used by both the regular crate code
//! and the build script to share some binary structure layouts.
//!
//! This is the most efficient way I could think of to store and transfer the
//! pre-computed light ray chart data.

use all_is_cubes_base::math::{Face7, FaceMap};

// conditionally defined to be equal to f32 except in the build script
use super::TargetEndian;

/// Information about a single one of the bundle of light rays that we follow
/// from a struck block towards light sources.
#[derive(Clone, Copy, Debug, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct OneRay {
    /// Unit direction vector specifying the ray direction.
    pub direction: [TargetEndian<f32>; 3],

    /// `FaceMap` data which stores the cosine (rescaled to 0-255)
    /// between each face normal and this ray.
    pub face_cosines: PodFaceMap<u8>,

    /// Guaranteed zero padding to make up a multiple of 4 bytes.
    pub _padding: [u8; 2],
}

/// The pre-computed sequence of cubes which is traversed by a [`OneRay`].
/// This format is used only while computing them.
#[derive(Clone, Debug)]
#[allow(dead_code)]
#[repr(C)]
pub(crate) struct Steps {
    pub info: OneRay,
    pub relative_cube_sequence: ::alloc::vec::Vec<Step>,
}

/// The pre-computed sequence of cubes which is traversed by a [`OneRay`].
/// This format is used in the static pre-computed data.
#[derive(Clone, Copy, Debug, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct IndirectSteps {
    pub info: OneRay,
    /// Sequence of steps to take, specified by an inclusive-exclusive range of a slice of
    /// separately stored [`Step`]s.
    pub relative_cube_sequence: [TargetEndian<u32>; 2],
}

#[derive(Clone, Copy, Debug, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct Step {
    /// Cube we just hit, relative to the origin of rays
    /// (the block we're computing light for).
    pub relative_cube: [TargetEndian<i8>; 3],

    /// Distance from ray origin that has been traversed so far to strike this cube,
    /// rounded up.
    pub distance: u8,

    /// Face struck.
    pub face: Face7Safe,
}

#[derive(Clone, Copy, Debug, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct Ray {
    pub origin: [TargetEndian<f64>; 3],
    pub direction: [TargetEndian<f64>; 3],
}

impl From<Ray> for all_is_cubes_base::raycast::Ray {
    fn from(value: Ray) -> Self {
        Self::new(value.origin.map(f64::from), value.direction.map(f64::from))
    }
}

/// [`Face6`] but without an enum's validity invariant.
/// Panics on unsuccessful conversion.
#[derive(Clone, Copy, Debug, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub(crate) struct Face7Safe(u8);
impl From<Face7Safe> for Face7 {
    fn from(value: Face7Safe) -> Self {
        match value.0 {
            0 => Face7::Within,
            1 => Face7::NX,
            2 => Face7::NY,
            3 => Face7::NZ,
            4 => Face7::PX,
            5 => Face7::PY,
            6 => Face7::PZ,
            _ => {
                if cfg!(debug_assertions) {
                    panic!("invalid {value:?}");
                } else {
                    // avoid generating a panic branch
                    Face7::Within
                }
            }
        }
    }
}
impl From<Face7> for Face7Safe {
    fn from(value: Face7) -> Self {
        Self(value as u8)
    }
}

/// Variant of [`FaceMap`] that can implement `bytemuck::Pod` because it is `repr(packed)`.
/// (The regular `FaceMap` could too, *unsafely*, but I don't want to do that unsafe yet.)
#[derive(Clone, Copy, Debug, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C, packed)]
pub(crate) struct PodFaceMap<T> {
    nx: T,
    ny: T,
    nz: T,
    pz: T,
    py: T,
    px: T,
}
impl<T> From<FaceMap<T>> for PodFaceMap<T> {
    #[rustfmt::skip]
    fn from(value: FaceMap<T>) -> Self {
        let FaceMap { nx, ny, nz, px, py, pz } = value;
        Self { nx, ny, nz, pz, py, px }
    }
}
impl<T> From<PodFaceMap<T>> for FaceMap<T> {
    #[rustfmt::skip]
    fn from(value: PodFaceMap<T>) -> Self {
        let PodFaceMap { nx, ny, nz, pz, py, px } = value;
        FaceMap { nx, ny, nz, px, py, pz }
    }
}

// Note: Most of the methods are either only used for reading or only used for writing,
// so they're defined in the respective crates to reduce complications like what they depend on,
// how `TargetEndian` is defined, and dead code warnings.

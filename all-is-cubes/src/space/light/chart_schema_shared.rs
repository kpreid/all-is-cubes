//! The single purpose of this file is to be used by both the regular crate code
//! and the build script to share some binary structure layouts.
//!
//! This is the most efficient way I could think of to store and transfer the
//! pre-computed light ray chart data.
//!
//! Currently, host and target endianness must be the same.
//! In the future, this may be handled by using number types wrapped to be explicitly
//! target endianness.

// conditionally defined to be equal to f32 except in the build script
use super::TargetEndian;

#[derive(Clone, Copy, Debug, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct OneRay {
    // This can't be a `Ray` because `FaceMap` is not `repr(C)`
    pub origin: [TargetEndian<f64>; 3],
    pub direction: [TargetEndian<f64>; 3],
    // This can't be a `FaceMap` because `FaceMap` is not `repr(C)`
    pub face_cosines: [TargetEndian<f32>; 6],
}

// Note: All of the methods are either only used for reading or only used for writing,
// so they're defined in the respective crates to reduce complications like what they depend on,
// how `TargetEndian` is defined, and dead code warnings.

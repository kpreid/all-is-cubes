//! Data structures for light storage and algorithms.

use std::collections::hash_map::Entry;
use std::collections::BTreeSet;
use std::fmt;

use cgmath::{Vector3, Vector4};

use crate::math::*;
use crate::space::*;

/// One component of a `PackedLight`.
pub(crate) type PackedLightScalar = u8;

/// Special reasons for a cube having zero light in it.
/// These may be used to help compute smoothed lighting across blocks.
///
/// The numeric values of this enum are used to transmit it to shaders by packing
/// it into an "RGBA" color value. They should not be considered a stable API element.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum LightStatus {
    /// The cube's light value has never been computed.
    #[allow(unused)]
    Uninitialized = 0,
    /// The cube has no surfaces to catch light and therefore the light value is not tracked.
    NoRays = 1,
    /// The cube contains an opaque block and therefore does not have any light entering.
    Opaque = 128,
    /// No special situation: if it's black then it's just dark.
    Visible = 255,
}

/// Lighting within a [`Space`]; an [`Rgb`] value stored with reduced precision and range.
///
/// TODO: This now stores additional information. Rename to '`SpaceLight`' or some such.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PackedLight {
    // LightStatus being other than Visible is mutually exclusive with value being nonzero,
    // so we could in theory make this an enum, but that wouldn't actually compact the
    // representation, and this representation maps to 8-bit-per-component RGBA which is
    // what the shader expects.
    value: Vector3<PackedLightScalar>,
    status: LightStatus,
}
// TODO: Once we've built out the rest of the game, do some performance testing and
// decide whether having colored lighting is worth the compute and storage cost.
// If memory vs. bit depth is an issue, consider switching to something like YCbCr
// representation, or possibly something that GPUs specifically do well with.

impl PackedLight {
    const LOG_SCALE: f32 = 16.0;
    const LOG_OFFSET: f32 = 128.0;

    pub(crate) const ZERO: Self = Self::none(LightStatus::Visible);
    pub(crate) const OPAQUE: Self = Self::none(LightStatus::Opaque);
    pub(crate) const NO_RAYS: Self = Self::none(LightStatus::NoRays);
    pub(crate) const ONE: PackedLight = PackedLight {
        status: LightStatus::Visible,
        value: Vector3 {
            x: Self::LOG_OFFSET as PackedLightScalar,
            y: Self::LOG_OFFSET as PackedLightScalar,
            z: Self::LOG_OFFSET as PackedLightScalar,
        },
    };

    pub(crate) fn some(value: Rgb) -> Self {
        PackedLight {
            value: Vector3::new(
                Self::scalar_in(value.red()),
                Self::scalar_in(value.green()),
                Self::scalar_in(value.blue()),
            ),
            status: LightStatus::Visible,
        }
    }

    pub(crate) const fn none(status: LightStatus) -> Self {
        PackedLight {
            value: Vector3 { x: 0, y: 0, z: 0 },
            status,
        }
    }

    #[inline]
    pub fn value(&self) -> Rgb {
        Rgb::new_nn(
            Self::scalar_out_nn(self.value[0]),
            Self::scalar_out_nn(self.value[1]),
            Self::scalar_out_nn(self.value[2]),
        )
    }

    // TODO: Expose LightStatus once we are more confident in its API stability
    // This call is used just for tests for now
    #[cfg(test)]
    pub(super) fn status(&self) -> LightStatus {
        self.status
    }

    /// Returns true if the light value is meaningful, or false if it is
    /// inside an opaque block or in empty unlit air (in which case [`Self::value`]
    /// always returns zero).
    pub(crate) fn valid(&self) -> bool {
        self.status == LightStatus::Visible
    }

    /// RGB color plus a fourth component which is a “weight” value which indicates how
    /// much this color should actually contribute to the surface color. It is usually
    /// 0 or 1, but is set slightly above zero for opaque blocks to create the ambient
    /// occlusion effect.
    pub(crate) fn value_with_ambient_occlusion(&self) -> Vector4<f32> {
        Vector4::new(
            Self::scalar_out(self.value[0]),
            Self::scalar_out(self.value[1]),
            Self::scalar_out(self.value[2]),
            match self.status {
                LightStatus::Uninitialized => 0.0,
                LightStatus::NoRays => 0.0,
                // TODO: Make this a graphics option
                LightStatus::Opaque => 0.25,
                LightStatus::Visible => 1.0,
            },
        )
    }

    #[inline]
    #[doc(hidden)] // TODO: used by all_is_cubes_gpu; but it should be doable equivalently using public functions
    pub fn as_texel(self) -> [u8; 4] {
        let Self {
            value: Vector3 { x, y, z },
            status,
        } = self;
        [x, y, z, status as u8]
    }

    /// Computes a degree of difference between two [`PackedLight`] values, used to decide
    /// update priority.
    /// The value is zero if and only if the two inputs are equal.
    #[inline]
    pub(crate) fn difference_priority(self, other: PackedLight) -> PackedLightScalar {
        fn abs_diff(a: PackedLightScalar, b: PackedLightScalar) -> PackedLightScalar {
            a.max(b) - a.min(b)
        }
        let mut difference = abs_diff(self.value[0], other.value[0])
            .max(abs_diff(self.value[1], other.value[1]))
            .max(abs_diff(self.value[2], other.value[2]));

        if other.status != self.status {
            // A non-opaque block changing to an opaque one, or similar, changes the
            // results of the rest of the algorithm so should be counted as a difference
            // even if it's still changing zero to zero.
            // TODO: Tune this number for fast settling and good results.
            difference = difference.saturating_add(PackedLightScalar::MAX / 4);
        }

        difference
    }

    fn scalar_in(value: impl Into<f32>) -> PackedLightScalar {
        // Note that `as` is a saturating cast.
        (value.into().log2() * Self::LOG_SCALE + Self::LOG_OFFSET) as PackedLightScalar
    }

    /// Convert a `PackedLightScalar` value to a linear color component value.
    /// This function is guaranteed (and tested) to only return finite floats.
    fn scalar_out(value: PackedLightScalar) -> f32 {
        // Special representation to ensure we don't "round" zero up to a small nonzero value.
        if value == 0 {
            0.0
        } else {
            ((f32::from(value) - Self::LOG_OFFSET) / Self::LOG_SCALE).exp2()
        }
    }

    fn scalar_out_nn(value: PackedLightScalar) -> NotNan<f32> {
        // Safety: a test verifies that `scalar_out` can never return NaN.
        unsafe { NotNan::new_unchecked(Self::scalar_out(value)) }
    }
}

impl fmt::Debug for PackedLight {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "PackedLight({}, {}, {}, {:?})",
            self.value.x, self.value.y, self.value.z, self.status
        )
    }
}

impl From<Rgb> for PackedLight {
    #[inline]
    fn from(value: Rgb) -> Self {
        PackedLight::some(value)
    }
}

/// An entry in the queue of cubes that need their light updated.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct LightUpdateRequest {
    pub(crate) priority: PackedLightScalar,
    pub(crate) cube: GridPoint,
}
impl LightUpdateRequest {
    /// A priority comparison for entries with equal specified priority:
    /// prefer cubes closer to the origin. (This is for prettier initial startup:
    /// assuming the viewpoint starts close to the origin it will see good nearby
    /// lighting sooner.)
    fn fallback_priority(&self) -> GridCoordinate {
        let GridPoint { x, y, z } = self
            .cube
            .map(|c| if c > 0 { -c } else { c } + GridCoordinate::MAX / 3);
        x.saturating_add(y).saturating_add(z)
    }
}
impl Ord for LightUpdateRequest {
    fn cmp(&self, other: &LightUpdateRequest) -> std::cmp::Ordering {
        self.priority
            .cmp(&other.priority)
            .then_with(|| self.fallback_priority().cmp(&other.fallback_priority()))
            // To obey Ord's contract we must not return equal ordering when unequal by Eq,
            // so we must break all ties until only completely identical remains.
            .then_with(|| self.cube.x.cmp(&other.cube.x))
            .then_with(|| self.cube.y.cmp(&other.cube.y))
            .then_with(|| self.cube.z.cmp(&other.cube.z))
    }
}
impl PartialOrd for LightUpdateRequest {
    fn partial_cmp(&self, other: &LightUpdateRequest) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// A priority queue for [`LightUpdateRequest`]s which contains cubes
/// at most once, even when added with different priorities.
pub(crate) struct LightUpdateQueue {
    /// Sorted storage of queue elements.
    /// This is a BTreeSet rather than a BinaryHeap so that items can be removed.
    queue: BTreeSet<LightUpdateRequest>,
    /// Maps GridPoint to priority value. This allows deduplicating entries, including
    /// removing low-priority entries in favor of high-priority ones
    table: HashMap<GridPoint, PackedLightScalar>,
}

impl LightUpdateQueue {
    pub fn new() -> Self {
        Self {
            queue: BTreeSet::new(),
            table: HashMap::new(),
        }
    }

    /// Insert a queue entry or increase the priority of an existing one.
    #[inline]
    pub fn insert(&mut self, request: LightUpdateRequest) {
        match self.table.entry(request.cube) {
            Entry::Occupied(mut e) => {
                let existing_priority = *e.get();
                if request.priority > existing_priority {
                    let removed = self.queue.remove(&LightUpdateRequest {
                        cube: request.cube,
                        priority: existing_priority,
                    });
                    debug_assert!(removed);
                    e.insert(request.priority);
                    self.queue.insert(request);
                }
            }
            Entry::Vacant(e) => {
                e.insert(request.priority);
                self.queue.insert(request);
            }
        }
    }

    #[inline]
    pub fn pop(&mut self) -> Option<LightUpdateRequest> {
        // This can become self.queue.pop_last() when that's stable
        let result = self.queue.iter().rev().next().copied();
        if let Some(request) = result {
            let removed_queue = self.queue.remove(&request);
            let removed_table = self.table.remove(&request.cube);
            debug_assert!(removed_queue && removed_table.is_some());
        }
        result
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.queue.len()
    }

    #[inline]
    pub fn peek_priority(&self) -> PackedLightScalar {
        // This can become self.queue.last() when that's stable
        self.queue
            .iter()
            .rev()
            .next()
            .copied()
            .map(|r| r.priority)
            .unwrap_or(0)
    }

    pub fn clear(&mut self) {
        self.queue.clear();
        self.table.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::once;

    fn packed_light_test_values() -> impl Iterator<Item = PackedLight> {
        (PackedLightScalar::MIN..PackedLightScalar::MAX)
            .flat_map(|s| {
                vec![
                    PackedLight {
                        value: Vector3::new(s, 0, 0),
                        status: LightStatus::Visible,
                    },
                    PackedLight {
                        value: Vector3::new(0, s, 0),
                        status: LightStatus::Visible,
                    },
                    PackedLight {
                        value: Vector3::new(0, 0, s),
                        status: LightStatus::Visible,
                    },
                    PackedLight {
                        value: Vector3::new(s, 127, 255),
                        status: LightStatus::Visible,
                    },
                ]
                .into_iter()
            })
            .chain(once(PackedLight::OPAQUE))
            .chain(once(PackedLight::NO_RAYS))
    }

    /// Test that unpacking and packing doesn't shift the value, which could lead
    /// to runaway light values.
    #[test]
    fn packed_light_roundtrip() {
        for i in PackedLightScalar::MIN..PackedLightScalar::MAX {
            assert_eq!(i, PackedLight::scalar_in(PackedLight::scalar_out(i)));
        }
    }

    /// Safety test: we want to skip the NaN checks for constructing `Rgb`
    /// from `PackedLight`, so it had better not be NaN for any possible input.
    #[test]
    fn packed_light_always_finite() {
        for i in PackedLightScalar::MIN..PackedLightScalar::MAX {
            assert!(PackedLight::scalar_out(i).is_finite(), "{}", i);
        }
    }

    /// Test out-of-range floats.
    #[test]
    fn packed_light_clipping_in() {
        assert_eq!(
            [
                PackedLight::scalar_in(notnan!(-1.)),
                PackedLight::scalar_in(notnan!(1e-30)),
                PackedLight::scalar_in(notnan!(1e+30)),
            ],
            [0, 0, 255],
        );
    }

    #[test]
    fn packed_light_is_packed() {
        // Technically this is not guaranteed by the compiler, but if it's false something probably went wrong.
        assert_eq!(std::mem::size_of::<PackedLight>(), 4);
    }

    /// Demonstrate what range and step sizes we get out of the encoding.
    #[test]
    fn packed_light_extreme_values_out() {
        assert_eq!(
            [
                PackedLight::scalar_out(0),
                PackedLight::scalar_out(1),
                PackedLight::scalar_out(2),
                PackedLight::scalar_out(254),
                PackedLight::scalar_out(255),
            ],
            [0.0, 0.0040791943, 0.004259796, 234.75304, 245.14644],
        );
    }

    #[test]
    fn packed_light_difference_vs_eq() {
        for v1 in packed_light_test_values() {
            for v2 in packed_light_test_values() {
                assert_eq!(
                    v1 == v2,
                    v1.difference_priority(v2) == 0,
                    "v1={:?} v2={:?}",
                    v1,
                    v2
                );
            }
        }
    }

    #[test]
    fn queue_ordering() {
        fn r(cube: [GridCoordinate; 3], priority: PackedLightScalar) -> LightUpdateRequest {
            LightUpdateRequest {
                cube: GridPoint::from(cube),
                priority,
            }
        }

        let mut queue = LightUpdateQueue::new();
        queue.insert(r([0, 0, 0], 1));
        queue.insert(r([2, 0, 0], 1));
        queue.insert(r([1, 0, 0], 1));
        queue.insert(r([0, 0, 2], 200));
        queue.insert(r([0, 0, 1], 100));
        assert_eq!(queue.pop(), Some(r([0, 0, 2], 200)));
        assert_eq!(queue.pop(), Some(r([0, 0, 1], 100)));
        assert_eq!(queue.pop(), Some(r([0, 0, 0], 1)));
        assert_eq!(queue.pop(), Some(r([1, 0, 0], 1)));
        assert_eq!(queue.pop(), Some(r([2, 0, 0], 1)));
        assert_eq!(queue.pop(), None);
    }

    // TODO: Test of queue priority updates
}

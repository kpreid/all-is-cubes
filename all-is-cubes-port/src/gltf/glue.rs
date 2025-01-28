//! Helpers for working with [`gltf_json`].

use std::fmt;

use all_is_cubes::euclid;

use gltf_json::Index;
use gltf_json::validation::Checked::Valid;

/// For a [`gltf_json::Accessor`], find the elementwise minimum and maximum values
/// in a slice of arrays of some kind of value.
pub(crate) fn accessor_minmax<I, const N: usize>(items: I) -> [Option<serde_json::Value>; 2]
where
    I: IntoIterator<Item = [f32; N]>,
{
    let mut mins = [f32::INFINITY; N];
    let mut maxes = [f32::NEG_INFINITY; N];
    for array in items {
        for i in 0..N {
            mins[i] = mins[i].min(array[i]);
            maxes[i] = maxes[i].max(array[i]);
        }
    }
    if mins[0].is_finite() {
        [
            Some(serde_json::to_value(mins.to_vec()).unwrap()),
            Some(serde_json::to_value(maxes.to_vec()).unwrap()),
        ]
    } else {
        [None, None]
    }
}

/// f32, but in guaranteed little-endian representation for copying into glTF
/// buffer data.
#[derive(Copy, Clone, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub(crate) struct Lef32([u8; 4]);

impl Lef32 {
    /// All bits zero is also the value 'positive zero'.
    pub const ZERO: Self = Lef32([0, 0, 0, 0]);

    pub(crate) fn from_vec3<U>(vector: euclid::Vector3D<f32, U>) -> [Self; 3] {
        Into::<[f32; 3]>::into(vector).map(Lef32::from)
    }
}

impl fmt::Debug for Lef32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f32::from(*self).fmt(f)
    }
}

impl PartialEq for Lef32 {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        f32::from(*self) == f32::from(*other)
    }
}

impl From<f32> for Lef32 {
    #[inline]
    fn from(input: f32) -> Self {
        Self(f32::to_le_bytes(input))
    }
}

impl From<Lef32> for f32 {
    #[inline]
    fn from(input: Lef32) -> Self {
        f32::from_le_bytes(input.0)
    }
}

pub(crate) fn convert_quaternion<Src, Dst>(
    q: euclid::Rotation3D<f64, Src, Dst>,
) -> gltf_json::scene::UnitQuaternion {
    let q = q.normalize(); // shouldn't be *necessary* but…
    gltf_json::scene::UnitQuaternion([q.i as f32, q.j as f32, q.k as f32, q.r as f32])
}

/// Shorthand to construct a `Node` having no contents until modified.
pub(crate) fn empty_node(name: Option<String>) -> gltf_json::Node {
    gltf_json::Node {
        camera: None,
        children: None,
        extensions: Default::default(),
        extras: Default::default(),
        matrix: None,
        mesh: None,
        name,
        rotation: None,
        scale: None,
        translation: None,
        skin: None,
        weights: None,
    }
}

/// Create an Accessor and compute the min and max from the data, which must be consistent with the buffer.
///
/// Currently the elements must be vectors (or scalars) of f32s.
#[track_caller]
pub(crate) fn create_accessor<I, const COMPONENTS: usize>(
    name: String,
    buffer_view: Index<gltf_json::buffer::View>,
    byte_offset: usize,
    data_view: I,
) -> gltf_json::Accessor
where
    I: IntoIterator<Item = [f32; COMPONENTS], IntoIter: ExactSizeIterator>,
{
    let iter = data_view.into_iter();
    let count = iter.len();
    let [min, max] = accessor_minmax(iter);

    // Catch bug early rather than generating bad data
    assert!(
        count > 0,
        "glTF accessor {name:?} must have a size greater than 0"
    );

    gltf_json::Accessor {
        buffer_view: Some(buffer_view),
        byte_offset: Some(byte_offset.into()),
        count: count.into(),
        component_type: Valid(gltf_json::accessor::GenericComponentType(
            gltf_json::accessor::ComponentType::F32,
        )),
        type_: Valid(match COMPONENTS {
            1 => gltf_json::accessor::Type::Scalar,
            2 => gltf_json::accessor::Type::Vec2,
            3 => gltf_json::accessor::Type::Vec3,
            4 => gltf_json::accessor::Type::Vec4,
            _ => panic!(
                "glTF accessor {name:?} must have a component count of 1 to 4, not {COMPONENTS}"
            ),
        }),
        min,
        max,
        name: Some(name),
        normalized: false,
        sparse: None,
        extensions: Default::default(),
        extras: Default::default(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn lef32_zero() {
        assert_eq!(Lef32::default(), bytemuck::Zeroable::zeroed());
        assert_eq!(Lef32::default(), Lef32::from(0.0));
        assert_eq!(Lef32::ZERO, Lef32::from(0.0));
    }

    #[test]
    fn lef32_round_trip() {
        assert_eq!(1234.56_f32, f32::from(Lef32::from(1234.56_f32)));
    }

    #[test]
    fn minmax() {
        assert_eq!(
            accessor_minmax([[1., 1., -10., -10.,], [2., 0.5, -11., 0.]]),
            [
                Some(json!([1., 0.5, -11., -10.])),
                Some(json!([2., 1., -10., 0.]))
            ]
        )
    }
}

// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Helpers for working with [`gltf_json`].

use std::fmt;

use all_is_cubes::cgmath;

use gltf_json::validation::Checked::Valid;
use gltf_json::Index;

use all_is_cubes::cgmath::Vector3;
use all_is_cubes::cgmath::Vector4;

// TODO: contribute this to gltf_json
pub(crate) fn push_and_return_index<T>(vec: &mut Vec<T>, value: T) -> Index<T> {
    let index: u32 = vec.len().try_into().expect("Too many items");
    vec.push(value);
    Index::new(index)
}

/// Convert to a `gltf_json` size value.
///
/// Note: the glTF standard does not actually impose this limit
pub(crate) fn u32size(size: usize) -> u32 {
    // TODO: worth making this a Result?
    size.try_into()
        .expect("Data overflowed 32-bit maximum size")
}

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
    // All bits zero is also the value 'positive zero'
    pub const ZERO: Self = Lef32([0, 0, 0, 0]);

    pub(crate) fn from_vec3(vector: Vector3<f32>) -> [Self; 3] {
        vector.map(Lef32::from).into()
    }
    pub(crate) fn from_vec4(vector: Vector4<f32>) -> [Self; 4] {
        vector.map(Lef32::from).into()
    }
}

impl fmt::Debug for Lef32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

pub(crate) fn convert_quaternion(rot: cgmath::Basis3<f64>) -> gltf_json::scene::UnitQuaternion {
    let q: cgmath::Quaternion<f64> = rot.into();
    let q: cgmath::Quaternion<f32> = q.cast().unwrap();
    gltf_json::scene::UnitQuaternion([q.v.x, q.v.y, q.v.z, q.s])
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
pub(crate) fn create_accessor<I, const COMPONENTS: usize>(
    name: String,
    buffer_view: Index<gltf_json::buffer::View>,
    byte_offset: usize,
    data_view: I,
) -> gltf_json::Accessor
where
    I: IntoIterator<Item = [f32; COMPONENTS]>,
    I::IntoIter: ExactSizeIterator,
{
    let iter = data_view.into_iter();
    let count = u32size(iter.len());
    let [min, max] = accessor_minmax(iter);

    // Catch bug early rather than generating bad data
    assert!(
        count > 0,
        "glTF accessor {name} must have a size greater than 0"
    );

    gltf_json::Accessor {
        buffer_view: Some(buffer_view),
        byte_offset: u32size(byte_offset),
        count,
        component_type: Valid(gltf_json::accessor::GenericComponentType(
            gltf_json::accessor::ComponentType::F32,
        )),
        type_: Valid(match COMPONENTS {
            1 => gltf_json::accessor::Type::Scalar,
            2 => gltf_json::accessor::Type::Vec2,
            3 => gltf_json::accessor::Type::Vec3,
            4 => gltf_json::accessor::Type::Vec4,
            _ => panic!("Invalid component count"),
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
    }

    #[test]
    fn lef32_round_trip() {
        assert_eq!(1234.56_f32, f32::from(Lef32::from(1234.56_f32)));
    }

    #[test]
    fn minmax() {
        assert_eq!(
            accessor_minmax([[1., 1., -10., -10.,], [2., 0.5, -11., 0.]].into_iter()),
            [
                Some(json!([1., 0.5, -11., -10.])),
                Some(json!([2., 1., -10., 0.]))
            ]
        )
    }
}
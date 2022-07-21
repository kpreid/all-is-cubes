//! [`GltfVertex`], vertex type for writing to glTF buffers.

use all_is_cubes::cgmath::{EuclideanSpace as _, Point3, Vector3, Vector4};
use all_is_cubes::{
    math::GridPoint,
    mesh::{BlockVertex, Coloring, GfxVertex},
};

use super::glue::Lef32;

/// [`GfxVertex`] type for glTF exports.
///
/// These vertices may be copied directly to glTF buffers; all fields are stored
/// little-endian as per the specification.
///
/// These vertices store no normals; they would be redundant since glTF 2.0 specification
/// § 3.7.2.1 specifies that flat normals must be assumed.
#[derive(Clone, Copy, Debug, Default, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct GltfVertex {
    pub(crate) position: [Lef32; 3],
    pub(crate) color_or_texture: [Lef32; 4],
}

impl GltfVertex {
    /// Placeholder constant value for offset_of calculations.
    pub(crate) const DUMMY: GltfVertex = GltfVertex {
        position: [Lef32::ZERO; 3],
        color_or_texture: [Lef32::ZERO; 4],
    };
}

impl From<BlockVertex> for GltfVertex {
    #[inline]
    fn from(vertex: BlockVertex) -> Self {
        let position = Lef32::from_vec3(vertex.position.cast::<f32>().unwrap().to_vec());
        match vertex.coloring {
            Coloring::Solid(color) => {
                let mut color_attribute: Vector4<f32> = color.into();
                // Clamp out-of-range alpha values so they fit into the
                // VertexColorOrTexture protocol (not less than zero).
                color_attribute.w = color_attribute.w.min(1.).max(0.);
                Self {
                    position,
                    color_or_texture: Lef32::from_vec4(color_attribute),
                }
            }
            Coloring::Texture {
                pos: tc,
                clamp_min: _,
                clamp_max: _,
            } => Self {
                position,
                color_or_texture: Lef32::from_vec4(tc.extend(-1.0)),
            },
        }
    }
}

impl GfxVertex for GltfVertex {
    const WANTS_DEPTH_SORTING: bool = false;
    type Coordinate = f32;
    type BlockInst = Vector3<f32>;

    #[inline]
    fn instantiate_block(cube: GridPoint) -> Self::BlockInst {
        cube.to_vec().map(|s| s as f32)
    }

    #[inline]
    fn instantiate_vertex(&mut self, cube: Self::BlockInst) {
        self.position = Lef32::from_vec3(self.position().to_vec() + cube);
    }

    #[inline]
    fn position(&self) -> Point3<Self::Coordinate> {
        Point3::<Lef32>::from(self.position).map(f32::from)
    }
}

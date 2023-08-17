//! [`GltfVertex`], vertex type for writing to glTF buffers.

use all_is_cubes::cgmath::{EuclideanSpace as _, Point3, Vector3};
use all_is_cubes::math::GridPoint;
use all_is_cubes_mesh::{BlockVertex, Coloring, GfxVertex};

use super::glue::Lef32;
use super::texture::TexPoint;

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
    /// glTF semantic `POSITION`
    pub(crate) position: [Lef32; 3],
    /// glTF semantic `COLOR_0`.
    /// Should be interpreted using the glTF `pbrMetallicRoughness` model.
    /// Per glTF 2.0 § 3.9.2, this will be multiplied by the texture color.
    pub(crate) base_color: [Lef32; 4],
    /// glTF semantic `TEXCOORD_0`.
    /// The texel obtained with these coordinates should be interpreted using the glTF
    /// `pbrMetallicRoughness` model.
    pub(crate) base_color_tc: [Lef32; 2],
}

impl GltfVertex {
    /// Placeholder constant value for [`bytemuck::offset_of`] calculations.
    pub(crate) const DUMMY: GltfVertex = GltfVertex {
        position: [Lef32::ZERO; 3],
        base_color: [Lef32::ZERO; 4],
        base_color_tc: [Lef32::ZERO; 2],
    };
}

impl From<BlockVertex<TexPoint>> for GltfVertex {
    #[inline]
    fn from(vertex: BlockVertex<TexPoint>) -> Self {
        let position = Lef32::from_vec3(vertex.position.cast::<f32>().unwrap().to_vec());
        match vertex.coloring {
            Coloring::Solid(color) => {
                Self {
                    position,
                    base_color: Lef32::from_vec4(color.clamp().into()),
                    // TODO: We need to ensure that the texture, if present, has white allocated here.
                    base_color_tc: [Lef32::ZERO; 2],
                }
            }
            Coloring::Texture {
                pos: tc,
                clamp_min: _,
                clamp_max: _,
            } => Self {
                position,
                // TODO: We need to later rewrite these texture coordinates
                // (possibly with the help of data stashed in the color field)
                // to point into the actual tile coordinates in the generated texture atlas.
                base_color: [Lef32::ONE; 4],
                base_color_tc: Lef32::from_vec2(tc),
            },
        }
    }
}

impl GfxVertex for GltfVertex {
    const WANTS_DEPTH_SORTING: bool = false;
    type Coordinate = f32;
    type BlockInst = Vector3<f32>;
    type TexPoint = TexPoint;

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

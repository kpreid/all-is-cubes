//! [`GltfVertex`], vertex type for writing to glTF buffers.

use all_is_cubes::euclid::{Point3D, Vector3D};
use all_is_cubes::math::Cube;
use all_is_cubes_mesh::{BlockVertex, Coloring, Vertex};

use super::glue::Lef32;
use super::texture::GltfAtlasPoint;

/// [`Vertex`] type for glTF exports.
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

impl From<BlockVertex<GltfAtlasPoint>> for GltfVertex {
    #[inline]
    fn from(vertex: BlockVertex<GltfAtlasPoint>) -> Self {
        let position = Lef32::from_vec3(vertex.position.to_f32().to_vector());
        match vertex.coloring {
            Coloring::Solid(color) => {
                Self {
                    position,
                    base_color: <[f32; 4]>::from(color.clamp()).map(Lef32::from),
                    // TODO: We need to ensure that the texture, if present, has white allocated here.
                    base_color_tc: [Lef32::ZERO; 2],
                }
            }
            Coloring::Texture {
                pos: tc,
                clamp_min: _,
                clamp_max: _,
                resolution: _,
            } => {
                // Temporarily pack the contents of the texture allocation info into our
                // vertex fields. TODO: Not actually sufficient yet.
                let GltfAtlasPoint {
                    plane_id,
                    point_within,
                } = tc;

                // Encode the plane ID in the color field as a value that is unambiguously
                // not a real color, because the blue and alpha components are negative.
                let base_color = [
                    Lef32::from(f32::from_bits(plane_id as u32)),
                    Lef32::from(f32::from_bits((plane_id >> 32) as u32)),
                    Lef32::from(-1.0),
                    Lef32::from(-1.0),
                ];

                Self {
                    position,
                    base_color,
                    base_color_tc: Into::<[f32; 2]>::into(point_within).map(Lef32::from),
                }
            }
        }
    }
}

impl Vertex for GltfVertex {
    const WANTS_DEPTH_SORTING: bool = false;
    type Coordinate = f32;
    type BlockInst = Vector3D<f32, Cube>;
    type TexPoint = GltfAtlasPoint;

    #[inline]
    fn instantiate_block(cube: Cube) -> Self::BlockInst {
        cube.lower_bounds().to_vector().map(|s| s as f32)
    }

    #[inline]
    fn instantiate_vertex(&mut self, cube: Self::BlockInst) {
        self.position = Lef32::from_vec3(self.position().to_vector() + cube);
    }

    #[inline]
    fn position(&self) -> Point3D<Self::Coordinate, Cube> {
        Point3D::<Lef32, _>::from(self.position).map(f32::from)
    }
}

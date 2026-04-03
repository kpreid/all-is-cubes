//! [`GltfVertex`], vertex type for writing to glTF buffers.

use all_is_cubes::euclid::{Point2D, Point3D, Vector3D};
use all_is_cubes::math::Cube;
use all_is_cubes_mesh::{self as mesh, BlockVertex, Coloring, Vertex};

use crate::gltf::texture::UvMap;

use super::glue::Lef32;
use super::texture::GltfAtlasPoint;

// -------------------------------------------------------------------------------------------------

/// Sentinel value stored in the blue and alpha components of [`GltfVertex::base_color`] to mark
/// when texture coordinates need to be updated once the atlas is built.
const UNPROCESSED_TEXTURE_COORDINATES_MARKER: Lef32 = Lef32::new(-1.0);

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
    ///
    /// If this vertex needs texture atlas UV coordinates and hasn’t been given them yet,
    /// the red and green components store an encoded `PlaneId` and the blue and alpha components
    /// are equal to [`UNPROCESSED_TEXTURE_COORDINATES_MARKER`].
    pub(crate) base_color: [Lef32; 4],

    /// glTF semantic `TEXCOORD_0`.
    /// The texel obtained with these coordinates should be interpreted using the glTF
    /// `pbrMetallicRoughness` model.
    pub(crate) base_color_tc: [Lef32; 2],
}

impl GltfVertex {
    /// Returns whether this vertex needs updating to contain texture coordinates.
    pub(crate) fn needs_texture_coordinates(&self) -> bool {
        let [_r, _g, _b, a] = self.base_color;
        a == UNPROCESSED_TEXTURE_COORDINATES_MARKER
    }

    /// If this is a textured vertex with placeholder coordinates, as constructed by
    /// [`Self::from_block_vertex()`], then update it to have real texture coordinates as
    /// mapped by `uv_map`.
    pub(crate) fn update_texture_coordinates(&mut self, uv_map: &UvMap) {
        if !self.needs_texture_coordinates() {
            return;
        }

        // Recover the plane_id encoded in the color field.
        let [r, g, _b, _a] = self.base_color;
        let lo = u64::from(f32::from(r).to_bits());
        let hi = u64::from(f32::from(g).to_bits());
        let plane_id = crate::gltf::texture::PlaneId(lo | (hi << 32));

        let uv = uv_map.plane_to_atlas(plane_id, Point2D::from(self.base_color_tc).map(f32::from));

        self.base_color_tc = [Lef32::new(uv.x), Lef32::new(uv.y)]; // real texture coords
        self.base_color = const { [Lef32::new(1.0); 4] }; // white
    }
}

impl Vertex for GltfVertex {
    const WANTS_DEPTH_SORTING: bool = false;
    type SecondaryData = ();
    type BlockInst = Vector3D<f32, mesh::MeshRel>;
    type TexPoint = GltfAtlasPoint;

    #[inline]
    fn instantiate_block(cube: Cube) -> Self::BlockInst {
        cube.lower_bounds().to_vector().map(|s| s as f32).cast_unit()
    }

    #[inline]
    fn instantiate_vertex(&mut self, cube: Self::BlockInst) {
        self.position = Lef32::from_vec3(self.position().to_vector() + cube);
    }

    #[inline]
    fn position(&self) -> mesh::Position {
        Point3D::<Lef32, _>::from(self.position).map(f32::from)
    }

    fn from_block_vertex(vertex: BlockVertex<Self::TexPoint>) -> (Self, Self::SecondaryData) {
        let position = Lef32::from_vec3(vertex.position.to_f32().to_vector());
        match vertex.coloring {
            Coloring::Solid(color) => {
                (
                    Self {
                        position,
                        base_color: <[f32; 4]>::from(color.clamp()).map(Lef32::from),
                        // TODO: We need to ensure that the texture, if present, has white allocated here — or add a rule to update these like we update other texture coordinates.
                        base_color_tc: [Lef32::ZERO; 2],
                    },
                    (),
                )
            }
            Coloring::Texture {
                pos: tc,
                resolution: _,
            } => {
                // Temporarily pack the contents of the texture allocation info into our
                // vertex fields.
                let GltfAtlasPoint {
                    plane_id,
                    point_within,
                } = tc;

                // Encode the plane ID in the color field as a value that is unambiguously
                // not a real color, because the blue and alpha components are negative.
                let plane_id: u64 = plane_id.0;
                let base_color = [
                    Lef32::new(f32::from_bits(plane_id as u32)),
                    Lef32::new(f32::from_bits((plane_id >> 32) as u32)),
                    UNPROCESSED_TEXTURE_COORDINATES_MARKER,
                    UNPROCESSED_TEXTURE_COORDINATES_MARKER,
                ];

                (
                    Self {
                        position,
                        base_color,
                        base_color_tc: Into::<[f32; 2]>::into(point_within).map(Lef32::from),
                    },
                    (),
                )
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::gltf::texture::PlaneId;

    use super::*;
    use all_is_cubes::block::Resolution;
    use all_is_cubes::euclid::{point2, point3};
    use all_is_cubes::math::{Face, Rgba};

    #[test]
    fn not_textured() {
        let (mut vertex, ()) = GltfVertex::from_block_vertex(BlockVertex {
            position: point3(1.0, 2.0, 3.0),
            face: Face::PX,
            coloring: Coloring::Solid(Rgba::new(0.0, 1.0, 0.0, 1.0)),
        });

        assert_eq!(vertex.needs_texture_coordinates(), false);

        let backup = vertex;
        vertex.update_texture_coordinates(&UvMap::new_for_testing(PlaneId(0), point2(0, 0)));
        assert_eq!(vertex, backup, "should not be changed by update");
    }

    #[test]
    fn textured() {
        let plane_id = PlaneId(0x123456789abcdef0);
        let uv_map = UvMap::new_for_testing(plane_id, point2(400, 500));
        let (mut vertex, ()) = GltfVertex::from_block_vertex(BlockVertex {
            position: point3(1.0, 2.0, 3.0),
            face: Face::PX,
            coloring: Coloring::Texture {
                pos: GltfAtlasPoint {
                    plane_id,
                    point_within: point2(10.0, 20.0),
                },
                resolution: Resolution::R4,
            },
        });

        assert_eq!(vertex.needs_texture_coordinates(), true);

        vertex.update_texture_coordinates(&uv_map);

        assert_eq!(vertex.needs_texture_coordinates(), false);
        assert_eq!(vertex.base_color_tc, [Lef32::new(410.0), Lef32::new(520.0)]);

        let backup = vertex;
        vertex.update_texture_coordinates(&uv_map);
        assert_eq!(
            vertex, backup,
            "should not be changed by calling update again"
        );
    }
}

use all_is_cubes::euclid::{Point3D, Vector3D};
use all_is_cubes::math::{Cube, GridVector};
use all_is_cubes_mesh::{BlockVertex, Coloring, GfxVertex};

use crate::DebugLineVertex;

/// Texture coordinates in the 3D atlas textures.
///
/// This type is public out of necessity; you should not need to refer to it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TexPoint {
    // Which texture atlas to use; corresponds to [`all_is_cubes_mesh::texture::Channels`].
    pub(crate) atlas_id: u8,

    /// Texture coordinates, in units of texels (i.e. the range is 0..256 or similar, not 0..1).
    pub(crate) tc: Point3D<FixTexCoord, AtlasTexel>,
}

/// Coordinate system for texels in the 3D atlas texture.
#[doc(hidden)]
pub enum AtlasTexel {}

/// Coordinate system for within-cube positions divided by 256.
pub(crate) enum CubeFix256 {}

/// Triangle mesh vertex type that is used for rendering [blocks].
///
/// [blocks]: all_is_cubes::block::Block
#[derive(Clone, Copy, Debug, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct WgpuBlockVertex {
    /// Chunk-relative position of the cube containing the triangle this vertex belongs to,
    /// packed into a u32 as `x | (y << 8) | (z << 16)`.
    ///
    /// Note that this is not the same as `floor()` of the final coordinates, since a
    /// block's mesh coordinates range from 0 to 1 inclusive.
    cube_packed: u32,

    /// Vertex position within the cube, fixed point; and vertex normal in [`Face7`] format.
    ///
    /// * The first `u32` is a bitwise combination of two u16s:
    ///   `(position.x * 256) | (position.y * 256) << 16`.
    ///   The scale factor 256 is chosen as being greater than the smallest [`Resolution`]
    ///   available. (Equal would also work.)
    /// * The second `u32` is
    ///   `position.z * 256 | (face << 16)`
    ///   where `face` is a `Face6` converted to integer.
    ///
    /// Vertex position is added to `cube` to make the true vertex position.
    ///
    /// There is no reason that the position and normal are packed together other than
    /// convenience and making efficient use of `u32` bits. (`u32` is the minimum size
    /// of integer that WGSL allows.)
    position_in_cube_and_normal_packed: [u32; 2],

    /// Packed format:
    /// * If `[3]` is in the range 0.0 to 1.0, then the attribute is a linear RGBA color.
    /// * If `[3]` is negative, then the first three components are 3D texture coordinates,
    ///   stored in texel units rather than normalized 0-1 units, and the fourth component
    ///   is the `(-1 - atlas_id)` where `atlas_id` identifies which texture atlas to use.
    ///
    /// TODO: we don't need `f32` precision here.
    color_or_texture: [f32; 4],

    /// Interpolated texture coordinates are clamped to be within these ranges,
    /// to avoid bleeding.
    ///
    /// Each `u32` is two packed [`FixTexCoord`], `min | (max << 16)`.
    ///
    /// Design note: It would be more straightforward to use `f16` here, but that's a
    /// WebGPU optional extension; and there are no `[f16; 3]` vectors so it would still
    /// require some data shuffling.
    clamp_min_max: [u32; 3],
}

impl WgpuBlockVertex {
    pub const LAYOUT: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: size_of::<Self>() as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &wgpu::vertex_attr_array![
            0 => Uint32, // cube_packed
            1 => Uint32x2, // position_in_cube_and_normal_packed
            2 => Float32x4, // color_or_texture
            3 => Uint32x3, // clamp_min_max
            // location numbers must not clash with WgpuInstanceData
        ],
    };
}

impl From<BlockVertex<TexPoint>> for WgpuBlockVertex {
    #[inline]
    fn from(vertex: BlockVertex<TexPoint>) -> Self {
        let position_in_cube_fixed: Point3D<u32, CubeFix256> = vertex
            .position
            .map(|coord| (coord * 256.) as u32)
            .cast_unit();
        let cube_packed = 0; // will be overwritten later by instantiate_vertex()
        let normal = vertex.face as u32;

        let position_in_cube_and_normal_packed = [
            position_in_cube_fixed.x | (position_in_cube_fixed.y << 16),
            position_in_cube_fixed.z | (normal << 16),
        ];
        match vertex.coloring {
            Coloring::Solid(color) => {
                let mut color_attribute: [f32; 4] = color.into();
                // Clamp out-of-range alpha values so they fit into the
                // VertexColorOrTexture protocol (not less than zero).
                color_attribute[3] = color_attribute[3].clamp(0., 1.);
                Self {
                    cube_packed,
                    position_in_cube_and_normal_packed,
                    color_or_texture: color_attribute,
                    clamp_min_max: [0, 0, 0],
                }
            }
            Coloring::Texture {
                pos: TexPoint { tc, atlas_id },
                clamp_min,
                clamp_max,
                resolution: _,
            } => Self {
                cube_packed,
                position_in_cube_and_normal_packed,
                color_or_texture: [
                    tc.x.into(),
                    tc.y.into(),
                    tc.z.into(),
                    -1.0 - f32::from(atlas_id),
                ],
                clamp_min_max: clamp_min.tc.zip(clamp_max.tc, FixTexCoord::pack).into(),
            },
        }
    }
}

impl GfxVertex for WgpuBlockVertex {
    const WANTS_DEPTH_SORTING: bool = true;
    /// TODO: no reason this should be f32 other than scaling to fractional integers.
    /// The depth sorting system should be made more flexible here.
    type Coordinate = f32;

    /// Packed cube coordinates
    type BlockInst = u32;
    type TexPoint = TexPoint;

    #[inline]
    fn instantiate_block(cube: Cube) -> Self::BlockInst {
        let cube = cube.lower_bounds().map(|c| c as u32);
        cube.x | (cube.y << 8) | (cube.z << 16)
    }

    #[inline]
    fn instantiate_vertex(&mut self, cube_packed: Self::BlockInst) {
        self.cube_packed = cube_packed;
    }

    #[inline]
    fn position(&self) -> Point3D<Self::Coordinate, Cube> {
        let cube = Point3D::new(
            self.cube_packed & 0xFF,
            (self.cube_packed >> 8) & 0xFF,
            (self.cube_packed >> 16) & 0xFF,
        );
        let pos_packed = self.position_in_cube_and_normal_packed;
        let position_in_cube_fixed = Vector3D::new(
            pos_packed[0] & 0xFFFF,
            (pos_packed[0] >> 16) & 0xFFFF,
            pos_packed[1] & 0xFFFF,
        );
        cube.map(|c| c as f32) + position_in_cube_fixed.map(|c| c as f32 / 256.)
    }
}

/// Data for a block mesh instance (in the sense of _instancing_).
#[derive(Clone, Copy, Debug, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct WgpuInstanceData {
    /// Translation vector which may be added to [`WgpuBlockVertex::position`]
    /// to obtain world (that is, [`Space`]) coordinates.
    ///
    /// TODO: Eventually we want to be able to use camera-relative coordinates, to make
    /// the best use of f32 precision in the shader.
    pub translation: [f32; 3],
}

impl WgpuInstanceData {
    pub const LAYOUT: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: size_of::<Self>() as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Instance,
        attributes: const {
            &wgpu::vertex_attr_array![
                // location numbers must start after WgpuBlockVertex ends
                6 => Float32x3,
            ]
        },
    };

    pub fn new(translation: GridVector) -> Self {
        Self {
            translation: translation.map(|int| int as f32).into(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct WgpuLinesVertex {
    position: [f32; 3],
    /// Linear RGBA color.
    color: [f32; 4],
}

impl WgpuLinesVertex {
    pub const LAYOUT: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: size_of::<Self>() as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &wgpu::vertex_attr_array![
            0 => Float32x3,
            1 => Float32x4,
        ],
    };
}

impl DebugLineVertex for WgpuLinesVertex {
    fn from_position_color(
        position: all_is_cubes::math::FreePoint,
        color: all_is_cubes::math::Rgba,
    ) -> Self {
        Self {
            position: position.map(|c| c as f32).into(),
            color: color.into(),
        }
    }
}

/// Fixed-point texture coordinate, with a multiplier of 2 so it can represent edges and
/// middles of texels (0.0, 0.5, 1.0, 1.5, ...)
///
/// This type should not be used directly; it is public as an element of [`WgpuBlockVertex`]'s
/// trait implementations.
#[derive(Clone, Copy, Debug, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[allow(unnameable_types)]
#[repr(C)]
pub struct FixTexCoord(u16);

impl FixTexCoord {
    pub(crate) fn from_float(float_tc: f32) -> Self {
        debug_assert!(float_tc >= 0.0);
        Self((float_tc * 2.).round() as u16)
    }

    pub(crate) fn pack(low: Self, high: Self) -> u32 {
        u32::from(low.0) | (u32::from(high.0) << 16)
    }
}

impl From<FixTexCoord> for f32 {
    fn from(tc: FixTexCoord) -> Self {
        f32::from(tc.0) / 2.
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::math::{Face6, Rgba};

    /// Assert the vertex's size, just so that we're reminded to think about it when we
    /// change the amount of data in it. This assertion is not platform-dependent because
    /// the struct is designed to have a fixed layout communicating to the shader anyway.
    #[test]
    fn vertex_size() {
        assert_eq!(size_of::<WgpuBlockVertex>(), 40);
        assert_eq!(size_of::<WgpuLinesVertex>(), 28);
    }

    /// Test implementation of [`GfxVertex::position()`],
    /// because if it's wrong the only thing that breaks is depth-sorting,
    /// and because it is a useful test of the outgoing coordinate processing logic too.
    #[test]
    fn block_vertex_position() {
        let mut vertex = WgpuBlockVertex::from(BlockVertex {
            position: Point3D::new(0.25, 0.0, 1.0),
            face: Face6::PX,
            coloring: Coloring::Solid(Rgba::new(0.0, 0.5, 1.0, 0.5)),
        });
        vertex.instantiate_vertex(WgpuBlockVertex::instantiate_block(Cube::new(100, 50, 7)));
        assert_eq!(
            GfxVertex::position(&vertex),
            Point3D::new(100.25, 50.0, 8.0)
        );
    }

    #[test]
    fn fix_tex_coord() {
        for int_part in 0..=256u16 {
            for frac_part in 0..=1 {
                let float = f32::from(frac_part).mul_add(0.5, f32::from(int_part));
                let fix = FixTexCoord::from_float(float);
                assert_eq!(fix.0, int_part * 2 + frac_part);
                assert_eq!(float, f32::from(fix));
            }
        }
    }
}

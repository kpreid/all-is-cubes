use core::fmt::{self, Write as _};

use all_is_cubes::euclid::{Point3D, Vector3D};
use all_is_cubes::math::{Cube, GridVector, Rgba};
use all_is_cubes_mesh::{self as mesh, BlockVertex, Vertex};

use crate::DebugLineVertex;

/// If true, label meshes, in the rendering, with which instance they came from.
const DEBUG_INSTANCES: bool = false;

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

/// Coordinate system for within-cube positions divided by 128.
pub(crate) enum CubeFix128 {}

/// Triangle mesh [`Vertex`] type that is used for rendering [blocks].
///
/// This struct contains the vertex position, and [`BColor`] contains additional information
/// for rendering which does not affect the position.
///
/// # Notes on data packing
///
/// In order to minimize data transfer costs, we want our vertices to be as small as possible.
/// Therefore, we take advantage of the strict voxel nature of the world to represent the vertices
/// using small integers and fixed-point numbers.
///
/// The vertex struct must have a size that is a multiple of [`wgpu::VERTEX_ALIGNMENT`], which is 4.
/// Therefore, while we could use [`u8`] vertex attributes, they are not useful except for
/// convenience, and take up more attribute slots. Instead, we use [`u32`] (4 bytes) containing
/// data which we explicitly pack and unpack.
///
/// (We could in principle use smaller fields here on the Rust side at no cost, but then we would
/// be using fields on one side and bit operations on the other, which seems confusing for little
/// benefit. It would also make the code endianness-sensitive, but I believe WebGPU/WGSL aren’t
/// intended to support big-endian.)
///
/// Counting individual bits, the data we actually need to store per [`BPosition`] is:
///
/// * Position of cube in chunk, range `0..=15`, 4 bits × 3 dimensions = 12 bits
/// * Position of vertex in cube, range `0..=128`, 8 bits × 3 dimensions = 24 bits
///
/// This adds up to 36 bits, so we cannot make `BPosition` fit in a single [`u32`].
/// Therefore, it is two [`u32`]s, and we use the additional bits for further information
/// that is not strictly the position:
///
/// * Face/normal: range `0..=5`, 3 bits
/// * Texture resolution: range `0..=7` (logarithm of 1 to 128), 3 bits
///
/// This makes a total of 42 used bits, out of a total of 64 bits.
///
/// [blocks]: all_is_cubes::block::Block
#[derive(Clone, Copy, Debug, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct BPosition {
    /// Chunk-relative position of the cube containing the triangle this vertex belongs to,
    /// packed into a u32 as `x | (y << 8) | (z << 16)`. The uppermost 8 bits are not used.
    ///
    /// Note that this is not the same as `floor()` of the final coordinates, since a
    /// block's mesh coordinates range from 0 to 1 inclusive.
    cube_packed: u32,

    /// Vertex position within the cube, fixed point; vertex normal in [`Face7`] format;
    /// and the resolution of the texture data.
    /// A packed form of five values:
    /// `(x * 128) | ((y * 128) << 8) | ((z * 128) << 16) | (face << 24) | (log2(resolution) << 28)`.
    ///
    /// * The lowest 24 bits are the position within the cube, in fixed point.
    ///   The scale factor 128 is equal to the finest [`Resolution`] available;
    ///   it must be less than 2^8 because coordinates range from 0 to 1 inclusive, so
    ///   0..128 is okay but 0..256 would overflow into neighboring fields.
    ///
    ///   This block-relative vertex position is added to `cube_packed` to obtain the chunk-relative
    ///   vertex position.
    ///
    /// * Bits 24..28 indicate the normal of the face, a `Face6` converted to integer.
    ///   (There is an unused high bit here.)
    ///
    /// * Bits 28..32 indicate the logarithm of the resolution of the texture data.
    ///   (There is an unused high bit here.)
    position_in_cube_and_normal_and_resolution_packed: u32,
}

/// Additional vertex data accompanying [`BPosition`].
#[derive(Clone, Copy, Debug, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct BColor {
    /// Packed format:
    /// * If `[3]` is in the range 0.0 to 1.0, then the attribute is a linear RGBA color.
    /// * If `[3]` is negative, then the first three components are 3D texture coordinates,
    ///   stored in texel units rather than normalized 0-1 units, and the fourth component
    ///   is the `(-1 - atlas_id)` where `atlas_id` identifies which texture atlas to use.
    ///
    /// Design note: It would be adequate to use `f16` here, but that's a WebGPU optional extension.
    color_or_texture: [f32; 4],

    /// Interpolated texture coordinates are clamped to be within these ranges,
    /// to avoid bleeding.
    ///
    /// Each `u32` is two packed [`FixTexCoord`], `min | (max << 16)`.
    ///
    /// Design note: It would be more straightforward to use `f16` here, but that's a
    /// WebGPU optional extension.
    ///
    /// # Note on data packing
    ///
    /// One might wonder if we could pack this smaller by using relative coordinates — (min,size)
    /// instead of (min,max). The size is at most 256, so we need 9 bits per size, so the total
    /// number of bits for a full box would be (16 + 9) × 3 = 75 bits, which still exceeds
    /// 64 bits. Therefore, we cannot do better here without using spare bits from
    /// `color_or_texture` — which is possible but not simple.
    clamp_min_max: [u32; 3],
}

impl BPosition {
    pub const LAYOUT: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: size_of::<Self>() as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &wgpu::vertex_attr_array![
            0 => Uint32, // cube_packed
            1 => Uint32, // position_in_cube_and_normal_and_resolution_packed
            // location numbers must not clash with WgpuInstanceData
        ],
    };
}

impl BColor {
    pub const LAYOUT: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: size_of::<Self>() as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &wgpu::vertex_attr_array![
            2 => Float32x4, // color_or_texture
            3 => Uint32x3, // clamp_min_max
            // location numbers must not clash with WgpuInstanceData
        ],
    };
}

impl Vertex for BPosition {
    const WANTS_DEPTH_SORTING: bool = true;
    type SecondaryData = BColor;
    /// Packed cube coordinates
    type BlockInst = u32;
    type TexPoint = TexPoint;

    #[inline]
    fn from_block_vertex(vertex: BlockVertex<Self::TexPoint>) -> (BPosition, BColor) {
        let position_in_cube_fixed: Point3D<u32, CubeFix128> =
            vertex.position.map(|coord| (coord * 128.) as u32).cast_unit();
        let cube_packed = 0; // will be overwritten later by instantiate_vertex()
        let normal = vertex.face as u32;

        let position_in_cube_and_normal_packed = position_in_cube_fixed.x
            | (position_in_cube_fixed.y << 8)
            | (position_in_cube_fixed.z << 16)
            | (normal << 24);
        match vertex.coloring {
            mesh::Coloring::Solid(color) => {
                let mut color_attribute: [f32; 4] = color.into();
                // Clamp out-of-range alpha values so they fit into the
                // VertexColorOrTexture protocol (not less than zero).
                color_attribute[3] = color_attribute[3].clamp(0., 1.);
                (
                    BPosition {
                        cube_packed,
                        position_in_cube_and_normal_and_resolution_packed:
                            position_in_cube_and_normal_packed,
                    },
                    BColor {
                        color_or_texture: color_attribute,
                        clamp_min_max: [0, 0, 0],
                    },
                )
            }
            mesh::Coloring::Texture {
                pos: TexPoint { tc, atlas_id },
                clamp_min,
                clamp_max,
                resolution,
            } => (
                BPosition {
                    cube_packed,
                    position_in_cube_and_normal_and_resolution_packed:
                        position_in_cube_and_normal_packed | (u32::from(resolution.log2()) << 28),
                },
                BColor {
                    color_or_texture: [
                        tc.x.into(),
                        tc.y.into(),
                        tc.z.into(),
                        -1.0 - f32::from(atlas_id),
                    ],
                    clamp_min_max: clamp_min.tc.zip(clamp_max.tc, FixTexCoord::pack).into(),
                },
            ),
        }
    }

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
    fn position(&self) -> mesh::Position {
        let cube = Point3D::new(
            self.cube_packed & 0xFF,
            (self.cube_packed >> 8) & 0xFF,
            (self.cube_packed >> 16) & 0xFF,
        );
        let pos_packed = self.position_in_cube_and_normal_and_resolution_packed;
        let position_in_cube_fixed = Vector3D::new(
            pos_packed & 0xFF,
            (pos_packed >> 8) & 0xFF,
            (pos_packed >> 16) & 0xFF,
        );
        cube.map(|c| c as f32) + position_in_cube_fixed.map(|c| c as f32 / 128.)
    }
}

/// Data for a block mesh instance (in the sense of _instancing_).
#[derive(Clone, Copy, Debug, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct WgpuInstanceData {
    /// Translation vector which may be added to [`BPosition::position`]
    /// to obtain world (that is, [`Space`]) coordinates.
    ///
    /// TODO: Eventually we want to be able to use camera-relative coordinates, to make
    /// the best use of f32 precision in the shader.
    pub translation: [f32; 3],

    /// Text to draw onto the meshes, up to 16 ISO-8859-1 characters, packed into four `u32`s,
    pub debug_text: [u32; 4],
}

impl WgpuInstanceData {
    pub const LAYOUT: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: size_of::<Self>() as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Instance,
        attributes: const {
            &wgpu::vertex_attr_array![
                // location numbers must start after BPosition ends
                6 => Float32x3,
                7 => Uint32x4,
            ]
        },
    };

    /// `debug_text` should format to ASCII (or ISO-8859-1) text of no more than 16 characters
    /// which describes what kind of instance this is (e.g. chunk or block).
    /// It may be ignored if debug is not enabled.
    pub fn new(translation: GridVector, debug_text: &dyn fmt::Display) -> Self {
        Self {
            translation: translation.map(|int| int as f32).into(),
            debug_text: if DEBUG_INSTANCES {
                format_into_debug_text_vector(debug_text)
            } else {
                [0; 4]
            },
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
    fn from_position_color(position: all_is_cubes::math::FreePoint, color: Rgba) -> Self {
        Self {
            position: position.map(|c| c as f32).into(),
            color: color.into(),
        }
    }
}

/// Fixed-point texture coordinate, with a multiplier of 2 so it can represent edges and
/// middles of texels (0.0, 0.5, 1.0, 1.5, ...)
///
/// This type should not be used directly; it is public as an element of [`BPosition`]'s
/// trait implementations.
#[derive(Clone, Copy, Debug, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[allow(unnameable_types)]
#[repr(C)]
pub struct FixTexCoord(u16);

impl FixTexCoord {
    pub(crate) fn from_float(float_tc: f32) -> Self {
        debug_assert!(
            float_tc >= 0.0,
            "texture coordinate {float_tc:?} should not be negative"
        );
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

/// Packs text into the WGSL-compatible `debug_text: vec4<u32>` format.
/// If the input is too long, silently truncates it.
fn format_into_debug_text_vector(message: &dyn fmt::Display) -> [u32; 4] {
    fn little_endian_byte_refs(int: &mut u32) -> [&mut u8; 4] {
        let [b0, b1, b2, b3] = bytemuck::bytes_of_mut(int) else {
            unreachable!()
        };
        if cfg!(target_endian = "big") {
            [b3, b2, b1, b0]
        } else {
            [b0, b1, b2, b3]
        }
    }

    struct Writer<I>(I);
    impl<'a, I: Iterator<Item = &'a mut u8>> fmt::Write for Writer<I> {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            for ch in s.chars() {
                match self.0.next() {
                    Some(byte) => *byte = ch.try_into().unwrap_or(b'\xFF'),
                    // Stop formatting if we hit the length limit.
                    None => return Err(fmt::Error),
                }
            }
            Ok(())
        }
    }

    let mut buf: [u32; 4] = [0; 4];
    // Ignore errors, which occur upon truncation.
    let _ = write!(
        Writer(buf.iter_mut().flat_map(little_endian_byte_refs)),
        "{message}"
    );
    buf
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
        assert_eq!(size_of::<BPosition>(), 8);
        assert_eq!(size_of::<BColor>(), 28);
        assert_eq!(size_of::<WgpuLinesVertex>(), 28);
    }

    /// Tests the implementation of [`Vertex::position()`],
    /// because if it's wrong the only thing that breaks is depth-sorting,
    /// and because it is a useful test of the outgoing coordinate processing logic too.
    #[test]
    fn block_vertex_position() {
        let mut vertex = BPosition::from_block_vertex(BlockVertex {
            position: Point3D::new(0.25, 0.0, 1.0),
            face: Face6::PX,
            coloring: mesh::Coloring::Solid(Rgba::new(0.0, 0.5, 1.0, 0.5)),
        })
        .0;
        vertex.instantiate_vertex(BPosition::instantiate_block(Cube::new(100, 50, 7)));
        assert_eq!(Vertex::position(&vertex), Point3D::new(100.25, 50.0, 8.0));
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

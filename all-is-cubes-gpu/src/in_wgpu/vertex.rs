use all_is_cubes::cgmath::{EuclideanSpace as _, Point3, Vector3};
use all_is_cubes::math::GridPoint;
use all_is_cubes::mesh::{BlockVertex, Coloring, GfxVertex};

use crate::DebugLineVertex;

pub(crate) type TexPoint = Point3<f32>;

/// Triangle mesh vertex type that is used for rendering [blocks].
///
/// [blocks]: all_is_cubes::block::Block
#[derive(Clone, Copy, Debug, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub(crate) struct WgpuBlockVertex {
    /// Vertex position.
    position: [f32; 3],
    /// Position of the cube containing the triangle this vertex belongs to.
    /// Note that this is not the same as floor() of `self.position`.
    ///
    /// TODO: Once we implement storing chunks in relative coordinates for better
    /// precision, we can reduce this representation size down to i8 or u8.
    cube: [f32; 3],
    /// Vertex normal in [`Face7`] format.
    /// TODO: Make use of all the spare bits here for something.
    normal: u32,
    /// Packed format:
    /// * If `[3]` is in the range 0.0 to 1.0, then the attribute is a linear RGBA color.
    /// * If `[3]` is -1.0, then the first three components are 3D texture coordinates.
    color_or_texture: [f32; 4],
    /// Interpolated texture coordinates are clamped to be ≥ this value, to avoid bleeding.
    clamp_min: [f32; 3],
    /// Interpolated texture coordinates are clamped to be ≤ this value, to avoid bleeding.
    clamp_max: [f32; 3],
}

impl WgpuBlockVertex {
    const ATTRIBUTE_LAYOUT: &'static [wgpu::VertexAttribute] = &wgpu::vertex_attr_array![
        0 => Float32x3, // position
        1 => Float32x3, // cube
        2 => Uint32, // normal
        3 => Float32x4, // color_or_texture
        4 => Float32x3, // clamp_min
        5 => Float32x3, // clamp_max
    ];

    pub fn desc() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Self>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: Self::ATTRIBUTE_LAYOUT,
        }
    }
}

impl From<BlockVertex<TexPoint>> for WgpuBlockVertex {
    #[inline]
    fn from(vertex: BlockVertex<TexPoint>) -> Self {
        let position = vertex.position.cast::<f32>().unwrap().to_vec();
        let cube = [0., 0., 0.];
        let normal = vertex.face as u32;
        match vertex.coloring {
            Coloring::Solid(color) => {
                let mut color_attribute: [f32; 4] = color.into();
                // Clamp out-of-range alpha values so they fit into the
                // VertexColorOrTexture protocol (not less than zero).
                color_attribute[3] = color_attribute[3].min(1.).max(0.);
                Self {
                    position: position.into(),
                    cube,
                    normal,
                    color_or_texture: color_attribute,
                    clamp_min: [0., 0., 0.],
                    clamp_max: [0., 0., 0.],
                }
            }
            Coloring::Texture {
                pos: tc,
                clamp_min,
                clamp_max,
            } => Self {
                position: position.into(),
                cube,
                normal,
                color_or_texture: [tc[0], tc[1], tc[2], -1.0],
                clamp_min: clamp_min.into(),
                clamp_max: clamp_max.into(),
            },
        }
    }
}

impl GfxVertex for WgpuBlockVertex {
    const WANTS_DEPTH_SORTING: bool = true;
    type Coordinate = f32;
    type BlockInst = Vector3<f32>;
    type TexPoint = TexPoint;

    #[inline]
    fn instantiate_block(cube: GridPoint) -> Self::BlockInst {
        cube.to_vec().map(|s| s as f32)
    }

    #[inline]
    fn instantiate_vertex(&mut self, cube: Self::BlockInst) {
        self.position[0] += cube.x;
        self.position[1] += cube.y;
        self.position[2] += cube.z;
        self.cube = cube.into();
    }

    #[inline]
    fn position(&self) -> Point3<Self::Coordinate> {
        Point3::from(self.position)
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
    const ATTRIBUTE_LAYOUT: &'static [wgpu::VertexAttribute] = &wgpu::vertex_attr_array![
        0 => Float32x3,
        1 => Float32x2,
    ];

    pub fn desc() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Self>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: Self::ATTRIBUTE_LAYOUT,
        }
    }
}

impl DebugLineVertex for WgpuLinesVertex {
    fn from_position_color(
        position: Point3<all_is_cubes::math::FreeCoordinate>,
        color: all_is_cubes::math::Rgba,
    ) -> Self {
        Self {
            position: position.map(|c| c as f32).into(),
            color: color.into(),
        }
    }
}

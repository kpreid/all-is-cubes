// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use all_is_cubes::cgmath::{EuclideanSpace as _, Point3, Vector3};
use all_is_cubes::math::{Face7, GridCoordinate, GridPoint, GridVector};
use all_is_cubes::mesh::{BlockVertex, Coloring, GfxVertex};
use all_is_cubes::space::PackedLight;

use crate::DebugLineVertex;

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
    /// Vertex normal (should be length 1).
    /// TODO: Try storing this as an enum
    normal: [f32; 3],
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
        0 => Float32x3,
        1 => Float32x3,
        2 => Float32x3,
        3 => Float32x4,
        4 => Float32x3,
        5 => Float32x3,
    ];

    pub fn desc() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Self>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: Self::ATTRIBUTE_LAYOUT,
        }
    }
}

impl From<BlockVertex> for WgpuBlockVertex {
    #[inline]
    fn from(vertex: BlockVertex) -> Self {
        let position = vertex.position.cast::<f32>().unwrap().to_vec();
        let cube = [0., 0., 0.];
        let normal = vertex.face.normal_vector::<f32>().into();
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
    type Coordinate = f32;
    type BlockInst = Vector3<f32>;
    const WANTS_LIGHT: bool = false;

    #[inline]
    fn instantiate_block(cube: GridPoint) -> Self::BlockInst {
        cube.to_vec().map(|s| s as f32)
    }

    #[inline]
    fn instantiate_vertex(&mut self, cube: Self::BlockInst, _lighting: PackedLight) {
        self.position[0] += cube.x;
        self.position[1] += cube.y;
        self.position[2] += cube.z;
        self.cube = cube.into();
    }

    #[inline]
    fn position(&self) -> Point3<Self::Coordinate> {
        Point3::from(self.position)
    }

    #[inline]
    fn face(&self) -> Face7 {
        let normal: GridVector = Vector3::from(self.normal).map(|c| c as GridCoordinate);
        Face7::try_from(normal).unwrap_or(Face7::Within)
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

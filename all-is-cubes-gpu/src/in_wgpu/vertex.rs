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
    /// World position of the cube containing the triangle this vertex belongs to.
    ///
    /// This is used for looking up light data, so the light data texture must be in the
    /// same coordinate system.
    ///
    /// Note that this is not the same as floor() of the final coordinates, since a
    /// block's mesh coordinates range from 0 to 1 inclusive.
    ///
    /// TODO: Once we implement storing chunks in relative coordinates for better
    /// precision, we can reduce this representation size.
    cube: [i32; 3],
    /// Vertex position within the cube. Ranges from 0 to 1 inclusive.
    /// This is added to `cube` to make the true vertex position.
    ///
    /// TODO: Try storing this in fixed point u16s
    position_in_cube: [f32; 3],
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
        0 => Sint32x3, // cube
        1 => Float32x3, // position_in_cube
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
        let position_in_cube: [f32; 3] = vertex.position.cast::<f32>().unwrap().to_vec().into();
        let cube = [0, 0, 0]; // will be overwritten later by instantiate_vertex()
        let normal = vertex.face as u32;
        match vertex.coloring {
            Coloring::Solid(color) => {
                let mut color_attribute: [f32; 4] = color.into();
                // Clamp out-of-range alpha values so they fit into the
                // VertexColorOrTexture protocol (not less than zero).
                color_attribute[3] = color_attribute[3].clamp(0., 1.);
                Self {
                    position_in_cube,
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
                position_in_cube,
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
    type BlockInst = [i32; 3];
    type TexPoint = TexPoint;

    #[inline]
    fn instantiate_block(cube: GridPoint) -> Self::BlockInst {
        cube.into()
    }

    #[inline]
    fn instantiate_vertex(&mut self, cube: Self::BlockInst) {
        self.cube = cube;
    }

    #[inline]
    fn position(&self) -> Point3<Self::Coordinate> {
        Point3::from(self.cube).map(|c| c as f32) + Vector3::from(self.position_in_cube)
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

#[cfg(test)]
mod tests {
    use all_is_cubes::math::{Face6, Rgba};
    use std::mem;

    use super::*;

    /// Assert the vertex's size, just so that we're reminded to think about it when we
    /// change the amount of data in it. This assertion is not platform-dependent because
    /// the struct is designed to have a fixed layout communicating to the shader anyway.
    #[test]
    fn vertex_size() {
        assert_eq!(mem::size_of::<WgpuBlockVertex>(), 68);
        assert_eq!(mem::size_of::<WgpuLinesVertex>(), 28);
    }

    /// Test implementation of [`GfxVertex::position()`],
    /// because if it's wrong the only thing that breaks is depth-sorting,
    /// and because it is a useful test of the outgoing coordinate processing logic too.
    #[test]
    fn block_vertex_position() {
        let mut vertex = WgpuBlockVertex::from(BlockVertex {
            position: Point3::new(0.25, 0.0, 1.0),
            face: Face6::PX,
            coloring: Coloring::Solid(Rgba::new(0.0, 0.5, 1.0, 0.5)),
        });
        vertex.instantiate_vertex(WgpuBlockVertex::instantiate_block(Point3::new(
            100, -100, 7,
        )));
        assert_eq!(
            GfxVertex::position(&vertex),
            Point3::new(100.25, -100.0, 8.0)
        );
    }
}

// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::fmt;

use all_is_cubes::{
    cgmath::Vector2,
    drawing::embedded_graphics::{
        self,
        draw_target::DrawTarget,
        pixelcolor::Rgb888,
        prelude::{OriginDimensions, RgbColor, Size},
        Pixel,
    },
};

use all_is_cubes::space::Grid;

use crate::in_wgpu::glue::write_texture_by_grid;

/// A RGBA [`wgpu::Texture`] with a local buffer that can be drawn on.
pub(crate) struct DrawableTexture {
    texture: Option<wgpu::Texture>,
    texture_view: Option<wgpu::TextureView>,
    size: wgpu::Extent3d,
    local_data: Box<[[u8; 4]]>,
}

impl DrawableTexture {
    pub fn new() -> Self {
        Self {
            texture: None,
            texture_view: None,
            size: wgpu::Extent3d {
                width: 0,
                height: 0,
                depth_or_array_layers: 1,
            },
            local_data: Box::new([]),
        }
    }

    /// Adjusts the texture size.
    ///
    /// The current texture data will be discarded if and only if the given size is
    /// different than the previous size.
    pub fn resize(&mut self, device: &wgpu::Device, label: Option<&str>, size: Vector2<u32>) {
        let new_extent = wgpu::Extent3d {
            width: size.x,
            height: size.y,
            depth_or_array_layers: 1,
        };
        if new_extent == self.size && self.texture.is_some() {
            return;
        }

        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label,
            size: new_extent,
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        });

        *self = Self {
            // TODO: systematic overflow checks
            local_data: vec![[0; 4]; (size.x as usize) * (size.y as usize)].into_boxed_slice(),
            texture_view: Some(texture.create_view(&wgpu::TextureViewDescriptor::default())),
            texture: Some(texture),
            size: new_extent,
        };
    }

    pub fn data(&mut self) -> &mut [[u8; 4]] {
        &mut self.local_data
    }

    pub fn view(&self) -> Option<&wgpu::TextureView> {
        self.texture_view.as_ref()
    }

    pub fn upload(&mut self, queue: &wgpu::Queue) {
        if let Some(texture) = &self.texture {
            // wrte_texture_by_grid isn't exactly meant for this but it saves us computing byte lengths
            write_texture_by_grid(
                queue,
                texture,
                // TODO: we shouldn't be recomputing this but cache it
                Grid::new(
                    [0, 0, 0],
                    [self.size.width as i32, self.size.height as i32, 1],
                ),
                &*self.local_data,
            );
        }
    }
}

impl fmt::Debug for DrawableTexture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DrawableTexture")
            .field("texture", &self.texture.is_some())
            .field("size", &self.size)
            // Skipping .local_data because it's a large image
            .finish_non_exhaustive()
    }
}

impl DrawTarget for DrawableTexture {
    type Color = Rgb888;
    type Error = std::convert::Infallible;

    #[inline]
    fn draw_iter<I>(&mut self, pixels: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = embedded_graphics::Pixel<Self::Color>>,
    {
        // Borrow Grid's indexing logic to do our 2D indexing (and set up for a Y flip)
        let wgpu::Extent3d { width, height, .. } = self.size;
        let grid = Grid::new(
            [0, (1 - height as i32), 0],
            [1, height as i32, width as i32],
        );

        for Pixel(point, color) in pixels.into_iter() {
            if let Some(index) = grid.index([0, -point.y, point.x]) {
                self.local_data[index] = [color.r(), color.g(), color.b(), 255];
            }
        }
        Ok(())
    }
}

impl OriginDimensions for DrawableTexture {
    fn size(&self) -> Size {
        Size {
            width: self.size.width,
            height: self.size.height,
        }
    }
}

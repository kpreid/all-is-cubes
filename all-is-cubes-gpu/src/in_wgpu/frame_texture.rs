// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::fmt;

use all_is_cubes::cgmath::Vector2;
use all_is_cubes::drawing::embedded_graphics::prelude::{OriginDimensions, Size};

use crate::EgFramebuffer;

/// A RGBA [`wgpu::Texture`] with a local buffer that can be drawn on.
pub(crate) struct DrawableTexture {
    texture: Option<wgpu::Texture>,
    texture_view: Option<wgpu::TextureView>,
    size: wgpu::Extent3d,
    local_buffer: EgFramebuffer,
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
            local_buffer: EgFramebuffer::new(Size::zero()),
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
            local_buffer: EgFramebuffer::new(Size {
                width: new_extent.width,
                height: new_extent.height,
            }),
            texture_view: Some(texture.create_view(&wgpu::TextureViewDescriptor::default())),
            texture: Some(texture),
            size: new_extent,
        };
    }

    pub fn draw_target(&mut self) -> &mut EgFramebuffer {
        &mut self.local_buffer
    }

    pub fn view(&self) -> Option<&wgpu::TextureView> {
        self.texture_view.as_ref()
    }

    pub fn is_nonzero(&self) -> bool {
        self.local_buffer.is_nonzero()
    }

    pub fn upload(&mut self, queue: &wgpu::Queue) {
        let dirty_rect = self.local_buffer.dirty_rect();
        if !dirty_rect.is_zero_sized() {
            if let Some(texture) = &self.texture {
                let full_width = self.local_buffer.size().width;
                queue.write_texture(
                    wgpu::ImageCopyTexture {
                        texture,
                        mip_level: 0,
                        origin: wgpu::Origin3d {
                            x: dirty_rect.top_left.x as u32,
                            y: dirty_rect.top_left.y as u32,
                            z: 0,
                        },
                        aspect: wgpu::TextureAspect::All,
                    },
                    bytemuck::cast_slice(
                        &self.local_buffer.data()[full_width as usize
                            * dirty_rect.top_left.y as usize
                            + dirty_rect.top_left.x as usize..],
                    ),
                    wgpu::ImageDataLayout {
                        offset: 0,
                        bytes_per_row: std::num::NonZeroU32::new(4 * full_width),
                        rows_per_image: None,
                    },
                    wgpu::Extent3d {
                        width: dirty_rect.size.width,
                        height: dirty_rect.size.height,
                        depth_or_array_layers: 1,
                    },
                );

                self.local_buffer.mark_not_dirty();
            }
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

impl OriginDimensions for DrawableTexture {
    fn size(&self) -> Size {
        Size {
            width: self.size.width,
            height: self.size.height,
        }
    }
}

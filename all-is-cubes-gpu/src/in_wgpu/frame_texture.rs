use std::fmt;

use all_is_cubes::camera::GraphicsOptions;
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

/// Resources for storing intermediate states of the frame being drawn, that depend on the
/// current viewport (surface) size and must be entirely recreated if it changes.
///
/// The details of this structure are likely to change in parallel with
/// [`super::EverythingRenderer`]. TODO: Find a better code organization.
#[derive(Debug)]
pub(crate) struct FramebufferTextures {
    /// Framebuffer/viewport size, remembered for comparison purposes.
    size: wgpu::Extent3d,

    /// Texture into which geometry is drawn before postprocessing.
    ///
    /// Is a floating-point texture allowing HDR rendering, if the backend supports that.
    /// "Linear" in that the values stored in it are not sRGB-encoded as read and written,
    /// though they are if that's all we can support.
    pub(crate) linear_scene_texture_view: wgpu::TextureView,
    pub(crate) linear_scene_texture_format: wgpu::TextureFormat,

    /// If multisampling is enabled, provides the “resolve target” companion to
    /// `linear_scene_texture_view`. This texture has a sample_count of 1, is
    /// automatically written to when we do multisampled rendering, and is the *input*
    /// to postprocessing. If multisampling is not enabled, we use the
    /// `linear_scene_texture_view` directly as input to postprocessing.
    pub(crate) linear_scene_resolved: Option<wgpu::TextureView>,

    /// Depth texture to pair with `linear_scene_texture`.
    pub(crate) depth_texture_view: wgpu::TextureView,

    pub(crate) sample_count: u32,
}

impl FramebufferTextures {
    /// Depth bufffer format that [`FramebufferTextures`] will always use.
    pub(crate) const DEPTH_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Depth32Float;

    /// Texture usages that the `linear_scene_texture_format` must support.
    pub(crate) const LINEAR_SCENE_TEXTURE_USAGES: wgpu::TextureUsages =
        wgpu::TextureUsages::RENDER_ATTACHMENT.union(wgpu::TextureUsages::TEXTURE_BINDING);

    /// `config` must be valid (in particular, not zero sized).
    pub(crate) fn new(
        device: &wgpu::Device,
        config: &wgpu::SurfaceConfiguration,
        linear_scene_texture_format: wgpu::TextureFormat,
        sample_count: u32,
    ) -> Self {
        let size = wgpu::Extent3d {
            width: config.width,
            height: config.height,
            depth_or_array_layers: 1,
        };
        let linear_scene_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("linear_scene_texture"),
            size,
            mip_level_count: 1,
            sample_count,
            dimension: wgpu::TextureDimension::D2,
            format: linear_scene_texture_format,
            usage: Self::LINEAR_SCENE_TEXTURE_USAGES,
        });
        let depth_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("depth_texture"),
            size,
            mip_level_count: 1,
            sample_count,
            dimension: wgpu::TextureDimension::D2,
            format: Self::DEPTH_FORMAT,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
        });

        Self {
            size,
            linear_scene_resolved: if sample_count > 1 {
                Some(
                    device
                        .create_texture(&wgpu::TextureDescriptor {
                            label: Some("linear_scene_texture_resolve_target"),
                            size: wgpu::Extent3d {
                                width: config.width,
                                height: config.height,
                                depth_or_array_layers: 1,
                            },
                            mip_level_count: 1,
                            sample_count: 1,
                            dimension: wgpu::TextureDimension::D2,
                            format: linear_scene_texture_format,
                            usage: Self::LINEAR_SCENE_TEXTURE_USAGES,
                        })
                        .create_view(&wgpu::TextureViewDescriptor::default()),
                )
            } else {
                None
            },
            linear_scene_texture_format,
            linear_scene_texture_view: linear_scene_texture
                .create_view(&wgpu::TextureViewDescriptor::default()),
            depth_texture_view: depth_texture.create_view(&Default::default()),
            sample_count,
        }
    }

    pub(crate) fn sample_count_from_options(options: &GraphicsOptions) -> u32 {
        if options.antialiasing.is_msaa() {
            4
        } else {
            1
        }
    }

    /// Returns the color attachment for rendering the scene, taking into account
    /// considerations for multisampling.
    pub(crate) fn color_attachment_for_scene(
        &self,
        color_load_op: wgpu::LoadOp<wgpu::Color>,
    ) -> wgpu::RenderPassColorAttachment<'_> {
        wgpu::RenderPassColorAttachment {
            view: &self.linear_scene_texture_view,
            resolve_target: self.linear_scene_resolved.as_ref(),
            ops: wgpu::Operations {
                load: color_load_op,
                store: true,
            },
        }
    }

    pub(crate) fn scene_for_postprocessing_input(&self) -> &wgpu::TextureView {
        if let Some(resolved) = &self.linear_scene_resolved {
            resolved
        } else {
            &self.linear_scene_texture_view
        }
    }

    /// Update `self` to be as if it had been recreated with [`Self::new()`]
    /// if any input is different in a way that requires it.
    ///
    /// Returns whether any update was made.
    pub(crate) fn rebuild_if_changed(
        &mut self,
        device: &wgpu::Device,
        config: &wgpu::SurfaceConfiguration,
        options: &GraphicsOptions,
    ) -> bool {
        let new_size = wgpu::Extent3d {
            width: config.width,
            height: config.height,
            depth_or_array_layers: 1,
        };
        let new_sample_count = Self::sample_count_from_options(options);

        if new_size != self.size || new_sample_count != self.sample_count {
            *self = Self::new(
                device,
                config,
                self.linear_scene_texture_format,
                new_sample_count,
            );
            true
        } else {
            false
        }
    }
}

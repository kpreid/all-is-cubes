use std::fmt;
use std::sync::atomic::AtomicBool;

use all_is_cubes::camera::{Flaws, GraphicsOptions};
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::drawing::embedded_graphics::prelude::{OriginDimensions, Size};

use super::bloom;
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
            view_formats: &[],
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
                        bytes_per_row: Some(4 * full_width),
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
    /// Pure-data (no GPU resources) record of what format we chose.
    config: FbtConfig,

    /// Texture into which geometry is drawn before postprocessing.
    ///
    /// Is a floating-point texture allowing HDR rendering, if the backend supports that.
    /// "Linear" in that the values stored in it are not sRGB-encoded as read and written,
    /// though they are if that's all we can support.
    linear_scene_tex: wgpu::Texture,
    /// View for writing into [`Self::linear_scene_texture`], and reading if multisampling
    /// is not enabled.
    linear_scene_view: wgpu::TextureView,

    /// If multisampling is enabled, provides the “resolve target” companion to
    /// `linear_scene_texture`. This texture has a sample_count of 1, is
    /// automatically written to when we do multisampled rendering, and is the *input*
    /// to postprocessing. If multisampling is not enabled, we use the
    /// `linear_scene_texture` directly as input to postprocessing.
    linear_scene_resolved_tex: Option<wgpu::Texture>,
    /// View for reading [`Self::linear_scene_resolved_tex`].
    linear_scene_resolved_view: Option<wgpu::TextureView>,

    /// Depth texture to pair with `linear_scene_texture`.
    pub(crate) depth_texture_view: wgpu::TextureView,

    /// Resources necessary for computing bloom if enabled.
    pub(crate) bloom: Option<bloom::BloomResources>,
}

impl FramebufferTextures {
    /// Depth bufffer format that [`FramebufferTextures`] will always use.
    pub(crate) const DEPTH_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Depth32Float;

    /// Texture usages that the `linear_scene_texture_format` must support.
    pub const LINEAR_SCENE_TEXTURE_USAGES: wgpu::TextureUsages =
        wgpu::TextureUsages::RENDER_ATTACHMENT.union(wgpu::TextureUsages::TEXTURE_BINDING);

    /// `config` must be valid (in particular, not zero sized).
    pub(crate) fn new(
        features: FbtFeatures,
        device: &wgpu::Device,
        config: &wgpu::SurfaceConfiguration,
        options: &GraphicsOptions,
        enable_copy_out: bool,
    ) -> Self {
        Self::new_from_config(
            device,
            FbtConfig::new(config, features, options, enable_copy_out),
        )
    }

    fn new_from_config(device: &wgpu::Device, config: FbtConfig) -> Self {
        let mut linear_scene_texture_usages_with_copy = Self::LINEAR_SCENE_TEXTURE_USAGES;
        if config.enable_copy_out {
            linear_scene_texture_usages_with_copy |= wgpu::TextureUsages::COPY_SRC;
        }

        let linear_scene_tex = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("linear_scene_texture"),
            size: config.size,
            mip_level_count: 1,
            sample_count: config.sample_count,
            dimension: wgpu::TextureDimension::D2,
            format: config.linear_scene_texture_format,
            view_formats: &[],
            usage: linear_scene_texture_usages_with_copy,
        });
        let linear_scene_resolved_tex = if config.sample_count > 1 {
            Some(device.create_texture(&wgpu::TextureDescriptor {
                label: Some("linear_scene_texture_resolve_target"),
                size: config.size,
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format: config.linear_scene_texture_format,
                view_formats: &[],
                usage: linear_scene_texture_usages_with_copy,
            }))
        } else {
            None
        };

        let depth_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("depth_texture"),
            size: config.size,
            mip_level_count: 1,
            sample_count: config.sample_count,
            dimension: wgpu::TextureDimension::D2,
            format: Self::DEPTH_FORMAT,
            view_formats: &[],
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
        });

        let linear_scene_view = linear_scene_tex.create_view(&Default::default());
        let linear_scene_resolved_view = linear_scene_resolved_tex
            .as_ref()
            .map(|t| t.create_view(&Default::default()));

        // TODO: duplicative with scene_for_postprocessing_input
        let bloom_input_view = if let Some(resolved) = &linear_scene_resolved_view {
            resolved
        } else {
            &linear_scene_view
        };

        Self {
            bloom: Some(bloom::BloomResources::new(
                device,
                // TODO: Don't reconstruct on every resize, but reuse it. Has a circularity
                // problem with needing FbtConfig, but it really only needs FbtFeatures.
                bloom::BloomPipelines::new(device, config.linear_scene_texture_format),
                &config,
                bloom_input_view,
            )),
            linear_scene_view,
            linear_scene_resolved_view,
            depth_texture_view: depth_texture.create_view(&Default::default()),

            config,
            linear_scene_tex,
            linear_scene_resolved_tex,
        }
    }

    /// Returns the color attachment for rendering the scene, taking into account
    /// considerations for multisampling.
    pub(crate) fn color_attachment_for_scene(
        &self,
        color_load_op: wgpu::LoadOp<wgpu::Color>,
    ) -> wgpu::RenderPassColorAttachment<'_> {
        wgpu::RenderPassColorAttachment {
            view: &self.linear_scene_view,
            resolve_target: self.linear_scene_resolved_view.as_ref(),
            ops: wgpu::Operations {
                load: color_load_op,
                store: true,
            },
        }
    }

    pub(crate) fn scene_for_postprocessing_input(&self) -> &wgpu::TextureView {
        if let Some(resolved) = &self.linear_scene_resolved_view {
            resolved
        } else {
            &self.linear_scene_view
        }
    }

    #[allow(unused)] // Used only in cfg(test) and so are the fields; this is simple.
    pub(crate) fn scene_for_test_copy(&self) -> &wgpu::Texture {
        if let Some(resolved) = &self.linear_scene_resolved_tex {
            resolved
        } else {
            &self.linear_scene_tex
        }
    }

    pub(crate) fn linear_scene_multisample_state(&self) -> wgpu::MultisampleState {
        wgpu::MultisampleState {
            count: self.config.sample_count,
            mask: !0,
            alpha_to_coverage_enabled: false,
        }
    }

    pub(crate) fn linear_scene_texture_format(&self) -> wgpu::TextureFormat {
        self.config.linear_scene_texture_format
    }

    /// Update `self` to be as if it had been recreated with [`Self::new()`]
    /// if any input is different in a way that requires it.
    ///
    /// Returns whether any update was made.
    pub(crate) fn rebuild_if_changed(
        &mut self,
        device: &wgpu::Device,
        surface_config: &wgpu::SurfaceConfiguration,
        options: &GraphicsOptions,
    ) -> bool {
        let new_config = FbtConfig::new(
            surface_config,
            self.config.features,
            options,
            self.config.enable_copy_out,
        );
        if new_config != self.config {
            *self = Self::new_from_config(device, new_config);
            true
        } else {
            false
        }
    }

    pub(crate) fn flaws(&self) -> Flaws {
        self.config.flaws
    }

    pub(crate) fn bloom_data_texture(&self) -> &wgpu::TextureView {
        &self
            .bloom
            .as_ref()
            .expect("TODO: disabling bloom not implemented")
            .bloom_output_texture_view
    }
}

/// A decision about what texture format [`FramebufferTextures`] should employ,
/// separated from the format itself.
///
/// This struct's `==` can be used to decide whether re-creation is needed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct FbtConfig {
    /// Features remembered for future use.
    features: FbtFeatures,

    /// Size of all textures.
    pub(crate) size: wgpu::Extent3d,

    /// Texture format of the `linear_scene` and `linear_scene_resolved`.
    ///
    /// This is a floating-point format in order to support HDR rendering, *if* the
    /// adapter supports the necessary features (blending and multisample).
    pub(crate) linear_scene_texture_format: wgpu::TextureFormat,

    /// Sample count of `linear_scene_tex`.
    /// Currently always 1 or 4.
    pub(crate) sample_count: u32,

    /// Whether to enable copying out of the linear scene texture for testing purposes.
    enable_copy_out: bool,

    /// Rendering flaws resulting from necessary compromises.
    flaws: Flaws,
}

impl FbtConfig {
    fn new(
        surface_config: &wgpu::SurfaceConfiguration,
        features: FbtFeatures,
        options: &GraphicsOptions,
        enable_copy_out: bool,
    ) -> Self {
        let size = wgpu::Extent3d {
            width: surface_config.width,
            height: surface_config.height,
            depth_or_array_layers: 1,
        };

        let wants_antialiasing = options.antialiasing.is_msaa();
        let sample_count_if_ok = if wants_antialiasing { 4 } else { 1 };
        // TODO: define a "wants_hdr" once we have sorted out how graphics options work for that

        // If possible, we want a float-valued format to allow HDR rendering
        // (but not necessarily HDR *output*; it may be merely tone mapped to SDR).
        // WebGPU specifies Rgba16Float support, but wgpu downlevel backends may not
        // support it.
        //
        // Furthermore, backends may or may not support multisampling together with Rgba16Float.
        if features.float_can_render {
            // We can definitely use Rgba16Float. But can we multisample?
            if features.float_can_multisample {
                // All features present -- choose HDR format and whatever sample_count
                // is wanted.
                Self {
                    features,
                    size,
                    linear_scene_texture_format: wgpu::TextureFormat::Rgba16Float,
                    sample_count: sample_count_if_ok,
                    enable_copy_out,
                    flaws: Flaws::empty(),
                }
            } else {
                // Cannot multisample with HDR -- if it was requested, report flaw.
                Self {
                    features,
                    size,
                    linear_scene_texture_format: wgpu::TextureFormat::Rgba16Float,
                    sample_count: 1,
                    enable_copy_out,
                    flaws: if sample_count_if_ok != 1 {
                        Flaws::NO_ANTIALIASING
                    } else {
                        Flaws::empty()
                    },
                }
            }
        } else {
            // Rendering to Rgba16Float is unavailable.
            // TODO: report no HDR in Flaws.
            Self {
                features,
                size,
                linear_scene_texture_format: wgpu::TextureFormat::Rgba8UnormSrgb,
                sample_count: sample_count_if_ok,
                enable_copy_out,
                flaws: Flaws::empty(),
            }
        }
    }
}

/// Information about an adapter/device's capabilities, obtained from [`wgpu::Adapter`]
/// and used by [`FramebufferTextures`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct FbtFeatures {
    /// True if [`wgpu::TextureFormat::Rgba16Float`] can be used for rendering.
    float_can_render: bool,
    /// True if [`wgpu::TextureFormat::Rgba16Float`] supports multisampling
    /// and `float_can_render` is also true.
    float_can_multisample: bool,
    /// True if [`wgpu::TextureFormat::Rgba8UnormSrgb`] supports multisampling.
    rgba8_can_multisample: bool,
}

impl FbtFeatures {
    pub fn new(adapter: &wgpu::Adapter) -> Self {
        let float_format = wgpu::TextureFormat::Rgba16Float;
        let multisample_flags = wgpu::TextureFormatFeatureFlags::MULTISAMPLE_X4
            | wgpu::TextureFormatFeatureFlags::MULTISAMPLE_RESOLVE;

        let float_tff = adapter.get_texture_format_features(float_format);
        let float_can_render = float_tff
            .allowed_usages
            .contains(FramebufferTextures::LINEAR_SCENE_TEXTURE_USAGES);

        if !float_can_render {
            // Fall back to sRGB, which loses HDR support.
            // TODO: Also disable tone mapping since it'll make things worse.
            // TODO: expose this "not working as intended" via RenderInfo and eventually the UI
            // (except when user settings are such that it doesn't matter).
            static HDR_WARNING_SENT: AtomicBool = AtomicBool::new(false);
            if !HDR_WARNING_SENT.swap(true, std::sync::atomic::Ordering::Relaxed) {
                log::warn!("{float_format:?} texture support unavailable; HDR rendering disabled.");
            }
        }

        Self {
            float_can_render,
            float_can_multisample: float_can_render && float_tff.flags.contains(multisample_flags),
            // Kludge: multisampling does not succeed with this software renderer.
            rgba8_can_multisample: adapter
                .get_texture_format_features(wgpu::TextureFormat::Rgba8UnormSrgb)
                .flags
                .contains(multisample_flags)
                && adapter.get_info().name != "llvmpipe (LLVM 12.0.0, 256 bits)",
        }
    }
}

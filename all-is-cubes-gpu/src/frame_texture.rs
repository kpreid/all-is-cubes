use core::fmt;
use core::sync::atomic::AtomicBool;

use all_is_cubes::drawing::embedded_graphics::prelude::{OriginDimensions, Size};
use all_is_cubes::euclid::Size2D;
use all_is_cubes::math::{PositiveSign, ps32};
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::{GraphicsOptions, ImagePixel};

use super::bloom;
use crate::common::{EgFramebuffer, Identified};
use crate::glue::size2d_to_extent;
use crate::shaders::Shaders;

/// A RGBA [`wgpu::Texture`] with a CPU-side buffer that can be drawn on.
pub(crate) struct DrawableTexture<In, Out> {
    texture_format: wgpu::TextureFormat,
    texture: Option<wgpu::Texture>,
    texture_view: Option<Identified<wgpu::TextureView>>,
    size: wgpu::Extent3d,
    local_buffer: EgFramebuffer<In, Out>,
}

impl<In, Out: Copy + Default + bytemuck::Pod> DrawableTexture<In, Out> {
    /// `texture_format` must be an uncompressed format and the same bytes-per-texel as the
    /// `Out` generic parameter.
    pub fn new(texture_format: wgpu::TextureFormat) -> Self {
        Self {
            texture_format,
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
    pub fn resize(
        &mut self,
        device: &wgpu::Device,
        label: Option<&str>,
        size: Size2D<u32, ImagePixel>,
    ) {
        let new_extent = size2d_to_extent(size);
        if new_extent == self.size && self.texture.is_some() {
            return;
        }

        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label,
            size: new_extent,
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: self.texture_format,
            view_formats: &[],
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        });

        *self = Self {
            texture_format: self.texture_format,
            local_buffer: EgFramebuffer::new(Size {
                width: new_extent.width,
                height: new_extent.height,
            }),
            texture_view: Some(Identified::new(
                texture.create_view(&wgpu::TextureViewDescriptor::default()),
            )),
            texture: Some(texture),
            size: new_extent,
        };
    }

    pub fn draw_target(&mut self) -> &mut EgFramebuffer<In, Out> {
        &mut self.local_buffer
    }

    pub fn view(&self) -> Option<&Identified<wgpu::TextureView>> {
        self.texture_view.as_ref()
    }

    pub fn is_nonzero(&self) -> bool {
        self.local_buffer.is_nonzero()
    }

    pub fn upload(&mut self, queue: &wgpu::Queue) {
        let dirty_rect = self.local_buffer.dirty_rect();
        if dirty_rect.is_zero_sized() {
            return;
        }
        let Some(texture) = &self.texture else { return };
        let full_width = self.local_buffer.size().width;

        queue.write_texture(
            wgpu::TexelCopyTextureInfo {
                texture,
                mip_level: 0,
                origin: wgpu::Origin3d {
                    x: dirty_rect.top_left.x.cast_unsigned(),
                    y: dirty_rect.top_left.y.cast_unsigned(),
                    z: 0,
                },
                // kludge that only works as long as we don't plan to use stencil textures
                aspect: if texture.format().has_depth_aspect() {
                    wgpu::TextureAspect::DepthOnly
                } else {
                    wgpu::TextureAspect::All
                },
            },
            bytemuck::must_cast_slice::<Out, u8>(
                &self.local_buffer.data()[full_width as usize * dirty_rect.top_left.y as usize
                    + dirty_rect.top_left.x as usize..],
            ),
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(const { size_of::<Out>() as u32 } * full_width),
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

impl<In, Out> fmt::Debug for DrawableTexture<In, Out> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DrawableTexture")
            .field("texture", &self.texture.is_some())
            .field("size", &self.size)
            // Skipping .local_data because it's a large image
            .finish_non_exhaustive()
    }
}

impl<In, Out> OriginDimensions for DrawableTexture<In, Out> {
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
    /// Pure-data (no GPU resources) record of what format and size we chose.
    config: FbtConfig,

    /// Texture into which geometry is drawn before postprocessing.
    ///
    /// Is a floating-point texture allowing HDR rendering, if the backend supports that.
    /// "Linear" in that the values stored in it are not sRGB-encoded as read and written,
    /// though they are if that's all we can support.
    linear_scene_tex: wgpu::Texture,
    /// View for writing into [`Self::linear_scene_texture`], and reading if multisampling
    /// is not enabled.
    linear_scene_view: Identified<wgpu::TextureView>,

    /// If multisampling is enabled, provides the “resolve target” companion to
    /// `linear_scene_texture`. This texture has a `sample_count` of 1, is
    /// automatically written to when we do multisampled rendering, and is the *input*
    /// to postprocessing. If multisampling is not enabled, we use the
    /// `linear_scene_texture` directly as input to postprocessing.
    linear_scene_resolved_tex: Option<wgpu::Texture>,
    /// View for reading [`Self::linear_scene_resolved_tex`].
    linear_scene_resolved_view: Option<Identified<wgpu::TextureView>>,

    /// Depth texture to pair with `linear_scene_texture`.
    depth_texture_view: wgpu::TextureView,

    /// If `enable_copy_out`, then we keep a separate depth texture for world and UI, rather
    /// than clearing and reusing a single one.
    ui_depth_texture_view: Option<wgpu::TextureView>,

    /// Resources necessary for computing bloom if enabled.
    pub(crate) bloom: Option<bloom::BloomResources>,
}

impl FramebufferTextures {
    /// Depth bufffer format that [`FramebufferTextures`] will always use.
    pub(crate) const DEPTH_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Depth32Float;

    /// Texture usages that the `linear_scene_texture_format` must support.
    pub const LINEAR_SCENE_TEXTURE_USAGES: wgpu::TextureUsages =
        wgpu::TextureUsages::RENDER_ATTACHMENT.union(wgpu::TextureUsages::TEXTURE_BINDING);

    pub fn new(device: &wgpu::Device, shaders: &Shaders, config: FbtConfig) -> Self {
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
            usage: Self::LINEAR_SCENE_TEXTURE_USAGES,
        });
        let ui_depth_texture_view = if config.enable_copy_out {
            Some(
                device
                    .create_texture(&wgpu::TextureDescriptor {
                        label: Some("ui_depth_texture"),
                        size: config.size,
                        mip_level_count: 1,
                        sample_count: config.sample_count,
                        dimension: wgpu::TextureDimension::D2,
                        format: Self::DEPTH_FORMAT,
                        view_formats: &[],
                        usage: Self::LINEAR_SCENE_TEXTURE_USAGES,
                    })
                    .create_view(&Default::default()),
            )
        } else {
            None
        };

        let linear_scene_view = Identified::new(linear_scene_tex.create_view(&Default::default()));
        let linear_scene_resolved_view = linear_scene_resolved_tex
            .as_ref()
            .map(|t| Identified::new(t.create_view(&Default::default())));

        // TODO: duplicative with scene_for_postprocessing_input
        let bloom_input_view = if let Some(resolved) = &linear_scene_resolved_view {
            resolved
        } else {
            &linear_scene_view
        };

        Self {
            // TODO: create bloom resources only if graphics options say bloom
            bloom: Some(bloom::create_bloom_texture(
                device,
                config.size,
                bloom_input_view,
                // TODO: Don't reconstruct on every resize, but reuse it.
                &bloom::create_bloom_pipelines(device, shaders, config.linear_scene_texture_format),
            )),
            linear_scene_view,
            linear_scene_resolved_view,
            depth_texture_view: depth_texture.create_view(&Default::default()),
            ui_depth_texture_view,

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
            depth_slice: None,
            resolve_target: self.linear_scene_resolved_view.as_deref(),
            ops: wgpu::Operations {
                load: color_load_op,
                store: wgpu::StoreOp::Store,
            },
        }
    }

    /// Returns the depth attachment for rendering the scene
    pub(crate) fn depth_attachment_for_scene(
        &self,
        mut operations: wgpu::Operations<f32>,
        is_ui_layer: bool,
    ) -> wgpu::RenderPassDepthStencilAttachment<'_> {
        if self.config.enable_copy_out {
            // Then always store depth
            operations.store = wgpu::StoreOp::Store;
        }
        wgpu::RenderPassDepthStencilAttachment {
            view: self
                .ui_depth_texture_view
                .as_ref()
                .filter(|_| is_ui_layer)
                .unwrap_or(&self.depth_texture_view),
            depth_ops: Some(operations),
            stencil_ops: None,
        }
    }
    pub(crate) fn scene_for_postprocessing_input(&self) -> &Identified<wgpu::TextureView> {
        if let Some(resolved) = &self.linear_scene_resolved_view {
            resolved
        } else {
            &self.linear_scene_view
        }
    }

    #[allow(
        unused,
        reason = "Used only in special cases such as cfg(test) and cfg(feature = \"rerun\")"
    )]
    pub(crate) fn scene_for_test_copy(&self) -> &wgpu::Texture {
        debug_assert!(self.config.enable_copy_out);
        if let Some(resolved) = &self.linear_scene_resolved_tex {
            resolved
        } else {
            &self.linear_scene_tex
        }
    }

    /// May be used only if `enable_copy_out`
    #[allow(unused)]
    pub(crate) fn scene_depth_for_test(&self) -> &wgpu::TextureView {
        debug_assert!(self.config.enable_copy_out);
        &self.depth_texture_view
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
    /// if any part of the needed configuration is different in a way that requires it.
    ///
    /// Returns whether any update was made.
    pub(crate) fn rebuild_if_changed(
        &mut self,
        device: &wgpu::Device,
        shaders: &Shaders,
        new_config: FbtConfig,
    ) -> bool {
        if new_config != self.config
            || self
                .bloom
                .as_ref()
                .is_some_and(|b| b.shader_id() != shaders.resampling.get().global_id())
        {
            *self = Self::new(device, shaders, new_config);
            true
        } else {
            false
        }
    }

    /// Returns the [`FbtConfig`] this was constructed with.
    pub(crate) fn config(&self) -> &FbtConfig {
        &self.config
    }

    pub(crate) fn flaws(&self) -> Flaws {
        self.config.flaws
    }

    pub(crate) fn bloom_data_texture(&self) -> &wgpu::TextureView {
        let [ref view] = self
            .bloom
            .as_ref()
            .expect("TODO: disabling bloom not implemented")
            .output_texture_views;
        view
    }

    pub(crate) fn global_id(&self) -> FbtId {
        FbtId {
            scene_id: self.linear_scene_view.global_id(),
        }
    }
}

/// An ID whose comparison is sufficient to identify whether a [`FramebufferTextures`]
/// has the same GPU resources or not, for [`common::Memo`] purposes.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct FbtId {
    scene_id: crate::Id<wgpu::TextureView>,
}

/// Pure-data inputs to [`FramebufferTextures`]'s choice of texture format and size.
///
/// This struct's `==` can be used to decide whether re-creation is needed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct FbtConfig {
    /// Features remembered for future use.
    pub(crate) features: FbtFeatures,

    /// Size of all textures and the surface.
    pub(crate) size: wgpu::Extent3d,

    /// If the surface is known or guessed to support HDR,
    /// this is the maximum color channel value that should be produced.
    /// This is used as an upper bound on [`GraphicsOptions::maximum_intensity`].
    pub(crate) maximum_intensity: PositiveSign<f32>,

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
    /// `surface_config` must be valid (in particular, not zero sized).
    pub fn new(
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

        // TODO: surface format doesn't *actually* mean we have HDR hardware, and doesn't tell us
        // what the actual range is; we should be fetching actual monitor information, but there
        // is no API in wgpu (or winit or anything else involved) to do that yet.
        let maximum_intensity = match surface_config.format {
            wgpu::TextureFormat::Rgba16Float => ps32(10.0),
            wgpu::TextureFormat::Rgba32Float => ps32(10.0),
            _ => ps32(1.0),
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
                    maximum_intensity,
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
                    maximum_intensity,
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
                maximum_intensity,
                linear_scene_texture_format: wgpu::TextureFormat::Rgba8UnormSrgb,
                sample_count: if features.rgba8_can_multisample {
                    sample_count_if_ok
                } else {
                    1
                },
                enable_copy_out,
                flaws: if !features.rgba8_can_multisample && sample_count_if_ok != 1 {
                    Flaws::NO_ANTIALIASING
                } else {
                    Flaws::empty()
                },
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

        let adapter_info = adapter.get_info();
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

        let allow_multisampling = {
            // Workaround: multisampling does not succeed with this software renderer.
            adapter_info.name != "llvmpipe (LLVM 12.0.0, 256 bits)"
            // Bug workaround <https://github.com/kpreid/all-is-cubes/issues/471>:
            // on WebGL we cannot read, in a shader, from a multisampled texture.
            // Since we need to read to implement any screen-space processing,
            // pretend multisampling is unavailable even though we could do otherwise.
            && !(adapter_info.backend == wgpu::Backend::Gl && cfg!(target_family = "wasm"))
        };

        Self {
            float_can_render,
            float_can_multisample: allow_multisampling
                && float_can_render
                && float_tff.flags.contains(multisample_flags),
            rgba8_can_multisample: allow_multisampling
                && adapter
                    .get_texture_format_features(wgpu::TextureFormat::Rgba8UnormSrgb)
                    .flags
                    .contains(multisample_flags),
        }
    }
}

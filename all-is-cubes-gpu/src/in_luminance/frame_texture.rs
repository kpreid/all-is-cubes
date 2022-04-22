// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

use all_is_cubes::drawing::embedded_graphics::prelude::{OriginDimensions, Size};
use luminance::context::GraphicsContext;
use luminance::pipeline::{Pipeline, TextureBinding};
use luminance::pixel::{NormRGBA8UI, NormUnsigned};
use luminance::render_state::RenderState;
use luminance::shader::{Program, Uniform};
use luminance::shading_gate::ShadingGate;
use luminance::tess::{Mode, Tess};
use luminance::texture::TexelUpload;
use luminance::texture::{Dim2, MagFilter, MinFilter, Sampler, Texture, Wrap};
use luminance::UniformInterface;

use all_is_cubes::camera::Viewport;
use all_is_cubes::listen::{DirtyFlag, ListenableSource};

use crate::in_luminance::shading::map_shader_result;
use crate::in_luminance::types::AicLumBackend;
use crate::EgFramebuffer;
use crate::GraphicsResourceError;

/// Resources for drawing a texture onto the entire framebuffer.
/// This is stateless and can be shared by multiple textures,
/// but requires a [`GraphicsContext`] to be constructed.
pub(crate) struct FullFramePainter<Backend>
where
    Backend: AicLumBackend,
{
    fragment_shader_text: ListenableSource<Arc<str>>,
    fragment_shader_dirty: DirtyFlag,
    /// Using a `Program` requires `&mut`.
    program: RefCell<Program<Backend, (), (), FullFrameUniformInterface>>,
    tess: Tess<Backend, ()>,
}

impl<Backend> FullFramePainter<Backend>
where
    Backend: AicLumBackend,
{
    /// Construct a [`FullFramePainter`] with the default vertex shader and given fragment shader.
    pub fn new<C: GraphicsContext<Backend = Backend>>(
        context: &mut C,
        fragment_shader: ListenableSource<Arc<str>>,
    ) -> Result<Rc<Self>, GraphicsResourceError> {
        let fragment_shader_dirty = DirtyFlag::listening(false, |l| fragment_shader.listen(l));
        let program = compile_full_frame_program(&fragment_shader.get(), context)?;

        Ok(Rc::new(FullFramePainter {
            fragment_shader_text: fragment_shader,
            fragment_shader_dirty,
            program: RefCell::new(program),
            tess: context
                .new_tess()
                .set_render_vertex_nb(3)
                .set_mode(Mode::Triangle)
                .build()?,
        }))
    }

    pub fn new_texture(self: &Rc<Self>) -> FullFrameTexture<Backend> {
        FullFrameTexture {
            ff: self.clone(),
            texture: None,
            scaled_viewport: None,
            local_buffer: EgFramebuffer::new(Size::zero()),
            texture_is_valid: false,
        }
    }

    pub fn reload_shader_if_changed<C: GraphicsContext<Backend = Backend>>(&self, context: &mut C) {
        // Update shader
        if self.fragment_shader_dirty.get_and_clear() {
            match compile_full_frame_program(&self.fragment_shader_text.get(), context) {
                Ok(p) => *self.program.borrow_mut() = p,
                Err(e) => log::error!("Failed to recompile fullframe shader: {}", e),
            }
        }
    }

    /// Draw the given texture.
    ///
    /// TODO: Explain how the texture's dimensions are treated.
    pub fn render(
        &self,
        render_state: &RenderState,
        pipeline: &Pipeline<'_, Backend>,
        shading_gate: &mut ShadingGate<'_, Backend>,
        texture: &mut Texture<Backend, Dim2, NormRGBA8UI>,
    ) -> Result<(), GraphicsResourceError> {
        let tess = &self.tess;
        let bound_texture = pipeline.bind_texture(texture)?;
        shading_gate.shade(
            &mut self.program.borrow_mut(),
            |ref mut program_iface, uniform_iface, mut render_gate| {
                program_iface.set(&uniform_iface.frame_texture, bound_texture.binding());
                render_gate.render(
                    render_state,
                    |mut tess_gate| -> Result<(), GraphicsResourceError> { tess_gate.render(tess) },
                )
            },
        )
    }
}

fn compile_full_frame_program<C: GraphicsContext>(
    fragment_shader_text: &str,
    context: &mut C,
) -> Result<Program<C::Backend, (), (), FullFrameUniformInterface>, GraphicsResourceError>
where
    C::Backend: AicLumBackend,
{
    map_shader_result(
        &("FullFramePainter", fragment_shader_text), // debug info
        context.new_shader_program().from_strings(
            include_str!("shaders/full-frame-vertex.glsl"),
            None,
            None,
            fragment_shader_text,
        ),
    )
}

pub(crate) struct FullFrameTexture<Backend>
where
    Backend: AicLumBackend,
{
    /// Reference to the [`Program`] and [`Tess`] we're using.
    ff: Rc<FullFramePainter<Backend>>,
    texture: Option<Texture<Backend, Dim2, NormRGBA8UI>>,
    /// Viewport whose `framebuffer_size` is the size of our texture.
    scaled_viewport: Option<Viewport>,
    local_buffer: EgFramebuffer,
    texture_is_valid: bool,
}

impl<Backend> FullFrameTexture<Backend>
where
    Backend: AicLumBackend,
{
    /// Adjusts the texture size to the given framebuffer size.
    ///
    /// The current texture data will be discarded if and only if the given size is
    /// different than the previous size.
    pub fn resize<C>(
        &mut self,
        context: &mut C,
        viewport: Viewport,
        scale_policy: fn(Viewport) -> Viewport,
        // TODO: Make filters part of scale_policy?
        filters: (MagFilter, MinFilter),
    ) -> Result<(), GraphicsResourceError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let scaled_viewport = scale_policy(viewport);
        if Some(scaled_viewport) == self.scaled_viewport {
            return Ok(());
        }
        let size = scaled_viewport.framebuffer_size;

        // Invalidate previous size
        self.scaled_viewport = None;

        // TODO: systematic overflow checks
        self.local_buffer = EgFramebuffer::new(Size {
            width: size.x,
            height: size.y,
        });
        self.texture_is_valid = false;
        self.texture = Some(context.new_texture(
            [size.x, size.y],
            Sampler {
                wrap_s: Wrap::ClampToEdge,
                wrap_t: Wrap::ClampToEdge,
                mag_filter: filters.0,
                min_filter: filters.1,
                ..Sampler::default()
            },
            TexelUpload::reserve(0),
        )?);

        self.scaled_viewport = Some(scaled_viewport);
        Ok(())
    }

    /// Returns a [`Viewport`] whose `framebuffer_size` describes this texture,
    /// or `None` if the size is not yet set by [`Self::resize`].
    pub fn scaled_viewport(&self) -> Option<Viewport> {
        self.scaled_viewport
    }

    pub fn draw_target(&mut self) -> &mut EgFramebuffer {
        &mut self.local_buffer
    }

    pub fn upload(&mut self) -> Result<(), GraphicsResourceError> {
        if !self.texture_is_valid {
            self.texture
                .as_mut()
                .expect("upload() without resize()")
                .upload(TexelUpload::base_level(self.local_buffer.data(), 0))?;
            self.texture_is_valid = true;
        }
        let dirty_rect = self.local_buffer.dirty_rect();
        if !dirty_rect.is_zero_sized() {
            let width = self.local_buffer.size().width;
            self.texture
                .as_mut()
                .expect("upload() without resize()")
                .upload_part(
                    // We can't specify stride, but we can specify a contiguous Y span
                    [0, dirty_rect.top_left.y as u32],
                    [width, dirty_rect.bottom_right().unwrap().y as u32 + 1],
                    TexelUpload::base_level(
                        &self.local_buffer.data()
                            [width as usize * dirty_rect.top_left.y as usize..],
                        0,
                    ),
                )?;
            self.local_buffer.mark_not_dirty();
        }
        Ok(())
    }

    pub fn reload_shader_if_changed<C: GraphicsContext<Backend = Backend>>(&self, context: &mut C) {
        self.ff.reload_shader_if_changed(context);
    }

    /// Returns whether there was actually anything to draw.
    pub fn render(
        &mut self,
        render_state: &RenderState,
        pipeline: &Pipeline<'_, Backend>,
        shading_gate: &mut ShadingGate<'_, Backend>,
    ) -> Result<bool, GraphicsResourceError> {
        if self.texture_is_valid {
            if let Some(texture) = &mut self.texture {
                self.ff
                    .render(render_state, pipeline, shading_gate, texture)?;
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl<Backend> fmt::Debug for FullFrameTexture<Backend>
where
    Backend: AicLumBackend,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FullFrameTexture")
            // Skipping .ff because it can't be usefully printed
            .field("texture", &self.texture.is_some())
            // Skipping .local_buffer because it's a large image
            .field("texture_is_valid", &self.texture_is_valid)
            .finish()
    }
}

#[derive(Debug, UniformInterface)]
pub(crate) struct FullFrameUniformInterface {
    #[uniform(unbound)] // At least one GL backend implementation has falsely reported this unused
    frame_texture: Uniform<TextureBinding<Dim2, NormUnsigned>>,
}

// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use cgmath::Vector2;
use cgmath::Zero;
use embedded_graphics::draw_target::DrawTarget;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::prelude::OriginDimensions;
use embedded_graphics::prelude::RgbColor;
use embedded_graphics::prelude::Size;
use embedded_graphics::Pixel;
use luminance::backend::shader::Uniformable;
use luminance::context::GraphicsContext;
use luminance::pipeline::{Pipeline, TextureBinding};
use luminance::pixel::{NormRGBA8UI, NormUnsigned};
use luminance::render_state::RenderState;
use luminance::shader::{Program, Uniform};
use luminance::shading_gate::ShadingGate;
use luminance::tess::{Mode, Tess};
use luminance::texture::{Dim2, GenMipmaps, MagFilter, MinFilter, Sampler, Texture, Wrap};
use luminance::UniformInterface;

use crate::camera::Viewport;
use crate::lum::shading::map_shader_result;
use crate::lum::types::AicLumBackend;
use crate::lum::GraphicsResourceError;
use crate::space::Grid;

/// Resources for drawing a texture onto the entire framebuffer.
/// This is stateless and can be shared by multiple textures,
/// but requires a [`GraphicsContext`] to be constructed.
pub(crate) struct FullFramePainter<Backend>
where
    Backend: AicLumBackend,
{
    /// Using a `Program` requires `&mut`.
    program: RefCell<Program<Backend, (), (), FullFrameUniformInterface>>,
    tess: Tess<Backend, ()>,
}

impl<Backend> FullFramePainter<Backend>
where
    Backend: AicLumBackend,
    TextureBinding<Dim2, NormUnsigned>: Uniformable<Backend>,
{
    /// Construct a [`FullFramePainter`] with the default shader program.
    ///
    /// This program copies values to the framebuffer with no conversion, and as such,
    /// expects the texture to produce sRGB values. That is, the pixel format should
    /// *not* be one which implicitly converts sRGB to linear.
    pub fn basic_program<C: GraphicsContext<Backend = Backend>>(
        context: &mut C,
    ) -> Result<Rc<Self>, GraphicsResourceError> {
        let program = map_shader_result(context.new_shader_program().from_strings(
            include_str!("shaders/full-frame-vertex.glsl"),
            None,
            None,
            include_str!("shaders/full-frame-fragment.glsl"),
        ))?;

        Ok(Rc::new(FullFramePainter {
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
            last_size: Vector2::zero(),
            local_data: Box::new([]),
            texture_is_valid: false,
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
                program_iface.set(&uniform_iface.texture, bound_texture.binding());
                render_gate.render(
                    render_state,
                    |mut tess_gate| -> Result<(), GraphicsResourceError> { tess_gate.render(tess) },
                )
            },
        )
    }
}

pub(crate) struct FullFrameTexture<Backend>
where
    Backend: AicLumBackend,
{
    /// Reference to the [`Program`] and [`Tess`] we're using.
    ff: Rc<FullFramePainter<Backend>>,
    texture: Option<Texture<Backend, Dim2, NormRGBA8UI>>,
    last_size: Vector2<u32>,
    local_data: Box<[u8]>,
    texture_is_valid: bool,
}

impl<Backend> FullFrameTexture<Backend>
where
    Backend: AicLumBackend,
    TextureBinding<Dim2, NormUnsigned>: Uniformable<Backend>,
{
    /// Adjusts the texture size to the given framebuffer size.
    ///
    /// The current texture data will be discarded if and only if the given size is
    /// different than the previous size.
    pub fn resize<C>(
        &mut self,
        context: &mut C,
        viewport: Viewport,
    ) -> Result<(), GraphicsResourceError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let size = viewport.framebuffer_size;
        if size == self.last_size {
            return Ok(());
        }

        self.local_data = vec![0; (size.x as usize) * (size.y as usize) * 4].into_boxed_slice();
        self.texture_is_valid = false;
        self.texture = Some(context.new_texture_no_texels(
            [size.x, size.y],
            0, // mipmaps
            Sampler {
                wrap_s: Wrap::ClampToEdge,
                wrap_t: Wrap::ClampToEdge,
                mag_filter: MagFilter::Nearest,
                min_filter: MinFilter::Linear,
                ..Sampler::default()
            },
        )?);
        self.last_size = size;

        Ok(())
    }

    pub fn data(&mut self) -> &mut [u8] {
        &mut self.local_data
    }

    pub fn upload(&mut self) -> Result<(), GraphicsResourceError> {
        self.texture
            .as_mut()
            .expect("upload() without resize()")
            .upload_raw(GenMipmaps::No, &self.local_data)?;
        self.texture_is_valid = true;
        Ok(())
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
            // Skipping .local_data because it's a large image
            .field("texture_is_valid", &self.texture_is_valid)
            .finish()
    }
}

impl<Backend> DrawTarget for FullFrameTexture<Backend>
where
    Backend: AicLumBackend,
{
    type Color = Rgb888;
    type Error = std::convert::Infallible;

    #[inline]
    fn draw_iter<I>(&mut self, pixels: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = embedded_graphics::Pixel<Self::Color>>,
    {
        if let Some(texture) = &self.texture {
            // Borrow Grid's indexing logic to do our 2D indexing (and set up for a Y flip)
            let [width, height] = texture.size();
            let grid = Grid::new(
                [0, (1 - height as i32), 0],
                [1, height as i32, width as i32],
            );

            for Pixel(point, color) in pixels.into_iter() {
                if let Some(index) = grid.index([0, -point.y, point.x]) {
                    let index = index * 4; // four channels
                    self.local_data[index] = color.r();
                    self.local_data[index + 1] = color.g();
                    self.local_data[index + 2] = color.b();
                    self.local_data[index + 3] = 255;
                }
            }
        }
        Ok(())
    }
}

impl<Backend> OriginDimensions for FullFrameTexture<Backend>
where
    Backend: AicLumBackend,
{
    fn size(&self) -> Size {
        if let Some(texture) = &self.texture {
            let [width, height] = texture.size();
            Size { width, height }
        } else {
            Size {
                width: 0,
                height: 0,
            }
        }
    }
}

#[derive(Debug, UniformInterface)]
pub(crate) struct FullFrameUniformInterface {
    texture: Uniform<TextureBinding<Dim2, NormUnsigned>>,
}

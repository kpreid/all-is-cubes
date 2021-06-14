// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::cell::RefCell;
use std::rc::Rc;

use embedded_graphics::draw_target::DrawTarget;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::prelude::OriginDimensions;
use embedded_graphics::prelude::RgbColor;
use embedded_graphics::prelude::Size;
use embedded_graphics::Pixel;
use luminance::UniformInterface;
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::{Pipeline, TextureBinding};
use luminance_front::pixel::{NormRGBA8UI, NormUnsigned};
use luminance_front::render_state::RenderState;
use luminance_front::shader::{Program, Uniform};
use luminance_front::shading_gate::ShadingGate;
use luminance_front::tess::{Mode, Tess};
use luminance_front::texture::{Dim2, GenMipmaps, MagFilter, MinFilter, Sampler, Texture, Wrap};
use luminance_front::Backend;

use crate::camera::Viewport;
use crate::lum::shading::map_shader_result;
use crate::lum::GraphicsResourceError;
use crate::space::Grid;

/// Resources for drawing a texture onto the entire framebuffer.
/// This is stateless and can be shared by multiple textures,
/// but requires a [`GraphicsContext`] to be constructed.
pub(crate) struct FullFramePainter {
    /// Using a `Program` requires `&mut`.
    program: RefCell<Program<(), (), FullFrameUniformInterface>>,
    tess: Tess<()>,
}

impl FullFramePainter {
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
                .set_vertex_nb(3)
                .set_mode(Mode::Triangle)
                .build()?,
        }))
    }

    pub fn new_texture(self: &Rc<Self>) -> FullFrameTexture {
        FullFrameTexture {
            ff: self.clone(),
            texture: None,
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
        pipeline: &Pipeline<'_>,
        shading_gate: &mut ShadingGate<'_>,
        texture: &mut Texture<Dim2, NormRGBA8UI>,
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

pub(crate) struct FullFrameTexture {
    /// Reference to the [`Program`] and [`Tess`] we're using.
    ff: Rc<FullFramePainter>,
    texture: Option<Texture<Dim2, NormRGBA8UI>>,
    local_data: Box<[u8]>,
    texture_is_valid: bool,
}

impl FullFrameTexture {
    pub fn resize<C>(
        &mut self,
        context: &mut C,
        viewport: Viewport,
    ) -> Result<(), GraphicsResourceError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let size = viewport.framebuffer_size;
        self.local_data = vec![0; (size.x as usize) * (size.y as usize) * 4].into_boxed_slice();

        self.texture_is_valid = false;
        self.texture = Some(Texture::new(
            context,
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
        pipeline: &Pipeline<'_>,
        shading_gate: &mut ShadingGate<'_>,
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

impl std::fmt::Debug for FullFrameTexture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FullFrameTexture")
            // Skipping .ff because it can't be usefully printed
            .field("texture", &self.texture.is_some())
            // Skipping .local_data because it's a large image
            .field("texture_is_valid", &self.texture_is_valid)
            .finish()
    }
}

impl DrawTarget for FullFrameTexture {
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

impl OriginDimensions for FullFrameTexture {
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

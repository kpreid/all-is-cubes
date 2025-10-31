//! Functions for transferring monospaced fonts and text to the GPU to be efficiently rendered.

use all_is_cubes::drawing::embedded_graphics::prelude::Size;
use itertools::iproduct;

use all_is_cubes::drawing::embedded_graphics::{
    self as eg, Drawable as _, geometry::Point, transform::Transform as _,
};
use all_is_cubes::euclid::{Size2D, Vector2D, point2, size2};
use all_is_cubes_render::camera::{ImageSize, Viewport};

use crate::everything::InfoTextTexture;
use crate::{DrawableTexture, Identified};

/// Compute the size a texture of character cells should have to fit in the given viewport,
/// with up to 1 extra fractionally hidden cell for uneven division.
///
/// The output will always be at least 1 character/texel in size
/// (because we are rounding up anyway and this avoids needing zero-size handling).
pub(crate) fn character_texture_size(
    viewport: Viewport,
    font_metrics: &GpuFontMetrics,
) -> (ImageSize, Vector2D<f32, ()>) {
    // compensate for outline
    let font_cell_size = font_metrics.logical_cell_size();

    // Compute size in character cells, rounding up so we spill off the edge rather than truncating
    // any characters early. This uses the viewport nominal size so that the font size is in
    // nominal pixels
    let text_size_in_characters: ImageSize = size2(
        f64::ceil(viewport.nominal_size.width / f64::from(font_cell_size.width)) as u32,
        f64::ceil(viewport.nominal_size.height / f64::from(font_cell_size.height)) as u32,
    )
    .max(size2(1, 1));

    // Convert back to nominal pixels so we know the size of the whole text image.
    let desired_text_size_in_nominal_pixels =
        text_size_in_characters.to_vector().component_mul(font_cell_size.to_vector());

    // Compute the scale factor for texture coordinates which fits that image into the framebuffer
    // as an exact multiple.
    // TODO: This uses nominal size, but it would be more precise for cases of non-integer
    // display-scale-factors to map to framebuffer pixels but apply the scale factor directly
    let texture_coordinate_scale = viewport
        .nominal_size
        .to_vector()
        .cast_unit::<()>()
        .to_f32()
        .component_div(desired_text_size_in_nominal_pixels.to_f32().cast_unit::<()>());

    (text_size_in_characters, texture_coordinate_scale)
}

/// Perform naïve monospaced text layout, writing ISO-8859-1 (the first 256 elements of Unicode)
/// data to an 8-bit texture at 1 texel per character.
pub(crate) fn render_text_to_character_texture(text: &str, texture: &mut InfoTextTexture) {
    let mut x: u32 = 0;
    let mut y: u32 = 0;
    let draw_target = texture.draw_target();
    draw_target.clear_transparent();
    for character in text.chars() {
        match character {
            '\n' => {
                x = 0;
                y = y.saturating_add(1);
            }
            _ => {
                draw_target.set_pixel(point2(x, y), u8::try_from(character).unwrap_or(b'?'));
                x = x.saturating_add(1);
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Data produced by [`generate_texture_atlas()`] and used by the text rendering fragment shader.
#[derive(Clone, Copy, Debug)]
pub(crate) struct GpuFontMetrics {
    /// Size in the texture atlas of a single glyph’s cell.
    ///
    /// The atlas is, currently, always 16 times this size to hold 256 glyphs.
    pub atlas_cell_size: ImageSize,

    /// Margin inside of all four edges of `cell_size` which is not part of the logical character
    /// cell, but makes room for the text to have an outline that overlaps with other cells.
    ///
    /// The logical character cell's size is `cell_size - cell_margin * 2`.
    pub cell_margin: u32,
}

impl GpuFontMetrics {
    fn logical_cell_size(&self) -> ImageSize {
        self.atlas_cell_size - Size2D::splat(self.cell_margin * 2)
    }
}

/// Given a font covering the ISO-8859-1 character set, generate a corresponding atlas texture
/// arranged as 16 cells × 16 cells of premultiplied-alpha color.
///
/// Returns the texture and information needed to use it.
pub(crate) fn generate_texture_atlas(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    font: &eg::mono_font::MonoFont<'_>,
) -> (Identified<wgpu::TextureView>, GpuFontMetrics) {
    let outline_radius_u = 1u32;
    let outline_radius_i = outline_radius_u.cast_signed();

    // Size of the on-screen cell, which the outline may overflow.
    let logical_cell_size = size2(
        font.character_size.width + font.character_spacing,
        font.character_size.height,
    );
    // Size of the atlas glyph cell, which has to be big enough for the added outline.
    let atlas_cell_size = logical_cell_size + ImageSize::splat(outline_radius_u * 2);

    let mut dt: DrawableTexture<EgRgba, [u8; 4]> =
        DrawableTexture::new(wgpu::TextureFormat::Rgba8Unorm);
    dt.resize(device, Some("font atlas"), atlas_cell_size * 16);

    // Write characters into atlas
    let mut string_buf: [u8; 4] = [0; 4];
    for (character, (y, x)) in ('\u{00}'..='\u{FF}').into_iter().zip(iproduct!(0..16, 0..16)) {
        let string = character.encode_utf8(&mut string_buf);

        let position = Point::new(
            x * atlas_cell_size.width.cast_signed()
                    // center in cell, and if only 1px, prefer putting it on the left
                    + font.character_spacing.div_ceil(2).cast_signed(),
            y * atlas_cell_size.height.cast_signed(),
        ) + Size::new_equal(outline_radius_u); // offset by outline radius to fit into cell
        let character_style = eg::text::TextStyle::with_baseline(eg::text::Baseline::Top);

        let text = eg::text::Text::with_text_style(
            string,
            position,
            eg::mono_font::MonoTextStyle::new(font, EgRgba([255, 255, 255, 255])),
            character_style,
        );
        let shadow = eg::text::Text::with_text_style(
            string,
            position,
            eg::mono_font::MonoTextStyle::new(font, EgRgba([0, 0, 0, 255])),
            character_style,
        );

        for (dx, dy) in iproduct!(
            -outline_radius_i..=outline_radius_i,
            -outline_radius_i..=outline_radius_i
        ) {
            let Ok(_) = shadow.translate(Point::new(dx, dy)).draw(dt.draw_target());
        }

        let Ok(_) = text.draw(dt.draw_target());
    }

    dt.upload(queue);

    let metrics = GpuFontMetrics {
        atlas_cell_size,
        cell_margin: outline_radius_u,
    };
    (dt.view().unwrap().clone(), metrics)
}

// e-g doesn't have an RGBA color, nor does it support alpha yet, but we aren't asking it
// to do any blending so we can get away with this
#[derive(Clone, Copy, PartialEq)]
struct EgRgba([u8; 4]);

impl eg::pixelcolor::PixelColor for EgRgba {
    type Raw = (); // not used
}

impl crate::ToTexel<[u8; 4]> for EgRgba {
    fn to_texel(self) -> [u8; 4] {
        self.0
    }
}

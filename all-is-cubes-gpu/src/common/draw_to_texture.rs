//! Drawing to a CPU-side image buffer which is going to be transferred to the GPU.

use std::marker::PhantomData;

use all_is_cubes::drawing::embedded_graphics::{
    draw_target::DrawTarget,
    pixelcolor::{self, PixelColor},
    prelude::{OriginDimensions, Point, RgbColor, Size},
    primitives::Rectangle,
    Pixel,
};

/// Storage for an image which can be written to using [`DrawTarget`].
///
/// This storage tracks a “dirty rectangle” so that copying can be limited to affected areas.
pub(crate) struct EgFramebuffer<In, Out> {
    /// RGBA image buffer. Row-major, Y-up.
    data: Vec<Out>,
    /// Size of the buffer.
    size: Size,
    /// Region of the buffer not yet copied to the GPU.
    dirty: Rectangle,
    /// Region of the buffer that is not entirely zeroes.
    nonzero: Rectangle,
    _phantom_pixel_type: PhantomData<fn(In)>,
}

impl<In, Out: Copy + Default> EgFramebuffer<In, Out> {
    pub fn new(size: Size) -> Self {
        Self {
            data: vec![Out::default(); size.width as usize * size.height as usize],
            size,
            dirty: Rectangle::zero(),
            nonzero: Rectangle::zero(),
            _phantom_pixel_type: PhantomData,
        }
    }

    pub fn data(&self) -> &[Out] {
        &self.data
    }

    pub fn dirty_rect(&self) -> Rectangle {
        self.dirty
    }

    pub fn mark_not_dirty(&mut self) {
        self.dirty = Rectangle::zero();
    }

    pub fn clear_transparent(&mut self) {
        self.data.fill(Out::default());
        // Everything that wasn't zero is now dirty.
        if let Some(bottom_right) = self.nonzero.bottom_right() {
            expand_rectangle(&mut self.dirty, self.nonzero.top_left);
            expand_rectangle(&mut self.dirty, bottom_right);
        }
        self.nonzero = Rectangle::zero();
    }

    pub fn is_nonzero(&self) -> bool {
        !self.nonzero.is_zero_sized()
    }
}

impl<In: ToTexel<Out>, Out> DrawTarget for EgFramebuffer<In, Out> {
    type Color = In;
    type Error = std::convert::Infallible;

    #[inline]
    fn draw_iter<I>(&mut self, pixels: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = Pixel<In>>,
    {
        let bounds = Rectangle {
            top_left: Point::zero(),
            size: self.size,
        };
        for Pixel(point, color) in pixels {
            if bounds.contains(point) {
                self.data[point.y as usize * self.size.width as usize + point.x as usize] =
                    color.to_texel();
                expand_rectangle(&mut self.dirty, point);
                expand_rectangle(&mut self.nonzero, point);
            }
        }
        Ok(())
    }

    // TODO: implement other ops
}

impl<In, Out> OriginDimensions for EgFramebuffer<In, Out> {
    fn size(&self) -> Size {
        self.size
    }
}

/// A [`PixelColor`] that can be converted into data accepted as a GPU texture format.
///
/// The reason this isn't `From` is so that it can be implemented for foreign types.
pub(crate) trait ToTexel<T>: PixelColor + Copy {
    fn to_texel(self) -> T;
}

impl ToTexel<[u8; 4]> for pixelcolor::Rgb888 {
    fn to_texel(self) -> [u8; 4] {
        [self.r(), self.g(), self.b(), 255]
    }
}

impl ToTexel<u8> for pixelcolor::Gray8 {
    fn to_texel(self) -> u8 {
        pixelcolor::GrayColor::luma(&self)
    }
}

fn expand_rectangle(rectangle: &mut Rectangle, point: Point) {
    let relative = Point {
        x: point.x - rectangle.top_left.x,
        y: point.y - rectangle.top_left.y,
    };
    if relative.x < 0 {
        // Expand to the left.
        rectangle.top_left.x += relative.x;
        rectangle.size.width += (-relative.x) as u32;
    } else {
        // Maybe expand to the right.
        rectangle.size.width = rectangle.size.width.max(relative.x as u32 + 1);
    }
    if relative.y < 0 {
        rectangle.top_left.y += relative.y;
        rectangle.size.height += (-relative.y) as u32;
    } else {
        rectangle.size.height = rectangle.size.height.max(relative.y as u32 + 1);
    }
}

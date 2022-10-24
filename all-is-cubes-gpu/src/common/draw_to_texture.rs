//! Drawing to a CPU-side image buffer which is going to be transferred to the GPU.

use all_is_cubes::drawing::embedded_graphics::{
    draw_target::DrawTarget,
    pixelcolor::Rgb888,
    prelude::{OriginDimensions, Point, RgbColor, Size},
    primitives::Rectangle,
    Pixel,
};

/// Storage for an image which can be written to using [`DrawTarget`].
///
/// This storage tracks a “dirty rectangle” so that copying can be limited to affected areas.
pub(crate) struct EgFramebuffer {
    /// RGBA image buffer. Row-major, Y-up.
    data: Vec<[u8; 4]>,
    /// Size of the buffer.
    size: Size,
    /// Region of the buffer not yet copied to the GPU.
    dirty: Rectangle,
    /// Region of the buffer that is not entirely zeroes.
    nonzero: Rectangle,
}

impl EgFramebuffer {
    pub fn new(size: Size) -> Self {
        Self {
            data: vec![[0, 0, 0, 0]; size.width as usize * size.height as usize],
            size,
            dirty: Rectangle::zero(),
            nonzero: Rectangle::zero(),
        }
    }

    pub fn data(&self) -> &[[u8; 4]] {
        &self.data
    }

    pub fn dirty_rect(&self) -> Rectangle {
        self.dirty
    }

    pub fn mark_not_dirty(&mut self) {
        self.dirty = Rectangle::zero();
    }

    pub fn clear_transparent(&mut self) {
        self.data.fill([0, 0, 0, 0]);
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

impl DrawTarget for EgFramebuffer {
    type Color = Rgb888; // TODO: generalize
    type Error = std::convert::Infallible;

    #[inline]
    fn draw_iter<I>(&mut self, pixels: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = Pixel<Self::Color>>,
    {
        let bounds = Rectangle {
            top_left: Point::zero(),
            size: self.size,
        };
        for Pixel(point, color) in pixels.into_iter() {
            if bounds.contains(point) {
                self.data[point.y as usize * self.size.width as usize + point.x as usize] =
                    [color.r(), color.g(), color.b(), 255];
                expand_rectangle(&mut self.dirty, point);
                expand_rectangle(&mut self.nonzero, point);
            }
        }
        Ok(())
    }

    // TODO: implement other ops
}

impl OriginDimensions for EgFramebuffer {
    fn size(&self) -> Size {
        self.size
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

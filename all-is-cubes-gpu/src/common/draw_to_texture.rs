//! Drawing to a CPU-side image buffer which is going to be transferred to the GPU.

use alloc::vec;
use alloc::vec::Vec;

use all_is_cubes::{
    drawing::embedded_graphics::{
        prelude::{OriginDimensions, Point, Size},
        primitives::Rectangle,
    },
    euclid::Point2D,
};
use all_is_cubes_render::camera::ImagePixel;

/// Storage for an image which can be written one pixel at a time.
///
/// This storage tracks a “dirty rectangle” so that copying can be limited to affected areas.
pub(crate) struct EgFramebuffer<P> {
    /// RGBA image buffer. Row-major, Y-up.
    data: Vec<P>,
    /// Size of the buffer.
    size: Size,
    /// Region of the buffer not yet copied to the GPU.
    dirty: Rectangle,
    /// Region of the buffer that is not entirely zeroes.
    nonzero: Rectangle,
}

impl<P: Copy + Default> EgFramebuffer<P> {
    pub fn new(size: Size) -> Self {
        Self {
            data: vec![P::default(); size.width as usize * size.height as usize],
            size,
            dirty: Rectangle::zero(),
            nonzero: Rectangle::zero(),
        }
    }

    pub fn data(&self) -> &[P] {
        &self.data
    }

    pub fn dirty_rect(&self) -> Rectangle {
        self.dirty
    }

    pub fn mark_not_dirty(&mut self) {
        self.dirty = Rectangle::zero();
    }

    pub fn clear_transparent(&mut self) {
        self.data.fill(P::default());
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

    /// Directly write a single image pixel.
    /// Does nothing if out of bounds.
    pub fn set_pixel(&mut self, position: Point2D<u32, ImagePixel>, value: P) {
        if !(position.x < self.size.width && position.y < self.size.height) {
            return;
        }

        let position_eg = Point::new(position.x.cast_signed(), position.y.cast_signed());
        expand_rectangle(&mut self.dirty, position_eg);
        expand_rectangle(&mut self.nonzero, position_eg);

        self.data[position.x as usize + position.y as usize * self.size.width as usize] = value;
    }
}

impl<P> OriginDimensions for EgFramebuffer<P> {
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
        rectangle.size.width += (-relative.x).cast_unsigned();
    } else {
        // Maybe expand to the right.
        rectangle.size.width = rectangle.size.width.max(relative.x.cast_unsigned() + 1);
    }
    if relative.y < 0 {
        rectangle.top_left.y += relative.y;
        rectangle.size.height += (-relative.y).cast_unsigned();
    } else {
        rectangle.size.height = rectangle.size.height.max(relative.y.cast_unsigned() + 1);
    }
}

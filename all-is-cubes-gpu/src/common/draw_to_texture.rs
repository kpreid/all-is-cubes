//! Drawing to a CPU-side image buffer which is going to be transferred to the GPU.

use alloc::vec;
use alloc::vec::Vec;

use all_is_cubes::euclid::{Box2D, Point2D};
use all_is_cubes_render::camera::{ImagePixel, ImageSize};

// -------------------------------------------------------------------------------------------------

type Rectangle = Box2D<u32, ImagePixel>;
type Point = Point2D<u32, ImagePixel>;

/// Storage for an image which can be written one pixel at a time.
///
/// This storage tracks a “dirty rectangle” so that copying out of it can be limited to affected
/// areas.
pub(crate) struct EgFramebuffer<P> {
    /// RGBA image buffer. Row-major, Y-up.
    data: Vec<P>,
    /// Size of the buffer.
    size: ImageSize,
    /// Region of the buffer not yet copied to the GPU.
    dirty: Rectangle,
    /// Region of the buffer that is not entirely zeroes.
    nonzero: Rectangle,
}

impl<P: Copy + Default> EgFramebuffer<P> {
    pub fn new(size: ImageSize) -> Self {
        Self {
            data: vec![P::default(); size.width as usize * size.height as usize],
            size,
            dirty: Rectangle::zero(),
            nonzero: Rectangle::zero(),
        }
    }

    pub fn size(&self) -> ImageSize {
        self.size
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
        if !self.nonzero.is_empty() {
            self.dirty = self.dirty.union(&self.nonzero);
        }
        self.nonzero = Rectangle::zero();
    }

    pub fn is_nonzero(&self) -> bool {
        !self.nonzero.is_empty()
    }

    /// Directly write a single image pixel.
    /// Does nothing if out of bounds.
    pub fn set_pixel(&mut self, position: Point, value: P) {
        if !(position.x < self.size.width && position.y < self.size.height) {
            return;
        }

        let position_eg = Point2D::new(position.x, position.y);
        expand_rectangle(&mut self.dirty, position_eg);
        expand_rectangle(&mut self.nonzero, position_eg);

        self.data[position.x as usize + position.y as usize * self.size.width as usize] = value;
    }
}

fn expand_rectangle(rectangle: &mut Rectangle, pixel: Point) {
    *rectangle = rectangle.union(&Rectangle::from_origin_and_size(
        pixel,
        ImageSize::new(1, 1),
    ));
}

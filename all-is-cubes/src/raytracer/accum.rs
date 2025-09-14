//! [`Accumulate`] and output formats of the raytracer.

use core::marker::PhantomData;

use euclid::Vector3D;

use crate::block::Resolution;
use crate::camera::GraphicsOptions;
use crate::math::{Cube, Face7, Intensity, Rgb, Rgba, ZeroOne, rgb_const, zo32};
use crate::space::SpaceBlockData;

// -------------------------------------------------------------------------------------------------

/// Borrowed data which may be used to customize the result of raytracing.
///
/// This is provided by the raytracer to implementations of [`RtBlockData`] and [`Accumulate`].
#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct RtOptionsRef<'a, C> {
    #[allow(missing_docs)]
    pub graphics_options: &'a GraphicsOptions,
    /// Arbitrary data for the [`RtBlockData`] in use.
    pub custom_options: &'a C,
}

impl<'a, C> RtOptionsRef<'a, C> {
    #[doc(hidden)]
    pub fn _new_but_please_do_not_construct_this_if_you_are_not_all_is_cubes_itself(
        graphics_options: &'a GraphicsOptions,
        custom_options: &'a C,
    ) -> Self {
        Self {
            graphics_options,
            custom_options,
        }
    }
}

// Non-derived implementations for no `C: Clone` bound.
impl<C> Copy for RtOptionsRef<'_, C> {}
impl<C> Clone for RtOptionsRef<'_, C> {
    fn clone(&self) -> Self {
        *self
    }
}

// -------------------------------------------------------------------------------------------------

/// Data passed by the raytracer to [`Accumulate::add()`] implementations.
//--
// Design note: Not every accumulator uses every part of this struct.
// We hope that optimization will drop the code to compute the unused parts.
// However, further work might be wanted, in particular to explicitly skip color.
//
// Also, a name other than `Hit` might be better but I haven't thought of one.
#[derive(Debug)]
#[non_exhaustive]
pub struct Hit<'d, D> {
    /// If [`Some`], indicates that this hit denotes an exceptional situation rather than
    /// an ordinary interaction of the ray with a block in the space.
    pub exception: Option<Exception>,

    /// Contains the opacity/transmittance and the light output of the encountered surface,
    /// in the direction towards the camera.
    ///
    /// This may also represent the ray traversing a thickness of transparent material
    /// when volume rendering is enabled, but it should be treated the same regardless.
    pub surface: ColorBuf,

    /// Distance along the ray at which the encountered surface was intersected.
    ///
    /// This distance is in units of the length of the original ray’s direction vector.
    /// Therefore, when forming an image, whether this value is Euclidean distance from
    /// the camera, or distance along the Z axis, depends on whether the rays are constructed
    /// with a constant length or a constant Z component.
    ///
    /// It is [`None`] when the event creating this hit does not correspond to a single
    /// point in physical space, such as error indicators or 2D overlays.
    pub t_distance: Option<f64>,

    /// [`RtBlockData`] value for the block this surface or volume is part of.
    pub block: &'d D,

    /// Which cube and voxel of the space the encountered surface belongs to,
    /// or [`None`] if this is a special case without a definite position such as the sky or
    /// an error report.
    ///
    /// If a position in continuous space is desired, use `t_distance` and the original ray
    /// to reconstruct it instead.
    // TODO: Use Point3D<u8> instead or not?
    pub position: Option<Position>,
}

/// Component of [`Hit`] indicating a situation other than an ordinary interaction of the ray
/// with a block in the space.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Exception {
    /// The ray entered the bounds of the space (or started inside it).
    /// This may be used to notice whether or not a ray intersected the space at all,
    /// even if it hit no blocks.
    EnterSpace,

    /// The ray exited the space and is considered to have hit the sky.
    Sky,

    /// For external reasons such as computational limits,
    /// the trace is being terminated and the the result will be incomplete.
    Incomplete,

    /// Result of calling [`Accumulate::paint()`].
    Paint,

    /// The hit color contains debug information.
    /// Its red and green channels should override previous transparent content.
    #[doc(hidden)] // used for debug purposes and only ColorBuf needs to consult it
    DebugOverrideRg,
}

/// Data about where a ray struck a voxel; part of [`Hit`].
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct Position {
    /// The cube in the [`Space`] containing the block whose surface was hit.
    ///
    /// [`Space`]: crate::space::Space
    pub cube: Cube,
    /// The voxel resolution of the block.
    pub resolution: Resolution,
    /// The voxel within the block.
    // TODO: Use Point3D<u8> instead or not?
    pub voxel: Cube,
    /// The surface normal of the hit surface, or [`Face7::Within`] if the ray started inside this
    /// voxel.
    pub face: Face7,
}

impl<'d, D> Hit<'d, D> {
    pub fn map_block_data<D2>(self, f: impl Fn(&D) -> &D2) -> Hit<'d, D2> {
        Hit {
            exception: self.exception,
            surface: self.surface,
            t_distance: self.t_distance,
            block: f(self.block),
            position: self.position,
        }
    }
}

impl<D> Clone for Hit<'_, D> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<D> Copy for Hit<'_, D> {}

// -------------------------------------------------------------------------------------------------

/// Implementations of [`Accumulate`] define output formats of the raytracer, by being
/// responsible for accumulating the color (and/or other information) for each image
/// pixel.
///
/// They should be an efficiently updatable buffer able to accumulate partial values,
/// and it must represent the transparency so as to be able to signal when to stop
/// tracing past an opaque surface.
///
/// They should, if possible, implement [`Default`] to provide a suitable initial state,
/// i.e. fully transparent/no light accumulated.
///
/// Each implementation should provide its own conversion to a final output format,
/// if any (e.g. an inherent method or an [`impl From<...`](From)).
pub trait Accumulate {
    /// Data precomputed for each distinct block or other type of visible object.
    ///
    /// If no data beyond color is needed, this may be `()`.
    type BlockData: RtBlockData;

    /// Returns whether `self` has recorded an opaque surface and therefore will not
    /// be affected by future calls to [`Self::add`].
    fn opaque(&self) -> bool;

    /// Adds the light from a surface or volume, and its opacity, to the accumulator.
    /// This surface is positioned behind/beyond all previous `add()`ed surfaces.
    ///
    /// Implementations are responsible for reducing this light according to the transmittance
    /// (inverse opacity) of previously encountered surfaces which obscure it.
    fn add(&mut self, hit: Hit<'_, Self::BlockData>);

    /// Called before the ray traverses any surfaces found in a block; that is,
    /// before all [`add()`](Self::add) calls pertaining to that block.
    fn enter_block(&mut self, block_data: &Self::BlockData) {
        _ = block_data;
    }

    /// Creates an accumulator already containing the given color.
    ///
    /// This may be useful when content that is not strictly raytraced is passing through
    /// an image buffer otherwise being used with the raytracer, such as a text overlay.
    fn paint(
        color: Rgba,
        options: RtOptionsRef<'_, <Self::BlockData as RtBlockData>::Options>,
    ) -> Self
    where
        Self: Default + Sized,
    {
        let mut result = Self::default();
        // TODO: Should give RtBlockData a dedicated method for this, but we haven't
        // yet had a use case where it matters.
        result.add(Hit {
            exception: Some(Exception::Paint),
            surface: color.into(),
            t_distance: None,
            block: &Self::BlockData::exception(Exception::Paint, options),
            position: None,
        });
        result
    }

    /// Combine multiple completed buffers into one, such as multiple samples for
    /// antialiasing.
    fn mean<const N: usize>(items: [Self; N]) -> Self
    where
        Self: Sized;
}

// -------------------------------------------------------------------------------------------------

/// Precomputed data about a [`Space`]'s blocks that may be used by [`Accumulate`] implementations.
///
/// Design note: This is a trait of the data type itself so as to require that there
/// cannot be more than one way to construct a given data type (other than explicit
/// configuration). This prevents multiple conflicting interpretations of a single data
/// type.
///
/// [`Space`]: crate::space::Space
pub trait RtBlockData: Send + Sync {
    /// Optional additional configuration.
    type Options: Send + Sync;

    /// Returns the data that should be stored for a particular block and passed
    /// to [`Accumulate::add()`] when that block is traced onto/through.
    fn from_block(options: RtOptionsRef<'_, Self::Options>, block: &SpaceBlockData) -> Self;

    /// Returns what should be passed to [`Accumulate::add()`] in an exceptional situation
    /// (one that is not the ray intersecting a block).
    ///
    /// Note that the [`Exception`] value is also passed directly as a field of [`Hit`].
    ///
    /// This function should be cheap enough to call once or twice per ray traced.
    /// If the raytracer has a reason to use this value many times, it is responsible for memoizing
    /// the result.
    fn exception(exception: Exception, options: RtOptionsRef<'_, Self::Options>) -> Self;
}

/// Trivial implementation of [`RtBlockData`] which stores nothing.
impl RtBlockData for () {
    type Options = ();
    fn from_block(_: RtOptionsRef<'_, Self::Options>, _: &SpaceBlockData) -> Self {}
    fn exception(_: Exception, _: RtOptionsRef<'_, Self::Options>) -> Self {}
}

/// Stores the block's resolution.
impl RtBlockData for Resolution {
    type Options = ();

    fn from_block(_: RtOptionsRef<'_, Self::Options>, block: &SpaceBlockData) -> Self {
        block.evaluated().resolution()
    }

    fn exception(_: Exception, _: RtOptionsRef<'_, Self::Options>) -> Self {
        Resolution::R1
    }
}

// -------------------------------------------------------------------------------------------------

/// Implements [`Accumulate`] for RGB(A) color with [`f32`] components,
/// and conversion to [`Rgba`].
///
/// In addition to its use in constructing images, this type is also used as an intermediate
/// format for surface colors presented to any other [`Accumulate`] implementation.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ColorBuf {
    /// Color buffer.
    ///
    /// The value can be interpreted as being “premultiplied alpha” value where the alpha
    /// is `1.0 - self.ray_alpha`, or equivalently we can say that it is the color to
    /// display supposing that everything not already traced is black.
    ///
    /// Note: Not using the [`Rgb`](crate::math::Rgb) type so as to skip NaN checks.
    /// This should never end up NaN, but we don't assert that property.
    light: Vector3D<f32, Intensity>,

    /// Fraction of the color value that is to be determined by future, rather than past,
    /// tracing; starts at 1.0 and decreases as surfaces are encountered.
    ///
    /// The range of this value is between 1.0 and 0.0, inclusive.
    pub(super) transmittance: f32,
}

impl ColorBuf {
    pub(crate) fn from_light_and_transmittance(light: Rgb, transmittance: f32) -> ColorBuf {
        Self {
            light: light.into(),
            transmittance,
        }
    }

    /// Returns the color (image pixel) accumulated in this buffer,
    /// in premultiplied RGBA form.
    ///
    /// Note that since the scene may, and typically will, contain light sources,
    /// it is not necessarily the case that the RGB components are less than or equal to
    /// the alpha components.
    //---
    // TODO: We should have data type(s) for premultiplied RGBA (that aren't just ColorBuf itself)
    // that this can return.
    // TODO: Use this instead of Rgba::from() most places?
    #[doc(hidden)] // TODO: unsure if good public API
    pub fn into_premultiplied_rgba(self) -> [f32; 4] {
        [
            self.light.x,
            self.light.y,
            self.light.z,
            (1.0 - self.transmittance).clamp(0.0, 1.0),
        ]
    }
}

impl Accumulate for ColorBuf {
    type BlockData = ();

    #[inline]
    fn opaque(&self) -> bool {
        // Let's suppose that we don't care about differences that can't be represented
        // in 8-bit color...not considering gamma.
        self.transmittance < 1.0 / 256.0
    }

    #[inline]
    fn add(&mut self, hit: Hit<'_, Self::BlockData>) {
        if matches!(hit.exception, Some(Exception::DebugOverrideRg)) {
            self.light = hit
                .surface
                .light
                .xy()
                .extend(Rgba::from(*self).to_rgb().luminance() * 0.2);
            self.transmittance = 0.0;
        } else {
            // Note that the order of these assignments matters.
            // surface.transmittance is only applied to surfaces *after* this one.
            self.light += hit.surface.light * self.transmittance;
            self.transmittance *= hit.surface.transmittance;
        }
    }

    #[inline]
    fn mean<const N: usize>(items: [Self; N]) -> Self {
        Self {
            light: items
                .iter()
                .map(|cb| cb.light)
                .sum::<Vector3D<f32, Intensity>>()
                / (N as f32),
            transmittance: items.iter().map(|cb| cb.transmittance).sum::<f32>() / (N as f32),
        }
    }
}

impl Default for ColorBuf {
    #[inline]
    fn default() -> Self {
        Self {
            light: Vector3D::zero(),
            transmittance: 1.0,
        }
    }
}

impl From<ColorBuf> for Rgba {
    /// Returns the color (image pixel) accumulated in this buffer.
    ///
    /// Not tone mapped; consider using [`Camera::post_process_color()`] for that.
    ///
    /// [`Camera::post_process_color()`]: crate::camera::Camera::post_process_color()
    //---
    // TODO: Converting arbitrary HDR+opacity colors to non-premultiplied RGBA is incorrect
    // in the general case. We should allow for tone-mapping in premultiplied form, either by
    // replacing this conversion with a custom method, or changing the [`Rgba`] type to be
    // premultiplied.
    //
    // We currently have `ColorBuf:into_premultiplied_alpha()` but aren't broadly using it.
    fn from(buf: ColorBuf) -> Rgba {
        if buf.transmittance >= 1.0 {
            // Special case to avoid dividing by zero
            Rgba::TRANSPARENT
        } else {
            let color_alpha = 1.0 - buf.transmittance;
            let non_premultiplied_color = buf.light / color_alpha;
            Rgb::try_from(non_premultiplied_color)
                .unwrap_or(rgb_const!(1.0, 0.0, 0.0))
                .with_alpha(ZeroOne::<f32>::try_from(color_alpha).unwrap_or(zo32(1.0)))
        }
    }
}

impl From<Rgba> for ColorBuf {
    /// Converts the given [`Rgba`] color value, interpreted as the reflectance and opacity of
    /// a surface lit by white light with luminance 1.0, into a [`ColorBuf`].
    fn from(value: Rgba) -> Self {
        let alpha = value.alpha().into_inner();
        Self {
            light: Vector3D::new(
                value.red().into_inner() * alpha,
                value.green().into_inner() * alpha,
                value.blue().into_inner() * alpha,
            ),
            transmittance: 1.0 - alpha,
        }
    }
}

// -------------------------------------------------------------------------------------------------

impl Accumulate for () {
    type BlockData = ();

    fn opaque(&self) -> bool {
        // "Opaque" is more precisely "no longer collecting information"
        true
    }

    fn add(&mut self, _: Hit<'_, Self::BlockData>) {}

    fn mean<const N: usize>(_: [Self; N]) -> Self {}
}

impl<A> Accumulate for (A,)
where
    A: Accumulate,
{
    type BlockData = A::BlockData;

    fn opaque(&self) -> bool {
        self.0.opaque()
    }

    fn add(&mut self, hit: Hit<'_, Self::BlockData>) {
        self.0.add(hit);
    }

    fn mean<const N: usize>(items: [Self; N]) -> Self {
        (A::mean(items.map(|(a,)| a)),)
    }
}

impl<A, B> Accumulate for (A, B)
where
    A: Copy + Accumulate,
    B: Copy + Accumulate<BlockData = A::BlockData>,
{
    type BlockData = A::BlockData;

    fn opaque(&self) -> bool {
        self.0.opaque() && self.1.opaque()
    }

    fn add(&mut self, hit: Hit<'_, Self::BlockData>) {
        self.0.add(hit);
        self.1.add(hit);
    }

    fn mean<const N: usize>(items: [Self; N]) -> Self {
        // TODO: Rewrite this in a way which avoids the `Copy` requirement
        (
            A::mean(items.map(|(a, _)| a)),
            B::mean(items.map(|(_, b)| b)),
        )
    }
}

// -------------------------------------------------------------------------------------------------

/// Adapts an [`Accumulator`] which takes `BlockData = ()` to take a different type and ignore it.
///
/// Ideally, this would not be necessary and there would be some kind of automatic adaptation
/// for any compatible data, but doing that would require introducing more traits.
/// The obvious choice of trait would be [`AsRef`], but there is no `impl AsRef<()> for ()`.
/// Another possibility would be `trait Accumulate<D>` — making the associated type into a parameter
/// so that impls look like `impl<D> Accumulate<D> for ColorBuf` — but that causes awkward things
/// like having different `mean()` per data type. Might be worthwhile anyway.
///
/// This isn’t a generic “map the block data” adapter because the `Default` requirement means the
/// mapping function can’t be provided. Perhaps we should replace that requirement with a factory.
#[derive(Debug)]
pub(crate) struct IgnoreBlockData<D, A> {
    pub inner: A,
    _phantom: PhantomData<fn(&D)>,
}

impl<D, A: Default> Default for IgnoreBlockData<D, A> {
    fn default() -> Self {
        Self {
            inner: A::default(),
            _phantom: PhantomData,
        }
    }
}

impl<D: RtBlockData, A: Accumulate<BlockData = ()>> Accumulate for IgnoreBlockData<D, A> {
    type BlockData = D;

    fn opaque(&self) -> bool {
        self.inner.opaque()
    }

    fn add(&mut self, hit: Hit<'_, Self::BlockData>) {
        self.inner.add(hit.map_block_data(|_| &()))
    }

    fn mean<const N: usize>(items: [Self; N]) -> Self {
        Self {
            inner: A::mean(items.map(|item| item.inner)),
            _phantom: PhantomData,
        }
    }
}

impl<D, A: Copy> Copy for IgnoreBlockData<D, A> {}
impl<D, A: Clone> Clone for IgnoreBlockData<D, A> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _phantom: PhantomData,
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn color_buf() {
        // TODO: This entire test should be revisited with our new understanding about
        // premultiplied alpha’s role in color accumulation.
        // Maybe we can fix the failing assertions.
        // <https://github.com/kpreid/all-is-cubes/issues/504>

        let color_1 = Rgba::new(1.0, 0.0, 0.0, 0.75);
        let color_2 = Rgba::new(0.0, 1.0, 0.0, 0.5);
        let color_3 = Rgba::new(0.0, 0.0, 1.0, 1.0);

        let mut buf = ColorBuf::default();
        assert_eq!(Rgba::from(buf), Rgba::TRANSPARENT);
        assert!(!buf.opaque());

        buf.add(Hit {
            exception: None,
            surface: color_1.into(),
            t_distance: None,
            block: &(),
            position: None,
        });
        assert_eq!(Rgba::from(buf), color_1);
        assert!(!buf.opaque());

        buf.add(Hit {
            exception: None,
            surface: color_2.into(),
            t_distance: None,
            block: &(),
            position: None,
        });
        // TODO: this is not the right assertion because it's the premultiplied form.
        // assert_eq!(
        //     buf.result(),
        //     (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125)
        //         .with_alpha(NotNan::new(0.875).unwrap())
        // );
        assert!(!buf.opaque());

        buf.add(Hit {
            exception: None,
            surface: color_3.into(),
            t_distance: None,
            block: &(),
            position: None,
        });
        assert!(Rgba::from(buf).fully_opaque());
        //assert_eq!(
        //    buf.result(),
        //    (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125 + color_3.to_rgb() * 0.125)
        //        .with_alpha(NotNan::one())
        //);
        assert!(buf.opaque());
    }
}

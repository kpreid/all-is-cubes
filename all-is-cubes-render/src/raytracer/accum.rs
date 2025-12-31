//! [`Accumulate`] and output formats of the raytracer.

use core::marker::PhantomData;

use all_is_cubes::block::Resolution;
use all_is_cubes::math::{PositiveSign, Rgb, Rgba};
use all_is_cubes::space::SpaceBlockData;

#[cfg(doc)]
use all_is_cubes::space::Space;

use crate::camera::GraphicsOptions;
use crate::raytracer::{ColorBuf, Exception, Hit};

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

/// Precomputed data about a [`Space`]'s blocks that may be used by [`Accumulate`] implementations.
///
/// Design note: This is a trait of the data type itself so as to require that there
/// cannot be more than one way to construct a given data type (other than explicit
/// configuration). This prevents multiple conflicting interpretations of a single data
/// type.
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

impl Accumulate for ColorBuf {
    type BlockData = ();

    #[inline]
    fn opaque(&self) -> bool {
        // delegate to intrinsic impl
        self.opaque()
    }

    #[inline]
    fn add(&mut self, hit: Hit<'_, Self::BlockData>) {
        if matches!(hit.exception, Some(Exception::DebugOverrideRg)) {
            // TODO: this is messy and doing more conversion work than needed.
            let red = PositiveSign::<f32>::new_clamped(hit.surface.light.x);
            let green = PositiveSign::<f32>::new_clamped(hit.surface.light.y);
            let blue =
                PositiveSign::<f32>::new_clamped(Rgba::from(*self).to_rgb().luminance() * 0.2);
            *self = ColorBuf::from_light_and_transmittance(Rgb::new_ps(red, green, blue), 0.0);
        } else {
            self.add_color_internal(hit.surface);
        }
    }

    #[inline]
    fn mean<const N: usize>(items: [Self; N]) -> Self {
        // delegate to intrinsic impl
        Self::mean(items)
    }
}

// -------------------------------------------------------------------------------------------------

/// Implements [`Accumulate`] to collect depth data.
///
/// It returns the depth of the nearest surface, even if that surface is transparent.
#[derive(Clone, Copy, Debug)]
#[allow(clippy::exhaustive_structs)]
pub struct DepthBuf {
    depth: f64,
}

impl Default for DepthBuf {
    fn default() -> Self {
        Self {
            depth: f64::INFINITY,
        }
    }
}

impl Accumulate for DepthBuf {
    type BlockData = ();

    fn opaque(&self) -> bool {
        // Note that if we get a NaN, this returns false, which is correct because the min()
        // in add() will overwrite the NaN.
        self.depth < f64::INFINITY
    }

    fn add(&mut self, hit: Hit<'_, Self::BlockData>) {
        if let Some(d) = hit.t_distance {
            // Note: If the caller respects self.opaque(), then this could just be
            //     self.depth = d;
            // but we don't make that part of the contract.
            self.depth = self.depth.min(d);
        }
    }

    fn mean<const N: usize>(items: [Self; N]) -> Self {
        // In general, it doesn’t make sense to literally take the mean of depth values,
        // since it could create a position floating in space between two objects.
        // Use the minimum as a less-bad choice.
        //
        // More broadly, this method is used to do antialiasing via multiple samples,
        // and depth value processing doesn't fit well with that.
        //
        // TODO: Choosing the approximate mode might make more sense than choosing the minimum.
        // Also,
        Self {
            depth: items.into_iter().map(|db| db.depth).reduce(f64::min).unwrap_or(f64::INFINITY),
        }
    }
}

impl DepthBuf {
    /// The depth value, or [`f64::INFINITY`] if no surface was hit.
    /// See [`Hit::t_distance`] for more information on the interpretation of this number.
    ///
    /// Currently, it is not possible to distinguish different kinds of nothing hit.
    /// If that is needed, compose this with another `Accumulate` implementation.
    //---
    // Design note: This is a method so that we have the option of changing the representation.
    pub fn depth(self) -> f64 {
        self.depth
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
    use crate::camera::GraphicsOptions;
    use crate::raytracer::ColorBuf;
    use crate::raytracer::SpaceRaytracer;
    use all_is_cubes::block::Resolution::*;
    use all_is_cubes::content;
    use all_is_cubes::math::GridAab;
    use all_is_cubes::math::Rgba;
    use all_is_cubes::raycast::Ray;
    use all_is_cubes::space::Space;
    use all_is_cubes::universe::Universe;

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

    #[test]
    fn depth_buf() {
        let universe = &mut Universe::new();
        let slab = content::make_slab(universe, 1, R2);
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .read_ticket(universe.read_ticket())
            .filled_with(slab)
            .build();
        let rt = &SpaceRaytracer::<()>::new(&space.read(), GraphicsOptions::default(), ());

        #[track_caller]
        fn assert_depth(rt: &SpaceRaytracer<()>, expected_depth: f64, ray: Ray, label: &str) {
            let mut depth_buf = DepthBuf::default();
            rt.trace_ray(ray, &mut depth_buf, false);
            assert_eq!(depth_buf.depth, expected_depth, "{label}");
        }

        assert_depth(
            rt,
            0.0,
            Ray::new([0.25, 0.25, 0.], [0., 0., 1.]),
            "immediate collision",
        );

        let d = 0.25;
        assert_depth(
            rt,
            d,
            Ray::new([0.25, 0.25, -d], [0., 0., 1.]),
            "some distance back",
        );

        let scale = 4.0;
        assert_depth(
            rt,
            d / scale,
            Ray::new([0.25, 0.25, -d], [0., 0., scale]),
            "ray length scales results",
        );

        let starting_y = 5.25;
        assert_depth(
            rt,
            starting_y - 0.5,
            Ray::new([0.5, starting_y, 0.5], [0.0, -1.0, 0.0]),
            "ray onto top of slab",
        );

        assert_depth(
            rt,
            f64::INFINITY,
            Ray::new([0.5, 0.75, -0.5], [0.0, 0.0, 1.0]),
            "miss slab over the top",
        );
    }
}

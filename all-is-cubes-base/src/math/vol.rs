use alloc::boxed::Box;
use alloc::sync::Arc;
use core::fmt;
use core::ops::{Deref, DerefMut};

#[cfg(doc)]
use alloc::vec::Vec;

use euclid::{Point3D, Size3D, vec3};
use manyfmt::Refmt as _;

use crate::math::{Axis, Cube, GridAab, GridCoordinate, GridIter, GridPoint, GridVector};

// #[derive(Clone, Copy, Debug)]
// pub struct XMaj;

/// Z-major ordering: linearly adjacent elements have adjacent Z coordinates.
///
/// `[0, 0, 0], [0, 0, 1], [0, 0, 2], ..., [0, 1, 0], [0, 1, 1], ...`
///
/// Use this type with [`Vol`] to store volume data in this order.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct ZMaj;

/// Type for volume data stored in a slice, or for generating linear indexing.
///
/// * `C` is some slice container type, e.g. `&[T]` or `Box<[T]>`.
///   It may also be `()` to describe a linearization without actually storing data.
/// * `O` specifies the choice of linearization.
///   Currently, only one choice exists, [`ZMaj`].
///
/// In addition to the data, each [`Vol`] stores the [`GridAab`] defining its size;
/// the container's length must be equal to the volume of that AAB. Whether that can be
/// relied upon entirely depends on whether the specific container value produces
/// the same length of slice every time it is [`Deref`]erenced without mutating it directly.
/// For example, `Vec<T>` and `Box<[T]>` satisfy this criterion; the fact that [`Vec`] has
/// length-mutating operations is irrelevant because no `&mut Vec<T>` is exposed.
///
/// A [`Vol`] whose volume exceeds [`usize::MAX`] cannot exist.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Vol<C, O = ZMaj> {
    /// Invariant: `bounds` has a volume that is at most [`usize::MAX`].
    bounds: GridAab,
    ordering: O,
    /// Invariant: `contents.deref().len()`, if it exists, equals `bounds.volume()`.
    contents: C,
}

impl<O> Vol<(), O> {
    /// Use `GridAab::to_vol()` to call this.
    pub(crate) fn new_dataless(bounds: GridAab, ordering: O) -> Result<Self, VolLengthError> {
        if bounds.volume().is_none() {
            Err(VolLengthError {
                input_length: None,
                bounds,
            })
        } else {
            Ok(Self {
                bounds,
                ordering,
                contents: (),
            })
        }
    }

    /// Constructs a [`Vol`] from 8-bit integers that cannot overflow.
    ///
    /// This constructor is limited so that it is `const` and infallible;
    /// It is the [`Vol`] version of [`GridAab::tiny()`].
    #[inline]
    pub const fn tiny(
        lower_bounds: Point3D<i8, Cube>,
        size: Size3D<u8, Cube>,
        ordering: O,
    ) -> Self {
        Self {
            bounds: GridAab::tiny(lower_bounds, size),
            ordering,
            contents: (),
        }
    }

    /// Attach some data to this dataless `Vol`.
    ///
    /// Returns a [`VolLengthError`] if the number of elements does not match
    /// [`bounds.volume()`](GridAab::volume).
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
    pub fn with_elements<C, V>(self, elements: C) -> Result<Vol<C, O>, VolLengthError>
    where
        C: Deref<Target = [V]>,
    {
        if elements.len() == self.volume() {
            Ok(Vol {
                bounds: self.bounds,
                ordering: self.ordering,
                contents: elements,
            })
        } else {
            Err(VolLengthError {
                input_length: Some(elements.len()),
                bounds: self.bounds(),
            })
        }
    }
}

impl Vol<(), ZMaj> {
    /// Divide `self` into two approximately equal-sized parts which, if they had elements, would
    /// each be contiguous in the linear ordering.
    ///
    /// Returns [`None`] if `self` does not have at least two cubes.
    ///
    /// Note that this is one of several `subdivide()` methods for different container types;
    /// it is also implemented for immutable and mutable references.
    /// These are intended to be useful in executing parallel algorithms on volume data.
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn subdivide(self) -> Result<[Self; 2], Self> {
        match find_zmaj_subdivision(self) {
            Some((lower_half, upper_half, _)) => Ok([lower_half, upper_half]),
            _ => Err(self),
        }
    }
}

/// Constructors from linear containers.
impl<C, O: Default, V> Vol<C, O>
where
    C: Deref<Target = [V]>,
{
    /// Constructs a `Vol<C>` containing the provided elements, which must be in the
    /// ordering specified by `O`.
    ///
    /// Returns a [`VolLengthError`] if the number of elements does not match
    /// [`bounds.volume()`](GridAab::volume).
    //---
    // TODO: Remove this in favor of with_elements()?
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
    pub fn from_elements(bounds: GridAab, elements: impl Into<C>) -> Result<Self, VolLengthError> {
        let elements = elements.into();
        if Some(elements.len()) == bounds.volume() {
            Ok(Vol {
                bounds,
                ordering: O::default(),
                contents: elements,
            })
        } else {
            Err(VolLengthError {
                input_length: Some(elements.len()),
                bounds,
            })
        }
    }
}

impl<'a, O, V> Vol<&'a [V], O> {
    /// Constructs a [`Vol`] containing the provided elements, of which there must be no more than
    /// 255³.
    ///
    /// This constructor is limited so that it is `const` and infallible;
    /// see also [`GridAab::tiny()`] and [`Vol::tiny()`].
    ///
    /// Panics if `data`’s length does not match the volume of `size`.
    #[inline]
    pub const fn from_tiny_elements(
        lower_bounds: Point3D<i8, Cube>,
        size: Size3D<u8, Cube>,
        ordering: O,
        contents: &'a [V],
    ) -> Self {
        let bounds = GridAab::tiny(lower_bounds, size);
        assert!(
            bounds.volume().unwrap() == contents.len(),
            "element count does not match AAB volume"
        );
        Self {
            bounds,
            ordering,
            contents,
        }
    }
}

/// Constructors from elements not already stored linearly.
//---
// TODO: This should be `O: Ordering` instead of `ZMaj` once we have alternative orderings
#[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
impl<C, V> Vol<C, ZMaj>
where
    // Note that the Deref bound is necessary to give this a unique `V`.
    C: Deref<Target = [V]> + FromIterator<V>,
{
    /// Constructs a `Vol<C>` by using the provided function to compute a value
    /// for each point.
    ///
    /// Panics if `bounds` has a volume exceeding `usize::MAX`.
    /// (But there will likely be a memory allocation failure well below that point.)
    #[inline]
    pub fn from_fn<F>(bounds: GridAab, f: F) -> Self
    where
        F: FnMut(Cube) -> V,
    {
        let bounds = bounds.to_vol::<ZMaj>().unwrap();
        bounds
            .with_elements(bounds.iter_cubes().map(f).collect())
            .unwrap()
    }

    /// Constructs a `Vol<C>` by cloning the provided value for each point.
    #[inline]
    pub fn repeat(bounds: GridAab, value: V) -> Self
    where
        V: Clone,
    {
        Self::from_fn(bounds, |_| value.clone())
    }

    /// Constructs a `Vol<C>` with a single value, in bounds `ORIGIN_CUBE`.
    ///
    /// If the single element should be at a different location, you can call
    /// [`.translate(offset)`](Self::translate), or use [`Vol::from_elements()`]
    /// instead.
    #[inline]
    pub fn from_element(value: V) -> Self {
        Self::from_elements(GridAab::ORIGIN_CUBE, core::iter::once(value).collect::<C>()).unwrap()
    }

    /// Constructs a [`Vol<Box<[V]>>`] from nested Rust arrays in \[Z\]\[Y\]\[X\] order
    /// with the Y axis mirrored.
    /// The result's bounds's lower bounds are zero.
    ///
    /// Note: The current implementation requires that `V` implement [`Clone`], and will
    /// clone each element once, but this may be improved in the future.
    // TODO: Decide if this is a good public interface.
    // TODO: Reimplement this in terms of adopting the elements as a linear array.
    // TODO: Test.
    #[doc(hidden)] // used by all-is-cubes-content
    #[expect(clippy::needless_pass_by_value)]
    pub fn from_y_flipped_array<const DX: usize, const DY: usize, const DZ: usize>(
        array: [[[V; DX]; DY]; DZ],
    ) -> Self
    where
        V: Clone,
    {
        Self::from_fn(
            GridAab::from_lower_size([0, 0, 0], vec3(DX, DY, DZ).to_u32()),
            |p| array[p.z as usize][(DY - 1) - (p.y as usize)][p.x as usize].clone(),
        )
    }
}

impl<C, O> Vol<C, O> {
    /// Returns the [`GridAab`] specifying the bounds of this volume data.
    #[inline]
    pub fn bounds(&self) -> GridAab {
        self.bounds
    }

    /// Returns the volume, also known as the number of elements.
    #[inline]
    pub fn volume(&self) -> usize {
        // Ideally, we could specialize on C and return self.contents.len() if possible,
        // as it doesn't require doing any multiplications, but that's not currently possible
        // in Rust.

        let size = self.bounds.size();
        // This will not overflow, as an invariant of the `Vol` type.
        size.width as usize * size.height as usize * size.depth as usize
    }

    /// Extracts the linear contents, discarding the bounds and ordering.
    #[inline]
    pub fn into_elements(self) -> C {
        self.contents
    }

    /// Returns a `Vol` with the same bounds and ordering but no data.
    ///
    /// This is the inverse operation to [`Vol::with_elements()`].
    #[inline]
    pub fn without_elements(&self) -> Vol<(), O>
    where
        O: Clone,
    {
        Vol {
            bounds: self.bounds,
            ordering: self.ordering.clone(),
            contents: (),
        }
    }

    /// Translates the volume without affecting its contents.
    ///
    /// Panics if this would cause numeric overflow.
    ///
    /// TODO: example
    #[must_use]
    #[track_caller]
    #[inline]
    pub fn translate(self, offset: impl Into<GridVector>) -> Self {
        self.translate_impl(offset.into())
    }

    #[track_caller]
    #[inline]
    fn translate_impl(mut self, offset: GridVector) -> Self {
        let new_bounds = self.bounds.translate(offset);
        if new_bounds.size() != self.bounds.size() {
            // We can't just continue like `GridAab::translate` does, because that would
            // break the invariant that self.volume() == self.contents.len().
            panic!("Vol::translate() offset caused numeric overflow");
        }
        self.bounds = new_bounds;
        self
    }

    // TODO: reconcile this with from_elements() — should only be implemented once.
    #[doc(hidden)] // TODO: good public api?
    #[inline]
    pub fn map_container<C2, V2, F>(self, f: F) -> Vol<C2, O>
    where
        F: FnOnce(C) -> C2,
        C2: Deref<Target = [V2]>,
    {
        let bounds = self.bounds;
        let volume = self.volume();
        let contents = f(self.contents);
        if contents.len() != volume {
            panic!(
                "{}",
                VolLengthError {
                    input_length: Some(contents.len()),
                    bounds: self.bounds,
                }
            )
        }
        Vol {
            bounds,
            ordering: self.ordering,
            contents,
        }
    }
}

impl<C> Vol<C, ZMaj> {
    /// Iterate over all cubes that this contains, in the order of the linearization,
    /// without including the stored data (if there is any).
    #[inline]
    pub fn iter_cubes(&self) -> GridIter {
        GridIter::new(self.bounds)
    }

    /// Determines whether a unit cube lies within this volume and, if it does, returns the
    /// linearized slice index into it.
    ///
    /// The linearized element order is defined by the `O` type.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Vol, GridAab};
    ///
    /// let vol = GridAab::from_lower_size([0, 0, 0], [10, 10, 10]).to_vol().unwrap();
    ///
    /// assert_eq!(vol.index([0, 0, 0].into()), Some(0));
    /// assert_eq!(vol.index([1, 2, 3].into()), Some(123));
    /// assert_eq!(vol.index([9, 9, 9].into()), Some(999));
    /// assert_eq!(vol.index([0, 0, -1].into()), None);
    /// assert_eq!(vol.index([0, 0, 10].into()), None);
    /// ```
    ///
    /// TODO: more example, less unit-test
    #[inline(always)] // very hot code
    pub fn index(&self, cube: Cube) -> Option<usize> {
        let sizes = self.bounds.size();

        // This might overflow and wrap, but if it does, the result will still be out
        // of bounds, just in the other direction, because wrapping subtraction is an
        // injective mapping of integers, and every in-bounds maps to in-bounds, so
        // every out-of-bounds must also map to out-of-bounds.
        let deoffsetted: GridPoint = GridPoint::from(cube)
            .zip(self.bounds.lower_bounds(), GridCoordinate::wrapping_sub)
            .to_point();

        // Bounds check, expressed as a single unsigned comparison.
        if (deoffsetted.x as u32 >= sizes.width)
            | (deoffsetted.y as u32 >= sizes.height)
            | (deoffsetted.z as u32 >= sizes.depth)
        {
            return None;
        }

        // Convert to usize for indexing.
        // This cannot overflow because:
        // * We just checked it is not negative
        // * We just checked it is not greater than `self.sizes[i]`, which is an `i32`
        // * We don't support platforms with `usize` smaller than 32 bits
        // We use `as usize` rather than `deoffsetted.to_usize()` because the latter has an
        // overflow check.
        let ixvec: Point3D<usize, _> = deoffsetted.map(|s| s as usize);

        // Compute index.
        // Always use wrapping (rather than maybe-checked) arithmetic, because we
        // checked the criteria for it to not overflow.
        Some(
            (ixvec
                .x
                .wrapping_mul(sizes.height as usize)
                .wrapping_add(ixvec.y))
            .wrapping_mul(sizes.depth as usize)
            .wrapping_add(ixvec.z),
        )
    }
}

/// Linear data access.
#[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
impl<C, O, V> Vol<C, O>
where
    C: Deref<Target = [V]>,
    O: Copy,
{
    /// Return a [`Vol`] that borrows the contents of this one.
    pub fn as_ref(&self) -> Vol<&[V], O> {
        Vol {
            bounds: self.bounds,
            ordering: self.ordering,
            contents: self.as_linear(),
        }
    }

    /// Return a [`Vol`] that mutably borrows the contents of this one.
    pub fn as_mut(&mut self) -> Vol<&mut [V], O>
    where
        C: DerefMut,
    {
        Vol {
            bounds: self.bounds,
            ordering: self.ordering,
            contents: self.as_linear_mut(),
        }
    }

    /// Returns the linear contents viewed as a slice.
    pub fn as_linear(&self) -> &[V] {
        let s = &*self.contents;
        debug_assert_eq!(s.len(), self.volume());
        s
    }

    /// Returns the linear contents viewed as a mutable slice.
    pub fn as_linear_mut(&mut self) -> &mut [V]
    where
        C: DerefMut,
    {
        let s = &mut *self.contents;
        debug_assert_eq!(s.len(), self.bounds.volume().unwrap());
        s
    }
}

impl<'a, V> Vol<&'a [V], ZMaj> {
    /// Returns the element at `position` of this volume data, or [`None`] if `position` is out
    /// of bounds.
    ///
    /// This differs from [`Self::get()`] in that it inherits the lifetime of the container
    /// reference, rather than reborrowing. It is therefore only available for `Vol<&'a [V]>`.
    #[inline]
    pub fn get_ref(&self, position: impl Into<Cube>) -> Option<&'a V> {
        let index = self.index(position.into())?;
        Some(&self.contents[index])
    }

    /// Divide `self` into two approximately equal-sized parts,
    /// each of which refers to the appropriate sub-slice of elements.
    ///
    /// Returns [`None`] if `self` does not have at least two cubes.
    ///
    /// Note that this is one of several `subdivide()` methods for different container types;
    /// it is also implemented for mutable references and `()`.
    /// These are intended to be useful in executing parallel algorithms on volume data.
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn subdivide(self) -> Result<[Self; 2], Self> {
        match find_zmaj_subdivision(self.without_elements()) {
            Some((lower_half, upper_half, lower_half_len)) => {
                let (lower_contents, upper_contents) = self.contents.split_at(lower_half_len);

                Ok([
                    lower_half
                        .with_elements(lower_contents)
                        .unwrap_or_else(unreachable_wrong_size),
                    upper_half
                        .with_elements(upper_contents)
                        .unwrap_or_else(unreachable_wrong_size),
                ])
            }
            _ => Err(self),
        }
    }
}

impl<V> Vol<&mut [V], ZMaj> {
    /// Divide `self` into two approximately equal-sized parts.
    /// each of which refers to the appropriate sub-slice of elements.
    ///
    /// `filter` may be used to reject subdivisions that are too small or otherwise unsuitable;
    /// it is passed the bounds of the two slices that would be returned.
    ///
    /// Returns [`Err`] containing `self` if `self` does not have at least two cubes,
    /// or if `filter` returns `false`.
    ///
    /// Note that this is one of several `subdivide()` methods for different container types;
    /// it is also implemented for immutable references and `()`.
    /// These are intended to be useful in executing parallel algorithms on volume data.
    //---
    // Design note: This has the `filter` parameter, where other `subdivide()`s do not,
    // because otherwise the caller may have trouble with conditional returns of mutable borrows
    // (NLL Problem Case #3); the filter allows cancelling the split rather than discarding it.
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn subdivide(self, filter: impl FnOnce([Vol<()>; 2]) -> bool) -> Result<[Self; 2], Self> {
        match find_zmaj_subdivision(self.without_elements()) {
            Some((lower_half, upper_half, lower_half_len)) if filter([lower_half, upper_half]) => {
                let (lower_contents, upper_contents) = self.contents.split_at_mut(lower_half_len);

                Ok([
                    lower_half
                        .with_elements(lower_contents)
                        .unwrap_or_else(unreachable_wrong_size),
                    upper_half
                        .with_elements(upper_contents)
                        .unwrap_or_else(unreachable_wrong_size),
                ])
            }
            _ => Err(self),
        }
    }
}

impl<V: Clone, O> Vol<Arc<[V]>, O> {
    /// Returns the linear contents viewed as a mutable slice, as if by [`Arc::make_mut()`].
    #[doc(hidden)] // TODO: good public API?
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn make_linear_mut(&mut self) -> &mut [V] {
        let slice: &mut [V] = Arc::make_mut(&mut self.contents);
        debug_assert_eq!(slice.len(), self.bounds.volume().unwrap());
        slice
    }
}

/// Element lookup operations by 3D coordinates.
#[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
impl<C, V> Vol<C, ZMaj>
where
    C: Deref<Target = [V]>,
{
    /// Returns the element at `position` of this volume data, or [`None`] if `position` is out
    /// of bounds.
    #[inline]
    pub fn get(&self, position: impl Into<Cube>) -> Option<&V> {
        let index = self.index(position.into())?;
        Some(&self.as_linear()[index])
    }

    /// Returns a mutable reference to the element at `position` of this volume data,
    /// or [`None`] if `position` is out of bounds.
    #[inline]
    pub fn get_mut(&mut self, position: impl Into<Cube>) -> Option<&mut V>
    where
        C: DerefMut,
    {
        let index = self.index(position.into())?;
        Some(&mut self.as_linear_mut()[index])
    }

    /// Iterates over all the cubes and values in this volume data, in the ordering specified
    /// by the `O` type parameter.
    pub fn iter<'s>(&'s self) -> impl Iterator<Item = (Cube, &'s V)> + Clone
    where
        V: 's,
    {
        self.iter_cubes().zip(self.as_linear().iter())
    }

    /// Iterates by mutable reference over all the cubes and values in this volume data,
    /// in the ordering specified by the `O` type parameter.
    pub fn iter_mut<'s>(&'s mut self) -> impl Iterator<Item = (Cube, &'s mut V)>
    where
        C: DerefMut,
        V: 's,
    {
        self.iter_cubes().zip(self.as_linear_mut().iter_mut())
    }
}

#[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
impl<V, O> Vol<Box<[V]>, O> {
    /// Apply `f` to each element and collect the results into the same shape and ordering.
    pub fn map<T, F>(self, f: F) -> Vol<Box<[T]>, O>
    where
        F: FnMut(V) -> T,
    {
        Vol {
            bounds: self.bounds,
            ordering: self.ordering,
            // When we switch to Rust 2024 edition, replace this qualified call with a method call.
            contents: <Box<[V]> as IntoIterator>::into_iter(self.contents)
                .map(f)
                .collect(),
        }
    }
}

#[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
impl<C: fmt::Debug, O: fmt::Debug> fmt::Debug for Vol<C, O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Note: If specialization was available we'd like to use it to print the elements under
        // our own control if there are elements, but as it is, we'd rather preserve functionality
        // for `Vol<()>`, which means we always print self.contents as a whole or not at all.

        let Self {
            bounds,
            ordering,
            contents,
        } = self;

        let mut ds = f.debug_struct(&format!(
            "Vol<{contents_type}, {ordering:?}>",
            contents_type = core::any::type_name::<C>(),
        ));
        ds.field("bounds", &bounds);
        let volume = self.volume();
        if core::any::type_name::<C>() == core::any::type_name::<()>() {
            // don't print "contents: ()"
        } else if volume > 32 {
            ds.field(
                "contents",
                &format!("[...{volume} elements]").refmt(&manyfmt::formats::Unquote),
            );
        } else {
            ds.field("contents", &contents);
        }
        ds.finish()
    }
}

impl<P, C, V> core::ops::Index<P> for Vol<C, ZMaj>
where
    P: Into<Cube>,
    C: Deref<Target = [V]>,
{
    type Output = V;

    /// Returns the element at `position` of this volume data,
    /// or panics if `position` is out of bounds.
    ///
    /// Use [`Vol::get()`] for a non-panicing alternative.
    #[inline(always)] // measured faster on wasm32 in worldgen
    fn index(&self, position: P) -> &Self::Output {
        let position: Cube = position.into();
        if let Some(index) = self.index(position) {
            &self.contents[index]
        } else {
            panic!(
                "position {:?} out of Vol bounds {:?}",
                position, self.bounds
            )
        }
    }
}
impl<P, C, V> core::ops::IndexMut<P> for Vol<C, ZMaj>
where
    P: Into<Cube>,
    C: DerefMut<Target = [V]>,
{
    /// Returns the element at `position` of this volume data,
    /// or panics if `position` is out of bounds.
    #[inline(always)]
    fn index_mut(&mut self, position: P) -> &mut Self::Output {
        let position: Cube = position.into();
        if let Some(index) = self.index(position) {
            &mut self.contents[index]
        } else {
            panic!(
                "position {:?} out of Vol bounds {:?}",
                position, self.bounds
            )
        }
    }
}

mod aab_compat {
    use super::*;

    impl<O> PartialEq<GridAab> for Vol<(), O> {
        #[inline]
        fn eq(&self, other: &GridAab) -> bool {
            self.bounds() == *other
        }
    }

    impl<O> PartialEq<Vol<(), O>> for GridAab {
        #[inline]
        fn eq(&self, other: &Vol<(), O>) -> bool {
            *self == other.bounds()
        }
    }
}

#[cfg(feature = "arbitrary")]
#[allow(clippy::missing_inline_in_public_items)]
pub(crate) mod vol_arb {
    use super::*;
    use arbitrary::Arbitrary;

    /// Let's not spend too much memory on generating arbitrary length vectors.
    /// This does reduce coverage...
    const MAX_VOLUME: usize = 2_usize.pow(16);

    /// Size hint for [`Vol::arbitrary_with_max_volume()`].
    pub(crate) const ARBITRARY_BOUNDS_SIZE_HINT: (usize, Option<usize>) = {
        // 6 bounding coordinates plus one permutation selection.
        // Depending on the volume, we could end up consuming as little as 1 byte each
        // for the sizes, and none for the positions, because `int_in_range()` uses the range
        // to decide how many bytes to consume.
        let gc = size_of::<GridCoordinate>();
        (3 + 1, Some(gc * 6 + 1))
    };

    impl<O: Default> Vol<(), O> {
        #[cfg(feature = "arbitrary")]
        #[doc(hidden)]
        pub fn arbitrary_with_max_volume(
            u: &mut arbitrary::Unstructured<'_>,
            volume: usize,
        ) -> arbitrary::Result<Self> {
            // Pick sizes within the volume constraint.
            let mut limit: u32 = volume.try_into().unwrap_or(u32::MAX);
            let size_1 = u.int_in_range(0..=limit)?;
            limit /= size_1.max(1);
            let size_2 = u.int_in_range(0..=limit)?;
            limit /= size_2.max(1);
            let size_3 = u.int_in_range(0..=limit)?;

            // Shuffle the sizes to remove any bias.
            let sizes = *u.choose(&[
                vec3(size_1, size_2, size_3),
                vec3(size_1, size_3, size_2),
                vec3(size_2, size_1, size_3),
                vec3(size_2, size_3, size_1),
                vec3(size_3, size_1, size_2),
                vec3(size_3, size_2, size_1),
            ])?;

            // Compute lower bounds that are valid for the sizes.
            let possible_lower_bounds = sizes.map(|coord| {
                GridCoordinate::MIN..=GridCoordinate::MAX.saturating_sub_unsigned(coord)
            });
            let lower_bounds = GridPoint::new(
                u.int_in_range(possible_lower_bounds.x)?,
                u.int_in_range(possible_lower_bounds.y)?,
                u.int_in_range(possible_lower_bounds.z)?,
            );

            Ok(GridAab::from_lower_size(lower_bounds, sizes)
                .to_vol()
                .expect("GridAab as Arbitrary failed to compute valid bounds"))
        }
    }

    impl<'a, V: Arbitrary<'a>, C> Arbitrary<'a> for Vol<C, ZMaj>
    where
        C: FromIterator<V> + Deref<Target = [V]>,
    {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            let bounds = Vol::<()>::arbitrary_with_max_volume(u, MAX_VOLUME)?;
            let contents: C = u
                .arbitrary_iter()?
                .take(bounds.volume())
                .collect::<Result<C, _>>()?;
            bounds
                .with_elements(contents)
                .map_err(|_| arbitrary::Error::NotEnoughData)
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            // recommended impl from trait documentation
            Self::try_size_hint(depth).unwrap_or_default()
        }

        fn try_size_hint(
            depth: usize,
        ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
            arbitrary::size_hint::try_recursion_guard(depth, |depth| {
                let (lower, upper) = V::try_size_hint(depth)?;
                Ok(arbitrary::size_hint::and(
                    ARBITRARY_BOUNDS_SIZE_HINT,
                    (
                        lower.saturating_mul(MAX_VOLUME),
                        upper.map(|u| u.saturating_mul(MAX_VOLUME)),
                    ),
                ))
            })
        }
    }
}

/// Error from [`Vol::from_elements()`] being given the wrong length,
/// or from constructing a [`Vol`] with a volume greater than [`usize::MAX`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct VolLengthError {
    /// The length of the linear data, or [`None`] if we're constructing a dataless [`Vol`].
    input_length: Option<usize>,
    /// The attempted bounds, whose volume is either unequal to `input_length` or overflowing.
    bounds: GridAab,
}

impl core::error::Error for VolLengthError {}

impl fmt::Display for VolLengthError {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            input_length,
            bounds,
        } = self;
        match (input_length, bounds.volume()) {
            (Some(input_length), Some(volume)) => write!(
                f,
                "data of length {input_length} cannot fill volume {volume} of {bounds:?}",
            ),

            (Some(input_length), None) => write!(
                f,
                "data of length {input_length} cannot fill {bounds:?}, \
                    which is too large to be linearized at all",
            ),

            (None, None) => write!(
                f,
                "{bounds:?} has a volume of {volume_u128}, \
                    which is too large to be linearized",
                // u128 is large enough to hold the multiplication of three u32s
                volume_u128 = bounds.size().cast::<u128>().volume(),
            ),

            (None, Some(_)) => write!(f, "<malformed error {self:?}>"),
        }
    }
}

/// Find a way to split the bounds of a `Vol` which results in two adjacent volumes
/// whose linear elements are also adjacent.
/// Returns the two boxes and the linear split point.
fn find_zmaj_subdivision(vol: Vol<()>) -> Option<(Vol<()>, Vol<()>, usize)> {
    // The order of these tests must reflect the ordering in use
    // for the result to be valid.
    let bounds = vol.bounds();
    for axis in [Axis::X, Axis::Y, Axis::Z] {
        let axis_range = bounds.axis_range(axis);
        let size: u32 = bounds.size()[axis];
        if size >= 2 {
            let split_coordinate = axis_range.start + (size / 2).cast_signed();

            let mut lower_half_ub = bounds.upper_bounds();
            lower_half_ub[axis] = split_coordinate;
            let lower_half = GridAab::from_lower_upper(bounds.lower_bounds(), lower_half_ub)
                .to_vol()
                .unwrap_or_else(unreachable_wrong_size);

            let mut upper_half_lb = bounds.lower_bounds();
            upper_half_lb[axis] = split_coordinate;
            let upper_half = GridAab::from_lower_upper(upper_half_lb, bounds.upper_bounds())
                .to_vol()
                .unwrap_or_else(unreachable_wrong_size);

            let lower_half_volume = lower_half.volume();
            debug_assert_eq!(lower_half_volume + upper_half.volume(), vol.volume());
            return Some((lower_half, upper_half, lower_half_volume));
        }
    }
    None
}

/// Function for `.unwrap_or_else()`s inside subdivision operations.
/// The advantage of this over many `unwrap()`s is generating fewer distinct panic sites for
/// these cases which are impossible.
#[cold]
fn unreachable_wrong_size<T>(error: VolLengthError) -> T {
    panic!("impossible size mismatch: {error}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::String;
    use euclid::{point3, size3};
    use pretty_assertions::assert_eq;

    type VolBox<T> = Vol<Box<[T]>>;

    fn cube(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate) -> Cube {
        Cube::new(x, y, z)
    }

    #[test]
    fn debug_no_elements() {
        let vol = GridAab::from_lower_size([10, 0, 0], [4, 1, 1])
            .to_vol::<ZMaj>()
            .unwrap();
        assert_eq!(
            format!("{vol:#?}"),
            indoc::indoc! {"
                Vol<(), ZMaj> {
                    bounds: GridAab(
                        10..14 (4),
                        0..1 (1),
                        0..1 (1),
                    ),
                }\
            "}
        )
    }

    #[test]
    fn debug_with_contents() {
        let vol = VolBox::from_fn(GridAab::from_lower_size([10, 0, 0], [4, 1, 1]), |p| p.x);
        assert_eq!(
            format!("{vol:#?}"),
            indoc::indoc! {"
                Vol<alloc::boxed::Box<[i32]>, ZMaj> {
                    bounds: GridAab(
                        10..14 (4),
                        0..1 (1),
                        0..1 (1),
                    ),
                    contents: [
                        10,
                        11,
                        12,
                        13,
                    ],
                }\
            "}
        )
    }

    #[test]
    fn debug_without_contents() {
        let vol = VolBox::from_fn(GridAab::from_lower_size([0, 0, 0], [64, 1, 1]), |p| p.x);
        assert_eq!(
            format!("{vol:#?}"),
            indoc::indoc! {"
                Vol<alloc::boxed::Box<[i32]>, ZMaj> {
                    bounds: GridAab(
                        0..64 (64),
                        0..1 (1),
                        0..1 (1),
                    ),
                    contents: [...64 elements],
                }\
            "}
        )
    }

    #[test]
    fn tiny_is_sufficiently_tiny() {
        // Obtain the maximum without specifying what we think the type is.
        let maximum_size = num_traits::Bounded::max_value();

        let tiny_vol = Vol::tiny(point3(0, 0, 0), Size3D::splat(maximum_size), ZMaj);
        assert_eq!(tiny_vol.volume(), usize::from(maximum_size).pow(3));

        // Try the constructor with elements
        let tiny_vol_data = Vol::from_tiny_elements(
            point3(0, 0, 0),
            size3(2, 2, 2),
            ZMaj,
            &[0, 1, 2, 3, 4, 5, 6, 7],
        );
        assert_eq!(tiny_vol_data.volume(), 8);
        assert_eq!(tiny_vol_data[[1, 1, 0]], 6);
    }

    #[test]
    #[should_panic = "element count does not match AAB volume"]
    fn tiny_error() {
        _ = Vol::from_tiny_elements(point3(0, 0, 0), size3(2, 2, 2), ZMaj, &[3, 3, 3]);
    }

    #[test]
    fn from_elements() {
        let bounds = GridAab::from_lower_size([10, 0, 0], [4, 1, 1]);
        assert_eq!(
            VolBox::from_fn(bounds, |p| p.x),
            VolBox::from_elements(bounds, vec![10i32, 11, 12, 13]).unwrap(),
        );
    }

    #[test]
    fn from_elements_error() {
        let bounds = GridAab::from_lower_size([10, 0, 0], [4, 1, 1]);
        assert_eq!(
            VolBox::from_elements(bounds, vec![10i32, 11, 12]),
            Err(VolLengthError {
                input_length: Some(3),
                bounds,
            })
        );
    }

    #[test]
    fn repeat() {
        let bounds = GridAab::from_lower_size([10, 0, 0], [2, 2, 1]);
        assert_eq!(
            VolBox::repeat(bounds, 9),
            VolBox::from_elements(bounds, vec![9, 9, 9, 9]).unwrap(),
        );
    }

    #[test]
    fn from_element() {
        let element = String::from("x");
        assert_eq!(
            VolBox::from_element(element.clone()),
            VolBox::from_elements(GridAab::ORIGIN_CUBE, [element]).unwrap(),
        );
    }

    #[test]
    fn from_y_flipped() {
        let array = VolBox::from_y_flipped_array([
            [*b"abcd", *b"efgh", *b"ijkl"],
            [*b"mnop", *b"qrst", *b"uvwx"],
        ]);
        assert_eq!(
            array,
            Vol::from_elements(
                GridAab::from_lower_size([0, 0, 0], [4, 3, 2]),
                *b"iueqamjvfrbnkwgscolxhtdp"
            )
            .unwrap()
        );
    }

    #[test]
    fn index_overflow_low() {
        // Indexing calculates (point - lower_bounds), so this would overflow in the negative direction if the overflow weren't checked.
        // Note that MAX - 1 is the highest allowed lower bound since the exclusive upper bound must be representable.
        let low = GridAab::from_lower_size([GridCoordinate::MAX - 1, 0, 0], [1, 1, 1])
            .to_vol::<ZMaj>()
            .unwrap();
        assert_eq!(low.index(cube(0, 0, 0)), None);
        assert_eq!(low.index(cube(-1, 0, 0)), None);
        assert_eq!(low.index(cube(-2, 0, 0)), None);
        assert_eq!(low.index(cube(GridCoordinate::MIN, 0, 0)), None);
        // But, an actually in-bounds cube should still work.
        assert_eq!(low.index(cube(GridCoordinate::MAX - 1, 0, 0)), Some(0));
    }

    #[test]
    fn index_overflow_high() {
        let high = GridAab::from_lower_size([GridCoordinate::MAX - 1, 0, 0], [1, 1, 1])
            .to_vol::<ZMaj>()
            .unwrap();
        assert_eq!(high.index(cube(0, 0, 0)), None);
        assert_eq!(high.index(cube(1, 0, 0)), None);
        assert_eq!(high.index(cube(2, 0, 0)), None);
        assert_eq!(high.index(cube(GridCoordinate::MAX - 1, 0, 0)), Some(0));
    }

    #[test]
    fn index_not_overflow_large_volume() {
        let vol = GridAab::from_lower_size([0, 0, 0], [2000, 2000, 2000])
            .to_vol::<ZMaj>()
            .unwrap();
        // This value fits in a 32-bit `usize` and is therefore a valid index,
        // but it does not fit in a `GridCoordinate` = `i32`.
        assert_eq!(
            vol.index(cube(1500, 1500, 1500)),
            Some(((1500 * 2000) + 1500) * 2000 + 1500)
        );
    }

    /// Test the properties of the `subdivide()` operations, starting from this example.
    #[inline(never)]
    fn check_subdivide_case(vol: Vol<&mut [Cube]>) {
        fn filter(_: [Vol<()>; 2]) -> bool {
            true
        }

        eprintln!("Checking {:?}", vol.bounds());

        // Check the elements are as expected
        for (cube, &value) in vol.iter() {
            assert_eq!(cube, value);
        }

        if vol.volume() < 2 {
            // Never subdivide a cube or empty.
            assert!(vol.without_elements().subdivide().is_err()); // () version
            assert!(vol.as_ref().subdivide().is_err()); // & version
            assert!(vol.subdivide(filter).is_err()); // &mut version
        } else {
            let Ok([a, b]) = vol.without_elements().subdivide() else {
                panic!("{vol:?} failed to subdivide");
            };
            assert_ne!(a.volume(), 0);
            assert_ne!(b.volume(), 0);

            // Compare immutable slice subdivide
            let [aref, bref] = vol.as_ref().subdivide().unwrap();
            assert_eq!((a, b), (aref.without_elements(), bref.without_elements()));

            // Compare mutable slice subdivide
            let [amut, bmut] = vol.subdivide(filter).unwrap();
            assert_eq!((a, b), (amut.without_elements(), bmut.without_elements()));

            // Recurse
            check_subdivide_case(amut);
            check_subdivide_case(bmut);
        }
    }
    fn check_subdivide(bounds: GridAab) {
        check_subdivide_case(Vol::<Box<[Cube]>>::from_fn(bounds, std::convert::identity).as_mut());
    }

    #[test]
    fn subdivide_test() {
        check_subdivide(GridAab::ORIGIN_CUBE);
        check_subdivide(GridAab::ORIGIN_EMPTY);
        check_subdivide(GridAab::from_lower_upper([0, 0, 0], [2, 4, 5]));
    }

    #[cfg(feature = "arbitrary")]
    #[test]
    fn arbitrary_bounds_size_hint() {
        use arbitrary::{Arbitrary, Unstructured};
        let hint = vol_arb::ARBITRARY_BOUNDS_SIZE_HINT;
        let most_bytes_used = (0..=255)
            .map(|byte| {
                // TODO: sketchy coverage; would be better to generate some random/hashed data
                let data = [byte; 1000];
                let mut u = Unstructured::new(&data);
                GridAab::arbitrary(&mut u).unwrap();
                let bytes_used = 1000 - u.len();
                assert!(
                    bytes_used >= hint.0,
                    "used {}, less than {}",
                    bytes_used,
                    hint.0
                );
                bytes_used
            })
            .max();
        assert_eq!(most_bytes_used, hint.1);

        // TODO: Also look at the resulting Grids and see if they're good coverage.
    }

    #[cfg(feature = "arbitrary")]
    #[test]
    fn arbitrary_bounds_volume() {
        use arbitrary::Unstructured;
        use itertools::Itertools as _;
        let max_volume = 100;
        let minmax = (0..=255)
            .map(|byte| {
                // TODO: sketchy coverage; would be better to generate some random/hashed data
                let data = [byte; 25];
                let mut u = Unstructured::new(&data);
                Vol::<()>::arbitrary_with_max_volume(&mut u, max_volume)
                    .unwrap()
                    .volume()
            })
            .minmax()
            .into_option();
        assert_eq!(minmax, Some((0, max_volume)));
    }
}

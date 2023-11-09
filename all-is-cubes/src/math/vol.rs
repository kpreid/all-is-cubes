use alloc::boxed::Box;
use alloc::sync::Arc;
use core::fmt;
use core::ops::{Deref, DerefMut};

#[cfg(doc)]
use alloc::vec::Vec;

use crate::math::{Cube, GridAab, GridCoordinate, GridIter, GridVector};

// #[derive(Clone, Copy, Debug)]
// pub struct XMaj;

/// Z-major ordering: linearly adjacent elements have adjacent Z coordinates.
///
/// `[0, 0, 0], [0, 0, 1], [0, 0, 2], ..., [0, 1, 0], [0, 1, 1], ...`
///
/// Use this type with [`Vol`] to store volume data in this order.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct ZMaj;

/// A 3-dimensional array with arbitrary element type instead of [`Space`](crate::space::Space)'s
/// fixed types.
// ---
// TOOD: deprecate/replace this
pub type GridArray<V> = Vol<Box<[V]>, ZMaj>;

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
#[derive(Clone, Debug, Eq, Hash, PartialEq)] // TODO: nondefault Debug
pub struct Vol<C, O = ZMaj> {
    /// Invariant: `bounds` has a volume that is at most [`usize::MAX`].
    bounds: GridAab,
    ordering: O,
    /// Invariant: `contents.deref().len()`, if it exists, equals `bounds.volume()`.
    contents: C,
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
    pub fn from_elements(bounds: GridAab, elements: impl Into<C>) -> Result<Self, VolLengthError> {
        let elements = elements.into();
        if elements.len() == bounds.volume() {
            Ok(Vol {
                bounds,
                ordering: O::default(),
                contents: elements,
            })
        } else {
            Err(VolLengthError {
                input_length: elements.len(),
                bounds,
            })
        }
    }
}

/// Constructors from elements.
impl<C, O: Default, V> Vol<C, O>
where
    // Note that the Deref bound is necessary to give this a unique `V`.
    C: Deref<Target = [V]> + FromIterator<V>,
{
    /// Constructs a `Vol<C>` by using the provided function to compute a value
    /// for each point.
    pub fn from_fn<F>(bounds: GridAab, f: F) -> Self
    where
        F: FnMut(Cube) -> V,
    {
        Vol {
            bounds,
            ordering: O::default(),
            contents: bounds.interior_iter().map(f).collect(),
        }
    }

    /// Constructs a `Vol<C>` by cloning the provided value for each point.
    ///
    /// TODO: This feels like it should be called 'filled' or 'cloned', but if so,
    /// maybe [`FaceMap::repeat`](crate::math::FaceMap::repeat) should also change?
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
    pub fn from_element(value: V) -> Self {
        Self::from_elements(GridAab::ORIGIN_CUBE, core::iter::once(value).collect::<C>()).unwrap()
    }

    /// Constructs a [`Vol<Box<[V]>>`] from nested Rust arrays in [Z][Y][X] order with the Y axis
    /// mirrored. The result's bounds's lower bounds are zero.
    ///
    /// Note: The current implementation requires that `V` implement [`Clone`], and will
    /// clone each element once, but this may be improved in the future.
    // TODO: Decide if this is a good public interface.
    // TODO: Reimplement this in terms of adopting the elements as a linear array.
    // TODO: Test.
    #[doc(hidden)] // used by all-is-cubes-content
    pub fn from_y_flipped_array<const DX: usize, const DY: usize, const DZ: usize>(
        array: [[[V; DX]; DY]; DZ],
    ) -> Self
    where
        V: Clone,
    {
        Self::from_fn(
            GridAab::from_lower_size(
                [0, 0, 0],
                [
                    DX as GridCoordinate,
                    DY as GridCoordinate,
                    DZ as GridCoordinate,
                ],
            ),
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

    /// Returns the linear contents without copying.
    pub(crate) fn into_elements(self) -> C {
        self.contents
    }

    /// Translates the volume without affecting its contents.
    ///
    /// Panics if this would cause numeric overflow.
    ///
    /// TODO: example
    #[must_use]
    #[track_caller]
    pub fn translate(self, offset: impl Into<GridVector>) -> Self {
        self.translate_impl(offset.into())
    }

    #[track_caller]
    fn translate_impl(mut self, offset: GridVector) -> Self {
        let new_bounds = self.bounds.translate(offset);
        if new_bounds.size() != self.bounds.size() {
            // We can't just continue like `GridAab::translate` does, because that would
            // break the invariant that self.bounds.volume() == self.contents.len().
            panic!("Vol::translate() offset caused numeric overflow");
        }
        self.bounds = new_bounds;
        self
    }
}

impl<C> Vol<C, ZMaj> {
    /// Iterate over all cubes that this contains, in the order of the linearization,
    /// without including the stored data (if there is any).
    pub fn iter_cubes(&self) -> GridIter {
        self.bounds.interior_iter()
    }
}

/// Linear data access.
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
        debug_assert_eq!(s.len(), self.bounds.volume());
        s
    }

    /// Returns the linear contents viewed as a mutable slice.
    pub fn as_linear_mut(&mut self) -> &mut [V]
    where
        C: DerefMut,
    {
        let s = &mut *self.contents;
        debug_assert_eq!(s.len(), self.bounds.volume());
        s
    }
}

impl<V: Clone, O> Vol<Arc<[V]>, O> {
    /// Returns the linear contents viewed as a mutable slice, as if by [`Arc::make_mut()`].
    pub(crate) fn make_linear_mut(&mut self) -> &mut [V] {
        let slice: &mut [V] = arc_make_mut_slice(&mut self.contents);
        debug_assert_eq!(slice.len(), self.bounds.volume());
        slice
    }
}

/// Element lookup operations by 3D coordinates.
impl<C, V> Vol<C, ZMaj>
where
    C: Deref<Target = [V]>,
{
    /// Returns the element at `position` of this volume data, or [`None`] if `position` is out
    /// of bounds.
    #[inline]
    pub fn get(&self, position: impl Into<Cube>) -> Option<&V> {
        let index = self.bounds.index(position.into())?;
        Some(&self.as_linear()[index])
    }

    /// Returns a mutable reference to the element at `position` of this volume data,
    /// or [`None`] if `position` is out of bounds.
    #[inline]
    pub fn get_mut(&mut self, position: impl Into<Cube>) -> Option<&mut V>
    where
        C: DerefMut,
    {
        let index = self.bounds.index(position.into())?;
        Some(&mut self.as_linear_mut()[index])
    }

    /// Iterates over all the cubes and values in this volume data, in the ordering specified
    /// by the `O` type parameter.
    pub fn iter<'s>(&'s self) -> impl Iterator<Item = (Cube, &V)>
    where
        V: 's,
    {
        self.bounds.interior_iter().zip(self.as_linear().iter())
    }

    /// Iterates by mutable reference over all the cubes and values in this volume data,
    /// in the ordering specified by the `O` type parameter.
    pub fn iter_mut<'s>(&'s mut self) -> impl Iterator<Item = (Cube, &mut V)>
    where
        C: DerefMut,
        V: 's,
    {
        self.bounds
            .interior_iter()
            .zip(self.as_linear_mut().iter_mut())
    }
}

impl<V, O> Vol<Box<[V]>, O> {
    /// Apply `f` to each element and collect the results into the same shape and ordering.
    pub fn map<T, F>(self, f: F) -> Vol<Box<[T]>, O>
    where
        F: FnMut(V) -> T,
    {
        Vol {
            bounds: self.bounds,
            ordering: self.ordering,
            contents: self.contents.into_vec().into_iter().map(f).collect(),
        }
    }
}

impl<P, C, O, V> core::ops::Index<P> for Vol<C, O>
where
    P: Into<Cube>,
    C: Deref<Target = [V]>,
    O: Copy,
{
    type Output = V;

    /// Returns the element at `position` of this volume data,
    /// or panics if `position` is out of bounds.
    ///
    /// Use [`Vol::get()`] for a non-panicing alternative.
    #[inline(always)] // measured faster on wasm32 in worldgen
    fn index(&self, position: P) -> &Self::Output {
        let position: Cube = position.into();
        if let Some(index) = self.bounds.index(position) {
            &self.contents[index]
        } else {
            panic!(
                "position {:?} out of Vol bounds {:?}",
                position, self.bounds
            )
        }
    }
}
impl<P, C, O, V> core::ops::IndexMut<P> for Vol<C, O>
where
    P: Into<Cube>,
    C: DerefMut<Target = [V]>,
    O: Copy,
{
    /// Returns the element at `position` of this volume data,
    /// or panics if `position` is out of bounds.
    #[inline(always)]
    fn index_mut(&mut self, position: P) -> &mut Self::Output {
        let position: Cube = position.into();
        if let Some(index) = self.bounds.index(position) {
            &mut self.contents[index]
        } else {
            panic!(
                "position {:?} out of Vol bounds {:?}",
                position, self.bounds
            )
        }
    }
}

#[cfg(feature = "arbitrary")]
mod vol_arb {
    use super::*;
    use arbitrary::Arbitrary;

    /// Let's not spend too much memory on generating arbitrary length vectors.
    /// This does reduce coverage...
    const MAX_VOLUME: usize = 2_usize.pow(16);

    impl<'a, V: Arbitrary<'a>, C> Arbitrary<'a> for Vol<C, ZMaj>
    where
        C: FromIterator<V> + Deref<Target = [V]>,
    {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            let bounds = GridAab::arbitrary_with_max_volume(u, MAX_VOLUME)?;
            let contents: C = u
                .arbitrary_iter()?
                .take(bounds.volume())
                .collect::<Result<C, _>>()?;
            Vol::from_elements(bounds, contents).map_err(|_| arbitrary::Error::NotEnoughData)
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            arbitrary::size_hint::recursion_guard(depth, |depth| {
                let (lower, upper) = V::size_hint(depth);
                (
                    lower.saturating_mul(MAX_VOLUME),
                    upper.map(|u| u.saturating_mul(MAX_VOLUME)),
                )
            })
        }
    }
}

/// Error from [`Vol::from_elements()`] being given the wrong length.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct VolLengthError {
    input_length: usize,
    bounds: GridAab,
}

#[cfg(feature = "std")]
impl std::error::Error for VolLengthError {}

impl fmt::Display for VolLengthError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            input_length,
            bounds,
        } = self;
        write!(
            f,
            "data of length {input_length} cannot fill volume {v} of {bounds:?}",
            v = self.bounds.volume()
        )
    }
}

/// As [`Arc::make_mut()`], but for slices, `Arc<[_]>`.
fn arc_make_mut_slice<T: Clone>(mut arc: &mut Arc<[T]>) -> &mut [T] {
    // Use `get_mut()` to emulate `make_mut()`.
    // And since this is a "maybe return a mutable borrow" pattern, we have to appease
    // the borrow checker about it, hence `polonius_the_crab` getting involved.
    polonius_the_crab::polonius!(|arc| -> &'polonius mut [T] {
        if let Some(slice) = Arc::get_mut(arc) {
            polonius_the_crab::polonius_return!(slice);
        }
    });
    *arc = Arc::from_iter(arc.iter().cloned());
    Arc::get_mut(arc).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::String;

    type VolBox<T> = Vol<Box<[T]>>;

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
                input_length: 3,
                bounds
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
}

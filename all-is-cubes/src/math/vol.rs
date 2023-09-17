use alloc::boxed::Box;
use core::fmt;

use crate::math::{Cube, GridAab, GridCoordinate, GridVector};

/// A 3-dimensional array with arbitrary element type instead of [`Space`](crate::space::Space)'s
/// fixed types.
///
/// TODO: Should we rebuild Space on top of this?
#[derive(Clone, Debug, Eq, Hash, PartialEq)] // TODO: nondefault Debug
pub struct GridArray<V> {
    bounds: GridAab,
    contents: Box<[V]>,
}

impl<V> GridArray<V> {
    /// Constructs a [`GridArray`] by using the provided function to compute a value
    /// for each point.
    pub fn from_fn<F>(bounds: GridAab, f: F) -> Self
    where
        F: FnMut(Cube) -> V,
    {
        GridArray {
            bounds,
            contents: bounds.interior_iter().map(f).collect(),
        }
    }

    /// Constructs a [`GridArray`] by cloning the provided value for each point.
    ///
    /// TODO: This feels like it should be called 'filled' or 'cloned', but if so,
    /// maybe [`FaceMap::repeat`](crate::math::FaceMap::repeat) should also change?
    pub fn repeat(bounds: GridAab, value: V) -> Self
    where
        V: Clone,
    {
        Self::from_fn(bounds, |_| value.clone())
    }

    /// Constructs a [`GridArray`] with a single value, in bounds `ORIGIN_CUBE`.
    ///
    /// If the single element should be at a different location, you can call
    /// [`.translate(offset)`](Self::translate), or use [`GridArray::from_elements()`]
    /// instead.
    pub fn from_element(value: V) -> Self {
        Self::from_elements(GridAab::ORIGIN_CUBE, [value]).unwrap()
    }

    /// Constructs a [`GridArray`] containing the provided elements, which must be in the
    /// ordering used by [`GridAab::interior_iter()`].
    ///
    /// Returns an [`ArrayLengthError`] if the number of elements does not match
    /// [`bounds.volume()`](GridAab::volume).
    pub fn from_elements(
        bounds: GridAab,
        elements: impl Into<Box<[V]>>,
    ) -> Result<Self, ArrayLengthError> {
        let elements = elements.into();
        if elements.len() == bounds.volume() {
            Ok(GridArray {
                bounds,
                contents: elements,
            })
        } else {
            Err(ArrayLengthError {
                input_length: elements.len(),
                bounds,
            })
        }
    }

    /// Constructs a [`GridArray`] from nested Rust arrays in [Z][Y[X] order with the Y axis
    /// mirrored. The result's bounds's lower bounds are zero.
    ///
    /// Note: The current implementation requires that `V` implement [`Clone`], and will
    /// clone each element once, but this may be improved in the future.
    // TODO: Decide if this is a good public interface.
    // TODO: Reimplement this in terms of adopting the elements as a linear array, then performing an axis swap.
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

    /// Returns the [`GridAab`] specifying the bounds of this array.
    #[inline]
    pub fn bounds(&self) -> GridAab {
        self.bounds
    }

    /// Returns the element at `position` of this array, or [`None`] if `position` is out
    /// of bounds.
    #[inline]
    pub fn get(&self, position: impl Into<Cube>) -> Option<&V> {
        self.bounds
            .index(position.into())
            .map(|index| &self.contents[index])
    }

    /// Returns a mutable reference to the element at `position` of this array,
    /// or [`None`] if `position` is out of bounds.
    #[inline]
    pub fn get_mut(&mut self, position: impl Into<Cube>) -> Option<&mut V> {
        self.bounds
            .index(position.into())
            .map(|index| &mut self.contents[index])
    }

    /// Adds to the origin of the array without affecting the contents.
    ///
    /// Panics if this would cause numeric overflow.
    ///
    /// TODO: example
    #[must_use]
    #[track_caller]
    pub fn translate(mut self, offset: impl Into<GridVector>) -> Self {
        let new_bounds = self.bounds.translate(offset);
        if new_bounds.size() != self.bounds.size() {
            // We can't just continue like `GridAab::translate` does, because that would
            // break the invariant that self.bounds.volume() == self.contents.len().
            panic!("GridArray::translate() offset caused numeric overflow");
        }
        self.bounds = new_bounds;
        self
    }

    /// Iterates over all the cubes and values in this array, in the ordering used by
    /// [`GridAab::interior_iter()`].
    pub fn iter(&self) -> impl Iterator<Item = (Cube, &V)> {
        self.bounds.interior_iter().zip(self.contents.iter())
    }

    /// Iterates over all the cubes and values in this array, in the ordering used by
    /// [`GridAab::interior_iter()`].
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Cube, &mut V)> {
        self.bounds.interior_iter().zip(self.contents.iter_mut())
    }

    /// Returns mutable access to the contents. They are ordered in the same order that
    /// [`GridArray::from_elements()`] expects.
    pub(crate) fn elements_mut(&mut self) -> &mut [V] {
        // Note that since we only return a reference to the _slice_, providing this access
        // cannot break the length invariant.
        &mut self.contents
    }

    /// Apply `f` to each element of the array, producing a new array of the results.
    pub fn map<T, F>(self, f: F) -> GridArray<T>
    where
        F: FnMut(V) -> T,
    {
        GridArray {
            bounds: self.bounds,
            contents: self.contents.into_vec().into_iter().map(f).collect(),
        }
    }

    /// Returns the contents without copying. They are ordered in the same order that
    /// [`GridArray::from_elements()`] expects.
    pub(crate) fn into_elements(self) -> Box<[V]> {
        self.contents
    }
}

impl<P: Into<Cube>, V> core::ops::Index<P> for GridArray<V> {
    type Output = V;

    /// Returns the element at `position` of this array, or panics if `position` is out of
    /// bounds.
    ///
    /// Use [`GridArray::get`] for a non-panicing alternative.
    #[inline(always)] // measured faster on wasm32 in worldgen
    fn index(&self, position: P) -> &Self::Output {
        let position: Cube = position.into();
        if let Some(index) = self.bounds.index(position) {
            &self.contents[index]
        } else {
            panic!(
                "GridArray position out of range {:?} in {:?}",
                position, self.bounds
            )
        }
    }
}
impl<P: Into<Cube>, V> core::ops::IndexMut<P> for GridArray<V> {
    /// Returns the element at `position` of this array, or panics if `position` is out of
    /// bounds.
    #[inline(always)]
    fn index_mut(&mut self, position: P) -> &mut Self::Output {
        let position: Cube = position.into();
        if let Some(index) = self.bounds.index(position) {
            &mut self.contents[index]
        } else {
            panic!(
                "GridArray position out of range {:?} in {:?}",
                position, self.bounds
            )
        }
    }
}

#[cfg(feature = "arbitrary")]
mod grid_array_arb {
    use super::*;
    use arbitrary::Arbitrary;

    /// Let's not spend too much memory on generating arbitrary length arrays.
    /// This does reduce coverage...
    const MAX_VOLUME: usize = 2_usize.pow(16);

    impl<'a, V: Arbitrary<'a>> Arbitrary<'a> for GridArray<V> {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            let bounds = GridAab::arbitrary_with_max_volume(u, MAX_VOLUME)?;
            let contents: Box<[V]> = u
                .arbitrary_iter()?
                .take(bounds.volume())
                .collect::<Result<Box<[V]>, _>>()?;
            GridArray::from_elements(bounds, contents).map_err(|_| arbitrary::Error::NotEnoughData)
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

/// Error from [`GridArray::from_elements`] being given the wrong length.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ArrayLengthError {
    input_length: usize,
    bounds: GridAab,
}

#[cfg(feature = "std")]
impl std::error::Error for ArrayLengthError {}

impl fmt::Display for ArrayLengthError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            input_length,
            bounds,
        } = self;
        write!(
            f,
            "array of length {input_length} cannot fill volume {v} of {bounds:?}",
            v = self.bounds.volume()
        )
    }
}

#[cfg(test)]
mod tests {
    use alloc::string::String;

    use super::*;

    #[test]
    fn array_from_elements() {
        let bounds = GridAab::from_lower_size([10, 0, 0], [4, 1, 1]);
        assert_eq!(
            GridArray::from_fn(bounds, |p| p.x),
            GridArray::from_elements(bounds, vec![10i32, 11, 12, 13]).unwrap(),
        );
    }

    #[test]
    fn array_from_elements_error() {
        let bounds = GridAab::from_lower_size([10, 0, 0], [4, 1, 1]);
        assert_eq!(
            GridArray::from_elements(bounds, vec![10i32, 11, 12]),
            Err(ArrayLengthError {
                input_length: 3,
                bounds
            })
        );
    }

    #[test]
    fn array_repeat() {
        let bounds = GridAab::from_lower_size([10, 0, 0], [2, 2, 1]);
        assert_eq!(
            GridArray::repeat(bounds, 9),
            GridArray::from_elements(bounds, vec![9, 9, 9, 9]).unwrap(),
        );
    }

    #[test]
    fn array_from_element() {
        let element = String::from("x");
        assert_eq!(
            GridArray::from_element(element.clone()),
            GridArray::from_elements(GridAab::ORIGIN_CUBE, [element]).unwrap(),
        );
    }

    #[test]
    fn array_from_y_flipped() {
        let array = GridArray::from_y_flipped_array([
            [*b"abcd", *b"efgh", *b"ijkl"],
            [*b"mnop", *b"qrst", *b"uvwx"],
        ]);
        assert_eq!(
            array,
            GridArray::from_elements(
                GridAab::from_lower_size([0, 0, 0], [4, 3, 2]),
                *b"iueqamjvfrbnkwgscolxhtdp"
            )
            .unwrap()
        );
    }
}

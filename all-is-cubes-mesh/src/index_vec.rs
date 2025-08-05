use alloc::collections::VecDeque;
use alloc::vec::Vec;
use core::{mem, ops};

use either::Either;

// -------------------------------------------------------------------------------------------------

/// Data storage for meshes’ index lists, automatically choosing an element type which is
/// large enough for the range of the index values.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum IndexVec {
    /// 16-bit indices.
    U16(Vec<u16>),
    /// 32-bit indices.
    U32(Vec<u32>),
}

/// Temporary storage for building an index list.
///
/// This is identical to [`IndexVec`] except that it allows extending from either end, which can be
/// used to control overall drawing order.
/// It should be constructed by conversion from [`IndexVec`].
///
/// (Note that in practice, the “either end” part, and thus the [`VecDeque`], is not needed per se;
/// an equally valid implementation would be a gap buffer, where we have two fixed ends and insert
/// on either side of a gap in the middle. These are identical except for the choice of ordering
/// of the two halves of the storage.)
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum IndexVecDeque {
    /// 16-bit indices.
    U16(VecDeque<u16>),
    /// 32-bit indices.
    U32(VecDeque<u32>),
}

/// Data for meshes’ index lists, which may use either 16 or 32-bit values.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[expect(clippy::exhaustive_enums)]
pub enum IndexSlice<'a> {
    /// 16-bit indices.
    U16(&'a [u16]),
    /// 32-bit indices.
    U32(&'a [u32]),
}

pub(crate) enum IndexSliceMut<'a> {
    /// 16-bit indices.
    U16(&'a mut [u16]),
    /// 32-bit indices.
    U32(&'a mut [u32]),
}

// -------------------------------------------------------------------------------------------------

impl IndexVec {
    /// Creates an empty [`IndexVec`].
    ///
    /// This operation does not allocate heap memory and is generally similar to [`Vec::new()`].
    #[inline]
    pub const fn new() -> Self {
        Self::U16(Vec::new())
    }

    /// Creates an empty [`IndexVec`] with the given capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        // Predict whether we will need 32-bit indices. Generally, indices will be of quads,
        // meaning that we have 6 indices for 4 vertices. Therefore, if capacity * 4/6
        // is greater than u16::MAX, some of the index *values* will be greater than u16::MAX.
        // TODO: Ask callers for this info directly?
        const THRESHOLD: usize = u16::MAX as usize / 2 * 3;
        if capacity > THRESHOLD {
            Self::U32(Vec::with_capacity(capacity))
        } else {
            Self::U16(Vec::with_capacity(capacity))
        }
    }

    /// Returns a slice-reference-like handle to the contents of this [`IndexVec`].
    /// Use this for all read operations.
    ///
    /// Panics if the given range exceeds the length of `self`.
    #[inline]
    pub fn as_slice<R>(&self, range: R) -> IndexSlice<'_>
    where
        [u16]: ops::Index<R, Output = [u16]>,
        [u32]: ops::Index<R, Output = [u32]>,
    {
        match self {
            Self::U16(vec) => IndexSlice::U16(&vec.as_slice()[range]),
            Self::U32(vec) => IndexSlice::U32(&vec.as_slice()[range]),
        }
    }

    #[inline]
    pub(crate) fn as_mut_slice<R>(&mut self, range: R) -> IndexSliceMut<'_>
    where
        [u16]: ops::IndexMut<R, Output = [u16]>,
        [u32]: ops::IndexMut<R, Output = [u32]>,
    {
        match self {
            Self::U16(vec) => IndexSliceMut::U16(&mut vec.as_mut_slice()[range]),
            Self::U32(vec) => IndexSliceMut::U32(&mut vec.as_mut_slice()[range]),
        }
    }

    /// As per [`Vec::len()`].
    #[inline]
    pub fn len(&self) -> usize {
        self.as_slice(..).len()
    }

    #[cfg_attr(not(feature = "dynamic"), allow(dead_code))]
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.as_slice(..).is_empty()
    }

    /// Resets the vector to have zero length and the same capacity.
    /// Note that this preserves the previous index type.
    #[inline]
    pub fn clear(&mut self) {
        match self {
            Self::U16(vec) => vec.clear(),
            Self::U32(vec) => vec.clear(),
        }
    }

    #[inline]
    pub(crate) fn capacity_bytes(&self) -> usize {
        match self {
            Self::U16(vec) => vec.capacity() * 2,
            Self::U32(vec) => vec.capacity() * 4,
        }
    }

    /// As per [`Vec::reserve_exact()`].
    #[inline]
    pub fn reserve_exact(&mut self, additional: usize) {
        match self {
            Self::U16(vec) => vec.reserve_exact(additional),
            Self::U32(vec) => vec.reserve_exact(additional),
        }
    }

    /// Appends `source` to `self`, with `offset` added to each element.
    #[inline(always)]
    pub(crate) fn extend_with_offset(&mut self, source: IndexSlice<'_>, offset: u32) {
        // TODO: see if we can improve this by specializing to individual. u16/u32 cases.
        // If we track the maximum value in self and source, then we can also predict whether
        // a change to u32 will be necessary or not.
        self.extend(source.iter_u32().map(|i| i + offset));
    }

    /// Convert to `Vec<u32>`, reallocating if needed.
    pub(crate) fn into_u32s(self) -> Vec<u32> {
        match self {
            IndexVec::U32(vec) => vec,
            IndexVec::U16(vec) => vec.into_iter().map(u32::from).collect(),
        }
    }
}

impl IndexVecDeque {
    /// Appends `source` to `self`, with `offset` added to each element.
    ///
    /// If `on_front` is true, then the indices are inserted in the “front” of `self`,
    /// and hence will be drawn before existing indices.
    /// The internal order of the indices in [`source`] is not reversed.
    #[inline(always)]
    pub(crate) fn extend_with_offset(
        &mut self,
        source: IndexSlice<'_>,
        offset: u32,
        on_front: bool,
    ) {
        // TODO: see if we can improve this by specializing to individual. u16/u32 cases.
        // If we track the maximum value in self and source, then we can also predict whether
        // a change to u32 will be necessary or not.
        if on_front {
            self.extend_front(source.iter_u32().map(|i| i + offset));
        } else {
            self.extend(source.iter_u32().map(|i| i + offset));
        }
    }
}

impl<'a> IndexSlice<'a> {
    /// Returns the number of indices in this slice.
    #[inline]
    pub fn len(&self) -> usize {
        match self {
            IndexSlice::U16(slice) => slice.len(),
            IndexSlice::U32(slice) => slice.len(),
        }
    }

    /// Returns the index data interpreted as bytes in **native endianness**.
    #[inline]
    pub fn as_bytes(&self) -> &'a [u8] {
        match self {
            IndexSlice::U16(slice) => bytemuck::must_cast_slice::<u16, u8>(slice),
            IndexSlice::U32(slice) => bytemuck::must_cast_slice::<u32, u8>(slice),
        }
    }

    /// Returns whether this slice is empty (`len() == 0`).
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the indices in this slice, each converted unconditionally to [`u32`].
    #[inline]
    pub fn iter_u32(&self) -> impl DoubleEndedIterator<Item = u32> + '_ {
        match self {
            IndexSlice::U16(slice) => Either::Left(slice.iter().copied().map(u32::from)),
            IndexSlice::U32(slice) => Either::Right(slice.iter().copied()),
        }
    }
}

impl IndexSliceMut<'_> {
    #[inline]
    pub fn len(&self) -> usize {
        match self {
            IndexSliceMut::U16(slice) => slice.len(),
            IndexSliceMut::U32(slice) => slice.len(),
        }
    }
}

// -------------------------------------------------------------------------------------------------

impl Default for IndexVec {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Extend<u16> for IndexVec {
    #[inline]
    fn extend<T: IntoIterator<Item = u16>>(&mut self, iter: T) {
        match self {
            IndexVec::U16(vec) => vec.extend(iter),
            IndexVec::U32(vec) => vec.extend(iter.into_iter().map(u32::from)),
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Implementations of extending `IndexVec` and `IndexVecDeque`.

impl Extend<u32> for IndexVec {
    // This inline attribute validated by somewhat dubious benchmark.
    #[inline(always)]
    fn extend<T: IntoIterator<Item = u32>>(&mut self, iter: T) {
        match self {
            IndexVec::U16(u16_vec) => {
                // In this case, we have u32s which we want to fit into a vector of u16s,
                // if possible.
                let mut iter = iter.into_iter();

                let non_fitting_element: u32;
                'try_to_use_u16: {
                    for big_index in iter.by_ref() {
                        match u16::try_from(big_index) {
                            Ok(small_index) => u16_vec.push(small_index),
                            Err(_) => {
                                // Save the element that failed to fit,
                                // so that we can put it in the u32 vector.
                                non_fitting_element = big_index;
                                break 'try_to_use_u16;
                            }
                        }
                    }
                    return; // succeeded at fitting all u32s in u16s
                }

                *self = upgrade_to_u32(u16_vec, non_fitting_element, iter);

                #[cold]
                #[inline(never)]
                fn upgrade_to_u32(
                    u16_vec: &mut Vec<u16>,
                    non_fitting_element: u32,
                    iter: impl Iterator<Item = u32>,
                ) -> IndexVec {
                    // Construct new u32 vector using
                    // * the existing items
                    // * the new item triggering the conversion
                    // * the remaining new items
                    IndexVec::U32(Vec::from_iter(
                        mem::take(u16_vec)
                            .into_iter()
                            .map(u32::from)
                            .chain([non_fitting_element])
                            .chain(iter),
                    ))
                }
            }
            IndexVec::U32(vec) => vec.extend(iter),
        }
    }
}

impl Extend<u32> for IndexVecDeque {
    // This inline attribute validated by somewhat dubious benchmark.
    #[inline(always)]
    fn extend<T: IntoIterator<Item = u32>>(&mut self, iter: T) {
        match self {
            IndexVecDeque::U16(u16_vec) => {
                // In this case, we have u32s which we want to fit into a vector of u16s,
                // if possible.
                let mut iter = iter.into_iter();

                let non_fitting_element: u32;
                'try_to_use_u16: {
                    for big_index in iter.by_ref() {
                        match u16::try_from(big_index) {
                            Ok(small_index) => u16_vec.push_back(small_index),
                            Err(_) => {
                                // Save the element that failed to fit,
                                // so that we can put it in the u32 vector.
                                non_fitting_element = big_index;
                                break 'try_to_use_u16;
                            }
                        }
                    }
                    return; // succeeded at fitting all u32s in u16s
                }

                *self = upgrade_to_u32(u16_vec, non_fitting_element, iter);

                #[cold]
                #[inline(never)]
                fn upgrade_to_u32(
                    u16_vec: &mut VecDeque<u16>,
                    non_fitting_element: u32,
                    iter: impl Iterator<Item = u32>,
                ) -> IndexVecDeque {
                    // Construct new u32 vector using
                    // * the existing items
                    // * the new item triggering the conversion
                    // * the remaining new items
                    IndexVecDeque::U32(VecDeque::from_iter(
                        mem::take(u16_vec)
                            .into_iter()
                            .map(u32::from)
                            .chain([non_fitting_element])
                            .chain(iter),
                    ))
                }
            }
            IndexVecDeque::U32(vecdeque) => vecdeque.extend(iter),
        }
    }
}

impl IndexVecDeque {
    /// Identical to [`Extend::extend()`] except that the new indices are positioned before the
    /// existing indices instead of after.
    #[inline(always)]
    fn extend_front<T: DoubleEndedIterator<Item = u32>>(&mut self, iter: T) {
        let mut iter_rev = iter.into_iter().rev();
        match self {
            IndexVecDeque::U16(u16_vec) => {
                let non_fitting_element: u32;
                'try_to_use_u16: {
                    for big_index in iter_rev.by_ref() {
                        match u16::try_from(big_index) {
                            Ok(small_index) => u16_vec.push_front(small_index),
                            Err(_) => {
                                // Save the element that failed to fit,
                                // so that we can put it in the u32 vector.
                                non_fitting_element = big_index;
                                break 'try_to_use_u16;
                            }
                        }
                    }
                    return; // succeeded at fitting all u32s in u16s
                }

                #[cold]
                #[inline(never)]
                fn upgrade_to_u32(
                    u16_vec: &mut VecDeque<u16>,
                    non_fitting_element: u32,
                    iter_rev: impl DoubleEndedIterator<Item = u32>,
                ) -> IndexVecDeque {
                    // Construct new VecDeque<u32> using
                    // * the remaining new items
                    // * the new item triggering the conversion
                    // * the existing items
                    IndexVecDeque::U32(VecDeque::from_iter(
                        iter_rev
                            .rev()
                            .chain([non_fitting_element])
                            .chain(mem::take(u16_vec).into_iter().map(u32::from)),
                    ))
                }

                *self = upgrade_to_u32(u16_vec, non_fitting_element, iter_rev);
            }
            IndexVecDeque::U32(vec) => {
                // TODO: reserve?
                for index in iter_rev {
                    vec.push_front(index);
                }
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Interconversions.

impl From<IndexVec> for IndexVecDeque {
    fn from(value: IndexVec) -> Self {
        match value {
            IndexVec::U16(vec) => IndexVecDeque::U16(vec.into()),
            IndexVec::U32(vec) => IndexVecDeque::U32(vec.into()),
        }
    }
}

impl From<IndexVecDeque> for IndexVec {
    fn from(value: IndexVecDeque) -> Self {
        match value {
            // As noted in the documentation of `VecDeque`, this conversion will implicitly
            // move elements to make them contiguous.
            IndexVecDeque::U16(vecdeque) => IndexVec::U16(vecdeque.into()),
            IndexVecDeque::U32(vecdeque) => IndexVec::U32(vecdeque.into()),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Helper trait defining operations and supertraits that all our allowed index types
/// ([`u16`] and [`u32`]) meet.
pub(crate) trait IndexInt: Ord + bytemuck::Pod + num_traits::NumCast + From<u16> {
    /// Convert this index value to a [`usize`].
    ///
    /// This operation is infallible because we do not support 16-bit `usize`.
    fn to_slice_index(self) -> usize;
}

impl IndexInt for u16 {
    fn to_slice_index(self) -> usize {
        self as usize
    }
}
impl IndexInt for u32 {
    fn to_slice_index(self) -> usize {
        const {
            assert!(
                size_of::<usize>() >= 4,
                "16-bit platforms are not supported by all-is-cubes-mesh"
            );
        }

        // Given the condition we statically checked, this cannot overflow.
        self as usize
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extend_u32_upgrades_only_when_necessary() {
        let mut v = IndexVec::new();
        v.extend([1, 2, 3_u32]);
        // Still u16 even though the inputs were u32
        assert_eq!(v, IndexVec::U16(vec![1, 2, 3]));

        v.extend([4, 5, 1_000_001, 1_000_002, 1_000_003_u32]);
        // Now u32 is necessary.
        assert_eq!(
            v,
            IndexVec::U32(vec![1, 2, 3, 4, 5, 1_000_001, 1_000_002, 1_000_003])
        );
    }
}

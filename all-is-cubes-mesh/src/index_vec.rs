use alloc::collections::VecDeque;
use alloc::vec::Vec;
use core::fmt;
use core::iter;
use core::mem;
use core::ops;

use either::Either;

use all_is_cubes::math::u32size;

use crate::{OutOfMemory, heap};

// -------------------------------------------------------------------------------------------------

/// Data storage for meshes’ index lists, automatically choosing an element type which is
/// large enough for the range of the index values.
#[derive(Clone)]
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
#[derive(Clone)]
pub(crate) enum IndexVecDeque {
    /// 16-bit indices.
    U16(VecDeque<u16>),
    /// 32-bit indices.
    U32(VecDeque<u32>),
}

/// Data for meshes’ index lists, which may use either 16 or 32-bit values.
#[derive(Clone, Copy)]
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
    pub fn with_capacity(capacity: usize) -> Result<Self, OutOfMemory> {
        // Predict whether we will need 32-bit indices. Most often, indices will be of quads,
        // meaning that we have 6 indices for 4 vertices. Therefore, if capacity * 4/6
        // is greater than u16::MAX, some of the index *values* will be greater than u16::MAX.
        // TODO: Ask callers for this info directly?
        const THRESHOLD: usize = u16::MAX as usize / 2 * 3;
        if capacity > THRESHOLD {
            let mut vec: Vec<u32> = Vec::new();
            vec.try_reserve(capacity)?;
            Ok(Self::U32(vec))
        } else {
            let mut vec: Vec<u16> = Vec::new();
            vec.try_reserve(capacity)?;
            Ok(Self::U16(vec))
        }
    }

    /// Returns a slice-reference-like handle to the contents of this [`IndexVec`].
    /// Use this for all read operations.
    ///
    /// # Panics
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

    /// As per [`Vec::reserve_exact()`].
    #[inline]
    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), OutOfMemory> {
        match self {
            Self::U16(vec) => vec.try_reserve_exact(additional),
            Self::U32(vec) => vec.try_reserve_exact(additional),
        }
        .map_err(OutOfMemory::from)
    }
}

impl IndexVecDeque {
    /// Returns the two disjoint slices making up this deque, in the same way as
    /// [`VecDeque::as_slices()`].
    #[inline]
    fn as_slices(&self) -> [IndexSlice<'_>; 2] {
        match self {
            Self::U16(vecdeque) => {
                let (front, back) = vecdeque.as_slices();
                [IndexSlice::U16(front), IndexSlice::U16(back)]
            }
            Self::U32(vecdeque) => {
                let (front, back) = vecdeque.as_slices();
                [IndexSlice::U32(front), IndexSlice::U32(back)]
            }
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

    /// Returns the [`size_of`] one element of this slice.
    ///
    /// This value is always either 2 or 4.
    pub fn element_size(&self) -> usize {
        match self {
            IndexSlice::U16(_) => size_of::<u16>(),
            IndexSlice::U32(_) => size_of::<u32>(),
        }
    }

    /// Returns whether this slice is empty (`len() == 0`).
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the indices in this slice, each converted unconditionally to [`u32`].
    #[inline]
    pub fn iter_u32(&self) -> impl DoubleEndedIterator<Item = u32> + ExactSizeIterator + '_ {
        match self {
            IndexSlice::U16(slice) => Either::Left(slice.iter().copied().map(u32::from)),
            IndexSlice::U32(slice) => Either::Right(slice.iter().copied()),
        }
    }

    /// Returns the indices in this slice, each converted unconditionally to [`usize`].
    //---
    // Design note: This allows callers to make use of our project-wide assumption that usize
    // is at least 32 bits, rather than needing to do their own such assumption.
    #[inline]
    pub fn iter_usize(&self) -> impl DoubleEndedIterator<Item = usize> + '_ {
        match self {
            IndexSlice::U16(slice) => Either::Left(slice.iter().copied().map(usize::from)),
            IndexSlice::U32(slice) => Either::Right(slice.iter().copied().map(u32size)),
        }
    }
}

#[cfg(false)] // currently unused
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

const impl Default for IndexVec {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl heap::HeapUsage for IndexVec {
    #[inline]
    fn heap_bytes_owned(&self) -> usize {
        match self {
            Self::U16(vec) => heap::capacity_bytes(vec),
            Self::U32(vec) => heap::capacity_bytes(vec),
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Implementations of appending/prepending data to `IndexVec` and `IndexVecDeque`.

/// Append data to an [`IndexVec`] or [`IndexVecDeque`].
///
/// Unlike [`Extend`] which accepts arbitrary iterators, this trait demands concretely typed input,
/// which is expected to be some kind of slice reference (including [`IndexSlice`]).
pub(crate) trait Ixtend<S> {
    /// Appends `source`’s elements to `self`, with `offset` added to each one.
    ///
    /// The addition of `offset` is calculated modulo 2<sup>32</sup>.
    /// Callers are expected to check for overflow when building their vertex vector.
    fn ixtend_with_offset(&mut self, source: S, offset: u32) -> Result<(), OutOfMemory>;

    /// Appends `source`’s elements to `self`.
    #[inline(always)] // trivial dispatch
    fn ixtend_without_offset(&mut self, source: S) -> Result<(), OutOfMemory> {
        self.ixtend_with_offset(source, 0)
    }
}

/// Append data to the front of an [`IndexVecDeque`].
pub(crate) trait IxtendFront<S> {
    /// Appends `source`’s elements to the front of `self`, with `offset` added to each one.
    ///
    /// The internal order of the indices in [`source`] is not reversed.
    ///
    /// The addition of `offset` is calculated modulo 2<sup>32</sup>.
    /// Callers are expected to check for overflow when building their vertex vector.
    fn ixtend_front_with_offset(&mut self, source: S, offset: u32) -> Result<(), OutOfMemory>;

    /// Appends `source` to `self`, with `offset` added to each element.
    ///
    /// If `on_front` is true, then the indices are inserted in the “front” of `self`,
    /// and hence will be drawn before existing indices.
    /// The internal order of the indices in [`source`] is not reversed.
    #[inline(always)] // trivial dispatch
    fn ixtend_with_offset_and_direction(
        &mut self,
        source: S,
        offset: u32,
        on_front: bool,
    ) -> Result<(), OutOfMemory>
    where
        Self: Ixtend<S>, // in addition to IxtendFront<S>
    {
        if on_front {
            self.ixtend_front_with_offset(source, offset)
        } else {
            self.ixtend_with_offset(source, offset)
        }
    }
}

impl Ixtend<IndexSlice<'_>> for IndexVec {
    #[inline(always)] // trivial dispatch
    fn ixtend_with_offset(
        &mut self,
        source: IndexSlice<'_>,
        offset: u32,
    ) -> Result<(), OutOfMemory> {
        // Dispatch to the implementation for the dynamic type of the source.
        match source {
            IndexSlice::U16(source) => self.ixtend_with_offset(source, offset),
            IndexSlice::U32(source) => self.ixtend_with_offset(source, offset),
        }
    }

    #[inline(always)] // trivial dispatch
    fn ixtend_without_offset(&mut self, source: IndexSlice<'_>) -> Result<(), OutOfMemory> {
        // Dispatch to the implementation for the dynamic type of the source.
        match source {
            IndexSlice::U16(source) => self.ixtend_without_offset(source),
            IndexSlice::U32(source) => self.ixtend_without_offset(source),
        }
    }
}

impl Ixtend<&[u16]> for IndexVec {
    #[inline(always)]
    fn ixtend_with_offset(&mut self, source: &[u16], offset: u32) -> Result<(), OutOfMemory> {
        let mut iter_with_offset = source.iter().copied().map(map_offset(offset));
        match self {
            IndexVec::U32(u32_vec) => {
                u32_vec.try_reserve(iter_with_offset.len())?;
                u32_vec.extend(iter_with_offset);
                Ok(())
            }

            // With an offset, we cannot rely on the input being in-range.
            IndexVec::U16(u16_vec) => {
                u16_vec.try_reserve(iter_with_offset.len())?;

                let non_fitting_element: u32;
                'try_to_use_u16: {
                    for big_index in iter_with_offset.by_ref() {
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
                    return Ok(()); // succeeded at fitting all u32s in u16s
                }

                *self = upgrade_vec_to_u32(u16_vec, non_fitting_element, iter_with_offset)?;
                Ok(())
            }
        }
    }

    #[inline(always)] // trivial dispatch
    fn ixtend_without_offset(&mut self, source: &[u16]) -> Result<(), OutOfMemory> {
        match self {
            IndexVec::U16(vec) => {
                vec.try_reserve(source.len())?;
                vec.extend(source);
            }
            IndexVec::U32(vec) => {
                vec.try_reserve(source.len())?;
                vec.extend(source.iter().copied().map(u32::from));
            }
        }
        Ok(())
    }
}

impl Ixtend<&[u32]> for IndexVec {
    #[inline]
    fn ixtend_with_offset(&mut self, source: &[u32], offset: u32) -> Result<(), OutOfMemory> {
        let mut iter_with_offset = source.iter().copied().map(map_offset(offset));
        match self {
            IndexVec::U32(u32_vec) => {
                u32_vec.try_reserve(iter_with_offset.len())?;
                u32_vec.extend(iter_with_offset);
                Ok(())
            }
            IndexVec::U16(u16_vec) => {
                // In this case, we have u32s which we want to fit into a vector of u16s,
                // if possible.

                // TODO: If we track the maximum value in self and source, then we can predict whether
                // a change to u32 will be necessary or not.

                u16_vec.try_reserve(iter_with_offset.len())?;

                let non_fitting_element: u32;
                'try_to_use_u16: {
                    for big_index in iter_with_offset.by_ref() {
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
                    return Ok(()); // succeeded at fitting all u32s in u16s
                }

                *self = upgrade_vec_to_u32(u16_vec, non_fitting_element, iter_with_offset)?;
                Ok(())
            }
        }
    }

    #[inline(always)]
    fn ixtend_without_offset(&mut self, source: &[u32]) -> Result<(), OutOfMemory> {
        match self {
            IndexVec::U32(u32_vec) => {
                u32_vec.try_reserve(source.len())?;
                u32_vec.extend(source);
                Ok(())
            }
            IndexVec::U16(u16_vec) => {
                // In this case, we have u32s which we want to fit into a vector of u16s,
                // if possible.

                // TODO: If we track the maximum value in self and source, then we can predict whether
                // a change to u32 will be necessary or not.

                let mut iter = source.iter().copied();

                u16_vec.try_reserve(source.len())?;

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
                    return Ok(()); // succeeded at fitting all u32s in u16s
                }

                *self = upgrade_vec_to_u32(u16_vec, non_fitting_element, iter)?;
                Ok(())
            }
        }
    }
}

impl<'a> Ixtend<IndexSlice<'a>> for IndexVecDeque {
    #[inline(always)]
    fn ixtend_with_offset(
        &mut self,
        source: IndexSlice<'a>,
        offset: u32,
    ) -> Result<(), OutOfMemory> {
        let mut iter_with_offset = source.iter_u32().map(map_offset(offset));
        match self {
            IndexVecDeque::U32(vecdeque) => {
                vecdeque.try_reserve(iter_with_offset.len())?;
                vecdeque.extend(iter_with_offset);
                Ok(())
            }

            IndexVecDeque::U16(u16_vec) => {
                u16_vec.try_reserve(iter_with_offset.len())?;

                let non_fitting_element: u32;
                'try_to_use_u16: {
                    for big_index in iter_with_offset.by_ref() {
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
                    return Ok(()); // succeeded at fitting all u32s in u16s
                }

                *self =
                    upgrade_vecdeque_to_u32_back(u16_vec, non_fitting_element, iter_with_offset)?;
                Ok(())
            }
        }
    }

    // no ixtend_without_offset() because it isn't used.
}

impl<'a> IxtendFront<IndexSlice<'a>> for IndexVecDeque {
    #[inline(always)]
    fn ixtend_front_with_offset(
        &mut self,
        source: IndexSlice<'a>,
        offset: u32,
    ) -> Result<(), OutOfMemory> {
        let mut iter_rev = source.iter_u32().rev().map(map_offset(offset));
        match self {
            IndexVecDeque::U16(u16_vec) => {
                u16_vec.try_reserve(iter_rev.len())?;

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
                    return Ok(()); // succeeded at fitting all u32s in u16s
                }

                *self = upgrade_vecdeque_to_u32_front(u16_vec, non_fitting_element, iter_rev)?;
                Ok(())
            }
            IndexVecDeque::U32(vec) => {
                vec.try_reserve(iter_rev.len())?;
                for index in iter_rev {
                    vec.push_front(index);
                }
                Ok(())
            }
        }
    }
}

#[inline(always)]
fn apply_offset<T: Into<u32>>(index: T, offset: u32) -> u32 {
    index.into().wrapping_add(offset)
}
#[inline(always)]
fn map_offset<T: Into<u32>>(offset: u32) -> impl Fn(T) -> u32 {
    #[inline(always)]
    move |index| apply_offset::<T>(index, offset)
}

#[cold]
#[inline(never)]
fn upgrade_vec_to_u32(
    u16_vec: &mut Vec<u16>,
    non_fitting_element: u32,
    iter: impl ExactSizeIterator<Item = u32>,
) -> Result<IndexVec, OutOfMemory> {
    // Construct new Vec<u32> using
    // * the existing items
    // * the new item triggering the conversion
    // * the remaining new items

    let mut new_vec = Vec::new();
    new_vec.try_reserve(u16_vec.len() + 1 + iter.len())?;

    // Cannot OOM because enough space was allocated above.
    // Note that we do the mem::take() only if the allocation succeeded, so failure
    // won’t discard any elements.
    new_vec.extend(
        mem::take(u16_vec)
            .into_iter()
            .map(u32::from)
            .chain([non_fitting_element])
            .chain(iter),
    );

    Ok(IndexVec::U32(new_vec))
}

#[cold]
#[inline(never)]
fn upgrade_vecdeque_to_u32_back(
    u16_vec: &mut VecDeque<u16>,
    non_fitting_element: u32,
    iter: impl ExactSizeIterator<Item = u32>,
) -> Result<IndexVecDeque, OutOfMemory> {
    // Construct new u32 vector using
    // * the existing items
    // * the new item triggering the conversion
    // * the remaining new items

    let mut new_vec = VecDeque::new();
    new_vec.try_reserve(u16_vec.len() + 1 + iter.len())?;

    // Cannot OOM because enough space was allocated above.
    // Note that we do the mem::take() only if the allocation succeeded, so failure
    // won’t discard any elements.
    new_vec.extend(
        mem::take(u16_vec)
            .into_iter()
            .map(u32::from)
            .chain([non_fitting_element])
            .chain(iter),
    );

    Ok(IndexVecDeque::U32(new_vec))
}

#[cold]
#[inline(never)]
fn upgrade_vecdeque_to_u32_front(
    u16_vec: &mut VecDeque<u16>,
    non_fitting_element: u32,
    iter_rev: impl DoubleEndedIterator<Item = u32> + ExactSizeIterator,
) -> Result<IndexVecDeque, OutOfMemory> {
    // Construct new VecDeque<u32> using
    // * the remaining new items
    // * the new item triggering the conversion
    // * the existing items

    let mut new_vec = VecDeque::new();
    new_vec.try_reserve(u16_vec.len() + 1 + iter_rev.len())?;

    // Cannot OOM because enough space was allocated above.
    // Note that we do the mem::take() only if the allocation succeeded, so failure
    // won’t discard any elements.
    new_vec.extend(
        iter_rev
            .rev()
            .chain([non_fitting_element])
            .chain(mem::take(u16_vec).into_iter().map(u32::from)),
    );

    Ok(IndexVecDeque::U32(new_vec))
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
pub(crate) trait IndexInt:
    Ord + bytemuck::Pod + num_traits::NumCast + From<u16> + fmt::Display
{
    /// Convert this index value to a [`usize`].
    ///
    /// This operation is infallible because we do not support 16-bit `usize`.
    fn to_slice_index(self) -> usize;
}

impl IndexInt for u16 {
    fn to_slice_index(self) -> usize {
        usize::from(self)
    }
}
impl IndexInt for u32 {
    fn to_slice_index(self) -> usize {
        u32size(self)
    }
}

// -------------------------------------------------------------------------------------------------

impl Eq for IndexSlice<'_> {}
impl PartialEq for IndexSlice<'_> {
    /// Compare the contents of two index slices numerically, ignoring the element type.
    #[inline(never)]
    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (Self::U16(a), Self::U16(b)) => a == b,
            (Self::U32(a), Self::U32(b)) => a == b,
            (Self::U16(slice16), Self::U32(slice32)) | (Self::U32(slice32), Self::U16(slice16)) => {
                let len = slice16.len();
                if len != slice32.len() {
                    return false;
                }

                // Perform the comparison in chunks to allow more SIMD/SWAR execution.
                // Chunk size chosen empirically.
                const CHUNK_SIZE: usize = 16;
                let (chunks16, rem16) = slice16.as_chunks::<CHUNK_SIZE>();
                let (chunks32, rem32) = slice32.as_chunks::<CHUNK_SIZE>();

                for (&chunk16, &chunk32) in iter::zip(chunks16, chunks32) {
                    if chunk16.map(u32::from) != chunk32 {
                        return false;
                    }
                }
                for (&value16, &value32) in iter::zip(rem16, rem32) {
                    if u32::from(value16) != value32 {
                        return false;
                    }
                }

                // It is unlikely that two slices of different type will be equal.
                core::hint::cold_path();
                true
            }
        }
    }
}

impl Eq for IndexVec {}
impl PartialEq for IndexVec {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice(..) == other.as_slice(..)
    }
}

impl Eq for IndexVecDeque {}
impl PartialEq for IndexVecDeque {
    fn eq(&self, other: &Self) -> bool {
        self.as_slices() == other.as_slices()
    }
}

// -------------------------------------------------------------------------------------------------
// Debug formatting.

impl fmt::Debug for IndexSlice<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U16(slice) => fmt_index_list(f, "U16", slice.iter().copied()),
            Self::U32(slice) => fmt_index_list(f, "U32", slice.iter().copied()),
        }
    }
}

impl fmt::Debug for IndexVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_slice(..).fmt(f)
    }
}

impl fmt::Debug for IndexVecDeque {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U16(deque) => fmt_index_list(f, "U16", deque.iter().copied()),
            Self::U32(deque) => fmt_index_list(f, "U32", deque.iter().copied()),
        }
    }
}

fn fmt_index_list<Ix: IndexInt>(
    f: &mut fmt::Formatter<'_>,
    type_prefix: &'static str,
    iter: impl ExactSizeIterator<Item = Ix>,
) -> fmt::Result {
    if iter.len() == 0 {
        return write!(f, "{type_prefix}[]");
    }

    write!(f, "{type_prefix}[\n    ")?;
    for (i, elem) in iter.enumerate() {
        if i != 0 {
            if i.rem_euclid(18) == 0 {
                // Line breaks
                write!(f, "\n    ")?;
            } else if i.rem_euclid(6) == 0 {
                // Extra padding between rectangles (each having 6 indices).
                // Index lists most often (but do not always) contain rectangles.
                // TODO: Can we arrange to check the flag stored elsewhere that says whether or not
                // they do contain rectangles?
                write!(f, "  ")?;
            }
        }

        // TODO: pad based on index type in use?
        // 2 is a compromise between tidy arrangement and compactness for cases where printing is
        // actually useful.
        write!(f, "{elem:2}, ")?;
    }
    write!(f, "\n]")?;
    Ok(())
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::format;
    use alloc::vec;

    #[cfg(fmt_debug = "full")]
    #[test]
    fn debug_empty() {
        assert_eq!(format!("{:#?}", IndexVec::new()), "U16[]");
    }

    #[cfg(fmt_debug = "full")]
    #[test]
    fn debug_nonempty() {
        assert_eq!(
            format!("{:#?}", IndexVec::U16(Vec::from_iter(0..24))),
            indoc::indoc! {
                "U16[
                     0,  1,  2,  3,  4,  5,    6,  7,  8,  9, 10, 11,   12, 13, 14, 15, 16, 17, 
                    18, 19, 20, 21, 22, 23, 
                ]"
            }
        );
    }

    /// Comparison of [`IndexVec`] compares the numeric values only.
    #[test]
    fn eq_vec_ignores_type() {
        // Equal, whether or not the element types match.
        assert_eq!(IndexVec::U16(vec![1, 2, 3]), IndexVec::U16(vec![1, 2, 3]));
        assert_eq!(IndexVec::U16(vec![1, 2, 3]), IndexVec::U32(vec![1, 2, 3]));
        assert_eq!(IndexVec::U32(vec![1, 2, 3]), IndexVec::U16(vec![1, 2, 3]));
        assert_eq!(IndexVec::U32(vec![1, 2, 3]), IndexVec::U32(vec![1, 2, 3]));

        // Unequal, whether or not the element types match.
        assert_ne!(IndexVec::U16(vec![1, 2, 3]), IndexVec::U16(vec![1, 2, 4]));
        assert_ne!(IndexVec::U16(vec![1, 2, 3]), IndexVec::U32(vec![1, 2, 4]));
        assert_ne!(IndexVec::U32(vec![1, 2, 3]), IndexVec::U16(vec![1, 2, 4]));
        assert_ne!(IndexVec::U32(vec![1, 2, 3]), IndexVec::U32(vec![1, 2, 4]));
    }

    /// Comparison of [`IndexVecDeque`] compares the numeric values only.
    #[test]
    fn eq_deque_ignores_type() {
        use IndexVec::{U16, U32};
        fn df(v: IndexVec) -> IndexVecDeque {
            v.into()
        }

        // Equal, whether or not the element types match.
        assert_eq!(df(U16(vec![1, 2, 3])), df(U16(vec![1, 2, 3])));
        assert_eq!(df(U16(vec![1, 2, 3])), df(U32(vec![1, 2, 3])));
        assert_eq!(df(U32(vec![1, 2, 3])), df(U16(vec![1, 2, 3])));
        assert_eq!(df(U32(vec![1, 2, 3])), df(U32(vec![1, 2, 3])));

        // Unequal, whether or not the element types match.
        assert_ne!(df(U16(vec![1, 2, 3])), df(U16(vec![1, 2, 4])));
        assert_ne!(df(U16(vec![1, 2, 3])), df(U32(vec![1, 2, 4])));
        assert_ne!(df(U32(vec![1, 2, 3])), df(U16(vec![1, 2, 4])));
        assert_ne!(df(U32(vec![1, 2, 3])), df(U32(vec![1, 2, 4])));
    }

    /// Comparison of [`IndexSlice`] compares the numeric values only.
    #[test]
    fn eq_slice_ignores_type() {
        // Equal, whether or not the element types match.
        assert_eq!(IndexSlice::U16(&[1, 2, 3]), IndexSlice::U16(&[1, 2, 3]));
        assert_eq!(IndexSlice::U16(&[1, 2, 3]), IndexSlice::U32(&[1, 2, 3]));
        assert_eq!(IndexSlice::U32(&[1, 2, 3]), IndexSlice::U16(&[1, 2, 3]));
        assert_eq!(IndexSlice::U32(&[1, 2, 3]), IndexSlice::U32(&[1, 2, 3]));

        // Unequal, whether or not the element types match.
        assert_ne!(IndexSlice::U16(&[1, 2, 3]), IndexSlice::U16(&[1, 2, 4]));
        assert_ne!(IndexSlice::U16(&[1, 2, 3]), IndexSlice::U32(&[1, 2, 4]));
        assert_ne!(IndexSlice::U32(&[1, 2, 3]), IndexSlice::U16(&[1, 2, 4]));
        assert_ne!(IndexSlice::U32(&[1, 2, 3]), IndexSlice::U32(&[1, 2, 4]));
    }

    #[test]
    fn extend_u32_upgrades_only_when_necessary() {
        let mut v = IndexVec::new();
        v.ixtend_without_offset(IndexSlice::U32(&[1, 2, 3_u32])).unwrap();
        // Still u16 even though the inputs were u32
        assert_eq!(v, IndexVec::U16(vec![1, 2, 3]));

        v.ixtend_without_offset(IndexSlice::U32(&[
            4,
            5,
            1_000_001,
            1_000_002,
            1_000_003_u32,
        ]))
        .unwrap();
        // Now u32 is necessary.
        assert_eq!(
            v,
            IndexVec::U32(vec![1, 2, 3, 4, 5, 1_000_001, 1_000_002, 1_000_003])
        );
    }

    /// Correct ordering of elements in `IndexVecDeque`s upgraded while extending the back.
    #[test]
    fn extend_deque_upgrade_back() {
        let mut v = IndexVecDeque::U16(VecDeque::from_iter([1]));

        v.ixtend_with_offset(IndexSlice::U32(&[2, 3]), 1_000_000).unwrap();

        assert_eq!(
            v,
            IndexVecDeque::U32(VecDeque::from_iter([1, 1_000_002, 1_000_003]))
        );
    }

    /// Correct ordering of elements in `IndexVecDeque`s upgraded while extending the front.
    #[test]
    fn extend_deque_upgrade_front() {
        let mut v = IndexVecDeque::U16(VecDeque::from_iter([1]));

        v.ixtend_front_with_offset(IndexSlice::U32(&[2, 3]), 1_000_000).unwrap();

        assert_eq!(
            v,
            IndexVecDeque::U32(VecDeque::from_iter([1_000_002, 1_000_003, 1]))
        );
    }

    #[test]
    fn iter_u32() {
        assert_eq!(
            Vec::from_iter(IndexSlice::U16(&[5, 10, 72]).iter_u32()),
            [5u32, 10, 72]
        );
        assert_eq!(
            Vec::from_iter(IndexSlice::U32(&[5, 10, 72]).iter_u32()),
            [5u32, 10, 72]
        );
    }

    #[test]
    fn iter_usize() {
        assert_eq!(
            Vec::from_iter(IndexSlice::U16(&[5, 10, 72]).iter_usize()),
            [5usize, 10, 72]
        );
        assert_eq!(
            Vec::from_iter(IndexSlice::U32(&[5, 10, 72]).iter_usize()),
            [5usize, 10, 72]
        );
    }
}

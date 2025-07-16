use alloc::vec::Vec;
use core::{mem, ops};

use either::Either;

/// Data storage for meshes’ index lists, automatically choosing an element type which is
/// large enough for the index values.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum IndexVec {
    /// 16-bit indices.
    U16(Vec<u16>),
    /// 32-bit indices.
    U32(Vec<u32>),
}

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
            IndexVec::U16(vec) => IndexSlice::U16(&vec.as_slice()[range]),
            IndexVec::U32(vec) => IndexSlice::U32(&vec.as_slice()[range]),
        }
    }

    /// As per [`Vec::len()`].
    #[inline]
    pub fn len(&self) -> usize {
        self.as_slice(..).len()
    }

    /// Resets the vector to have zero length and the same capacity.
    /// Note that this preserves the previous index type.
    #[inline]
    pub fn clear(&mut self) {
        match self {
            IndexVec::U16(vec) => vec.clear(),
            IndexVec::U32(vec) => vec.clear(),
        }
    }

    #[inline]
    pub(crate) fn capacity_bytes(&self) -> usize {
        match self {
            IndexVec::U16(vec) => vec.capacity() * 2,
            IndexVec::U32(vec) => vec.capacity() * 4,
        }
    }

    /// As per [`Vec::reserve_exact()`].
    #[inline]
    pub fn reserve_exact(&mut self, additional: usize) {
        match self {
            IndexVec::U16(vec) => vec.reserve_exact(additional),
            IndexVec::U32(vec) => vec.reserve_exact(additional),
        }
    }
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
    pub fn iter_u32(&self) -> impl Iterator<Item = u32> + '_ {
        match self {
            IndexSlice::U16(slice) => Either::Left(slice.iter().copied().map(u32::from)),
            IndexSlice::U32(slice) => Either::Right(slice.iter().copied()),
        }
    }
}

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

impl Extend<u32> for IndexVec {
    #[inline]
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

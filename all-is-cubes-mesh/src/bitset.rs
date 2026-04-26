use alloc::vec::Vec;
use core::marker::PhantomData;
use core::{fmt, ops};

use num_traits::Zero;

use crate::OutOfMemory;

// -------------------------------------------------------------------------------------------------

#[derive(Clone, Eq, PartialEq)]
pub(crate) struct BitSet<T> {
    /// Invariant: the vector’s `len()` is no longer than necessary.
    /// (If this were not true, we would need a more elaborate `PartialEq`.)
    bits: Vec<u32>,
    _phantom: PhantomData<T>,
}

impl<T: Copy + Zero + Into<u32> + TryFrom<u32> + ops::Add<Output = T>> BitSet<T> {
    pub const fn new() -> Self {
        Self {
            bits: Vec::new(),
            _phantom: PhantomData,
        }
    }

    pub fn clear(&mut self) {
        self.bits.clear()
    }

    /// Adds `value` to the set.
    /// Returns whether `value` was not already present, or an error if memory allocation fails.
    pub fn insert(&mut self, value: T) -> Result<bool, OutOfMemory> {
        let word = (Into::<u32>::into(value) / u32::BITS) as usize;
        let bit_position = Into::<u32>::into(value) % u32::BITS;
        let bit_value = 1u32 << bit_position;

        if word >= self.bits.len() {
            self.bits.try_reserve((word + 1) - self.bits.len())?;
            self.bits.resize(word + 1, 0);
        }

        let place = &mut self.bits[word];
        let is_new = *place & bit_value == 0;
        *place |= bit_value;
        Ok(is_new)
    }

    #[cfg_attr(
        not(debug_assertions),
        allow(dead_code, reason = "used in SubMesh::consistency_check()")
    )]
    pub fn contains(&self, value: T) -> bool {
        let word = (Into::<u32>::into(value) / u32::BITS) as usize;
        let bit_position = Into::<u32>::into(value) % u32::BITS;
        let bit_value = 1u32 << bit_position;

        match self.bits.get(word) {
            Some(place) => place & bit_value != 0,
            _ => false,
        }
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.into_iter()
    }

    pub fn capacity_bytes(&self) -> usize {
        self.bits.capacity() * size_of::<u32>()
    }
}

impl<'a, T: Copy + Zero + Into<u32> + TryFrom<u32> + ops::Add<Output = T>> IntoIterator
    for &'a BitSet<T>
{
    type Item = T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            current_word: self.bits.first().copied().unwrap_or(0),
            future_words: self.bits.get(1..).unwrap_or(&[]),
            value_offset: T::zero(),
        }
    }
}

impl<T: Copy + Zero + Into<u32> + TryFrom<u32>> FromIterator<T> for BitSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = Self::new();
        for item in iter {
            set.insert(item).unwrap();
        }
        set
    }
}

impl<T: fmt::Debug + Copy + Zero + Into<u32> + TryFrom<u32>> fmt::Debug for BitSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

pub(crate) struct Iter<'a, T> {
    /// Word we are consuming to yield; becomes zero as we iterate.
    current_word: u32,
    /// Words that are not yet the word we are consuming.
    future_words: &'a [u32],
    /// Value to add to the position of the next 1 bit we extract to produce the element.
    value_offset: T,
}

impl<T: Copy + Into<u32> + TryFrom<u32> + ops::Add<Output = T>> Iterator for Iter<'_, T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        while self.current_word == 0 {
            self.current_word = *self.future_words.split_off_first()?;
            self.value_offset =
                self.value_offset + u32::BITS.try_into().unwrap_or_else(bits_fit_panic);
        }

        // self.current is now not zero.

        // Find the next bit to produce.
        let first_set_bit_position = self.current_word.trailing_zeros();
        let item =
            self.value_offset + first_set_bit_position.try_into().unwrap_or_else(bits_fit_panic);

        // Clear that bit so we don’t produce it again.
        self.current_word &= !(1 << first_set_bit_position);

        Some(item)
    }
}

fn bits_fit_panic<T, U>(_: T) -> U {
    panic!("shouldn't happen: bitset value type cannot hold a bit in the set")
}

use core::fmt;
use core::marker::PhantomData;
use core::sync::atomic::{AtomicU32, Ordering};

/// A mutable container for small values which supports atomic operations.
///
/// `T` must implement [`bytemuck::NoUninit`] and [`bytemuck::CheckedBitPattern`],
/// and be exactly four bytes in size.
pub(crate) struct AtomicCell32<T> {
    storage: AtomicU32,
    _phantom: PhantomData<T>,
}

impl<T> AtomicCell32<T>
where
    T: bytemuck::NoUninit + bytemuck::CheckedBitPattern,
{
    pub fn new(value: T) -> Self {
        Self {
            storage: AtomicU32::new(bytemuck::must_cast(value)),
            _phantom: PhantomData,
        }
    }

    pub fn compare_exchange(
        &self,
        current: T,
        new: T,
        success: Ordering,
        failure: Ordering,
    ) -> Result<T, T> {
        let current = bytemuck::must_cast(current);
        let new = bytemuck::must_cast(new);
        match self.storage.compare_exchange(current, new, success, failure) {
            Ok(loaded) => Ok(bytemuck::checked::cast(loaded)),
            Err(loaded) => Err(bytemuck::checked::cast(loaded)),
        }
    }

    pub fn load(&self, order: Ordering) -> T {
        bytemuck::checked::cast(self.storage.load(order))
    }

    pub fn swap(&self, new: T, order: Ordering) -> T {
        let new = bytemuck::must_cast(new);
        bytemuck::checked::cast(self.storage.swap(new, order))
    }
}

impl<T> fmt::Debug for AtomicCell32<T>
where
    T: fmt::Debug + bytemuck::NoUninit + bytemuck::CheckedBitPattern,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.load(Ordering::Relaxed).fmt(f)
    }
}

// -------------------------------------------------------------------------------------------------

/// Type which is always initialized to zero.
///
/// This may be used to help types satisfy [`bytemuck::NoUninit`] and be usable by [`AtomicCell32`]
/// by filling space that would be padding with zero instead..
#[derive(Clone, Copy, Eq, PartialEq, bytemuck::NoUninit, bytemuck::CheckedBitPattern)]
#[repr(u8)]
pub(crate) enum Zero {
    Zero = 0,
}
/// Allows [`Zero`][struct@Zero] to be used like a unit struct.
pub(crate) const ZERO: Zero = Zero::Zero;

/// Three copies of [`Zero`].
///
/// TODO: This would be redundant if [`bytemuck::CheckedBitPattern`] was implemented for arrays.
#[derive(Clone, Copy, Eq, PartialEq, bytemuck::NoUninit, bytemuck::CheckedBitPattern)]
#[repr(C)]
pub(crate) struct Zero3 {
    _zero1: Zero,
    _zero2: Zero,
    _zero3: Zero,
}
/// Allows [`Zero3`][struct@Zero3] to be used like a unit struct.
pub(crate) const ZERO3: Zero3 = Zero3 {
    _zero1: Zero::Zero,
    _zero2: Zero::Zero,
    _zero3: Zero::Zero,
};

impl fmt::Debug for Zero {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        // print nothing for minimum impact
        Ok(())
    }
}
impl fmt::Debug for Zero3 {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

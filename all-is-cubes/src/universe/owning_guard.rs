//! `unsafe` code to manage access to universe members.
//!
//! This is a narrowly restricted “self-referential struct” implementation; specifically, it is
//! an “owner and borrower” pair of the sort which `yoke` and `self_cell` support.
//! I am not using these libraries because:
//!
//! * `yoke` does not permit `&mut T` access to the borrower except by `'static`
//!   functions. This is intended to prevent the hazard of inserting a too-short reference
//!   into the `T` structure. In our case, this is not a problem because the `T` we expose
//!   via [`DerefMut`] is the `T` inside the [`RwLock`], not the [`RwLockWriteGuard`]
//!   that has the lifetime. It's also `'static`, but I don't think that is actually
//!   necessary.
//!
//! * `self_cell` (and `ouroboros ~0.16.0`) do not support type parameters.
//!   Our usage of type parameters is not strictly necessary — it is a finite set of
//!   universe member types — so it would be _possible_ to use them anyway.
//!
//! * `self_cell` and `ouroboros` both have to heap-allocate the owning field(s) in order
//!   to keep them unmoving. We can skip that since our owner is an `Arc` already.
//!
//! Further discussion of this code:
//! <https://users.rust-lang.org/t/unsafe-code-review-semi-owning-weak-rwlock-t-guard/95706>

use alloc::sync::Arc;
use core::{mem, ptr};

use super::handle::UEntry;
use crate::util::maybe_sync::{RwLock, RwLockReadGuard, RwLockWriteGuard, TryLockError};

#[cfg(doc)]
use super::Handle;

// -------------------------------------------------------------------------------------------------

type Lock<T> = RwLock<UEntry<T>>;
type Strong<T> = Arc<Lock<T>>;

// There are two near-identical implementations for read-only and exclusive access.
// For readability, they are presented in parallel; both structs then both impls, etc.
//
// The “impl” in the name refers to that these are not public API; they are solely concerned with
// maintaining their unsafe self-reference, and they dereference to `UEntry<T>` instead of `T`.

/// Owning wrapper around [`RwLockReadGuard`] for [`Handle`].
pub(super) struct ReadGuardImpl<T: 'static> {
    // SAFETY: `guard` must be dropped before `strong`,
    // which is accomplished by declaring it first.
    /// Lock guard that points into `strong`.
    guard: MaybeDangling<RwLockReadGuard<'static, UEntry<T>>>,

    /// [`Arc`] that keeps the data pointer alive as long as `guard` is in use.
    /// This field is never read, only dropped to decrement the reference count appropriately.
    #[expect(dead_code, reason = "used for drop effect")]
    strong: Strong<T>,
}

/// Owning wrapper around [`RwLockWriteGuard`] for [`Handle`].
pub(super) struct WriteGuardImpl<T: 'static> {
    // SAFETY: `guard` must be dropped before `strong`,
    // which is accomplished by declaring it first.
    /// Lock guard that points into `strong`.
    guard: MaybeDangling<RwLockWriteGuard<'static, UEntry<T>>>,

    /// [`Arc`] that keeps the data pointer alive as long as `guard` is in use.
    /// This field is never read, only dropped to decrement the reference count appropriately.
    #[expect(dead_code, reason = "used for drop effect")]
    strong: Strong<T>,
}

impl<T: 'static> ReadGuardImpl<T> {
    pub(super) fn new(strong: Strong<T>) -> Result<Self, LockError> {
        let ptr: *const Lock<T> = Arc::as_ptr(&strong);

        // SAFETY:
        // * `ptr` will remain valid until we drop `strong`
        // * we will not drop `strong` until after we drop `reference`
        // * we will not let `reference` escape to be copied and potentially outlive `strong`
        // * we will store the owner of the `reference` in `MaybeDangling` (see below)
        //   to avoid asserting its validity too long.
        let reference: &'static Lock<T> = unsafe { &*ptr };

        let guard = MaybeDangling::new(reference.try_read()?);

        Ok(ReadGuardImpl { guard, strong })
    }
}

impl<T: 'static> WriteGuardImpl<T> {
    pub(super) fn new(strong: Strong<T>) -> Result<Self, LockError> {
        let ptr: *const Lock<T> = Arc::as_ptr(&strong);

        // SAFETY:
        // * `ptr` will remain valid until we drop `strong`
        // * we will not drop `strong` until after we drop `reference`
        // * we will not let `reference` escape to be copied and potentially outlive `strong`
        // * we will store the owner of the `reference` in `MaybeDangling` (see below)
        //   to avoid asserting its validity too long.
        let reference: &'static Lock<T> = unsafe { &*ptr };

        let guard = MaybeDangling::new(reference.try_write()?);

        Ok(WriteGuardImpl { guard, strong })
    }
}

impl<T: 'static> core::ops::Deref for ReadGuardImpl<T> {
    type Target = UEntry<T>;

    fn deref(&self) -> &Self::Target {
        &self.guard
    }
}
impl<T: 'static> core::ops::Deref for WriteGuardImpl<T> {
    type Target = UEntry<T>;

    fn deref(&self) -> &Self::Target {
        &self.guard
    }
}
impl<T: 'static> core::ops::DerefMut for WriteGuardImpl<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard
    }
}

#[derive(Copy, Clone)]
pub(super) struct LockError;

impl<T> From<TryLockError<T>> for LockError {
    fn from(_: TryLockError<T>) -> Self {
        // TODO: Distinguish PoisonErrors (without granting access) for debugging purposes
        Self
    }
}

// -------------------------------------------------------------------------------------------------

/// Wrapper type that avoids implicitly asserting the contents are currently valid.
/// Therefore it is safe to contain a dangling reference, as long as it is not used.
///
/// Context on this code:
/// <https://users.rust-lang.org/t/unsafe-code-review-semi-owning-weak-rwlock-t-guard/95706/6>
///
/// TODO: Replace this with an official definitely-valid version when that is implemented.
/// <https://github.com/rust-lang/rfcs/pull/3336>
struct MaybeDangling<T> {
    inner: mem::MaybeUninit<T>,
}

impl<T> MaybeDangling<T> {
    fn new(value: T) -> Self {
        Self {
            inner: mem::MaybeUninit::new(value),
        }
    }
}

impl<T> core::ops::Deref for MaybeDangling<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY:
        // The `MaybeUninit` is never actually uninitialized. It might be dangling, but
        // in that case it is the caller's responsibility not to use `self`. We are only
        // using `MaybeUninit` to tell the _compiler_ to assume less.
        unsafe { self.inner.assume_init_ref() }
    }
}

impl<T> core::ops::DerefMut for MaybeDangling<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY:
        // The `MaybeUninit` is never actually uninitialized. It might be dangling, but
        // in that case it is the caller's responsibility not to use `self`. We are only
        // using `MaybeUninit` to tell the _compiler_ to assume less.
        unsafe { self.inner.assume_init_mut() }
    }
}

impl<T> Drop for MaybeDangling<T> {
    fn drop(&mut self) {
        let p = self.inner.as_mut_ptr();
        // SAFETY:
        // The `MaybeUninit` is never actually uninitialized, and it is the caller's
        // responsibility to drop it before it is invalid just like any normal type.
        unsafe {
            ptr::drop_in_place(p);
        }
    }
}

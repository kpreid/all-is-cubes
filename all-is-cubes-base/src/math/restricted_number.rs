use core::fmt;
use core::hash;
use core::ops;

use num_traits::{ConstOne as _, ConstZero as _};
use ordered_float::FloatCore;
use ordered_float::NotNan;

// -------------------------------------------------------------------------------------------------

/// A floating-point number which is not NaN and whose sign bit is positive.
///
/// The allowed values consist of positive zero, positive infinity,
/// and every value in between those. This set of values means that this type:
///
/// * Implements [`Eq`] straightforwardly; neither NaN nor signed zeroes
///   can cause problems with using it as a map key for caching, interning, etc.
/// * Is closed under multiplication and addition, so unlike [`NotNan`], cannot
///   cause a panic from uncautious arithmetic.
///
/// The permitted values of a `PositiveSign<T>` are a subset of those of a `NotNan<T>`.
/// (This may not be guaranteed if `T` implements numeric traits in inconsistent ways,
/// but may be assumed for `f32` and `f64`.)
///
/// The arithmetic behavior of `PositiveSign<T>` is *not* identical to `T`.
/// Specifically, the value of `0. * fXX::INFINITY` is NaN, but the value of
/// `PositiveSign(0.) * PositiveSign(fXX::INFINITY)` is `PositiveSign(0.)` instead.
/// Our assumption here is that for the applications where `PositiveSign` is suitable, infinities
/// are either impossible or arise as “would be finite but it is too large to be represented”,
/// and do not arise as the reciprocal of zero, and thus we can treat “zero times anything
/// is zero” as being a more important property than “infinity times anything is infinity”.
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct PositiveSign<T>(T);

/// A floating-point number which is within the range +0 to +1 (inclusive).
///
/// This may be used for alpha blending, reflectance, lerps, and anything else where values
/// outside the range 0 to 1 is meaningless. It is closed under multiplication.
///
/// Because NaN and negative zero are excluded, this type implements [`Eq`] straightforwardly,
/// and can reliably be used as a map key for caching, interning, etc.
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct ZeroOne<T>(T);

// --- Inherent implementations --------------------------------------------------------------------

impl<T: FloatCore> PositiveSign<T> {
    /// Construct [`PositiveSign`] without checking the value.
    ///
    /// # Safety
    ///
    /// `value` must not be NaN and must have a positive sign bit.
    /// Note that `value >= 0.` is not a sufficient condition,
    /// because it does not exclude negative zero.
    #[inline]
    pub const unsafe fn new_unchecked(value: T) -> Self {
        Self(value)
    }

    pub(crate) const fn into_nn(self) -> NotNan<T> {
        // SAFETY: `PositiveSign`’s restrictions are a superset of `NotNan`’s.
        unsafe { NotNan::new_unchecked(self.0) }
    }

    /// Returns whether the value is finite.
    ///
    /// Since the value is statically guaranteed to be neither NaN nor negative,
    /// the only case where this returns `false` is when the value is positive infinity.
    #[inline]
    pub fn is_finite(&self) -> bool {
        // We could delegate to FloatCore::is_finite() but that would perform an unnecessary
        // NaN check.
        self.0 != T::infinity()
    }

    #[cfg(test)]
    #[track_caller]
    pub(crate) fn consistency_check(self)
    where
        Self: fmt::Debug,
    {
        assert!(!self.0.is_nan() && self.0.is_sign_positive(), "{self:?}");
    }
}

impl<T: FloatCore> ZeroOne<T> {
    /// Construct [`ZeroOne`] without checking the value.
    ///
    /// # Safety
    ///
    /// `value` must be +0, 1, or some value in between those two.
    /// Note that `value >= 0. && value <= 1.` is not a sufficient condition,
    /// because it does not exclude negative zero.
    #[inline]
    pub const unsafe fn new_unchecked(value: T) -> Self {
        Self(value)
    }

    pub(crate) const fn into_nn(self) -> NotNan<T> {
        // SAFETY: `ZeroOne`’s restrictions are a superset of `NotNan`’s.
        unsafe { NotNan::new_unchecked(self.0) }
    }

    #[cfg(test)]
    #[track_caller]
    pub(crate) fn consistency_check(self)
    where
        Self: fmt::Debug,
    {
        assert!(
            self.0.is_sign_positive() && self.0 >= T::zero() && self.0 <= T::one(),
            "{self:?}"
        );
    }

    /// Returns `1.0 - self`.
    ///
    /// The result cannot be out of range, so this operation is always successful.
    #[inline]
    #[must_use]
    pub fn complement(self) -> Self {
        // Construction safety:
        // `self.0` is at most 1.0, so the result cannot be less than 0.0.
        // `self.0` is at least 0.0, so the result cannot be greater than 1.0.
        Self(T::one() - self.0)
    }
}

impl<T: FloatCore + num_traits::ConstZero + num_traits::ConstOne> ZeroOne<T> {
    /// The number zero, as a constant.
    // This cannot be a `ConstZero` implementation because `ZeroOne` does not implement `ops::Add`.
    pub const ZERO: Self = Self(T::ZERO);
    /// The number one, as a constant.
    // This exists too just for symmetry, though there is a `ConstOne` implementation
    pub const ONE: Self = Self(T::ONE);
}

// --- Non-generic macro-generated implementations -------------------------------------------------
//
// As it becomes possible, we should replace these with generic impls.

macro_rules! non_generic_impls {
    ($t:ty) => {
        impl PositiveSign<$t> {
            /// Positive infinity (∞).
            pub const INFINITY: Self = Self(<$t>::INFINITY);

            /// Wraps the given value in `PositiveSign`.
            ///
            /// * If `value` is positive (including positive infinity), returns wrapped `value`.
            /// * If `value` is zero of either sign, returns wrapped positive zero.
            ///   This is lossy, but corresponds to the IEEE 754 idea that -0.0 == +0.0.
            /// * If `value` is negative non-zero or NaN, panics.
            #[track_caller]
            #[inline]
            pub const fn new_strict(value: $t) -> Self {
                if value > 0. {
                    Self(value)
                } else if value == 0. {
                    Self(0.)
                } else {
                    positive_sign_not_positive_panic()
                }
            }

            /// Wraps the given value in `PositiveSign`.
            ///
            /// * If the value is NaN, panics.
            /// * If the value has a negative sign, it is replaced with positive zero.
            #[track_caller]
            #[inline]
            pub const fn new_clamped(value: $t) -> Self {
                if value > 0. {
                    Self(value)
                } else if value == value {
                    Self(0.)
                } else {
                    positive_sign_nan_panic()
                }
            }

            /// Const equivalent of `TryFrom::try_from()`.
            #[inline]
            pub(crate) const fn try_new(value: $t) -> Result<Self, NotPositiveSign<$t>> {
                if value > <$t>::ZERO {
                    Ok(Self(value))
                } else if value == <$t>::ZERO {
                    // must be zero, not NaN, but we don’t know the sign
                    Ok(Self::ZERO)
                } else {
                    Err(NotPositiveSign(value))
                }
            }

            /// Unwraps the value without modifying it.
            // TODO: When #![feature(const_precise_live_drops)] becomes stable, we can make this generic.
            #[inline]
            pub const fn into_inner(self) -> $t {
                self.0
            }

            /// Subtract `other` from `self`; if the result would be negative, it is zero instead.
            #[inline]
            #[must_use]
            pub fn saturating_sub(self, other: Self) -> Self {
                // should never be able to panic
                Self::new_clamped(self.0 - other.0)
            }
        }

        impl ZeroOne<$t> {
            /// Wraps the given value in `ZeroOne`.
            ///
            /// * If `value` is in range, returns wrapped `value`.
            /// * If `value` is zero of either sign, returns wrapped positive zero.
            ///   This is lossy, but corresponds to the IEEE 754 idea that -0.0 == +0.0.
            /// * If `value` is out of range or NaN, panics.
            #[track_caller]
            #[inline]
            pub const fn new_strict(value: $t) -> Self {
                if value > 0. && value <= 1. {
                    Self(value)
                } else if value == 0. {
                    Self(0.)
                } else {
                    zero_one_out_of_range_panic()
                }
            }

            /// Wraps the given value in `ZeroOne`.
            ///
            /// * If the value is NaN, panics.
            /// * If the value is out of range, replaces it with the nearest in-range value
            ///   (0 or 1).
            #[track_caller]
            #[inline]
            pub const fn new_clamped(value: $t) -> Self {
                if value > 0. && value <= 1. {
                    // note > 0, which excludes negative zero
                    Self(value)
                } else if value <= 0. {
                    Self(0.)
                } else if value >= 1. {
                    Self(1.)
                } else {
                    zero_one_nan_panic()
                }
            }

            /// Const equivalent of `TryFrom::try_from()`.
            #[inline]
            pub(crate) const fn try_new(value: $t) -> Result<Self, NotZeroOne<$t>> {
                if value > <$t>::ZERO && value <= <$t>::ONE {
                    Ok(Self(value))
                } else if value == <$t>::ZERO {
                    // must be zero, not NaN, but we don’t know the sign
                    Ok(Self(<$t>::ZERO))
                } else {
                    Err(NotZeroOne(value))
                }
            }

            /// Unwraps the value without modifying it.
            // TODO: When #![feature(const_precise_live_drops)] becomes stable, we can make this generic.
            #[inline]
            pub const fn into_inner(self) -> $t {
                self.0
            }

            /// Const version of `self == ZeroOne::ZERO`
            #[inline]
            pub const fn is_zero(self) -> bool {
                self.0 == <$t>::ZERO
            }

            /// Const version of `self == ZeroOne::ONE`
            #[inline]
            pub const fn is_one(self) -> bool {
                self.0 == <$t>::ONE
            }
        }

        impl From<PositiveSign<$t>> for NotNan<$t> {
            #[inline]
            fn from(value: PositiveSign<$t>) -> Self {
                value.into_nn()
            }
        }
        impl From<PositiveSign<$t>> for $t {
            #[inline]
            fn from(value: PositiveSign<$t>) -> Self {
                value.0
            }
        }
        impl From<ZeroOne<$t>> for NotNan<$t> {
            #[inline]
            fn from(value: ZeroOne<$t>) -> Self {
                value.into_nn()
            }
        }
        impl From<ZeroOne<$t>> for $t {
            #[inline]
            fn from(value: ZeroOne<$t>) -> Self {
                value.0
            }
        }

        impl From<ZeroOne<$t>> for PositiveSign<$t> {
            #[inline]
            fn from(value: ZeroOne<$t>) -> Self {
                // Construction safety:
                // Valid `PositiveSign` values are a superset of valid `ZeroOne` values.
                PositiveSign(value.0)
            }
        }

        impl TryFrom<$t> for PositiveSign<$t> {
            type Error = NotPositiveSign<$t>;

            /// Checks that `value` is non-negative and non-NaN.
            ///
            /// * If `value` is positive (including positive infinity), returns wrapped `value`.
            /// * If `value` is zero of either sign, returns wrapped positive zero.
            ///   This is lossy, but corresponds to the IEEE 754 idea that -0.0 == +0.0.
            /// * If `value` is negative non-zero or NaN, returns an error.
            #[inline]
            fn try_from(value: $t) -> Result<Self, Self::Error> {
                Self::try_new(value)
            }
        }
        impl TryFrom<$t> for ZeroOne<$t> {
            type Error = NotZeroOne<$t>;

            /// Checks that `value` is within the range 0 to 1.
            ///
            /// * If `value` > 0 and `value` ≤ 1, returns wrapped `value`.
            /// * If `value` is zero of either sign, returns wrapped positive zero.
            ///   This is lossy, but corresponds to the IEEE 754 idea that -0.0 == +0.0.
            /// * If `value` is out of range or NaN, returns an error.
            #[inline]
            fn try_from(value: $t) -> Result<Self, Self::Error> {
                Self::try_new(value)
            }
        }
    };
}

// When f16 and f128 are stable, implement for those too.
non_generic_impls!(f32);
non_generic_impls!(f64);

// --- Generic trait implementations ---------------------------------------------------------------

impl<T: fmt::Debug> fmt::Debug for PositiveSign<T> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Don't print the wrappers, just the value.
        let value: &T = &self.0;
        value.fmt(f)
    }
}
impl<T: fmt::Display> fmt::Display for PositiveSign<T> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Don't print the wrappers, just the value.
        let value: &T = &self.0;
        value.fmt(f)
    }
}
impl<T: fmt::Debug> fmt::Debug for ZeroOne<T> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Don't print the wrappers, just the value.
        let value: &T = &self.0;
        value.fmt(f)
    }
}
impl<T: fmt::Display> fmt::Display for ZeroOne<T> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Don't print the wrappers, just the value.
        let value: &T = &self.0;
        value.fmt(f)
    }
}

// The derived PartialEq implementation is okay, but we need to add Eq.
impl<T: PartialEq> Eq for PositiveSign<T> {}
impl<T: PartialEq> Eq for ZeroOne<T> {}

#[allow(clippy::derive_ord_xor_partial_ord)]
impl<T: FloatCore + PartialOrd> Ord for PositiveSign<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        // Correctness: All values that would violate `Ord`’s properties are prohibited.
        self.partial_cmp(other).unwrap()
    }
}
#[allow(clippy::derive_ord_xor_partial_ord)]
impl<T: FloatCore + PartialOrd> Ord for ZeroOne<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        // Correctness: All values that would violate `Ord`’s properties are prohibited.
        self.partial_cmp(other).unwrap()
    }
}

impl<T: FloatCore> hash::Hash for PositiveSign<T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.into_nn().hash(state)
    }
}
impl<T: FloatCore> hash::Hash for ZeroOne<T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.into_nn().hash(state)
    }
}

impl<T: FloatCore + num_traits::Zero> Default for PositiveSign<T> {
    /// The default is zero, regardless of what `T::default()` is.
    #[inline]
    fn default() -> Self {
        Self(T::zero())
    }
}
impl<T: FloatCore + num_traits::Zero> Default for ZeroOne<T> {
    /// The default is zero, regardless of what `T::default()` is.
    #[inline]
    fn default() -> Self {
        Self(T::zero())
    }
}

impl<T: FloatCore + num_traits::Zero> num_traits::Zero for PositiveSign<T> {
    #[inline]
    fn zero() -> Self {
        Self(T::zero())
    }

    #[inline]
    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}
impl<T: FloatCore + num_traits::One> num_traits::One for PositiveSign<T> {
    #[inline]
    fn one() -> Self {
        Self(T::one())
    }
}
impl<T: FloatCore + num_traits::ConstZero> num_traits::ConstZero for PositiveSign<T> {
    const ZERO: Self = Self(T::ZERO);
}
impl<T: FloatCore + num_traits::ConstOne> num_traits::ConstOne for PositiveSign<T> {
    const ONE: Self = Self(T::ONE);
}
impl<T: FloatCore + num_traits::One> num_traits::One for ZeroOne<T> {
    #[inline]
    fn one() -> Self {
        Self(T::one())
    }
}
impl<T: FloatCore + num_traits::ConstOne> num_traits::ConstOne for ZeroOne<T> {
    const ONE: Self = Self(T::ONE);
}

impl<T: FloatCore + ops::Add<Output = T>> ops::Add for PositiveSign<T> {
    type Output = Self;
    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        // Construction safety:
        // If the nonnegative subset of T isn't closed under addition, the number type is
        // too weird to be useful, and probably doesn’t honestly implement `FloatCore` either.
        Self(self.0 + rhs.0)
    }
}
impl<T: FloatCore + ops::Add<Output = T>> ops::AddAssign for PositiveSign<T> {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl<T: FloatCore + ops::Mul<Output = T>> ops::Mul for PositiveSign<T> {
    type Output = Self;

    /// This multiplication operation differs from standard floating-point multiplication
    /// in that multiplying zero by positive infinity returns zero instead of NaN.
    /// This is necessary for the type to be closed under multiplication.
    #[inline]
    fn mul(self, rhs: Self) -> Self::Output {
        let value = self.0 * rhs.0;
        if value.is_nan() {
            Self(T::zero())
        } else {
            debug_assert!(value.is_sign_positive());
            Self(value)
        }
    }
}
impl<T: FloatCore + ops::Mul<Output = T>> ops::Mul for ZeroOne<T> {
    type Output = Self;
    #[inline]
    fn mul(self, rhs: Self) -> Self::Output {
        // Construction safety:
        //
        // If the in-range subset of T isn't closed under multiplication, the number type is
        // too weird to be useful, and probably doesn’t honestly implement `FloatCore` either.
        //
        // In particular, unlike for `PositiveSign`, NaN cannot result because Infinity is out
        // of range for the arguments.
        let value = self.0 * rhs.0;
        Self(value)
    }
}
impl<T: FloatCore + ops::Mul<Output = T>> ops::MulAssign for PositiveSign<T> {
    #[inline]
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}
impl<T: FloatCore + ops::Mul<Output = T>> ops::MulAssign for ZeroOne<T> {
    #[inline]
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

// Mixed-type multiplications
impl<T: FloatCore> ops::Mul<ZeroOne<T>> for PositiveSign<T>
where
    PositiveSign<T>: From<ZeroOne<T>>,
{
    type Output = PositiveSign<T>;
    #[inline]
    fn mul(self, rhs: ZeroOne<T>) -> Self::Output {
        self * PositiveSign::from(rhs)
    }
}
impl<T: FloatCore> ops::Mul<PositiveSign<T>> for ZeroOne<T>
where
    PositiveSign<T>: From<ZeroOne<T>>,
{
    type Output = PositiveSign<T>;
    #[inline]
    fn mul(self, rhs: PositiveSign<T>) -> Self::Output {
        PositiveSign::from(self) * rhs
    }
}

impl<T> AsRef<T> for PositiveSign<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &self.0
    }
}
impl<T> AsRef<T> for ZeroOne<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> TryFrom<NotNan<T>> for PositiveSign<T>
where
    Self: TryFrom<T, Error = NotPositiveSign<T>>,
{
    type Error = NotPositiveSign<T>;

    /// Checks that `value` is non-negative.
    ///
    /// * If `value` is positive (including positive infinity), returns wrapped `value`.
    /// * If `value` is zero of either sign, returns wrapped positive zero.
    ///   This is lossy, but corresponds to the IEEE 754 idea that -0.0 == +0.0.
    /// * If `value` is negative non-zero or NaN, returns an error.
    #[inline]
    fn try_from(value: NotNan<T>) -> Result<Self, Self::Error> {
        Self::try_from(value.into_inner())
    }
}

/// Unsigned integers can be infallibly converted to `PositiveSign`.
mod integer_to_positive_sign {
    use super::*;
    macro_rules! integer_to_positive_sign {
        ($int:ident, $float:ident) => {
            impl From<$int> for PositiveSign<$float> {
                #[inline]
                fn from(value: $int) -> PositiveSign<$float> {
                    PositiveSign(value.into())
                }
            }
        };
    }
    // This is almost just a list of `T: FloatCore + From<U>`,
    // but that would be open to weird adversarial implementations.
    integer_to_positive_sign!(bool, f32);
    integer_to_positive_sign!(bool, f64);
    integer_to_positive_sign!(u8, f32);
    integer_to_positive_sign!(u8, f64);
    integer_to_positive_sign!(u16, f32);
    integer_to_positive_sign!(u16, f64);
    integer_to_positive_sign!(u32, f64);
}

impl<T: FloatCore> From<bool> for ZeroOne<T> {
    #[inline]
    fn from(value: bool) -> Self {
        if value {
            ZeroOne(T::one())
        } else {
            ZeroOne(T::zero())
        }
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
#[allow(clippy::missing_inline_in_public_items)]
impl<'a, T> arbitrary::Arbitrary<'a> for PositiveSign<T>
where
    NotNan<T>: arbitrary::Arbitrary<'a> + ops::Neg<Output = NotNan<T>> + Copy,
    Self: TryFrom<NotNan<T>>,
{
    #[inline(never)]
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let nn = NotNan::<T>::arbitrary(u)?;
        Self::try_from(nn)
            .or_else(|_| Self::try_from(-nn))
            .map_err(|_| unreachable!("all floats are positive or negative"))
    }

    #[inline(never)]
    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        NotNan::<T>::size_hint(depth)
    }
}
#[cfg(feature = "arbitrary")]
#[mutants::skip]
#[allow(clippy::missing_inline_in_public_items)]
impl<'a, T> arbitrary::Arbitrary<'a> for ZeroOne<T>
where
    T: FloatCore,
    NotNan<T>: arbitrary::Arbitrary<'a> + ops::Neg<Output = NotNan<T>> + Copy,
    Self: TryFrom<T>,
{
    #[inline(never)]
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let value: T = NotNan::<T>::arbitrary(u)?.into_inner().abs();
        Self::try_from(value)
            // if it's greater than 1, try to make it less than 1
            .or_else(|_| Self::try_from(value.recip()))
            .map_err(|_| arbitrary::Error::IncorrectFormat)
    }

    #[inline(never)]
    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        NotNan::<T>::size_hint(depth)
    }
}

// --- Errors --------------------------------------------------------------------------------------

/// Error from attempting to construct a [`PositiveSign`].
#[derive(Clone, Debug)]
pub struct NotPositiveSign<T>(T);

/// Error from attempting to construct a [`ZeroOne`].
#[derive(Clone, Debug)]
pub struct NotZeroOne<T>(T);

impl<T: FloatCore + fmt::Display> fmt::Display for NotPositiveSign<T> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = self.0;
        if value.is_nan() {
            write!(f, "value was NaN")
        } else {
            write!(f, "{value} did not have a positive sign bit")
        }
    }
}

impl<T: FloatCore + fmt::Display> fmt::Display for NotZeroOne<T> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = self.0;
        if value <= T::zero() {
            write!(f, "{value} was less than zero")
        } else if value >= T::one() {
            write!(f, "{value} was greater than one")
        } else {
            write!(f, "value was NaN")
        }
    }
}

impl<T: FloatCore + fmt::Display + fmt::Debug> core::error::Error for NotPositiveSign<T> {}
impl<T: FloatCore + fmt::Display + fmt::Debug> core::error::Error for NotZeroOne<T> {}

#[track_caller]
#[cold]
const fn positive_sign_nan_panic() -> ! {
    panic!("PositiveSign value must not be NaN")
}

#[track_caller]
#[cold]
const fn positive_sign_not_positive_panic() -> ! {
    panic!("PositiveSign value must not be NaN or negative")
}

#[track_caller]
#[cold]
const fn zero_one_nan_panic() -> ! {
    panic!("ZeroOne value must not be NaN")
}

#[track_caller]
#[cold]
const fn zero_one_out_of_range_panic() -> ! {
    panic!("ZeroOne value must be between zero and one")
}

// -------------------------------------------------------------------------------------------------

/// Convenient alias for [`PositiveSign::<f32>::new_strict()`],
/// to be used in tests and pseudo-literals.
#[inline]
pub const fn ps32(value: f32) -> PositiveSign<f32> {
    PositiveSign::<f32>::new_strict(value)
}

/// Convenient alias for [`PositiveSign::<f64>::new_strict()`],
/// to be used in tests and pseudo-literals.
#[inline]
pub const fn ps64(value: f64) -> PositiveSign<f64> {
    PositiveSign::<f64>::new_strict(value)
}

/// Convenient alias for [`ZeroOne::<f32>::new_strict()`],
/// to be used in tests and pseudo-literals.
#[inline]
pub const fn zo32(value: f32) -> ZeroOne<f32> {
    ZeroOne::<f32>::new_strict(value)
}

/// Convenient alias for [`ZeroOne::<f64>::new_strict()`],
/// to be used in tests and pseudo-literals.
#[inline]
pub const fn zo64(value: f64) -> ZeroOne<f64> {
    ZeroOne::<f64>::new_strict(value)
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
#[allow(clippy::manual_range_contains)]
mod tests {
    use super::*;
    use alloc::string::ToString as _;
    use exhaust::Exhaust as _;

    #[test]
    fn ps_canonicalizes_negative_zero() {
        let was_nz = PositiveSign::<f32>::new_strict(-0.0);
        assert!(was_nz.into_inner().is_sign_positive());
        assert_eq!(was_nz.to_string(), "0");
        assert_eq!(was_nz, PositiveSign::<f32>::try_from(-0.0).unwrap())
    }

    #[test]
    fn ps_exhaustive() {
        for f in f32::exhaust() {
            match PositiveSign::<f32>::try_from(f) {
                Ok(ps) => {
                    ps.consistency_check();
                    assert_eq!(ps, PositiveSign::<f32>::new_clamped(f));
                    assert_eq!(ps, PositiveSign::<f32>::new_strict(f));
                    assert_eq!(ps.into_inner(), f);
                    assert_eq!(f32::from(ps), f);
                    assert_eq!(NotNan::from(ps), NotNan::new(f).unwrap());
                }
                Err(_) => assert!(f.is_nan() || f < 0.0),
            }
        }
    }

    #[test]
    fn ps_closed_under_multiplication() {
        assert_eq!(ps32(0.0) * PositiveSign::<f32>::INFINITY, ps32(0.0));
    }

    #[test]
    fn zo_canonicalizes_negative_zero() {
        let was_nz = ZeroOne::<f32>::new_strict(-0.0);
        assert!(was_nz.into_inner().is_sign_positive());
        assert_eq!(was_nz.to_string(), "0");
        assert_eq!(was_nz, ZeroOne::<f32>::try_from(-0.0).unwrap())
    }

    #[test]
    fn zo_exhaustive() {
        for f in f32::exhaust() {
            match ZeroOne::<f32>::try_from(f) {
                Ok(zo) => {
                    zo.consistency_check();
                    assert_eq!(zo, ZeroOne::<f32>::new_clamped(f));
                    assert_eq!(zo, ZeroOne::<f32>::new_strict(f));
                    assert_eq!(zo.into_inner(), f);
                    assert_eq!(f32::from(zo), f);
                    assert_eq!(NotNan::from(zo), NotNan::new(f).unwrap());
                }
                Err(_) => assert!(f.is_nan() || f < 0.0 || f > 1.0),
            }
        }
    }

    #[test]
    fn zo_complement() {
        assert_eq!(zo32(0.0).complement(), zo32(1.0));
        assert_eq!(zo32(1.0).complement(), zo32(0.0));

        // Also check the most extreme cases that aren’t exact
        for f in [
            f32::from_bits(0x00000001), // next up from 0
            f32::from_bits(0x3f7fffff), // next down from 1
        ] {
            zo32(f).consistency_check();
        }
    }

    // TODO: could use some edge-case tests for being closed under arithmetic ops,
    // or is that best handled as a fuzz test?
}

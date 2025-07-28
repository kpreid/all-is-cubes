use core::cmp::Ordering;
use core::error::Error;
use core::fmt;
use core::hash;
use core::iter;
use core::ops;

use num_traits::{ConstOne as _, ConstZero as _, float::FloatCore};
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
#[derive(Clone, Copy, PartialEq)]
pub struct PositiveSign<T>(T);

/// A floating-point number which is within the range +0 to +1 (inclusive).
///
/// This may be used for alpha blending, reflectance, lerps, and anything else where values
/// outside the range 0 to 1 is meaningless. It is closed under multiplication.
///
/// Because NaN and negative zero are excluded, this type implements [`Eq`] straightforwardly,
/// and can reliably be used as a map key for caching, interning, etc.
#[derive(Clone, Copy, PartialEq)]
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

    /// As `Self::new_clamped()` but generic in exchange for not being `const fn`.
    #[track_caller]
    #[inline]
    #[allow(clippy::eq_op)]
    fn new_clamped_generic_nonconst(value: T) -> Self {
        if value > T::zero() {
            Self(value)
        } else if value == value {
            Self(T::zero())
        } else {
            positive_sign_nan_panic()
        }
    }

    /// Unwraps the value without modifying it.
    #[inline]
    pub const fn into_inner(self) -> T {
        self.0
    }

    pub(crate) const fn into_nn(self) -> NotNan<T> {
        // SAFETY: `PositiveSign`’s restrictions are a superset of `NotNan`’s.
        unsafe { NotNan::new_unchecked(self.0) }
    }

    /// Returns the largest integer less than or equal to `self`.
    #[inline]
    #[must_use]
    pub fn floor(self) -> Self {
        // should never need to clamp, but to be safe...
        Self::new_clamped_generic_nonconst(FloatCore::floor(self.0))
    }

    /// Returns the smallest integer greater than or equal to `self`.
    #[inline]
    #[must_use]
    pub fn ceil(self) -> Self {
        Self::new_clamped_generic_nonconst(FloatCore::ceil(self.0))
    }

    /// Returns the nearest integer to `self`.
    /// If a value is half-way between two integers, round up from 0.0.
    #[inline]
    #[must_use]
    pub fn round(self) -> Self {
        Self::new_clamped_generic_nonconst(FloatCore::round(self.0))
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

    /// Convert to [`ZeroOne`], replacing too-large values with 1.0.
    #[inline]
    pub fn clamp_01(self) -> ZeroOne<T> {
        let one = T::one();
        if self.0 < one {
            ZeroOne(self.0)
        } else {
            ZeroOne(one)
        }
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

    /// Unwraps the value without modifying it.
    #[inline]
    pub const fn into_inner(self) -> T {
        self.0
    }

    pub(crate) const fn into_nn(self) -> NotNan<T> {
        // SAFETY: `ZeroOne`’s restrictions are a superset of `NotNan`’s.
        unsafe { NotNan::new_unchecked(self.0) }
    }

    pub(crate) const fn into_ps(self) -> PositiveSign<T> {
        // Construction safety:
        // Valid `PositiveSign` values are a superset of valid `ZeroOne` values.
        PositiveSign(self.0)
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

        impl const From<PositiveSign<$t>> for NotNan<$t> {
            #[inline]
            fn from(value: PositiveSign<$t>) -> Self {
                value.into_nn()
            }
        }
        impl const From<PositiveSign<$t>> for $t {
            #[inline]
            fn from(value: PositiveSign<$t>) -> Self {
                value.0
            }
        }
        impl const From<ZeroOne<$t>> for NotNan<$t> {
            #[inline]
            fn from(value: ZeroOne<$t>) -> Self {
                value.into_nn()
            }
        }
        impl const From<ZeroOne<$t>> for $t {
            #[inline]
            fn from(value: ZeroOne<$t>) -> Self {
                value.0
            }
        }

        impl const From<ZeroOne<$t>> for PositiveSign<$t> {
            #[inline]
            fn from(value: ZeroOne<$t>) -> Self {
                value.into_ps()
            }
        }

        impl const TryFrom<$t> for PositiveSign<$t> {
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
        impl const TryFrom<$t> for ZeroOne<$t> {
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

impl<T> core::str::FromStr for PositiveSign<T>
where
    T: core::str::FromStr<Err = core::num::ParseFloatError>
        + TryInto<Self, Error = NotPositiveSign<T>>,
{
    type Err = ParseOrRangeError<NotPositiveSign<T>>;

    #[allow(clippy::missing_inline_in_public_items)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<T>()
            .map_err(ParseOrRangeError::Parse)?
            .try_into()
            .map_err(ParseOrRangeError::Range)
    }
}
impl<T> core::str::FromStr for ZeroOne<T>
where
    T: core::str::FromStr<Err = core::num::ParseFloatError> + TryInto<Self, Error = NotZeroOne<T>>,
{
    type Err = ParseOrRangeError<NotZeroOne<T>>;

    #[allow(clippy::missing_inline_in_public_items)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<T>()
            .map_err(ParseOrRangeError::Parse)?
            .try_into()
            .map_err(ParseOrRangeError::Range)
    }
}

// The derived PartialEq implementation is okay, but we need to add Eq.
impl<T: PartialEq> Eq for PositiveSign<T> {}
impl<T: PartialEq> Eq for ZeroOne<T> {}

// Allow comparison with the contents.
// Note: These cannot generalize to `<T: Trait<U>, U> Trait<U> for PositiveSign<T>`,
// because that would have potential overlap.
impl<T: PartialEq> PartialEq<T> for PositiveSign<T> {
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.0 == *other
    }
}
impl<T: PartialEq> PartialEq<T> for ZeroOne<T> {
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.0 == *other
    }
}
impl<T: PartialOrd> PartialOrd<T> for PositiveSign<T> {
    #[inline]
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}
impl<T: PartialOrd> PartialOrd<T> for ZeroOne<T> {
    #[inline]
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}

// This impl could be derived, but making it explicit will help us ensure it has identical
// properties to  the `Ord` impl, and quiets `clippy::derive_ord_xor_partial_ord`.
#[allow(
    clippy::non_canonical_partial_ord_impl,
    reason = "that way would have a needless unwrap"
)]
impl<T: FloatCore + PartialOrd> PartialOrd for PositiveSign<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T: FloatCore + PartialOrd> Ord for PositiveSign<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        // Correctness: All values that would violate `Ord`’s properties are prohibited
        // by the constructors.
        match self.0.partial_cmp(&other.0) {
            Some(o) => o,
            None => invalid_ps_panic(),
        }
    }
}
// This impl could be derived, but making it explicit will help us ensure it has identical
// properties to  the `Ord` impl, and quiets `clippy::derive_ord_xor_partial_ord`.
#[allow(
    clippy::non_canonical_partial_ord_impl,
    reason = "that way would have a needless unwrap"
)]
impl<T: FloatCore + PartialOrd> PartialOrd for ZeroOne<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T: FloatCore + PartialOrd> Ord for ZeroOne<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        // Correctness: All values that would violate `Ord`’s properties are prohibited
        // by the constructors.
        match self.0.partial_cmp(&other.0) {
            Some(o) => o,
            None => invalid_zo_panic(),
        }
    }
}

impl<T: ordered_float::PrimitiveFloat> hash::Hash for PositiveSign<T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.into_nn().hash(state)
    }
}
impl<T: ordered_float::PrimitiveFloat> hash::Hash for ZeroOne<T> {
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

impl<T: FloatCore + iter::Sum> iter::Sum for PositiveSign<T>
where
    Self: TryFrom<T>,
{
    #[allow(clippy::missing_inline_in_public_items)]
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let inner_sum: T = <T as iter::Sum>::sum(iter.map(PositiveSign::<T>::into_inner));
        // Note: we could in principle use new_clamped here, but not new_unchecked,
        // because std `Sum` impls return -0.0 for empty lists!
        Self::try_from(inner_sum).unwrap_or_else(|_| panic!("sum returned non-positive result"))
    }
}
// cannot implement Sum for ZeroOne, but could implement Product

impl<T> const AsRef<T> for PositiveSign<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &self.0
    }
}
impl<T> const AsRef<T> for ZeroOne<T> {
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
            impl const From<$int> for PositiveSign<$float> {
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

    #[inline(never)]
    fn try_size_hint(
        depth: usize,
    ) -> arbitrary::Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        NotNan::<T>::try_size_hint(depth)
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

    #[inline(never)]
    fn try_size_hint(
        depth: usize,
    ) -> arbitrary::Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        NotNan::<T>::try_size_hint(depth)
    }
}

// --- Errors --------------------------------------------------------------------------------------

/// Error from attempting to construct a [`PositiveSign`].
#[derive(Clone, Debug, PartialEq)]
pub struct NotPositiveSign<T>(T);

/// Error from attempting to construct a [`ZeroOne`].
#[derive(Clone, Debug, PartialEq)]
pub struct NotZeroOne<T>(T);

/// Error from attempting to parse a [`PositiveSign`] or [`ZeroOne`] from a string.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::exhaustive_enums)]
pub enum ParseOrRangeError<R> {
    /// The string was not parseable.
    Parse(core::num::ParseFloatError),
    /// The value was recognized but is not in the correct range.
    Range(R),
}

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

impl<R: fmt::Display> fmt::Display for ParseOrRangeError<R> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseOrRangeError::Parse(e) => e.fmt(f),
            ParseOrRangeError::Range(e) => e.fmt(f),
        }
    }
}

impl<T: FloatCore + fmt::Display + fmt::Debug> Error for NotPositiveSign<T> {}
impl<T: FloatCore + fmt::Display + fmt::Debug> Error for NotZeroOne<T> {}
impl<R: Error> Error for ParseOrRangeError<R> {
    #[allow(clippy::missing_inline_in_public_items)]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ParseOrRangeError::Parse(e) => e.source(),
            ParseOrRangeError::Range(e) => e.source(),
        }
    }
}

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

#[track_caller]
#[cold]
const fn invalid_zo_panic() -> ! {
    panic!("an invalid ZeroOne value was encountered; this should be impossible")
}

#[track_caller]
#[cold]
const fn invalid_ps_panic() -> ! {
    panic!("an invalid PositiveSign value was encountered; this should be impossible")
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
    fn ps_ord_self() {
        assert_eq!(ps32(1.0).partial_cmp(&ps32(2.0)), Some(Ordering::Less));
        assert_eq!(ps32(2.0).partial_cmp(&ps32(2.0)), Some(Ordering::Equal));
        assert_eq!(ps32(3.0).partial_cmp(&ps32(2.0)), Some(Ordering::Greater));
        assert_eq!(ps32(1.0).cmp(&ps32(2.0)), Ordering::Less);
        assert_eq!(ps32(2.0).cmp(&ps32(2.0)), Ordering::Equal);
        assert_eq!(ps32(3.0).cmp(&ps32(2.0)), Ordering::Greater);
    }

    #[test]
    fn zo_ord_self() {
        assert_eq!(zo32(0.1).partial_cmp(&zo32(0.2)), Some(Ordering::Less));
        assert_eq!(zo32(0.2).partial_cmp(&zo32(0.2)), Some(Ordering::Equal));
        assert_eq!(zo32(0.3).partial_cmp(&zo32(0.2)), Some(Ordering::Greater));
        assert_eq!(zo32(0.1).cmp(&zo32(0.2)), Ordering::Less);
        assert_eq!(zo32(0.2).cmp(&zo32(0.2)), Ordering::Equal);
        assert_eq!(zo32(0.3).cmp(&zo32(0.2)), Ordering::Greater);
    }

    #[test]
    fn ps_ord_inner() {
        assert_eq!(ps32(1.0).partial_cmp(&2.0), Some(Ordering::Less));
        assert_eq!(ps32(2.0).partial_cmp(&2.0), Some(Ordering::Equal));
        assert_eq!(ps32(3.0).partial_cmp(&2.0), Some(Ordering::Greater));
        // There is no `Ord` implementation to test.

        // Since the RHS is not a `PositiveSign`, out-of-range values can occur.
        assert_eq!(ps32(1.0).partial_cmp(&-1.0), Some(Ordering::Greater));
        assert_eq!(ps32(1.0).partial_cmp(&f32::NAN), None);
    }

    #[test]
    fn zo_ord_inner() {
        assert_eq!(zo32(0.1).partial_cmp(&0.2), Some(Ordering::Less));
        assert_eq!(zo32(0.2).partial_cmp(&0.2), Some(Ordering::Equal));
        assert_eq!(zo32(0.3).partial_cmp(&0.2), Some(Ordering::Greater));
        // There is no `Ord` implementation to test.

        // Since the RHS is not a `ZeroOne`, out-of-range values can occur.
        assert_eq!(zo32(0.0).partial_cmp(&-1.0), Some(Ordering::Greater));
        assert_eq!(zo32(1.0).partial_cmp(&2.0), Some(Ordering::Less));
        assert_eq!(zo32(1.0).partial_cmp(&f32::NAN), None);
    }

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
    fn ps_floor() {
        assert_eq!(ps32(0.0).floor(), ps32(0.0));
        assert_eq!(ps32(0.25).floor(), ps32(0.0));
        assert_eq!(ps32(0.5).floor(), ps32(0.0));
        assert_eq!(ps32(0.75).floor(), ps32(0.0));
        assert_eq!(ps32(1.0).floor(), ps32(1.0));
        assert_eq!(ps32(1.5).floor(), ps32(1.0));
        assert_eq!(ps32(7.89).floor(), ps32(7.0));
    }

    #[test]
    fn ps_ceil() {
        assert_eq!(ps32(0.0).ceil(), ps32(0.0));
        assert_eq!(ps32(0.25).ceil(), ps32(1.0));
        assert_eq!(ps32(0.5).ceil(), ps32(1.0));
        assert_eq!(ps32(0.75).ceil(), ps32(1.0));
        assert_eq!(ps32(1.0).ceil(), ps32(1.0));
        assert_eq!(ps32(1.5).ceil(), ps32(2.0));
        assert_eq!(ps32(7.89).ceil(), ps32(8.0));
    }

    #[test]
    fn ps_round() {
        assert_eq!(ps32(0.0).round(), ps32(0.0));
        assert_eq!(ps32(0.25).round(), ps32(0.0));
        assert_eq!(ps32(0.5).round(), ps32(1.0));
        assert_eq!(ps32(0.75).round(), ps32(1.0));
        assert_eq!(ps32(1.0).round(), ps32(1.0));
        assert_eq!(ps32(1.5).round(), ps32(2.0));
        assert_eq!(ps32(7.89).round(), ps32(8.0));
    }

    #[test]
    fn ps_clamp() {
        assert_eq!(ps32(0.9).clamp_01(), zo32(0.9));
        assert_eq!(ps32(1.0).clamp_01(), zo32(1.0));
        assert_eq!(ps32(1.1).clamp_01(), zo32(1.0));
    }

    #[test]

    fn ps_sum() {
        assert_eq!(
            iter::empty::<PositiveSign<f32>>().sum::<PositiveSign<f32>>(),
            ps32(0.0)
        );
        assert_eq!(
            [ps32(0.0)].into_iter().sum::<PositiveSign<f32>>(),
            ps32(0.0)
        );
        assert_eq!(
            [ps32(1.0), ps32(2.0)].into_iter().sum::<PositiveSign<f32>>(),
            ps32(3.0)
        );
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

    /// Test `impl FromStr for PositiveSign` in cases equivalent to the underlying type.
    #[test]
    fn ps_parse_in_range_or_err() {
        for string in [
            "-0.0", "0.0", "", "1.0", "+1.0", "2.5", "1e100", "INF", "Infinity",
        ] {
            assert_eq!(
                string.parse::<f32>().map(|value| PositiveSign::try_from(value).unwrap()),
                string.parse::<PositiveSign<f32>>().map_err(|e| {
                    if let ParseOrRangeError::Parse(pe) = e {
                        pe
                    } else {
                        panic!("unexpected error {e:?}")
                    }
                }),
                "{string:?}",
            )
        }
    }

    /// Test that negative zero is accepted, but does not leak through.
    #[test]
    fn ps_parse_negative_zero_as_positive_zero() {
        let negative_zero = "-0.0".parse::<PositiveSign<f32>>().unwrap();
        assert_eq!(negative_zero, ps32(0.0));
        assert_eq!(negative_zero.into_inner().signum(), 1.0);
    }

    #[test]
    fn ps_parse_out_of_range() {
        assert_eq!(
            "-1.0".parse::<PositiveSign<f32>>(),
            Err(ParseOrRangeError::Range(NotPositiveSign(-1.0f32)))
        );
    }

    /// Test `impl FromStr for ZeroOne` in cases equivalent to the underlying type.
    #[test]
    fn zo_parse_in_range_or_err() {
        for string in ["-0.0", "0.0", "0.5", "2e-3", "1.0", "+1.0", "1e0"] {
            assert_eq!(
                string.parse::<f32>().map(|value| ZeroOne::try_from(value).unwrap()),
                string.parse::<ZeroOne<f32>>().map_err(|e| {
                    if let ParseOrRangeError::Parse(pe) = e {
                        pe
                    } else {
                        panic!("unexpected error {e:?}")
                    }
                }),
                "{string:?}",
            )
        }
    }

    /// Test that negative zero is accepted, but does not leak through.
    #[test]
    fn zo_parse_negative_zero_as_positive_zero() {
        let negative_zero = "-0.0".parse::<ZeroOne<f32>>().unwrap();
        assert_eq!(negative_zero, zo32(0.0));
        assert_eq!(negative_zero.into_inner().signum(), 1.0);
    }

    #[test]
    fn zo_parse_out_of_range() {
        assert_eq!(
            "-1.0".parse::<ZeroOne<f32>>(),
            Err(ParseOrRangeError::Range(NotZeroOne(-1.0f32)))
        );
        assert_eq!(
            "1.1".parse::<ZeroOne<f32>>(),
            Err(ParseOrRangeError::Range(NotZeroOne(1.1f32)))
        );
    }

    #[cfg(feature = "arbitrary")]
    #[test]
    fn arbitrary_size_hints() {
        use arbitrary::Arbitrary as _;

        assert_eq!(ZeroOne::<f32>::size_hint(0), (4, Some(4)));
        assert_eq!(ZeroOne::<f32>::try_size_hint(0).unwrap(), (4, Some(4)));
        assert_eq!(PositiveSign::<f32>::size_hint(0), (4, Some(4)));
        assert_eq!(PositiveSign::<f32>::try_size_hint(0).unwrap(), (4, Some(4)));
        assert_eq!(ZeroOne::<f64>::size_hint(0), (8, Some(8)));
        assert_eq!(ZeroOne::<f64>::try_size_hint(0).unwrap(), (8, Some(8)));
        assert_eq!(PositiveSign::<f64>::size_hint(0), (8, Some(8)));
        assert_eq!(PositiveSign::<f64>::try_size_hint(0).unwrap(), (8, Some(8)));
    }

    // TODO: could use some edge-case tests for being closed under arithmetic ops,
    // or is that best handled as a fuzz test?
}

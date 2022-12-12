//! Tools that we could imagine being in the Rust standard library, but aren't.

use std::error::Error;
use std::fmt::{self, Debug, Display};
use std::future::Future;
use std::marker::PhantomData;
use std::ops::AddAssign;
use std::sync::Arc;
use std::time::Duration;

use cgmath::{Matrix4, Point3, Vector2, Vector3, Vector4};
use futures_core::future::BoxFuture;
use instant::Instant;

/// Generic extension to [`std::fmt`'s set of formatting traits](std::fmt#formatting-traits).
///
/// This can be thought of as a mechanism to easily create a new special-purpose
/// formatting trait, analogous to [`std::fmt::LowerHex`] or [`std::fmt::Pointer`].
/// Instead of implementing the entire necessary wrapper and setup code, implementations
/// need only be types representing the choice of formatting (e.g. [`ConciseDebug`]),
/// and [`CustomFormat::custom_format`] provides a wrapper which may be used to cause
/// a value implementing `CustomFormat<T>` to be formatted using
/// [`CustomFormat<T>::fmt`](Self::fmt).
pub trait CustomFormat<F: Copy> {
    /// Wrap this value so that when formatted with [`Debug`] or [`Display`] it uses
    /// the given custom format instead.
    fn custom_format(&self, format_type: F) -> CustomFormatWrapper<'_, F, Self> {
        CustomFormatWrapper(format_type, self)
    }

    /// Implement this to provide custom formatting for this type.
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: F) -> fmt::Result;
}

/// You can use [`CustomFormat::custom_format`] to construct this.
/// See its documentation.
///
/// To enable using the wrapper inside [`assert_eq`], it implements [`PartialEq`]
/// (comparing both value and format).
#[derive(Eq, PartialEq)]
pub struct CustomFormatWrapper<'a, F: Copy, T: CustomFormat<F> + ?Sized>(F, &'a T);
impl<'a, F: Copy, T: CustomFormat<F>> Debug for CustomFormatWrapper<'a, F, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as CustomFormat<F>>::fmt(self.1, fmt, self.0)
    }
}
impl<'a, F: Copy, T: CustomFormat<F>> Display for CustomFormatWrapper<'a, F, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as CustomFormat<F>>::fmt(self.1, fmt, self.0)
    }
}

impl<F: Copy, T: CustomFormat<F>> CustomFormat<F> for &'_ T {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: F) -> fmt::Result {
        <T as CustomFormat<F>>::fmt(&**self, fmt, format_type)
    }
}

/// Format type for [`CustomFormat`] which forces a string to be unquoted when [`Debug`]ged.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub(crate) struct Unquote;
impl CustomFormat<Unquote> for String {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: Unquote) -> fmt::Result {
        write!(fmt, "{}", self)
    }
}
impl CustomFormat<Unquote> for &'_ str {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: Unquote) -> fmt::Result {
        write!(fmt, "{}", self)
    }
}

/// Format type for [`CustomFormat`] which prints the name of a type.
/// The value is a `PhantomData` to avoid requiring an actual instance of the type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct TypeName;
impl<T> CustomFormat<TypeName> for PhantomData<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: TypeName) -> fmt::Result {
        write!(fmt, "{}", std::any::type_name::<T>())
    }
}

/// Format type for [`CustomFormat`] which is similar to [`Debug`], but uses an
/// alternate concise format.
///
/// This format may be on one line despite the pretty-printing option, and may lose
/// precision or Rust syntax in favor of a short at-a-glance representation.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ConciseDebug;

impl<T: CustomFormat<ConciseDebug>, const N: usize> CustomFormat<ConciseDebug> for [T; N] {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: ConciseDebug) -> fmt::Result {
        fmt.debug_list()
            .entries(self.iter().map(|item| item.custom_format(format_type)))
            .finish()
    }
}

// TODO: Macro time?
impl<S: Debug> CustomFormat<ConciseDebug> for Point3<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}

impl<S: Debug> CustomFormat<ConciseDebug> for Matrix4<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(
            fmt,
            "\n[{:?},\n {:?},\n {:?},\n {:?}]",
            self.x.custom_format(ConciseDebug),
            self.y.custom_format(ConciseDebug),
            self.z.custom_format(ConciseDebug),
            self.w.custom_format(ConciseDebug)
        )
    }
}

impl<S: Debug> CustomFormat<ConciseDebug> for Vector2<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?})", self.x, self.y)
    }
}
impl<S: Debug> CustomFormat<ConciseDebug> for Vector3<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}
impl<S: Debug> CustomFormat<ConciseDebug> for Vector4<S> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        write!(
            fmt,
            "({:+.3?}, {:+.3?}, {:+.3?}, {:+.3?})",
            self.x, self.y, self.z, self.w
        )
    }
}

/// Format type for [`CustomFormat`] which provides an highly condensed, ideally
/// constant-size, user-facing format for live-updating textual status messages.
/// This format does not follow Rust [`Debug`](fmt::Debug) syntax, and when implemented
/// for standard Rust types may have quirks. Values may have multiple lines.
#[allow(clippy::exhaustive_structs)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct StatusText;

/// Makes the assumption that [`Duration`]s are per-frame timings and hence the
/// interesting precision is in the millisecond-to-microsecond range.
impl CustomFormat<StatusText> for Duration {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(fmt, "{:5.2?} ms", (self.as_micros() as f32) / 1000.0)
    }
}

/// Formatting wrapper which prints an [`Error`] together with its
/// `source()` chain, with at least one newline between each.
///
/// The text begins with the [`std::fmt::Display`] format of the error.
///
/// Design note: This is not a [`CustomFormat`] because that has a blanket implementation
/// which interferes with this one for [`Error`].
#[doc(hidden)] // not something we wish to be stable public API
#[derive(Clone, Copy, Debug)]
#[allow(clippy::exhaustive_structs)]
pub struct ErrorChain<'a>(pub &'a (dyn Error + 'a));

impl Display for ErrorChain<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_error_chain(fmt, self.0)
    }
}
fn format_error_chain(fmt: &mut fmt::Formatter<'_>, mut error: &(dyn Error + '_)) -> fmt::Result {
    // Write the error's own message. This is expected NOT to contain the sources itself.
    write!(fmt, "{error}")?;

    while let Some(source) = error.source() {
        error = source;
        write!(fmt, "\n\nCaused by:\n    {error}")?;
    }

    Ok(())
}

/// Equivalent of [`Iterator::map`] but applied to an [`Extend`] instead, transforming
/// the incoming elements.
///
/// TODO: this is only used by the wireframe debug mesh mechanism and should be reconsidered
#[doc(hidden)] // pub to be used by all-is-cubes-gpu
#[derive(Debug)]
pub struct MapExtend<'a, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
    target: &'a mut T,
    function: F,
    _input: PhantomData<fn(A)>,
}

impl<'a, A, B, T, F> MapExtend<'a, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
    pub fn new(target: &'a mut T, function: F) -> Self {
        Self {
            target,
            function,
            _input: PhantomData,
        }
    }
}

impl<'a, A, B, T, F> Extend<A> for MapExtend<'a, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = A>,
    {
        self.target.extend(iter.into_iter().map(&self.function));
    }
}

/// Allows a long-running async task to report its progress, and to yield to the
/// scheduler if appropriate in the current environment (i.e. web).
///
/// * TODO: This might end up being converted to a trait to allow `Sync` depending
///   on the executor's choice.
/// * TODO: We should probably have a non-async interface for progress reporting
///   once that's built.
pub struct YieldProgress {
    start: f32,
    end: f32,
    yielder: Arc<dyn Fn() -> BoxFuture<'static, ()> + Send + Sync>,
    progressor: Arc<dyn Fn(f32) + Send + Sync>,
}

impl Debug for YieldProgress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("YieldProgress")
            .field("start", &self.start)
            .field("end", &self.end)
            .finish_non_exhaustive()
    }
}

impl YieldProgress {
    pub fn new<Y, YFut, P>(yielder: Y, progressor: P) -> Self
    where
        Y: Fn() -> YFut + Send + Sync + 'static,
        YFut: Future<Output = ()> + Send + 'static,
        P: Fn(f32) + Send + Sync + 'static,
    {
        Self {
            start: 0.0,
            end: 1.0,
            yielder: Arc::new(move || Box::pin(yielder())),
            progressor: Arc::new(progressor),
        }
    }

    /// Returns a [`YieldProgress`] that does no progress reporting and no yielding.
    pub fn noop() -> Self {
        Self::new(|| std::future::ready(()), |_| {})
    }

    /// Map a `0..=1` value to `self.start..=self.end`.
    #[track_caller]
    fn point_in_range(&self, mut x: f32) -> f32 {
        x = x.clamp(0.0, 1.0);
        if !x.is_finite() {
            if cfg!(debug_assertions) {
                panic!("NaN progress value");
            } else {
                x = 0.5;
            }
        }
        self.start + (x * (self.end - self.start))
    }

    /// Report the current amount of progress (a number from 0 to 1) and yield.
    pub async fn progress(&self, progress_fraction: f32) {
        (self.progressor)(self.point_in_range(progress_fraction));
        (self.yielder)().await;
    }

    /// Report that the given amount of progress has been made, then return
    /// a [`YieldProgress`] covering the remaining range.
    pub async fn finish_and_cut(self, progress_fraction: f32) -> Self {
        let [a, b] = self.split(progress_fraction);
        a.progress(1.0).await;
        b
    }

    fn with_new_range(&self, start: f32, end: f32) -> Self {
        Self {
            start,
            end,
            yielder: Arc::clone(&self.yielder),
            progressor: Arc::clone(&self.progressor),
        }
    }

    /// Construct two new [`YieldProgress`] which divide the progress value into two
    /// subranges.
    ///
    /// The returned instances should be used in sequence, but this is not enforced.
    pub fn split(self, cut: f32) -> [Self; 2] {
        let cut_abs = self.point_in_range(cut);
        [
            self.with_new_range(self.start, cut_abs),
            self.with_new_range(cut_abs, self.end),
        ]
    }

    /// Split into even subdivisions.
    pub fn split_evenly(self, count: usize) -> impl Iterator<Item = YieldProgress> {
        assert!(count < usize::MAX);
        (0..count).map(move |index| {
            self.with_new_range(
                self.point_in_range(index as f32 / count as f32),
                self.point_in_range((index + 1) as f32 / count as f32),
            )
        })
    }
}

/// Aggregation of the time taken by a set of events.
///
/// TODO: Consider including an identifier for the longest.
/// TODO: Consider generalizing this to quantities other than time? Probably not.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
pub struct TimeStats {
    pub count: usize,
    pub sum: Duration,
    pub min: Option<Duration>,
    pub max: Duration,
}

impl TimeStats {
    pub const fn one(duration: Duration) -> Self {
        Self {
            count: 1,
            sum: duration,
            min: Some(duration),
            max: duration,
        }
    }

    /// Record an event based on the given previous time and current time, then update
    /// the previous time value.
    ///
    /// Returns the duration that was recorded.
    pub(crate) fn record_consecutive_interval(
        &mut self,
        last_marked_instant: &mut Instant,
        now: Instant,
    ) -> Duration {
        let previous = *last_marked_instant;
        *last_marked_instant = now;

        let duration = now.duration_since(previous);
        *self += Self::one(duration);
        duration
    }
}

impl AddAssign for TimeStats {
    fn add_assign(&mut self, rhs: Self) {
        *self = TimeStats {
            count: self.count + rhs.count,
            sum: self.sum + rhs.sum,
            min: self.min.map_or(rhs.min, |value| Some(value.min(rhs.min?))),
            max: self.max.max(rhs.max),
        };
    }
}

impl Display for TimeStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.min {
            None => write!(
                f,
                "(-------- .. {}) for {:3}, total {}",
                self.max.custom_format(StatusText),
                self.count,
                self.sum.custom_format(StatusText),
            ),
            Some(min) => write!(
                f,
                "({} .. {}) for {:3}, total {}",
                min.custom_format(StatusText),
                self.max.custom_format(StatusText),
                self.count,
                self.sum.custom_format(StatusText),
            ),
        }
    }
}

#[cfg(test)]
pub(crate) fn assert_send_sync<T: Send + Sync>() {
    // We don't need to do anything in this function; the call to it having been successfully
    // compiled is the assertion.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_concise_debug() {
        #[derive(Debug)]
        struct Foo;
        impl CustomFormat<ConciseDebug> for Foo {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
                write!(fmt, "<Foo>")
            }
        }
        assert_eq!("Foo", format!("{:?}", Foo));
        assert_eq!("<Foo>", format!("{:?}", Foo.custom_format(ConciseDebug)));
    }

    #[test]
    fn error_chain() {
        #[derive(Debug, thiserror::Error)]
        #[error("TestError1")]
        struct TestError1;
        #[derive(Debug, thiserror::Error)]
        #[error("TestError2")]
        struct TestError2(#[source] TestError1);

        assert_eq!(
            format!("{}", ErrorChain(&TestError2(TestError1))),
            "TestError2\n\nCaused by:\n    TestError1"
        );
    }

    #[test]
    fn yield_progress_is_sync() {
        assert_send_sync::<YieldProgress>()
    }
}

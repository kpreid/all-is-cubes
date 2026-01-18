use core::iter::{Sum, empty, once};

use rayon::iter::{IntoParallelIterator, ParallelExtend, ParallelIterator as _};

/// Implements [`ParallelExtend`] to just sum things, so that
/// [`ParallelIterator::unzip`] can produce a sum.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct ParExtSum<T>(Option<T>);

impl<T: Sum> ParExtSum<T> {
    pub fn result(self) -> T {
        self.0.unwrap_or_else(|| empty().sum())
    }
}

impl<T: Sum + Send> ParallelExtend<T> for ParExtSum<T> {
    fn par_extend<I>(&mut self, par_iter: I)
    where
        I: IntoParallelIterator<Item = T>,
    {
        let new = par_iter.into_par_iter().sum();
        // The reason we use an `Option` at all is to make it possible to move the current
        // value.
        self.0 = Some(match self.0.take() {
            None => new,
            Some(previous) => once(previous).chain(once(new)).sum(),
        });
    }
}

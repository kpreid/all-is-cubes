use alloc::sync::{Arc, Weak};
use core::fmt;

use manyfmt::Refmt as _;
use manyfmt::formats::Unquote;

use crate::listen::Listener;

// -------------------------------------------------------------------------------------------------

/// A [`Listener`] which delivers messages by calling a function on a [`Weak`] reference's
/// referent, and stops when the weak reference breaks.
#[derive(Clone)]
pub struct FnListener<F, T> {
    function: F,
    weak_target: Weak<T>,
}

impl<F, T> FnListener<F, T> {
    #[allow(missing_docs)]
    pub fn new(target: &Arc<T>, function: F) -> Self {
        Self {
            function,
            weak_target: Arc::downgrade(target),
        }
    }
}

impl<F, T> fmt::Debug for FnListener<F, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FnListener")
            // function's type name may be the function name
            .field("function", &core::any::type_name::<F>().refmt(&Unquote))
            // not useful to print weak_target unless we were to upgrade and lock it
            .field("alive", &(self.weak_target.strong_count() > 0))
            .finish()
    }
}

impl<M, F, T> Listener<M> for FnListener<F, T>
where
    F: Fn(&T, &M) + Send + Sync,
    T: Send + Sync,
{
    fn receive(&self, messages: &[M]) -> bool {
        if let Some(strong_target) = self.weak_target.upgrade() {
            // TODO: Review whether changing FnListener to pass on the slice will be useful
            for message in messages {
                (self.function)(&*strong_target, message);
            }
            true
        } else {
            false
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(fmt_debug = "full")]
    #[test]
    fn fn_debug() {
        let listener = FnListener::new(&Arc::new(()), |_recipient: &(), _msg: ()| {});
        assert_eq!(
            format!("{listener:#?}"),
            indoc::indoc! { "
                FnListener {
                    function: all_is_cubes::listen::listeners::tests::fn_debug::{{closure}},
                    alive: false,
                }\
            " }
        );
    }
}

#![allow(
    clippy::module_name_repetitions,
    reason = "false positive; TODO: remove after Rust 1.84 is released"
)]

use core::fmt;

use alloc::sync::Arc;

use crate::listen::{self, Listen, Notifier};
use crate::util::maybe_sync::{Mutex, MutexGuard};

/// A interior-mutable container for a value which can notify that the value changed.
///
/// Access to the value requires cloning it, so if the clone is not cheap,
/// consider wrapping the value with [`Arc`] to reduce the cost to reference count changes.
pub struct ListenableCell<T> {
    storage: Arc<ListenableCellStorage<T>>,
}
/// Access to a value that might change (provided by a [`ListenableCell`]) or be [a
/// constant](ListenableSource::constant), and which can be listened to.
pub struct ListenableSource<T> {
    storage: Arc<ListenableCellStorage<T>>,
}
struct ListenableCellStorage<T> {
    /// The current value.
    cell: Mutex<T>,

    /// Notifier to track listeners.
    /// `None` if this is a constant cell.
    ///
    /// TODO: Add ability to diff the value and distribute that.
    /// TODO: If the `ListenableCell` is dropped, clear this to denote that nothing will ever
    /// be sent again.
    notifier: Option<Notifier<()>>,
}

impl<T: Clone> ListenableCell<T> {
    /// Creates a new [`ListenableCell`] containing the given value.
    pub fn new(value: T) -> Self {
        Self {
            storage: Arc::new(ListenableCellStorage {
                cell: Mutex::new(value),
                notifier: Some(Notifier::new()),
            }),
        }
    }

    /// Returns a reference to the current value of the cell.
    pub fn get(&self) -> T {
        self.storage.cell.lock().unwrap().clone()
    }

    /// Sets the contained value and sends out a change notification.
    ///
    /// Note that this does not test whether the current value is equal to avoid redundant
    /// notifications.
    ///
    /// Caution: While listeners are *expected* not to have immediate side effects on
    /// notification, this cannot be enforced.
    pub fn set(&self, value: T) {
        *self.storage.cell.lock().unwrap() = value;
        self.storage
            .notifier
            .as_ref()
            .expect("can't happen: set() on a constant cell")
            .notify(&());
    }

    /// Sets the contained value to the given value iff they are unequal.
    ///
    /// This avoids sending change notifications in the case where
    ///
    /// Caution: This executes `PartialEq::eq()` with the lock held; this may delay readers of
    /// the value, or cause permanent failure in the event of a panic.
    #[doc(hidden)] // TODO: good public API?
    pub fn set_if_unequal(&self, value: T)
    where
        T: PartialEq,
    {
        let mut guard: MutexGuard<'_, T> = self.storage.cell.lock().unwrap();
        if value == *guard {
            return;
        }

        *guard = value;

        // Don't hold the lock while notifying.
        // Listeners shouldn't be trying to read immediately, but we don't want to create
        // this deadlock opportunity regardless.
        drop(guard);

        self.storage
            .notifier
            .as_ref()
            .expect("can't happen: set() on a constant cell")
            .notify(&());
    }

    /// Sets the contained value by modifying a clone of the old value using the provided
    /// function.
    ///
    /// Note: this function is not atomic, in that other modifications can be made between
    /// the time this function reads the current value and writes the new one. It is not any more
    /// powerful than calling `get()` followed by `set()`.
    pub fn update_mut<F: FnOnce(&mut T)>(&self, f: F) {
        let mut value = self.get();
        f(&mut value);
        self.set(value);
    }

    /// Returns a [`ListenableSource`] which provides read-only access to the value
    /// managed by this cell.
    pub fn as_source(&self) -> ListenableSource<T> {
        ListenableSource {
            storage: self.storage.clone(),
        }
    }
}

impl<T: Clone> ListenableSource<T> {
    /// Creates a new [`ListenableSource`] containing the given value, which will
    /// never change.
    pub fn constant(value: T) -> Self {
        Self {
            storage: Arc::new(ListenableCellStorage {
                cell: Mutex::new(value),
                notifier: None,
            }),
        }
    }

    /// Returns a clone of the current value of the cell.
    pub fn get(&self) -> T {
        T::clone(&*self.storage.cell.lock().unwrap())
    }
}

impl<T> Clone for ListenableSource<T> {
    fn clone(&self) -> Self {
        Self {
            storage: Arc::clone(&self.storage),
        }
    }
}

impl<T> Listen for ListenableSource<T> {
    type Msg = ();

    fn listen_raw(&self, listener: listen::DynListener<Self::Msg>) {
        if let Some(notifier) = &self.storage.notifier {
            notifier.listen_raw(listener);
        }
    }
}

/// Convenience wrapper around [`ListenableCell`] which allows borrowing the current
/// value, at the cost of requiring `&mut` access to set it, and storing a clone.
#[doc(hidden)] // TODO: decide if good API -- currently used by all_is_cubes_gpu
pub struct ListenableCellWithLocal<T> {
    cell: ListenableCell<T>,
    value: T,
}

impl<T: Clone> ListenableCellWithLocal<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: value.clone(),
            cell: ListenableCell::new(value),
        }
    }

    pub fn set(&mut self, value: T) {
        self.cell.set(value.clone());
        self.value = value;
    }

    #[expect(clippy::should_implement_trait)] // TODO: consider renaming
    pub fn borrow(&self) -> &T {
        &self.value
    }

    /// Returns a [`ListenableSource`] which provides read-only access to the value
    /// managed by this cell.
    pub fn as_source(&self) -> ListenableSource<T> {
        self.cell.as_source()
    }
}

impl<T: Clone + fmt::Debug> fmt::Debug for ListenableCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = f.debug_struct("ListenableCell");
        ds.field("value", &self.get());
        format_cell_metadata(&mut ds, &self.storage);
        ds.finish()
    }
}
impl<T: Clone + fmt::Debug> fmt::Debug for ListenableSource<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = f.debug_struct("ListenableSource");
        ds.field("value", &self.get());
        format_cell_metadata(&mut ds, &self.storage);
        ds.finish()
    }
}
impl<T: fmt::Debug> fmt::Debug for ListenableCellWithLocal<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = f.debug_struct("ListenableCellWithLocal");
        ds.field("value", &self.value);
        format_cell_metadata(&mut ds, &self.cell.storage);
        ds.finish()
    }
}

// Pointer printing implementations to enable determining whether a cell and a source share
// state. Including the debug_struct to make it less ambiguous what role this pointer plays.
impl<T> fmt::Pointer for ListenableCell<T> {
    /// Prints the address of the cell's state storage, which is shared with
    /// [`ListenableSource`]s created from this cell.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = f.debug_struct("ListenableCell");
        ds.field("cell_address", &Arc::as_ptr(&self.storage));
        ds.finish()
    }
}
impl<T> fmt::Pointer for ListenableSource<T> {
    /// Prints the address of the state storage, which is shared with the originating
    /// [`ListenableCell`] and other [`ListenableSource`]s.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = f.debug_struct("ListenableSource");
        ds.field("cell_address", &Arc::as_ptr(&self.storage));
        ds.finish()
    }
}
impl<T> fmt::Pointer for ListenableCellWithLocal<T> {
    /// Prints the address of the cell's state storage, which is shared with
    /// [`ListenableSource`]s created from this cell.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = f.debug_struct("ListenableCellWithLocal");
        ds.field("cell_address", &Arc::as_ptr(&self.cell.storage));
        ds.finish()
    }
}

fn format_cell_metadata<T>(
    ds: &mut fmt::DebugStruct<'_, '_>,
    storage: &Arc<ListenableCellStorage<T>>,
) {
    ds.field("owners", &Arc::strong_count(storage));
    if let Some(notifier) = &storage.notifier {
        ds.field("listeners", &notifier.count());
    } else {
        ds.field("constant", &true);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::listen::Sink;
    use alloc::vec::Vec;
    use pretty_assertions::assert_eq;

    #[test]
    fn listenable_cell_and_source_debug() {
        let cell = ListenableCell::<Vec<&str>>::new(vec!["hi"]);
        let source = cell.as_source();
        assert_eq!(
            format!("{cell:#?}"),
            indoc::indoc! {
                r#"ListenableCell {
                    value: [
                        "hi",
                    ],
                    owners: 2,
                    listeners: 0,
                }"#
            }
        );
        assert_eq!(
            format!("{source:#?}"),
            indoc::indoc! {
               r#"ListenableSource {
                    value: [
                        "hi",
                    ],
                    owners: 2,
                    listeners: 0,
                }"#
            }
        );
    }

    #[test]
    fn constant_source_debug() {
        let source = ListenableSource::constant(vec!["hi"]);
        assert_eq!(
            format!("{source:#?}"),
            indoc::indoc! {
               r#"ListenableSource {
                    value: [
                        "hi",
                    ],
                    owners: 1,
                    constant: true,
                }"#
            }
        );
    }

    #[test]
    fn listenable_cell_usage() {
        let cell = ListenableCell::new(0);

        let s = cell.as_source();
        let sink = Sink::new();
        s.listen(sink.listener());

        assert_eq!(sink.drain(), vec![]);
        cell.set(1);
        assert_eq!(1, s.get());
        assert_eq!(sink.drain(), vec![()]);
    }

    #[test]
    fn constant_source_usage() {
        let s = ListenableSource::constant(123);
        assert_eq!(s.get(), 123);
        s.listen(Sink::new().listener()); // no panic
    }

    #[test]
    fn listenable_source_clone() {
        let cell = ListenableCell::new(0);
        let s = cell.as_source();
        let s = s.clone();
        cell.set(1);
        assert_eq!(s.get(), 1);
    }
}

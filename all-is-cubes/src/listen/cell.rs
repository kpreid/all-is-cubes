use std::sync::{Arc, Mutex};

use crate::listen::{Listen, Listener, Notifier};

/// A interior-mutable container for a value which can notify that the value changed,
/// and which has reference-counted read-only handles to read it.
#[derive(Debug)]
pub struct ListenableCell<T> {
    storage: Arc<ListenableCellStorage<T>>,
}
/// Access to a value that might change (provided by a [`ListenableCell`]) or be [a
/// constant](ListenableSource::constant), and which can be listened to.
#[derive(Clone, Debug)]
pub struct ListenableSource<T> {
    storage: Arc<ListenableCellStorage<T>>,
}
#[derive(Debug)]
struct ListenableCellStorage<T> {
    /// Mutex because it's mutable; Arc because we want to be able to clone out of it to
    /// avoid holding the cell borrowed.
    /// TODO: Look into strategies to make this cheaper?
    cell: Mutex<Arc<T>>,

    /// Notifier to track listeners.
    /// `None` if this is a constant cell.
    ///
    /// TODO: Add ability to diff the value and distribute that.
    /// TODO: If the ListenableCell is dropped, drop this.
    notifier: Option<Notifier<()>>,
}

impl<T: Sync> ListenableCell<T> {
    /// Creates a new [`ListenableCell`] containing the given value.
    pub fn new(value: impl Into<Arc<T>>) -> Self {
        Self {
            storage: Arc::new(ListenableCellStorage {
                cell: Mutex::new(value.into()),
                notifier: Some(Notifier::new()),
            }),
        }
    }

    /// Returns a reference to the current value of the cell.
    pub fn get(&self) -> Arc<T> {
        self.storage.cell.lock().unwrap().clone()
    }

    /// Sets the contained value and sends out a change notification.
    ///
    /// Note that this does not test whether the current value is equal to avoid redundant
    /// notifications.
    ///
    /// Caution: While listeners are *expected* not to have immediate side effects on
    /// notification, this cannot be enforced.
    pub fn set(&self, value: impl Into<Arc<T>>) {
        *self.storage.cell.lock().unwrap() = value.into();
        self.storage
            .notifier
            .as_ref()
            .expect("can't happen: set() on a constant cell")
            .notify(());
    }

    /// Sets the contained value by modifying a clone of the old value using the provided
    /// function.
    ///
    /// Note: this function is not atomic, in that other modifications can be made between
    /// the time this function reads the current value and writes the new one.
    pub fn update_mut<F>(&self, f: F)
    where
        T: Clone,
        F: FnOnce(&mut T),
    {
        let mut arc = self.get();
        f(Arc::make_mut(&mut arc));
        self.set(arc);
    }

    /// Returns a [`ListenableSource`] which provides read-only access to the value
    /// managed by this cell.
    pub fn as_source(&self) -> ListenableSource<T> {
        ListenableSource {
            storage: self.storage.clone(),
        }
    }
}

impl<T: Clone + Sync> ListenableSource<T> {
    /// Creates a new [`ListenableSource`] containing the given value, which will
    /// never change.
    pub fn constant(value: T) -> Self {
        Self {
            storage: Arc::new(ListenableCellStorage {
                cell: Mutex::new(Arc::new(value)),
                notifier: None,
            }),
        }
    }

    /// Returns a reference to the current value of the cell.
    // TODO: Consider storing a 'local' copy of the Rc so we can borrow it rather than cloning the Arc every time?
    pub fn get(&self) -> Arc<T> {
        Arc::clone(&*self.storage.cell.lock().unwrap())
    }

    /// Returns a clone of the current value of the cell.
    pub fn snapshot(&self) -> T {
        // TODO: This was originally written to avoid cloning the Rc if cloning the value is the final goal, but under threading we don't want to hold the lock unnecessarily or possibly cause it to be poisoned due to the clone operation panicking. What's the best option? Should this method just be deleted?
        T::clone(&*self.get())
    }
}

impl<T: Clone + Sync> Listen for ListenableSource<T> {
    type Msg = ();

    fn listen<L: Listener<Self::Msg> + Send + Sync + 'static>(&self, listener: L) {
        if let Some(notifier) = &self.storage.notifier {
            notifier.listen(listener);
        }
    }
}

/// Convenience wrapper around [`ListenableCell`] which allows borrowing the current
/// value, at the cost of requiring `&mut` access to set it.
#[derive(Debug)] // TODO: custom format ?
#[doc(hidden)] // TODO: decide if good API -- currently used by all_is_cubes_gpu
pub struct ListenableCellWithLocal<T> {
    cell: ListenableCell<T>,
    value: Arc<T>,
}

impl<T: Sync> ListenableCellWithLocal<T> {
    pub fn new(value: impl Into<Arc<T>>) -> Self {
        let value = value.into();
        Self {
            value: value.clone(),
            cell: ListenableCell::new(value),
        }
    }

    pub fn set(&mut self, value: impl Into<Arc<T>>) {
        let value = value.into();
        self.cell.set(Arc::clone(&value));
        self.value = value;
    }

    #[allow(clippy::should_implement_trait)] // TODO: consider renaming
    pub fn borrow(&self) -> &T {
        &self.value
    }

    /// Returns a [`ListenableSource`] which provides read-only access to the value
    /// managed by this cell.
    pub fn as_source(&self) -> ListenableSource<T> {
        self.cell.as_source()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::listen::Sink;

    #[test]
    fn listenable_cell() {
        let cell = ListenableCell::new(0);

        let s = cell.as_source();
        let sink = Sink::new();
        s.listen(sink.listener());

        assert_eq!(sink.drain(), vec![]);
        cell.set(1);
        assert_eq!(1, *s.get());
        assert_eq!(sink.drain(), vec![()]);
    }

    #[test]
    fn listenable_source_constant() {
        let s = ListenableSource::constant(123);
        assert_eq!(*s.get(), 123);
        s.listen(Sink::new().listener()); // no panic
    }

    #[test]
    fn listenable_source_clone() {
        let cell = ListenableCell::new(0);
        let s = cell.as_source();
        let s = s.clone();
        cell.set(1);
        assert_eq!(*s.get(), 1);
    }
}

use alloc::collections::BTreeMap;
use alloc::vec::Vec;

use crate::universe::{self, Handle, Universe};

/// A subset of the [`Handle`]s in one universe.
///
/// This structure is not currently publicly documented because it is a helper for
/// `all_is_cubes_port::ExportSet` and doesn't play a role in the API itself.
#[doc(hidden)]
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct HandleSet {
    /// Invariants:
    ///
    /// * The handles must all belong to the same universe.
    /// * The handles’ names are the corresponding map keys.
    handles: BTreeMap<universe::Name, universe::AnyHandle>,
}

impl HandleSet {
    pub fn all_of(universe: &Universe) -> Self {
        Self {
            handles: universe.iter().map(|handle| (handle.name(), handle)).collect(),
        }
    }

    pub fn len(&self) -> usize {
        self.handles.len()
    }

    pub fn is_empty(&self) -> bool {
        self.handles.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &universe::AnyHandle> {
        self.handles.values()
    }

    /// Removes every `Handle<T>` from this set and returns them.
    pub fn extract_type<T: universe::UniverseMember>(&mut self) -> Vec<Handle<T>> {
        self.handles
            .extract_if(.., |_, handle| handle.downcast_ref::<T>().is_ok())
            .map(|(_, handle)| handle.downcast::<T>().unwrap_or_else(|_| unreachable!()))
            .collect()
    }
}

impl<H: Into<universe::AnyHandle>> FromIterator<H> for HandleSet {
    /// Creates a [`HandleSet`] from handles.
    ///
    /// # Panics
    ///
    /// Panics if the handles are not all from the same universe.
    #[track_caller]
    fn from_iter<T: IntoIterator<Item = H>>(iter: T) -> Self {
        let mut common_universe_id = None;
        let handles = iter
            .into_iter()
            .map(|handle| {
                let handle = handle.into();
                match (common_universe_id, handle.universe_id()) {
                    (Some(all), Some(this)) if all == this => {}
                    (Some(_), Some(_)) => {
                        panic!("a HandleSet may not contain handles in multiple universes")
                    }
                    (None, Some(this)) => common_universe_id = Some(this),
                    (_, None) if matches!(handle.name(), universe::Name::Builtin(_)) => {
                        // Builtin handles are always permitted.
                        // TODO: Find a way to express that in general (e.g. `Handle::universe_id()`
                        // returning more detail than an `Option`), rather than needing a rule here.
                    }
                    (_, None) => panic!(
                        "handles in a HandleSet must be in a universe, but {handle:?} was not",
                        // deref so as not to print the `AnyHandle` wrapper
                        handle = &*handle
                    ),
                }

                (handle.name(), handle)
            })
            .collect();
        Self { handles }
    }
}

/// A subset of the [`Handle`]s in one universe, that may be serialized as if it was a [`Universe`].
#[doc(hidden)] // public to allow all-is-cubes-port to do exports
#[derive(Clone, Debug, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct PartialUniverse<'t> {
    // TODO: design API that doesn't rely on making these public, but still allows
    // exports to be statically exhaustive.
    pub read_ticket: universe::ReadTicket<'t>,
    pub handles: HandleSet,
}

impl<'t> PartialUniverse<'t> {
    pub fn all_of(universe: &'t Universe) -> Self {
        Self {
            read_ticket: universe.read_ticket(),
            handles: HandleSet::all_of(universe),
        }
    }
}

impl Default for PartialUniverse<'_> {
    fn default() -> Self {
        Self {
            read_ticket: universe::ReadTicket::stub(),
            handles: HandleSet::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block;
    use crate::universe::{ErasedHandle, Name};
    use alloc::vec;

    fn test_block_def() -> block::BlockDef {
        block::BlockDef::new(universe::ReadTicket::stub(), block::AIR)
    }
    fn names(set: &HandleSet) -> Vec<Name> {
        set.handles.keys().cloned().collect()
    }

    #[test]
    fn handle_set_extract_type() {
        let mut universe = Universe::new();
        let foo = universe.insert("foo".into(), test_block_def()).unwrap();
        let bar = universe.insert("bar".into(), crate::tag::TagDef).unwrap();
        let mut handle_set = HandleSet::all_of(&universe);

        let blocks = handle_set.extract_type::<block::BlockDef>();
        assert_eq!(blocks, vec![foo]);
        let tags = handle_set.extract_type::<crate::tag::TagDef>();
        assert_eq!(tags, vec![bar]);
        assert!(handle_set.is_empty());
    }

    #[test]
    fn handle_set_may_have_builtins_only() {
        // This call should not panic.
        let handle_set =
            HandleSet::from_iter([universe::Builtin::beep(), universe::Builtin::thump()]);

        // Also check the contents
        assert_eq!(
            names(&handle_set),
            vec![
                Name::Builtin(universe::Builtin::Beep),
                Name::Builtin(universe::Builtin::Thump)
            ]
        );
    }

    #[test]
    fn handle_set_may_have_builtins_first() {
        let mut universe = Universe::new();
        let foo = universe.insert("foo".into(), test_block_def()).unwrap();

        // This call should not panic.
        let handle_set = HandleSet::from_iter([
            universe::Builtin::beep().to_any_handle(),
            foo.to_any_handle(),
        ]);

        assert_eq!(
            names(&handle_set),
            vec![foo.name(), Name::Builtin(universe::Builtin::Beep)]
        );
    }

    #[test]
    fn handle_set_may_have_builtins_second() {
        let mut universe = Universe::new();
        let foo = universe.insert("foo".into(), test_block_def()).unwrap();

        // This call should not panic.
        let handle_set = HandleSet::from_iter([
            foo.to_any_handle(),
            universe::Builtin::beep().to_any_handle(),
        ]);

        assert_eq!(
            names(&handle_set),
            vec![foo.name(), Name::Builtin(universe::Builtin::Beep)]
        );
    }

    #[test]
    #[should_panic = "a HandleSet may not contain handles in multiple universes"]
    fn handle_set_may_not_have_two_universes() {
        let mut universe1 = Universe::new();
        let mut universe2 = Universe::new();
        let foo = universe1.insert("foo".into(), test_block_def()).unwrap();
        let bar = universe2.insert("bar".into(), test_block_def()).unwrap();

        _ = HandleSet::from_iter([foo.to_any_handle(), bar.to_any_handle()]);
    }

    #[test]
    #[should_panic = "handles in a HandleSet must be in a universe, but Handle('foo' in no universe) was not"]
    fn handle_set_may_not_have_pending() {
        let (handle, _txn) = universe::UniverseTransaction::insert(
            "foo".into(),
            block::BlockDef::new(universe::ReadTicket::stub(), block::AIR),
        );

        _ = HandleSet::from_iter([handle]);
    }
}

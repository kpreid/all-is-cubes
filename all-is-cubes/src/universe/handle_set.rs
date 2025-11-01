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
            .extract_if(.., |_, handle| handle.downcast_ref::<T>().is_some())
            .map(|(_, handle)| handle.downcast::<T>().unwrap())
            .collect()
    }
}

impl<H: universe::ErasedHandle> FromIterator<H> for HandleSet {
    /// Creates a [`HandleSet`] from handles.
    ///
    /// # Panics
    ///
    /// Panics if the handles are not all from the same universe.
    #[track_caller]
    fn from_iter<T: IntoIterator<Item = H>>(iter: T) -> Self {
        let mut universe_id = None;
        let handles = iter
            .into_iter()
            .map(|handle| {
                match (universe_id, handle.universe_id()) {
                    (Some(all), Some(this)) if all == this => {}
                    (Some(_), Some(_)) => {
                        panic!("handles in a HandleSet must be in the same universe")
                    }
                    (None, Some(this)) => universe_id = Some(this),
                    (_, None) => panic!("handles in a HandleSet must be in a universe"),
                }

                (handle.name(), handle.to_any_handle())
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
    use crate::block;

    use super::*;

    #[test]
    fn handle_set_extract_type() {
        let mut universe = Universe::new();
        let foo = universe
            .insert(
                "foo".into(),
                block::BlockDef::new(universe::ReadTicket::stub(), block::AIR),
            )
            .unwrap();
        let bar = universe.insert("bar".into(), crate::tag::TagDef).unwrap();
        let mut handle_set = HandleSet::all_of(&universe);

        let blocks = handle_set.extract_type::<block::BlockDef>();
        assert_eq!(blocks, vec![foo]);
        let tags = handle_set.extract_type::<crate::tag::TagDef>();
        assert_eq!(tags, vec![bar]);
        assert!(handle_set.is_empty());
    }
}

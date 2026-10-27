use hashbrown::HashMap;
use hashbrown::hash_map::Entry;

use all_is_cubes::sound::SoundDef;
use all_is_cubes::universe::{self, Handle};

// -------------------------------------------------------------------------------------------------

/// Caches the rendered form of [`SoundDef`]s, for use by platform-specific implementations
/// of sound playback.
//---
// TODO: The goal is that this cache will automatically be filled with each [`SoundDef`]
// belonging to the relevant [`Universe`], preloading rather than lazy loading.
// However, that is not yet implemented and the current goal of this type is just to be a shared
// implementation of what we already do for desktop and wasm.
#[derive(Debug)]
pub struct SoundCache<T> {
    /// Cached sounds.
    data: HashMap<Handle<SoundDef>, T>,
    // /// Universe which we are tracking. (Only used for sanity checks, since there
    // /// is no such thing as a persistent handle to a `Universe`.)
    // universe_id: universe::UniverseId,
}

impl<T> SoundCache<T> {
    /// Constructs an empty cache.
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    /// Retrieve a sound, preparing it if necessary.
    ///
    /// `loader` should call [`SoundDef::synthesize()`] and convert the result to `T`.
    /// It may return `None` in the case of resource exhaustion or another problem.
    pub fn get(
        &mut self,
        handle: Handle<SoundDef>,
        loader: impl FnOnce(&SoundDef) -> Option<T>,
    ) -> Option<&T> {
        match self.data.entry(handle) {
            Entry::Occupied(oe) => Some(oe.into_mut()),
            Entry::Vacant(ve) => {
                // TODO: We will need a non-stub read ticket for the game universe,
                // but that will only be possible by loading sounds eagerly rather than lazily.
                let value = loader(ve.key().read(universe::ReadTicket::stub()).ok()?)?;
                Some(ve.insert(value))
            }
        }
    }
}

impl<T> Default for SoundCache<T> {
    fn default() -> Self {
        Self::new()
    }
}

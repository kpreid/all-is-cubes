use core::fmt;

use hashbrown::HashMap;
use hashbrown::hash_map::Entry;

use all_is_cubes::listen;
use all_is_cubes::sound::SoundDef;
use all_is_cubes::universe::{self, Handle};

use crate::apps::session::AudioEvent;

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

// -------------------------------------------------------------------------------------------------

/// Create a channel that transports [`AudioEvent`], with prioritizing and dropping excess events.
#[doc(hidden)] // implementation helper of unclear general applicability
pub fn audio_event_channel(
    capacity: usize,
) -> (AudioEventChannelSender, flume::Receiver<AudioEvent>) {
    let (sender, receiver) = flume::bounded(capacity);
    (AudioEventChannelSender(sender), receiver)
}

/// Sender side of [`audio_event_channel()`].
///
/// Use this by passing it to [`listen_to_audio_events()`][crate::apps::Session::listen_to_audio_events].
#[doc(hidden)]
#[derive(Clone)]
pub struct AudioEventChannelSender(flume::Sender<AudioEvent>);

impl fmt::Debug for AudioEventChannelSender {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AudioEventChannelSender")
            .field("alive", &!self.0.is_disconnected())
            .finish_non_exhaustive()
    }
}

impl listen::Listener<AudioEvent> for AudioEventChannelSender {
    fn receive(&self, events: &[AudioEvent]) -> bool {
        if events.is_empty() {
            !self.0.is_disconnected()
        } else {
            for event in events {
                match self.0.try_send(event.clone()) {
                    Ok(()) => {}
                    Err(flume::TrySendError::Full(_)) => {} // drop message if full
                    Err(flume::TrySendError::Disconnected(_)) => return false,
                }
            }
            true
        }
    }
}

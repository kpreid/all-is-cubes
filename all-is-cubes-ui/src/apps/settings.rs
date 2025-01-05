use alloc::sync::Arc;
use core::fmt;

use all_is_cubes::listen;
use all_is_cubes_render::camera::GraphicsOptions;

/// Currently, the settings data is *only* graphics options.
/// We want to add more settings (e.g. keybindings, startup behavior options, etc),
/// and not use `GraphicsOptions`'s exact serialization, but that will come later.
type Data = Arc<GraphicsOptions>;

/// User-facing interactively editable and persisted settings for All is Cubes sessions.
///
/// Settings are user-visible configuration that is not specific to a particular session
/// or universe; for example, graphics options and key bindings.
///
/// This is separate from [`Session`](super::Session) so that it can be shared among
/// multiple sessions without conflict.
/// All such sessions will edit the same settings.
// TODO: Add settings inheritance for session-specific settings.
///
/// Having `&` access to a [`Settings`] grants permission to read the settings, follow
/// changes to the settings, and write the settings.
/// Read-only access may be obtained as [`Settings::as_source()`].
/// Cloning a [`Settings`] produces a clone which shares the same state.
#[derive(Clone)]
pub struct Settings(Arc<Inner>);

struct Inner {
    data: listen::Cell<Data>,
    persister: Arc<dyn Fn(&Data) + Send + Sync>,
}

impl Settings {
    /// Creates a new [`Settings`] with the given initial state, and no persistence.
    pub fn new(initial_state: Data) -> Self {
        Self::with_persistence(initial_state, Arc::new(|_| {}))
    }

    /// Creates a new [`Settings`] with the given initial state,
    /// and which calls the `persister` function immediately whenever it is modified.
    //---
    // TODO: Do we have any actual value for `persistence` that couldn’t be better handled
    // by calling as_source()? Revisit this when we have more non-graphics settings.
    pub fn with_persistence(
        initial_state: Data,
        persister: Arc<dyn Fn(&Data) + Send + Sync>,
    ) -> Self {
        Self(Arc::new(Inner {
            data: listen::Cell::new(initial_state),
            persister,
        }))
    }

    /// Returns a [`listen::Source`] of the settings.
    /// This may be used to follow changes to the settings.
    pub fn as_source(&self) -> listen::DynSource<Data> {
        self.0.data.as_source()
    }

    /// Returns the current graphics options.
    pub fn get_graphics_options(&self) -> Arc<GraphicsOptions> {
        self.0.data.get()
    }

    /// Overwrites the graphics options.
    pub fn set_graphics_options(&self, new_options: GraphicsOptions) {
        self.set_state(Arc::new(new_options));
    }

    /// Overwrites the graphics options with a modified version.
    ///
    /// This operation is not atomic; that is,
    /// if multiple threads are calling it, then one’s effect may be overwritten.
    // TODO: Fix that (will require support from ListenableCell...compare-and-swap?)
    #[doc(hidden)]
    pub fn mutate_graphics_options(&self, f: impl FnOnce(&mut GraphicsOptions)) {
        let mut options: Arc<GraphicsOptions> = self.0.data.get();
        f(Arc::make_mut(&mut options));
        self.set_state(options);
    }

    fn set_state(&self, state: Data) {
        (self.0.persister)(&state);
        self.0.data.set(state);
    }
}

impl fmt::Debug for Settings {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Settings")
            .field("data", &self.0.data)
            // can't print persister
            .finish_non_exhaustive()
    }
}

impl Default for Settings {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

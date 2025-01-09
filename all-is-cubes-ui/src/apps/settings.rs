use alloc::sync::Arc;
use core::fmt;

use all_is_cubes::listen::{self, Source as _};
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
/// All such sessions can edit the same settings.
///
/// Having `&` access to a [`Settings`] grants permission to read the settings, follow
/// changes to the settings, and write the settings.
/// Read-only access may be obtained as [`Settings::as_source()`].
/// Cloning a [`Settings`] produces a clone which shares the same state.
#[derive(Clone)]
pub struct Settings(Arc<Inner>);

struct Inner {
    /// If None, then use the hardcoded default.
    inherit: Option<Settings>,

    /// If `None`, then inherited or default.
    /// TODO: should have individual settings be individual cells with individual inheritance.
    state: listen::Cell<Option<Data>>,

    /// Source which returns the [`Data`] that is stored *or* inherited.
    final_data_source: listen::DynSource<Data>,

    persister: Arc<dyn Fn(&Data) + Send + Sync>,
}

impl Settings {
    /// Creates a new [`Settings`] with the given initial state, and no persistence.
    pub fn new(initial_state: Data) -> Self {
        Self::new_general(None, Some(initial_state), Arc::new(|_| {}))
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
        Self::new_general(None, Some(initial_state), persister)
    }

    /// Creates a new [`Settings`] which reads and writes the given [`Settings`],
    /// until such time as it is explicitly detached.
    pub fn inherit(other: Settings) -> Self {
        Self::new_general(Some(other), None, Arc::new(|_| {}))
    }

    fn new_general(
        inherit: Option<Settings>,
        initial_state: Option<Data>,
        persister: Arc<dyn Fn(&Data) + Send + Sync>,
    ) -> Self {
        let state = listen::Cell::new(initial_state);
        Self(Arc::new(Inner {
            final_data_source: Arc::new(
                state
                    .as_source()
                    .map({
                        let inherit = inherit.clone();
                        move |data: Option<Data>| -> listen::DynSource<Data> {
                            match (data, &inherit) {
                                (Some(data), _) => listen::constant(data),
                                (None, Some(settings)) => settings.as_source(),
                                (None, None) => {
                                    listen::constant(Arc::new(GraphicsOptions::default()))
                                }
                            }
                        }
                    })
                    .flatten(),
            ),
            inherit,
            state,
            persister,
        }))
    }

    /// Returns a [`listen::Source`] of the settings.
    /// This may be used to follow changes to the settings.
    pub fn as_source(&self) -> listen::DynSource<Data> {
        self.0.final_data_source.clone()
    }

    /// Returns the current graphics options.
    pub fn get_graphics_options(&self) -> Arc<GraphicsOptions> {
        self.as_source().get()
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
        let mut options: Arc<GraphicsOptions> = self.get_graphics_options();
        f(Arc::make_mut(&mut options));
        self.set_state(options);
    }

    /// If this `Settings` was constructed to share another’s state using
    /// [`inherit()`](Self::inherit), make it stop.
    /// This means future changes will not be persisted.
    pub fn disinherit(&self) {
        // TODO: this should be an atomic transaction but is not
        self.0
            .state
            .set(Some(self.0.state.get().unwrap_or_default()));
    }

    fn set_state(&self, state: Data) {
        // TODO: kludge: this condition is only vaguely reasonable because state never transitions
        // from Some to None. In reality, we should be using our own mutex instead of depending on
        // listen::Cell.
        if let Some(parent) = self
            .0
            .inherit
            .as_ref()
            .filter(|_| self.0.state.get().is_none())
        {
            parent.set_state(state);
        } else {
            (self.0.persister)(&state);
            self.0.state.set(Some(state));
        }
    }
}

impl fmt::Debug for Settings {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Settings")
            .field("state", &self.0.state)
            // can't print persister
            .finish_non_exhaustive()
    }
}

impl Default for Settings {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::math::ps64;

    #[test]
    fn disinherit() {
        let parent = Settings::new(Arc::new({
            let mut g = GraphicsOptions::default();
            g.fov_y = ps64(20.0);
            g
        }));
        let child = Settings::inherit(parent.clone());

        // Inherited values before child
        assert_eq!(
            (
                parent.get_graphics_options().fov_y,
                child.get_graphics_options().fov_y
            ),
            (ps64(20.0), ps64(20.0))
        );
        assert_eq!(
            child.get_graphics_options().show_ui,
            true,
            "original value for show_ui"
        );

        // Writing upward and reading downward
        child.mutate_graphics_options(|g| g.show_ui = false);
        assert_eq!(
            (
                parent.get_graphics_options().show_ui,
                child.get_graphics_options().show_ui
            ),
            (false, false),
            "parent and child mutated"
        );

        // Then it doesn't any more
        child.disinherit();
        child.mutate_graphics_options(|g| g.fov_y = ps64(10.0));
        assert_eq!(
            (
                parent.get_graphics_options().fov_y,
                child.get_graphics_options().fov_y
            ),
            (ps64(20.0), ps64(10.0)),
            "after disinherit"
        );
    }
}

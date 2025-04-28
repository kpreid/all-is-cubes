use alloc::sync::Arc;
use core::fmt;

use all_is_cubes::character::{Character, Cursor, cursor_raycast};
use all_is_cubes::listen;
use all_is_cubes::math::FreeCoordinate;
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, HandleError, ReadTicket, Universe};

use crate::camera::{Camera, GraphicsOptions, NdcPoint2, ViewTransform, Viewport};

/// A collection of values associated with each of the layers of graphics that
/// is normally drawn (HUD on top of world, currently) by [`HeadlessRenderer`] or
/// other renderers.
///
/// [`HeadlessRenderer`]: crate::HeadlessRenderer
// Exhaustive: Changing this will probably be breaking anyway, until we make it a
// more thorough abstraction.
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct Layers<T> {
    /// The game world.
    pub world: T,
    /// The user interface, or HUD, drawn in front of the world.
    pub ui: T,
}

impl<T> Layers<T> {
    /// Clone the given value for each layer.
    pub fn splat(value: T) -> Self
    where
        T: Clone,
    {
        Layers {
            world: value.clone(),
            ui: value,
        }
    }

    // experimental API
    #[cfg(feature = "raytracer")]
    pub(crate) fn as_refs(&self) -> Layers<&T> {
        Layers {
            world: &self.world,
            ui: &self.ui,
        }
    }

    // experimental API
    #[cfg(feature = "raytracer")]
    pub(crate) fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> Layers<U> {
        Layers {
            world: f(self.world),
            ui: f(self.ui),
        }
    }

    #[doc(hidden)] // used in related crates, but it's ugly and experimental
    pub fn try_map_ref<U, E>(&self, mut f: impl FnMut(&T) -> Result<U, E>) -> Result<Layers<U>, E> {
        Ok(Layers {
            world: f(&self.world)?,
            ui: f(&self.ui)?,
        })
    }
}

/// Bundle of inputs specifying the “standard” configuration of [`Camera`]s and other
/// things to render an All is Cubes scene and user interface.
///
/// All of its data is provided through [`listen::DynSource`]s, and consists of:
///
/// * [`GraphicsOptions`].
/// * A [`Viewport`] specifying the dimensions of image to render.
/// * A [`Handle`] to the [`Character`] whose eyes we look through to render the “world”
///   [`Space`].
/// * A [`Handle`] to the UI/HUD [`Space`] overlaid on the world, if any.
///
/// When [`StandardCameras::update()`] is called, all of these data sources are read
/// and used to update the [`Camera`] data. Those cameras, and copies of the input
/// data, are then available for use while rendering.
///
/// Because every input is a [`listen::DynSource`], it is never necessary to call a setter.
/// Every [`StandardCameras`] which was created with the same sources will have the same
/// results (after `update()`).
///
/// Design note: The sense in which this is “standard” is that if an application wished
/// to, for example, have multiple views into the same [`Space`], it would need to create
/// additional [`Camera`]s (or multiple [`StandardCameras`]) and update them itself.
#[derive(Debug)]
pub struct StandardCameras {
    /// Cameras are synced with this
    graphics_options: listen::DynSource<Arc<GraphicsOptions>>,
    graphics_options_dirty: listen::Flag,

    character_source: listen::DynSource<Option<Handle<Character>>>,
    /// Tracks whether the character was replaced (not whether its view changed).
    character_dirty: listen::Flag,
    character: Option<Handle<Character>>,
    /// Cached and listenable version of character's space.
    /// TODO: This should be in a `Layers` along with `ui_state`...?
    world_space: listen::Cell<Option<Handle<Space>>>,

    ui_source: listen::DynSource<Arc<UiViewState>>,
    ui_dirty: listen::Flag,
    ui_space: Option<Handle<Space>>,

    viewport_source: listen::DynSource<Viewport>,
    viewport_dirty: listen::Flag,

    cameras: Layers<Camera>,
}

impl StandardCameras {
    /// Most general constructor; hidden because the details needed might vary and so we
    /// want to discourage use of this directly.
    #[doc(hidden)]
    pub fn new(
        read_tickets: Layers<ReadTicket<'_>>,
        graphics_options: listen::DynSource<Arc<GraphicsOptions>>,
        viewport_source: listen::DynSource<Viewport>,
        character_source: listen::DynSource<Option<Handle<Character>>>,
        ui_source: listen::DynSource<Arc<UiViewState>>,
    ) -> Self {
        // TODO: Add a unit test that each of these listeners works as intended.
        // TODO: This is also an awful lot of repetitive code; we should design a pattern
        // to not have it (some kind of "following cell")?
        let graphics_options_dirty = listen::Flag::listening(false, &graphics_options);
        let viewport_dirty = listen::Flag::listening(false, &viewport_source);

        let initial_options: &GraphicsOptions = &graphics_options.get();
        let initial_viewport: Viewport = viewport_source.get();

        let ui_state = ui_source.get();

        let mut new_self = Self {
            cameras: Layers {
                ui: Camera::new((*ui_state.graphics_options).clone(), initial_viewport),
                world: Camera::new(initial_options.clone(), initial_viewport),
            },

            graphics_options,
            graphics_options_dirty,

            character_dirty: listen::Flag::listening(true, &character_source),
            character_source,
            character: None, // update() will fix these up
            world_space: listen::Cell::new(None),

            ui_space: ui_state.space.clone(),
            ui_dirty: listen::Flag::listening(true, &ui_source),
            ui_source,

            viewport_dirty,
            viewport_source,
        };

        new_self.update(read_tickets);
        new_self
    }

    #[doc(hidden)]
    pub fn from_constant_for_test(
        graphics_options: GraphicsOptions,
        viewport: Viewport,
        universe: &Universe,
    ) -> Self {
        Self::new(
            Layers {
                world: universe.read_ticket(),
                ui: ReadTicket::stub(),
            },
            listen::constant(Arc::new(graphics_options)),
            listen::constant(viewport),
            listen::constant(universe.get_default_character()),
            listen::constant(Default::default()),
        )
    }

    /// Updates camera state from data sources.
    ///
    /// This should be called at the beginning of each frame or as needed when the
    /// cameras are to be used.
    ///
    /// Returns whether any values actually changed.
    /// (This does not include tracking changes to space content — only which part of which spaces
    /// are being looked at.)
    pub fn update(&mut self, read_tickets: Layers<ReadTicket<'_>>) -> bool {
        let mut anything_changed = false;

        let options_dirty = self.graphics_options_dirty.get_and_clear();
        if options_dirty {
            anything_changed = true;
            self.cameras
                .world
                .set_options((*self.graphics_options.get()).clone());
        }

        let ui_dirty = self.ui_dirty.get_and_clear();
        if ui_dirty || options_dirty {
            anything_changed = true;
            let UiViewState {
                space,
                view_transform: ui_transform,
                graphics_options: ui_options,
            } = if self.graphics_options.get().show_ui {
                (*self.ui_source.get()).clone()
            } else {
                UiViewState::default()
            };
            self.ui_space = space;
            self.cameras.ui.set_options((*ui_options).clone());
            self.cameras.ui.set_view_transform(ui_transform);
        }

        // Update viewports.
        // Note: The UI does its own independent re-layout when the viewport aspect ratio
        // changes.
        let viewport_dirty = self.viewport_dirty.get_and_clear();
        if viewport_dirty {
            anything_changed = true;
            let viewport: Viewport = self.viewport_source.get();
            // TODO: this should be a Layers::iter_mut() or something
            self.cameras.world.set_viewport(viewport);
            self.cameras.ui.set_viewport(viewport);
        }

        if self.character_dirty.get_and_clear() {
            anything_changed = true;
            self.character = self.character_source.get();
            if self.character.is_none() {
                // Reset transform so it isn't a *stale* transform.
                // TODO: set an error flag saying that nothing should be drawn
                self.cameras
                    .world
                    .set_view_transform(ViewTransform::identity());
            }
        }

        if let Some(character_handle) = &self.character {
            match character_handle.read(read_tickets.world) {
                Ok(character) => {
                    let view_transform = character.view();
                    if view_transform != self.cameras.world.view_transform() {
                        anything_changed = true;
                        self.cameras.world.set_view_transform(character.view());
                    }

                    // TODO: listen::Cell should make this easier and cheaper
                    if Option::as_ref(&self.world_space.get()) != Some(&character.space) {
                        anything_changed = true;

                        self.world_space.set(Some(character.space.clone()));
                    }

                    // Update camera exposure from character.
                    let old_actual_exposure = self.cameras.world.exposure();
                    self.cameras
                        .world
                        .set_measured_exposure(character.exposure());
                    anything_changed |= self.cameras.world.exposure() != old_actual_exposure;
                }
                Err(_) => {
                    // TODO: set an error flag indicating failure to update
                }
            }
        } else {
            if self.world_space.get().is_some() {
                anything_changed = true;
                self.world_space.set(None);
            }
        }

        anything_changed
    }

    /// Returns current graphics options as of the last [`update()`](Self::update).
    ///
    /// These options are to be used for the world and not the UI.
    pub fn graphics_options(&self) -> &GraphicsOptions {
        self.cameras.world.options()
    }

    /// Returns a clone of the source of graphics options that this [`StandardCameras`]
    /// was created with.
    ///
    /// These options are to be used for the world and not the UI.
    pub fn graphics_options_source(&self) -> listen::DynSource<Arc<GraphicsOptions>> {
        self.graphics_options.clone()
    }

    /// Returns [`Camera`]s appropriate for drawing each graphical layer.
    pub fn cameras(&self) -> &Layers<Camera> {
        &self.cameras
    }

    /// Returns the character's viewpoint to draw in the world layer.
    /// May be [`None`] if there is no current character.
    pub fn character(&self) -> Option<&Handle<Character>> {
        self.character.as_ref()
    }

    /// Returns the space that should be drawn as the game world, using `self.cameras().world`.
    ///
    /// This is a [`listen::DynSource`] to make it simple to cache the Space rendering data and
    /// follow space transitions.
    /// It updates when [`Self::update()`] is called.
    pub fn world_space(&self) -> listen::DynSource<Option<Handle<Space>>> {
        self.world_space.as_source()
    }

    /// Returns the UI space, that should be drawn on top of the world using `self.cameras().ui`.
    ///
    /// This implements [`GraphicsOptions::show_ui`] by returning [`None`] when the option is
    /// false.
    ///
    /// TODO: Make this also a [`listen::DynSource`]
    pub fn ui_space(&self) -> Option<&Handle<Space>> {
        self.ui_space.as_ref()
    }

    // TODO: unclear if good API; added so that we can get Source access to the graphics options,
    // and *something* of the sort should be public, but I don't know if exposing UiViewState
    // directly, as opposed to a source of a Camera, is right.
    #[cfg(feature = "raytracer")] // not used otherwise
    pub(crate) fn ui_view_source(&self) -> listen::DynSource<Arc<UiViewState>> {
        self.ui_source.clone()
    }

    /// Returns the current viewport.
    ///
    /// This is always equal to the viewports of all managed [`Camera`]s,
    /// and only updates when [`StandardCameras::update()`] is called.
    pub fn viewport(&self) -> Viewport {
        self.cameras.world.viewport()
    }

    /// Returns a clone of the viewport source this is following.
    pub fn viewport_source(&self) -> listen::DynSource<Viewport> {
        self.viewport_source.clone()
    }

    /// Perform a raycast through these cameras to find what the cursor hits.
    ///
    /// Make sure to call [`StandardCameras::update`] first so that the cameras are
    /// up to date with game state.
    pub fn project_cursor(
        &self,
        read_tickets: Layers<ReadTicket<'_>>,
        ndc_pos: NdcPoint2,
    ) -> Result<Option<Cursor>, HandleError> {
        if let Some(ui_space_handle) = self.ui_space.as_ref() {
            let ray = self.cameras.ui.project_ndc_into_world(ndc_pos);
            if let res @ (Ok(Some(_)) | Err(_)) = cursor_raycast(
                read_tickets.ui,
                ray,
                ui_space_handle,
                FreeCoordinate::INFINITY,
            ) {
                return res;
            }
        }

        if let Some(character_handle) = self.character.as_ref() {
            let ray = self.cameras.world.project_ndc_into_world(ndc_pos);
            // TODO: maximum distance should be determined by character/universe parameters
            // instead of hardcoded
            if let res @ (Ok(Some(_)) | Err(_)) = cursor_raycast(
                read_tickets.world,
                ray,
                &character_handle.read(read_tickets.world).unwrap().space,
                6.0,
            ) {
                return res;
            }
        }

        Ok(None)
    }
}

impl fmt::Pointer for StandardCameras {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print all and only the fields which point to external mutable state
        write!(
            f,
            indoc::indoc! {"\
                StandardCameras {{
                    graphics_options: {graphics_options:p},
                    viewport_source: {viewport_source:p},
                    character_source: {character_source:p},
                    ui_source: {ui_source:p},
                }}\
            "},
            graphics_options = self.graphics_options,
            viewport_source = self.viewport_source,
            character_source = self.character_source,
            ui_source = self.ui_source,
        )
    }
}

impl Clone for StandardCameras {
    /// Returns a [`StandardCameras`] which tracks the same data sources (graphics
    /// options, scene sources, viewport) as `self`, but whose local state (such as
    /// the last updated camera state) is independent.
    fn clone(&self) -> Self {
        Self::new(
            Layers::splat(ReadTicket::new()), // TODO(read_ticket): need to either require one passed in, or figure out a way to not do any reads when cloning
            self.graphics_options.clone(),
            self.viewport_source.clone(),
            self.character_source.clone(),
            self.ui_source.clone(),
        )
    }
}

/// Specifies what to render for the UI layer in front of the world.
///
/// This struct contains all the information needed to know how to render the UI
/// *specifically* (distinct from the world). It differs from [`Camera`] in that it
/// includes the [`Space`] and excludes the viewport.
///
/// TODO: This struct needs a better name. And is it good for non-UI, too?
/// Note that we may wish to revise this bundle if we start having continuously changing
/// `view_transform`.
#[derive(Clone, Debug, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct UiViewState {
    /// The [`Space`] to render as the UI.
    pub space: Option<Handle<Space>>,

    /// The viewpoint to render the `space` from.
    pub view_transform: ViewTransform,

    /// The graphics options to render the `space` with.
    //---
    // Design note: This is an `Arc` not because it strongly needs to be,
    // but because other parts of the system pass around `Arc`ed graphics options
    // and we want to be efficiently compatible with them.
    pub graphics_options: Arc<GraphicsOptions>,
}

impl Default for UiViewState {
    /// Draws no space, with default graphics options.
    fn default() -> Self {
        Self {
            space: Default::default(),
            view_transform: ViewTransform::identity(),
            graphics_options: Default::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cameras_follow_character_and_world() {
        let character_cell = listen::Cell::new(None);
        let mut cameras = StandardCameras::new(
            Layers::splat(ReadTicket::new()),
            listen::constant(Arc::new(GraphicsOptions::default())),
            listen::constant(Viewport::ARBITRARY),
            character_cell.as_source(),
            listen::constant(Arc::new(UiViewState::default())),
        );

        let world_source = cameras.world_space();
        let world_flag = listen::Flag::listening(false, &world_source);
        assert_eq!(world_source.get().as_ref(), None);

        // No redundant notification when world is absent
        {
            let changed = cameras.update(Layers::splat(ReadTicket::new()));
            assert_eq!((changed, world_flag.get_and_clear()), (false, false));
        }

        // Create a universe with space and character
        let mut universe = Universe::new();
        let space_handle = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
        let character = universe
            .insert(
                "character".into(),
                Character::spawn_default(universe.read_ticket(), space_handle.clone()),
            )
            .unwrap();
        character_cell.set(Some(character));

        // Now the world_source should be reporting the new space
        {
            assert!(!world_flag.get_and_clear());
            let changed = cameras.update(Layers {
                world: universe.read_ticket(),
                ui: ReadTicket::stub(),
            });
            assert_eq!((changed, world_flag.get_and_clear()), (true, true));
            assert_eq!(world_source.get().as_ref(), Some(&space_handle));
        }

        // No redundant notification when world is present
        {
            let changed = cameras.update(Layers {
                world: universe.read_ticket(),
                ui: ReadTicket::stub(),
            });
            assert_eq!((changed, world_flag.get_and_clear()), (false, false));
        }

        // TODO: test further changes
    }

    #[test]
    fn cameras_clone() {
        let options_cell = listen::Cell::new(Arc::new(GraphicsOptions::default()));
        let mut cameras = StandardCameras::new(
            Layers::splat(ReadTicket::new()),
            options_cell.as_source(),
            listen::constant(Viewport::ARBITRARY),
            listen::constant(None),
            listen::constant(Arc::new(UiViewState::default())),
        );
        let mut cameras2 = cameras.clone();

        let default_o = GraphicsOptions::default();
        let mut different_o = default_o.clone();
        different_o.debug_chunk_boxes = true;
        options_cell.set(Arc::new(different_o.clone()));

        // Each `StandardCameras` has independent updating from the same data sources.
        assert_eq!(cameras.cameras().world.options(), &default_o);
        assert_eq!(cameras2.cameras().world.options(), &default_o);
        cameras.update(Layers::splat(ReadTicket::new()));
        assert_eq!(cameras.cameras().world.options(), &different_o);
        assert_eq!(cameras2.cameras().world.options(), &default_o);
        cameras2.update(Layers::splat(ReadTicket::new()));
        assert_eq!(cameras.cameras().world.options(), &different_o);
        assert_eq!(cameras2.cameras().world.options(), &different_o);
    }
}

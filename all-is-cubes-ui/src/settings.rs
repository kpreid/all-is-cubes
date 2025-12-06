//! User-facing interactively editable and persisted settings for All is Cubes sessions.
//!
//! [`Settings`] is the primary type of this module.

use alloc::sync::Arc;
use core::any::Any;
use core::fmt;

use bevy_platform::sync::{LazyLock, OnceLock};
use hashbrown::HashMap;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::listen::{self, Source as _};
use all_is_cubes_render::camera::GraphicsOptions;

// -------------------------------------------------------------------------------------------------

// TODO: decide whether flattening all of this is good actually
mod schema;
pub use schema::*;

mod serialize;

// -------------------------------------------------------------------------------------------------

/// User-facing interactively editable and persisted settings for All is Cubes sessions.
///
/// Settings are user-visible configuration that is not specific to a particular session
/// or universe; for example, graphics options and key bindings.
///
/// This is separate from [`Session`][super::apps::Session] so that it can be shared among
/// multiple sessions without conflict (and to allow its use separately if desired).
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
                                (None, None) => listen::constant(Data::default()),
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

    /// Returns a snapshot of the current settings.
    ///
    /// This is slightly more efficient than `self.as_source().get()`.
    pub fn get(&self) -> Data {
        self.0.final_data_source.get()
    }

    /// Changes the value of one setting.
    ///
    /// This change may affect the persistent storage of the [`Settings`] these settings were
    /// [inherited][Self::inherit] from, unless [`Settings::disinherit()`] is called first.
    pub fn set<T: Send + Sync + 'static>(&self, key: &'static TypedKey<T>, value: T) {
        let sv = StoredValue {
            unparsed: (key.serialize)(&value),
            parsed: Some(Arc::new(value)),
        };
        self.set_raw(key.key, sv);
    }

    /// Changes the value of one setting, by giving the string representation of the value.
    ///
    /// This change may affect the persistent storage of the [`Settings`] these settings were
    /// [inherited][Self::inherit] from, unless [`Settings::disinherit()`] is called first.
    ///
    /// # Errors
    ///
    /// Returns an error if the value is not valid for the key.
    pub fn set_untyped(&self, key: Key, value: Value) -> Result<(), ParseError> {
        self.set_raw(
            key,
            StoredValue {
                parsed: Some(key.deserialize_erased(&value)?),
                unparsed: value,
            },
        );
        Ok(())
    }

    /// Common implementation of [`Self::set()`] and [`Self::set_untyped()`].
    /// Does not check that the value has a correct value type.
    fn set_raw(&self, key: Key, sv: StoredValue) {
        let current_data = self.0.final_data_source.get();
        if current_data.0.map.get(&key).map(|existing| &existing.unparsed) == Some(&sv.unparsed) {
            // No difference.
            return;
        }

        let mut new_map: HashMap<Key, StoredValue> = current_data.0.map.clone();

        new_map.insert(key, sv);

        self.set_state(Data(Arc::new(DataInner {
            map: new_map,
            graphics_options: OnceLock::new(),
        })));
    }

    /// Modify one setting by calling `updater` with the old value to compute a new value.
    ///
    /// This operation is not atomic; that is,
    /// if multiple threads are calling it, then one’s effect may be overwritten.
    /// Future versions might improve that situation.
    pub fn update<T: Clone + Send + Sync + 'static>(
        &self,
        key: &'static TypedKey<T>,
        updater: impl FnOnce(T, &Data) -> T,
    ) {
        let data = self.as_source().get();
        let old_value: T = data.get(key).clone();
        let new_value = updater(old_value, &data);
        self.set(key, new_value);
    }

    /// If this `Settings` was constructed to share another’s state using
    /// [`inherit()`](Self::inherit), make it stop.
    /// Current values will be kept, but future changes will not affect the parent.
    ///
    /// Note that this does not affect persistence of this `Settings`; it will continue writing
    /// to wherever it would have.
    pub fn disinherit(&self) {
        // TODO: this should be an atomic transaction but is not
        // TODO: We should also drop `self.inherit` since it will no longer be used
        self.0.state.set(Some(self.as_source().get()));
    }

    fn set_state(&self, state: Data) {
        // TODO: kludge: this condition is only vaguely reasonable because state never transitions
        // from Some to None. In reality, we should be using our own mutex instead of depending on
        // listen::Cell.
        if let Some(parent) = self.0.inherit.as_ref().filter(|_| self.0.state.get().is_none()) {
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

// -------------------------------------------------------------------------------------------------

/// Current state of [`Settings`].
///
/// Unlike [`Settings`], this struct does not provide shared *mutable* access to settings,
/// but is a snapshot.
//---
// TODO: rename this to a better name. Maybe this is Settings and the other is SettingsStore.
#[derive(Debug, Clone, Default)]
pub struct Data(Arc<DataInner>);

#[derive(Debug, Default)]
struct DataInner {
    map: HashMap<Key, StoredValue>,

    /// Derived graphics options.
    graphics_options: OnceLock<Arc<GraphicsOptions>>,
    // TODO: Data gets cloned a lot currently and so the graphics_options cache will work poorly
    // TODO: Add storage for unknown preserved keys.
}

impl Data {
    // Note: Does not check for consistency between key and value type.
    fn from_sv(map: HashMap<Key, StoredValue>) -> Self {
        Self(Arc::new(DataInner {
            map,
            graphics_options: OnceLock::new(),
        }))
    }

    /// Returns the current value of this setting, or the default if it is unset or invalid.
    pub fn get<T>(&self, key: &'static TypedKey<T>) -> &T {
        match self.0.map.get(&key.key) {
            Some(StoredValue {
                unparsed: _,
                parsed: Some(value),
            }) => value.downcast_ref().expect("downcast failure"),
            _ => &key.default,
        }
    }

    /// Returns [`GraphicsOptions`] derived from these settings.
    pub fn to_graphics_options(&self) -> Arc<GraphicsOptions> {
        Arc::clone(
            self.0
                .graphics_options
                .get_or_init(|| Arc::new(assemble_graphics_options(self))),
        )
    }

    /// Iterates over every key-value pair that has a value which may be different from the
    /// default.
    ///
    /// This is appropriate to use for saving settings. It produces the same results as
    /// the [`serde::Serialize`] implementation for [`Data`].
    /// It is not appropriate for listing the values of all settings that exist.
    pub fn iter_set(&self) -> impl Iterator<Item = (Key, Value)> {
        self.0.map.iter().map(|(k, sv)| (*k, sv.unparsed.clone()))
    }
}

impl Eq for Data {}
impl PartialEq for Data {
    fn eq(&self, other: &Self) -> bool {
        if self.0.map.len() != other.0.map.len() {
            return false;
        }
        for (key, value) in self.0.map.iter() {
            let Some(other_value) = other.0.map.get(key) else {
                return false;
            };
            if value.unparsed != other_value.unparsed {
                return false;
            }
        }
        true
    }
}

impl FromIterator<(ArcStr, Value)> for Data {
    fn from_iter<T: IntoIterator<Item = (ArcStr, Value)>>(iter: T) -> Self {
        Self::from_sv(HashMap::from_iter(iter.into_iter().filter_map(
            |(key_str, value_str)| {
                // TODO: preserve unknown keys instead of discarding them
                let key: Key = key_str.parse().ok()?;
                Some((
                    key,
                    StoredValue {
                        parsed: key.deserialize_erased(&value_str).ok(),
                        unparsed: value_str,
                    },
                ))
            },
        )))
    }
}

impl serde::Serialize for Data {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Convert HashMap to BTreeMap so that the keys are serialized in a deterministic order.
        let ordered_map: alloc::collections::BTreeMap<&str, &str> =
            self.0.map.iter().map(|(k, v)| (k.as_str(), v.unparsed.as_str())).collect();
        serde::Serialize::serialize(&ordered_map, serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Data {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let map = <HashMap<ArcStr, ArcStr> as serde::Deserialize>::deserialize(deserializer)?;
        Ok(Self::from_iter(map))
    }
}

// -------------------------------------------------------------------------------------------------

/// Identifies an individual setting in [`Settings`], and allows accessing it as type `T`.
///
/// You can obtain these from [the constants in this module](self#constants), such as [`FOG`].
pub struct TypedKey<T> {
    key: Key,
    serialize: fn(&T) -> ArcStr,
    deserialize: fn(&str) -> Result<T, serialize::DeserializeError>,
    default: LazyLock<T>,
}

impl<T: Send + Sync + 'static> TypedKey<T> {
    /// Returns the untyped form of this key.
    pub fn key(&self) -> &Key {
        &self.key
    }

    /// Gets the current value of this setting.
    // TODO: remove by inlining
    pub fn read<'d>(&'static self, data: &'d Data) -> &'d T {
        data.get(self)
    }

    /// Overwrites the current value of this setting.
    // TODO: remove by inlining
    pub fn write(&'static self, settings: &Settings, value: T) {
        settings.set(self, value)
    }
}

impl<T> fmt::Debug for TypedKey<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{key}: {type}", key = self.key, type = core::any::type_name::<T>())
    }
}

// -------------------------------------------------------------------------------------------------

// TODO: should we have an `enum Value`? Or are those more trouble than they’re worth due to
// mismatches with serialization formats (NaN, integer width, etc)?
type Value = ArcStr;

#[derive(Clone, Debug)]
struct StoredValue {
    unparsed: ArcStr,
    /// This field is [`Some`] if and only if the value successfully parsed.
    parsed: Option<Arc<dyn Any + Send + Sync>>,
}

// -------------------------------------------------------------------------------------------------

/// Error when a setting’s value is given as an unparseable string or an out-of-range value.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct ParseError {
    /// The key whose value this was trying to be.
    pub key: Key,
    /// The string value that was not valid.
    pub unparseable_value: ArcStr,
    /// Explanation of how the value was invalid.
    detail: ArcStr,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            key,
            unparseable_value,
            detail,
        } = self;
        write!(
            f,
            "could not parse “{unparseable_value}” as a value for setting “{key}”: {detail}"
        )
    }
}

impl core::error::Error for ParseError {}

// -------------------------------------------------------------------------------------------------

// -------------------------------------------------------------------------------------------------

/// Errors produced by [`Settings::set()`].
#[derive(Clone, Debug, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum SetError {
    // /// A key was specified that is not part of the settings schema.
    // #[displaydoc("unknown setting key “{key}”")]
    // UnknownKey {
    //     #[allow(missing_docs)]
    //     key: ArcStr,
    // },
    /// A value was provided as a string, but the string did not parse or otherwise is not valid
    /// for the setting.
    #[displaydoc("value “{value}” is not valid for key “{key}”")]
    InvalidValue {
        #[allow(missing_docs)]
        key: Key,
        /// The erroneous value.
        value: ArcStr,
    },
}

impl core::error::Error for SetError {}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::arcstr::literal;
    use all_is_cubes::math::ps64;

    #[test]
    fn update() {
        let settings = Settings::new(Data::default());
        settings.update(SHOW_UI, |value, _data| {
            assert!(value);
            false
        });
        assert_eq!(*settings.get().get(SHOW_UI), false);
    }

    #[test]
    fn disinherit() {
        let parent = Settings::new(Data::from_iter([(
            literal!("graphics/fov-y"),
            literal!("20.0"),
        )]));
        let child = Settings::inherit(parent.clone());

        // Inherited values before child
        assert_eq!(
            (*FOV_Y.read(&parent.get()), *FOV_Y.read(&child.get())),
            (ps64(20.0), ps64(20.0))
        );
        assert_eq!(
            *SHOW_UI.read(&child.get()),
            true,
            "original value for show_ui"
        );

        // Writing upward and reading downward
        child.set(SHOW_UI, false);
        assert_eq!(
            (*SHOW_UI.read(&parent.get()), *SHOW_UI.read(&child.get())),
            (false, false),
            "parent and child mutated"
        );

        child.disinherit();

        // After disinheriting, child still has values from parent
        assert_eq!(
            *FOV_Y.read(&child.get()),
            ps64(20.0),
            "inherited value after disinherit"
        );

        // After disinheriting, child doesn't mutate parent
        child.set(FOV_Y, ps64(10.0));
        assert_eq!(
            (*FOV_Y.read(&parent.get()), *FOV_Y.read(&child.get())),
            (ps64(20.0), ps64(10.0)),
            "mutated values after disinherit"
        );
    }

    #[test]
    fn persistence() {
        use std::sync::mpsc::TryRecvError::Empty;

        let (ptx, prx) = std::sync::mpsc::channel();
        let settings = Settings::with_persistence(
            Default::default(),
            Arc::new(move |data: &Data| ptx.send(data.clone()).unwrap()),
        );

        // No initial write
        assert_eq!(prx.try_recv(), Err(Empty));

        settings.set(SHOW_UI, false);
        assert_eq!(
            prx.try_recv(),
            Ok(Data::from_iter([(
                literal!("graphics/show-ui"),
                literal!("false"),
            )]))
        );
    }

    #[test]
    fn invalid_string_value_constructing_data() {
        let data = Data::from_iter([(literal!("graphics/fov-y"), literal!("nonsense"))]);

        // For typed reads, the default is used instead of the invalid value.
        assert_eq!(data.get(FOV_Y), &ps64(90.0));

        // But the invalid value is preserved in case it is wanted.
        let sv = &data.0.map[&FOV_Y.key];
        assert_eq!(
            (&sv.unparsed, sv.parsed.is_none()),
            (&literal!("nonsense"), true)
        );
    }
}

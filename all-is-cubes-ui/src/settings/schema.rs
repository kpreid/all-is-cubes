use alloc::borrow::Cow;
use alloc::string::String;
use alloc::sync::Arc;
use core::any::Any;
use core::fmt;
use core::str::FromStr;

use bevy_platform::sync::LazyLock;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::math::{FreeCoordinate, PositiveSign, ZeroOne};
use all_is_cubes_render::camera::{self, GraphicsOptions};

use crate::settings::serialize::{DeserializeError, StringForm};
use crate::settings::{ParseError, TypedKey};

#[cfg(doc)]
use crate::settings::Settings;

// -------------------------------------------------------------------------------------------------

macro_rules! derive_settings_schema_from_keys {
    ($(#[$_:meta])* pub enum Key {
        $(
            $(#[doc = $field_doc:literal])*
            #[custom(
                key = $key_string:literal,
                type = $value_type:ty,
                display_name = $display_name:literal,
                default = $default_expr:expr $(,)?
            )]
            $variant:ident,
        )*
    }) => {
        impl Key {
            /// Returns the unique string identifier for this setting, which should be used when
            /// serializing it in key-value maps.
            pub fn as_str(&self) -> &str {
                match self {
                    $( Key::$variant => $key_string, )*
                }
            }

            /// Returns the name for this setting to show to users.
            pub fn display_name(&self) -> &str {
                match self {
                    // TODO: have an actual name rather than reiterating the for-machines string.
                    $( Key::$variant => $key_string, )*
                }
            }

            /// Deserialize the value as by [`TypedKey`], but in boxed form.
            ///
            /// Design note: This function may need to go away when we make the schema extensible.
            /// We’ll see.
            pub(crate) fn deserialize_erased(&self, value: &str) -> Result<Arc<dyn Any + Send + Sync>, ParseError> {
                paste::paste! {
                    match self {
                        $( Key::$variant => {
                            match ([< $variant:snake:upper >].deserialize)(value) {
                                Ok(value) => Ok(Arc::new(value)),
                                Err(DeserializeError(detail)) => Err(ParseError {
                                    key: Key::$variant,
                                    unparseable_value: ArcStr::from(value),
                                    detail,
                                })
                            }
                        })*
                    }
                }
            }
        }

        impl fmt::Display for Key {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        Key::$variant => f.pad($key_string),
                    )*
                }
            }
        }

        impl FromStr for Key {
            type Err = UnknownSetting;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $(
                        $key_string => Ok(Key::$variant),
                    )*
                    _ => Err(UnknownSetting(s.into())),
                }
            }
        }

        impl serde::Serialize for Key {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer {
                self.as_str().serialize(serializer)
            }
        }

        paste::paste! {
            $(
                static [< STATIC_ $variant:snake:upper >]: TypedKey<$value_type> = TypedKey {
                    key: Key::$variant,
                    // TODO: if we stick to this, no need for the struct fields...?
                    serialize: <$value_type as StringForm>::serialize,
                    deserialize: <$value_type as StringForm>::deserialize,
                    default: LazyLock::new(|| { $default_expr }),
                };

                #[doc = concat!(
                    "Allows accessing the `",$key_string, "` setting as a `",
                    stringify!($value_type), "`."
                )]
                pub const [< $variant:snake:upper >]: &TypedKey<$value_type> =
                     &[< STATIC_ $variant:snake:upper >];
            )*
        }
    }
}

/// Identifies an individual setting in [`Settings`].
///
/// TODO: This will eventually be replaced with a more extensible mechanism, possibly involving
/// multiple enums.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, exhaust::Exhaust)]
#[expect(missing_docs, reason = "TODO")]
#[non_exhaustive]
#[macro_rules_attribute::derive(derive_settings_schema_from_keys!)]
pub enum Key {
    #[custom(
        key = "graphics/render-method",
        type = camera::RenderMethod,
        display_name = "Render method",
        default = GraphicsOptions::default().render_method
    )]
    RenderMethod,

    #[custom(
        key = "graphics/fog",
        type = camera::FogOption,
        display_name = "Fog",
        default = GraphicsOptions::default().fog
    )]
    Fog,

    #[custom(
        key = "graphics/fov-y",
        type = PositiveSign<FreeCoordinate>,
        display_name = "FOV",
        default = GraphicsOptions::default().fov_y
    )]
    FovY,

    #[custom(
        key = "graphics/tone-mapping",
        type = camera::ToneMappingOperator,
        display_name = "Tone mapping",
        default = GraphicsOptions::default().tone_mapping
    )]
    ToneMapping,

    #[custom(
        key = "graphics/maximum-intensity",
        type = PositiveSign<f32>,
        display_name = "Maximum intensity",
        default = GraphicsOptions::default().maximum_intensity
    )]
    MaximumIntensity,

    // TODO: should split into enum + separate number
    #[custom(
        key = "graphics/exposure",
        type = camera::ExposureOption,
        display_name = "Exposure",
        default = GraphicsOptions::default().exposure
    )]
    Exposure,

    #[custom(
        key = "graphics/bloom-intensity",
        type = ZeroOne<f32>,
        display_name = "Bloom",
        default = GraphicsOptions::default().bloom_intensity
    )]
    BloomIntensity,

    #[custom(
        key = "graphics/view-distance",
        type = PositiveSign<FreeCoordinate>,
        display_name = "View distance (blocks)",
        default = GraphicsOptions::default().view_distance
    )]
    ViewDistance,

    #[custom(
        key = "graphics/lighting-display",
        type = camera::LightingOption,
        display_name = "Lighting",
        default = GraphicsOptions::default().lighting_display
    )]
    LightingDisplay,

    #[custom(
        key = "graphics/transparency",
        type = camera::TransparencyOption, // TODO: split threshold to its own option to not lose it
        display_name = "Transparency",
        default = GraphicsOptions::default().transparency
    )]
    Transparency,

    #[custom(
        key = "graphics/show-ui",
        type = bool,
        display_name = "Show UI",
        default = GraphicsOptions::default().show_ui
    )]
    ShowUi,

    #[custom(
        key = "graphics/antialiasing",
        type = camera::AntialiasingOption,
        display_name = "Antialiasing",
        default = GraphicsOptions::default().antialiasing
    )]
    Antialiasing,

    #[custom(
        key = "graphics/debug-info-text",
        type = bool,
        display_name = "Debug: Show info text",
        default = GraphicsOptions::default().debug_info_text
    )]
    DebugInfoText,

    #[custom(
        key = "graphics/debug-info-text-contents",
        type = all_is_cubes::util::ShowStatus,
        display_name = "Debug: Info text contents",
        default = GraphicsOptions::default().debug_info_text_contents
    )]
    DebugInfoTextContents,

    #[custom(
        key = "graphics/debug-behaviors",
        type = bool,
        display_name = "Debug: Show behaviors",
        default = GraphicsOptions::default().debug_behaviors
    )]
    DebugBehaviors,

    #[custom(
        key = "graphics/debug-chunk-boxes",
        type = bool,
        display_name = "Debug: Show chunk boxes",
        default = GraphicsOptions::default().debug_chunk_boxes
    )]
    DebugChunkBoxes,

    #[custom(
        key = "graphics/debug-collision-boxes",
        type = bool,
        display_name = "Debug: Show collision boxes",
        default = GraphicsOptions::default().debug_collision_boxes
    )]
    DebugCollisionBoxes,

    #[custom(
        key = "graphics/debug-light-rays-at-cursor",
        type = bool,
        display_name = "Debug: Show light rays at cursor",
        default = GraphicsOptions::default().debug_light_rays_at_cursor
    )]
    DebugLightRaysAtCursor,

    #[custom(
        key = "graphics/debug-pixel-cost",
        type = bool,
        display_name = "Debug: Show rendering cost",
        default = GraphicsOptions::default().debug_pixel_cost
    )]
    DebugPixelCost,

    #[custom(
        key = "graphics/debug-reduce-view-frustum",
        type = bool,
        display_name = "Debug: Increase frustum culling",
        default = GraphicsOptions::default().debug_reduce_view_frustum
    )]
    DebugReduceViewFrustum,
}

impl<'de> serde::Deserialize<'de> for Key {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let string = <Cow<'de, str> as serde::Deserialize<'de>>::deserialize(deserializer)?;
        string.parse::<Self>().map_err(serde::de::Error::custom)
    }
}

pub(super) fn assemble_graphics_options(data: &super::Data) -> GraphicsOptions {
    // TODO: add exhaustivity, perhaps by moving code to the all-is-cubes crate.
    let mut options = GraphicsOptions::default();

    options.render_method = data.get(RENDER_METHOD).clone();
    options.fog = data.get(FOG).clone();
    options.fov_y = *data.get(FOV_Y);
    options.tone_mapping = data.get(TONE_MAPPING).clone();
    options.maximum_intensity = *data.get(MAXIMUM_INTENSITY);
    options.exposure = data.get(EXPOSURE).clone();
    options.bloom_intensity = *data.get(BLOOM_INTENSITY);
    options.view_distance = *data.get(VIEW_DISTANCE);
    options.lighting_display = data.get(LIGHTING_DISPLAY).clone();
    options.transparency = data.get(TRANSPARENCY).clone();
    options.show_ui = *data.get(SHOW_UI);
    options.antialiasing = data.get(ANTIALIASING).clone();
    options.debug_info_text = *data.get(DEBUG_INFO_TEXT);
    options.debug_info_text_contents = *data.get(DEBUG_INFO_TEXT_CONTENTS);
    options.debug_behaviors = *data.get(DEBUG_BEHAVIORS);
    options.debug_chunk_boxes = *data.get(DEBUG_CHUNK_BOXES);
    options.debug_collision_boxes = *data.get(DEBUG_COLLISION_BOXES);
    options.debug_light_rays_at_cursor = *data.get(DEBUG_LIGHT_RAYS_AT_CURSOR);
    options.debug_pixel_cost = *data.get(DEBUG_PIXEL_COST);
    options.debug_reduce_view_frustum = *data.get(DEBUG_REDUCE_VIEW_FRUSTUM);

    options
}

// -------------------------------------------------------------------------------------------------

/// Error from parsing a string as a [`Key`].
#[derive(Clone, Debug)]
#[allow(clippy::exhaustive_structs)]
pub struct UnknownSetting(pub String);

impl fmt::Display for UnknownSetting {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unknown setting: “{}”", self.0)
    }
}

impl core::error::Error for UnknownSetting {}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::settings::Settings;

    #[test]
    fn read_write_round_trip() {
        let settings = Settings::default();
        assert_eq!(
            *RENDER_METHOD.read(&settings.get()),
            camera::RenderMethod::Preferred
        );
        RENDER_METHOD.write(&settings, camera::RenderMethod::Reference);
        assert_eq!(
            *RENDER_METHOD.read(&settings.get()),
            camera::RenderMethod::Reference
        );
    }
}

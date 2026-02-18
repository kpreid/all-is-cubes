use alloc::borrow::Cow;
use alloc::string::String;
use alloc::sync::Arc;
use core::any::Any;
use core::fmt;
use core::str::FromStr;

use bevy_platform::sync::LazyLock;

use all_is_cubes::arcstr::{ArcStr, literal};
use all_is_cubes::math::{FreeCoordinate, PositiveSign, ZeroOne, ps32, zo32};
use all_is_cubes_render::camera::{self, GraphicsOptions};

use crate::settings::serialize::{
    DeserializeError, StringForm, impl_string_form_via_from_str_and_display,
};
use crate::settings::{ParseError, TypedKey};

#[cfg(doc)]
use {crate::settings::Settings, all_is_cubes::behavior::Behavior, all_is_cubes::space::Space};

// -------------------------------------------------------------------------------------------------

macro_rules! derive_settings_schema_from_keys {
    ($(#[$enum_meta:meta])* pub enum Key {
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
        $(#[$enum_meta])*
        pub enum Key {
        $(
            $(#[doc = $field_doc])*
            #[doc = concat!(" * Display name = ", stringify!($display_name), ".")]
            #[doc = concat!(" * Default value = `", stringify!($default_expr), "`.")]
            $variant,
        )*
    }

        impl Key {
            /// Returns the unique string identifier for this setting, which should be used when
            /// serializing it in key-value maps.
            pub fn as_str(&self) -> &str {
                match self {
                    $( Key::$variant => $key_string, )*
                }
            }

            /// Returns the name for this setting to show to users.
            ///
            /// The result is a “literal” [`ArcStr`] referring to a static allocation.
            pub fn display_name(&self) -> ArcStr {
                match self {
                    $( Key::$variant => literal!($display_name), )*
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
#[non_exhaustive]
#[macro_rules_attribute::apply(derive_settings_schema_from_keys!)]
pub enum Key {
    /// Overall rendering technique to use.
    ///
    /// * Directly sets [`GraphicsOptions::render_method`].
    #[custom(
        key = "graphics/render-method",
        type = camera::RenderMethod,
        display_name = "Render method",
        default = GraphicsOptions::default().render_method
    )]
    RenderMethod,

    /// Whether and how to draw fog obscuring the view distance limit.
    ///
    /// * Directly sets [`GraphicsOptions::fog`].
    #[custom(
        key = "graphics/fog",
        type = camera::FogOption,
        display_name = "Fog",
        default = GraphicsOptions::default().fog
    )]
    Fog,

    /// Field of view, in degrees from top to bottom edge of the viewport.
    ///
    /// * Directly sets [`GraphicsOptions::fov_y`].
    #[custom(
        key = "graphics/fov-y",
        type = PositiveSign<FreeCoordinate>,
        display_name = "FOV",
        default = GraphicsOptions::default().fov_y
    )]
    FovY,

    /// Method to use to remap colors to fit within the displayable range.
    ///
    /// * Directly sets [`GraphicsOptions::tone_mapping`].
    #[custom(
        key = "graphics/tone-mapping",
        type = camera::ToneMappingOperator,
        display_name = "Tone mapping",
        default = GraphicsOptions::default().tone_mapping
    )]
    ToneMapping,

    /// Maximum value to allow in the output image’s color channels.
    ///
    /// * Directly sets [`GraphicsOptions::maximum_intensity`].
    #[custom(
        key = "graphics/maximum-intensity",
        type = PositiveSign<f32>,
        display_name = "Maximum intensity",
        default = GraphicsOptions::default().maximum_intensity
    )]
    MaximumIntensity,

    /// Specifies how the exposure (brightness scaling) is determined.
    ///
    /// * Indirectly controls [`GraphicsOptions::exposure`].
    #[custom(
        key = "graphics/exposure-mode",
        type = ExposureMode,
        display_name = "Auto exposure",
        default = ExposureMode::default()
    )]
    ExposureMode,

    /// If [`Key::ExposureMode`] is [`ExposureMode::Fixed`], determines the fixed exposure.
    ///
    /// * Indirectly controls [`GraphicsOptions::exposure`].
    #[custom(
        key = "graphics/fixed-exposure",
        type = PositiveSign<f32>,
        display_name = "Exposure",
        default = ps32(1.0)
    )]
    Exposure,

    /// Proportion of bloom (blurred image) to mix into the original image.
    /// 0.0 is no bloom and 1.0 is no original image.
    ///
    /// * Directly sets [`GraphicsOptions::bloom_intensity`].
    #[custom(
        key = "graphics/bloom-intensity",
        type = ZeroOne<f32>,
        display_name = "Bloom",
        default = GraphicsOptions::default().bloom_intensity
    )]
    BloomIntensity,

    /// Distance, in unit cubes, from the camera to the farthest visible point.
    ///
    /// * Directly sets [`GraphicsOptions::view_distance`].
    #[custom(
        key = "graphics/view-distance",
        type = PositiveSign<FreeCoordinate>,
        display_name = "View distance (blocks)",
        default = GraphicsOptions::default().view_distance
    )]
    ViewDistance,

    /// Style in which to draw the lighting of [`Space`]s.
    /// This does not affect the *computation* of lighting.
    ///
    /// * Directly sets [`GraphicsOptions::lighting_display`].
    #[custom(
        key = "graphics/lighting-display",
        type = camera::LightingOption,
        display_name = "Lighting",
        default = GraphicsOptions::default().lighting_display
    )]
    LightingDisplay,

    /// Method/fidelity to use for transparency.
    ///
    /// * Indirectly controls [`GraphicsOptions::transparency`].
    #[custom(
        key = "graphics/transparency-mode",
        type = TransparencyMode,
        display_name = "Transparency",
        default = TransparencyMode::default()
    )]
    Transparency,

    /// If [`Key::Transparency`] is [`TransparencyMode::Threshold`], sets the threshold.
    ///
    /// * Indirectly controls [`GraphicsOptions::transparency`].
    #[custom(
        key = "graphics/transparency-threshold",
        type = ZeroOne<f32>,
        display_name = "Transparency Threshold",
        default = zo32(0.5)
    )]
    TransparencyThreshold,

    /// Whether to show the HUD or other UI elements.
    ///
    /// TODO: Currently, this does not affect the actual clickability of the UI.
    ///
    /// * Directly sets [`GraphicsOptions::show_ui`].
    #[custom(
        key = "graphics/show-ui",
        type = bool,
        display_name = "Show UI",
        default = GraphicsOptions::default().show_ui
    )]
    ShowUi,

    /// Whether to apply antialiasing techniques.
    ///
    /// * Directly sets [`GraphicsOptions::antialiasing`].
    #[custom(
        key = "graphics/antialiasing",
        type = camera::AntialiasingOption,
        display_name = "Antialiasing",
        default = GraphicsOptions::default().antialiasing
    )]
    Antialiasing,

    /// Draw text overlay showing debug information.
    ///
    /// * Directly sets [`GraphicsOptions::debug_info_text`].
    #[custom(
        key = "graphics/debug-info-text",
        type = bool,
        display_name = "Debug: Show info text",
        default = GraphicsOptions::default().debug_info_text
    )]
    DebugInfoText,

    /// What information should be displayed by [`Key::DebugInfoText`].
    ///
    /// * Directly sets [`GraphicsOptions::debug_info_text_contents`].
    #[custom(
        key = "graphics/debug-info-text-contents",
        type = all_is_cubes::util::ShowStatus,
        display_name = "Debug: Info text contents",
        default = GraphicsOptions::default().debug_info_text_contents
    )]
    DebugInfoTextContents,

    /// Visualize the cost of rendering each pixel, rather than the color of the scene.
    ///
    /// * Directly sets [`GraphicsOptions::debug_pixel_cost`].
    #[custom(
        key = "graphics/debug-pixel-cost",
        type = bool,
        display_name = "Debug: Show rendering cost",
        default = GraphicsOptions::default().debug_pixel_cost
    )]
    DebugPixelCost,

    /// Draw boxes around [`Behavior`]s attached to parts of [`Space`]s.
    /// This may also eventually include further in-world diagnostic information.
    ///
    /// * Directly sets [`GraphicsOptions::debug_behaviors`].
    #[custom(
        key = "graphics/debug-behaviors",
        type = bool,
        display_name = "Debug: Show behaviors",
        default = GraphicsOptions::default().debug_behaviors
    )]
    DebugBehaviors,

    /// Draw boxes around chunk borders and some debug info.
    ///
    /// * Directly sets [`GraphicsOptions::debug_chunk_boxes`].
    #[custom(
        key = "graphics/debug-chunk-boxes",
        type = bool,
        display_name = "Debug: Show chunk boxes",
        default = GraphicsOptions::default().debug_chunk_boxes
    )]
    DebugChunkBoxes,

    /// Draw collision boxes for some objects.
    ///
    /// * Directly sets [`GraphicsOptions::debug_collision_boxes`].
    #[custom(
        key = "graphics/debug-collision-boxes",
        type = bool,
        display_name = "Debug: Show collision boxes",
        default = GraphicsOptions::default().debug_collision_boxes
    )]
    DebugCollisionBoxes,

    /// Draw the light rays that contribute to the selected block.
    ///
    /// * Directly sets [`GraphicsOptions::debug_light_rays_at_cursor`].
    #[custom(
        key = "graphics/debug-light-rays-at-cursor",
        type = bool,
        display_name = "Debug: Show light rays at cursor",
        default = GraphicsOptions::default().debug_light_rays_at_cursor
    )]
    DebugLightRaysAtCursor,

    /// Causes [`Camera`][camera::Camera] to compute a falsified view frustum
    /// which is 1/2 the width and height it should be.
    ///
    /// * Directly sets [`GraphicsOptions::debug_reduce_view_frustum`].
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

// -------------------------------------------------------------------------------------------------
// Settings translating to `GraphicsOptions`

/// Value of the [`EXPOSURE_MODE`] setting.
///
/// Differs from [`camera::ExposureOption`] in that it is a value-less enum; the fixed exposure is
/// stored separately.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, strum::Display, strum::EnumString)]
#[non_exhaustive]
pub enum ExposureMode {
    /// Constant exposure; light values in the scene are multiplied by this value
    /// before the tone mapping operator is applied.
    Fixed,
    /// Exposure adjusts to compensate for the actual brightness of the scene.
    #[default]
    Automatic,
}
impl_string_form_via_from_str_and_display!(ExposureMode);

/// Value of the [`TRANSPARENCY`] setting.
///
/// Differs from [`camera::TransparencyOption`] in that it is a value-less enum; the threshold is
/// stored separately.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, strum::Display, strum::EnumString)]
#[non_exhaustive]
pub enum TransparencyMode {
    /// Conventional transparent surfaces.
    Surface,
    /// Accounts for the thickness of material passed through; colors' alpha values are
    /// interpreted as the opacity of a unit thickness of the material.
    #[default]
    Volumetric,
    /// Alpha above or below a threshold value will be rounded to fully opaque
    /// or fully transparent, respectively.
    Threshold,
}
impl_string_form_via_from_str_and_display!(TransparencyMode);

pub(super) fn assemble_graphics_options(data: &super::Data) -> GraphicsOptions {
    // TODO: add exhaustivity, perhaps by moving code to the all-is-cubes crate.
    let mut options = GraphicsOptions::default();

    options.render_method = data.get(RENDER_METHOD).clone();
    options.fog = data.get(FOG).clone();
    options.fov_y = *data.get(FOV_Y);
    options.tone_mapping = data.get(TONE_MAPPING).clone();
    options.maximum_intensity = *data.get(MAXIMUM_INTENSITY);
    options.exposure = match data.get(EXPOSURE_MODE) {
        ExposureMode::Fixed => camera::ExposureOption::Fixed(*data.get(EXPOSURE)),
        ExposureMode::Automatic => camera::ExposureOption::Automatic,
    };
    options.bloom_intensity = *data.get(BLOOM_INTENSITY);
    options.view_distance = *data.get(VIEW_DISTANCE);
    options.lighting_display = data.get(LIGHTING_DISPLAY).clone();
    options.transparency = match data.get(TRANSPARENCY) {
        TransparencyMode::Surface => camera::TransparencyOption::Surface,
        TransparencyMode::Volumetric => camera::TransparencyOption::Volumetric,
        TransparencyMode::Threshold => {
            camera::TransparencyOption::Threshold(*data.get(TRANSPARENCY_THRESHOLD))
        }
    };
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
    use crate::settings::{Data, Settings};

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

    /// [`camera::ExposureOption`] is built from [`EXPOSURE_MODE`] and [`EXPOSURE`].
    #[test]
    fn exposure() {
        assert_eq!(
            assemble_graphics_options(&Data::default()).exposure,
            camera::ExposureOption::Automatic
        );
        assert_eq!(
            assemble_graphics_options(&Data::from_iter([
                (EXPOSURE_MODE.key(), "Fixed".into()),
                (EXPOSURE.key(), "1.234".into())
            ]))
            .exposure,
            camera::ExposureOption::Fixed(ps32(1.234))
        );
        assert_eq!(
            assemble_graphics_options(&Data::from_iter([(
                EXPOSURE_MODE.key(),
                "Automatic".into()
            )]))
            .exposure,
            camera::ExposureOption::Automatic
        );
    }

    /// [`camera::TransparencyOption`] is built from [`TRANSPARENCY`] and [`TRANSPARENCY_THRESHOLD`].
    #[test]
    fn transparency() {
        assert_eq!(
            assemble_graphics_options(&Data::default()).transparency,
            GraphicsOptions::default().transparency
        );
        assert_eq!(
            assemble_graphics_options(&Data::from_iter([(TRANSPARENCY.key(), "Surface".into())]))
                .transparency,
            camera::TransparencyOption::Surface
        );
        assert_eq!(
            assemble_graphics_options(&Data::from_iter([(
                TRANSPARENCY.key(),
                "Volumetric".into()
            )]))
            .transparency,
            camera::TransparencyOption::Volumetric
        );
        assert_eq!(
            assemble_graphics_options(&Data::from_iter([(TRANSPARENCY.key(), "Threshold".into())]))
                .transparency,
            camera::TransparencyOption::Threshold(zo32(0.5))
        );
    }
}

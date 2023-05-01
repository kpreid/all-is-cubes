use cgmath::One;
use ordered_float::NotNan;

use crate::math::{FreeCoordinate, Rgb, Rgba};

/// Options for controlling rendering (not affecting gameplay except informationally).
///
/// Some options may be ignored by some renderers, such as when they request a particular
/// implementation approach or debug visualization. Renderers should make an effort to
/// report such failings via [`Flaws`](crate::camera::Flaws).
#[derive(Clone, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(default)]
#[non_exhaustive]
pub struct GraphicsOptions {
    /// Whether and how to draw fog obscuring the view distance limit.
    ///
    /// TODO: Implement fog in raytracer.
    pub fog: FogOption,

    /// Field of view, in degrees from top to bottom edge of the viewport.
    pub fov_y: NotNan<FreeCoordinate>,

    /// Method to use to remap colors to fit within the displayable range.
    pub tone_mapping: ToneMappingOperator,

    /// “Camera exposure” value: a scaling factor from scene luminance to displayed
    /// luminance. Note that the exact interpretation of this depends on the chosen
    /// [`tone_mapping`](ToneMappingOperator).
    pub exposure: ExposureOption,

    /// Proportion of bloom (blurred image) to mix into the original image.
    /// 0.0 is no bloom and 1.0 is no original image.
    pub bloom_intensity: NotNan<f32>,

    /// Distance, in unit cubes, from the camera to the farthest visible point.
    ///
    /// TODO: Implement view distance limit (and fog) in raytracer.
    pub view_distance: NotNan<FreeCoordinate>,

    /// Style in which to draw the lighting of [`Space`](crate::space::Space)s.
    /// This does not affect the *computation* of lighting.
    pub lighting_display: LightingOption,

    /// Method/fidelity to use for transparency.
    pub transparency: TransparencyOption,

    /// Whether to show the HUD or other UI elements.
    ///
    /// This does not affect UI state or clickability; it purely controls display.
    /// It is intended for the purpose of asking a [renderer] to produce an image
    /// of the scene without any UI.
    ///
    /// The cursor is not currently considered part of the UI. This may be revisited
    /// later. The “info text” is controlled separately by
    /// [`debug_info_text`](Self::debug_info_text).
    ///
    /// [renderer]: crate::camera::HeadlessRenderer
    pub show_ui: bool,

    /// Whether to apply antialiasing techniques.
    pub antialiasing: AntialiasingOption,

    /// Whether to use frustum culling for drawing only in-view chunks and objects.
    ///
    /// This option is for debugging and performance testing and should not have any
    /// visible effects.
    pub use_frustum_culling: bool,

    /// Draw text overlay showing debug information.
    pub debug_info_text: bool,

    /// Draw boxes around [`Behavior`]s attached to parts of [`Space`]s.
    /// This may also eventually include further in-world diagnostic information.
    ///
    /// [`Behavior`]: crate::behavior::Behavior
    /// [`Space`]: crate::space::Space
    pub debug_behaviors: bool,

    /// Draw boxes around chunk borders and some debug info.
    pub debug_chunk_boxes: bool,

    /// Draw collision boxes for some objects.
    pub debug_collision_boxes: bool,

    /// Draw the light rays that contribute to the selected block.
    pub debug_light_rays_at_cursor: bool,
}

impl GraphicsOptions {
    /// A set of graphics options which differs from [`GraphicsOptions::default()`] in
    /// that it disables all operations which change colors away from their obvious
    /// values; that is, the [`Rgba`] colors you get from a rendering will be identical
    /// (except for quantization error and background colors) to the [`Rgba`] colors
    /// in the depicted [`Atom`](crate::block::Primitive::Atom)s.
    ///
    /// * [`Self::bloom_intensity`] = `0`
    /// * [`Self::fog`] = [`FogOption::None`]
    /// * [`Self::lighting_display`] = [`LightingOption::None`]
    /// * [`Self::tone_mapping`] = [`ToneMappingOperator::Clamp`]
    ///
    /// Future versions may set other options as necessary to maintain the intended
    /// property.
    pub const UNALTERED_COLORS: Self = Self {
        fog: FogOption::None,
        fov_y: notnan!(90.),
        // TODO: Change tone mapping default once we have a good implementation.
        tone_mapping: ToneMappingOperator::Clamp,
        exposure: ExposureOption::Fixed(notnan!(1.)),
        bloom_intensity: notnan!(0.),
        view_distance: notnan!(200.),
        lighting_display: LightingOption::None,
        transparency: TransparencyOption::Volumetric,
        show_ui: true,
        antialiasing: AntialiasingOption::None,
        use_frustum_culling: true,
        debug_info_text: true,
        debug_behaviors: false,
        debug_chunk_boxes: false,
        debug_collision_boxes: false,
        debug_light_rays_at_cursor: false,
    };

    /// Constrain fields to valid/practical values.
    #[must_use]
    pub fn repair(mut self) -> Self {
        self.fov_y = self.fov_y.clamp(NotNan::from(1), NotNan::from(189));
        self.bloom_intensity = self.bloom_intensity.clamp(notnan!(0.0), notnan!(1.0));
        self.view_distance = self
            .view_distance
            .clamp(NotNan::from(1), NotNan::from(10000));
        self
    }
}

impl Default for GraphicsOptions {
    /// Default graphics options broadly have “everything reasonable” turned on
    /// (they may disable things that are not well-implemented yet).
    ///
    /// TODO: Explain exactly what the default is.
    fn default() -> Self {
        Self {
            fog: FogOption::Abrupt,
            fov_y: NotNan::from(90),
            // TODO: Change tone mapping default once we have a good implementation.
            tone_mapping: ToneMappingOperator::Clamp,
            exposure: ExposureOption::default(),
            bloom_intensity: notnan!(0.125),
            view_distance: NotNan::from(200),
            lighting_display: LightingOption::Smooth,
            transparency: TransparencyOption::Volumetric,
            show_ui: true,
            antialiasing: AntialiasingOption::default(),
            use_frustum_culling: true,
            debug_info_text: true,
            debug_behaviors: false,
            debug_chunk_boxes: false,
            debug_collision_boxes: false,
            debug_light_rays_at_cursor: false,
        }
    }
}

/// Choices for [`GraphicsOptions::fog`].
#[derive(Clone, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[non_exhaustive]
pub enum FogOption {
    /// No fog: objects will maintain their color and disappear raggedly.
    None,
    /// Fog starts just before the view distance ends.
    Abrupt,
    /// Compromise between `Abrupt` and `Physical` options.
    Compromise,
    /// Almost physically realistic fog of constant density.
    Physical,
}

/// Choices for [`GraphicsOptions::tone_mapping`].
#[derive(Clone, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[non_exhaustive]
pub enum ToneMappingOperator {
    /// Limit values above the maximum (or below zero) to lie within that range.
    ///
    /// This is the trivial tone mapping operation, and most “correct” for
    /// colors which do lie within the range, but will cause overly bright colors
    /// to change hue (as the RGB components are clamped independently).
    Clamp,

    /// TODO: As currently implemented this is an inadequate placeholder which is
    /// overly dark.
    Reinhard,
}

impl ToneMappingOperator {
    /// Apply this operator to the given high-dynamic-range color value.
    #[inline]
    pub fn apply(&self, input: Rgb) -> Rgb {
        match self {
            ToneMappingOperator::Clamp => input.clamp(),
            // From <https://64.github.io/tonemapping/>, this will cut brightness
            // too much, but the better versions require a parameter of max scene brightness,
            // or more likely for our use case, we'll hook this up to a model of eye
            // adaptation to average brightness.
            ToneMappingOperator::Reinhard => input * (1.0 + input.luminance()).recip(),
        }
    }
}

/// “Camera exposure” control: selection of algorithm to control the scaling factor from
/// scene luminance to displayed luminance. Part of a [`GraphicsOptions`].
///
/// Note that the exact interpretation of this value also depends on on the chosen
/// [`ToneMappingOperator`].
#[derive(Clone, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[non_exhaustive]
pub enum ExposureOption {
    /// Constant exposure; light values in the scene are multiplied by this value
    /// before the tone mapping operator is applied.
    Fixed(NotNan<f32>),
    /// Exposure adjusts to compensate for the actual brightness of the scene.
    ///
    /// Note: If [`GraphicsOptions::lighting_display`] is disabled,
    /// then this currently will act as `Fixed(1.0)`.
    Automatic,
}

impl ExposureOption {
    pub(crate) fn initial(&self) -> NotNan<f32> {
        match *self {
            ExposureOption::Fixed(value) => value,
            ExposureOption::Automatic => NotNan::one(),
        }
    }
}

impl Default for ExposureOption {
    fn default() -> Self {
        ExposureOption::Fixed(NotNan::one())
    }
}

/// How to display light in a [`Space`]; part of a [`GraphicsOptions`].
///
/// [`Space`]: crate::space::Space
#[derive(Clone, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[non_exhaustive]
pub enum LightingOption {
    /// No lighting: objects will be displayed with their original surface color.
    None,
    /// Light is taken from the volume immediately above a cube face.
    /// Edges between cubes are visible.
    Flat,
    /// Light varies across surfaces.
    Smooth,
}

/// How to render transparent objects; part of a [`GraphicsOptions`].
///
/// Note: There is not yet a consistent interpretation of alpha between the `Surface`
/// and `Volumetric` options; this will probably be changed in the future in favor
/// of the volumetric interpretation.
#[derive(Clone, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum TransparencyOption {
    /// Conventional transparent surfaces.
    Surface,
    /// Accounts for the thickness of material passed through; colors' alpha values are
    /// interpreted as the opacity of a unit thickness of the material.
    Volumetric,
    /// Alpha above or below the given threshold value will be rounded to fully opaque
    /// or fully transparent, respectively.
    Threshold(NotNan<f32>),
}

impl TransparencyOption {
    /// Replace a color's alpha value according to the requested threshold,
    /// if any.
    #[inline]
    pub fn limit_alpha(&self, color: Rgba) -> Rgba {
        match *self {
            Self::Threshold(t) => {
                if color.alpha() > t {
                    color.to_rgb().with_alpha_one()
                } else {
                    Rgba::TRANSPARENT
                }
            }
            _ => color,
        }
    }

    #[inline]
    #[doc(hidden)] // TODO: make public/documented?
    pub fn will_output_alpha(&self) -> bool {
        !matches!(self, Self::Threshold(_))
    }
}

/// Choices for [`GraphicsOptions::antialiasing`].
#[derive(Clone, Debug, Default, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum AntialiasingOption {
    /// Do not apply antialiasing. Every pixel of the rendered image will be the exact
    /// color of some part of the world, rather than a combination of adjacent parts.
    #[default]
    None,
    /// If [multisample anti-aliasing](https://en.wikipedia.org/wiki/Multisample_anti-aliasing)
    /// or similar functionality is available, allowing relatively cheap antialiasing,
    /// then enable it.
    IfCheap,
    /// Always perform antialiasing, even if it is expensive.
    Always,
}

impl AntialiasingOption {
    /// True if GPU renderers should enable multisampling
    #[doc(hidden)]
    pub fn is_msaa(&self) -> bool {
        match self {
            Self::None => false,
            Self::IfCheap => true,
            Self::Always => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn default_is_clean() {
        assert_eq!(
            GraphicsOptions::default(),
            GraphicsOptions::default().repair()
        );
    }

    #[test]
    fn unaltered_colors_is_clean() {
        assert_eq!(
            GraphicsOptions::UNALTERED_COLORS,
            GraphicsOptions::UNALTERED_COLORS.repair()
        );
    }

    #[test]
    fn unaltered_colors_differs_from_default_only_as_necessary() {
        // Note that this assertion will pass whether or not the specified values are
        // *also* in GraphicsOptions::default(). That's what we want.
        assert_eq!(
            GraphicsOptions::UNALTERED_COLORS,
            GraphicsOptions {
                fog: FogOption::None,
                tone_mapping: ToneMappingOperator::Clamp,
                exposure: ExposureOption::Fixed(NotNan::one()),
                bloom_intensity: NotNan::from(0u8),
                lighting_display: LightingOption::None,
                antialiasing: AntialiasingOption::None,
                ..GraphicsOptions::default()
            }
        )
    }
}

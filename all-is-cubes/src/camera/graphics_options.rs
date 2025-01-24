use core::fmt;

use num_traits::ConstOne as _;

use crate::math::{ps64, zo32, FreeCoordinate, PositiveSign, Rgb, Rgba, ZeroOne};
use crate::util::ShowStatus;

#[cfg(doc)]
use crate::{block::Block, camera::Camera, space::Space};

/// Options for controlling rendering (not affecting gameplay except informationally).
///
/// Some options may be ignored by some renderers, such as when they request a particular
/// implementation approach or debug visualization. Renderers should make an effort to
/// report such failings, such as via the `all_is_cubes_render::Flaws` type.
//---
// (Due to crate splitting that can't be a doc-link.)
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Clone, Eq, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "save", serde(default))]
#[non_exhaustive]
pub struct GraphicsOptions {
    /// Overall rendering technique to use.
    ///
    /// May be ignored if the method requested is not supported in the current
    /// environment.
    pub render_method: RenderMethod,

    /// Whether and how to draw fog obscuring the view distance limit.
    ///
    /// TODO: Implement fog in raytracer.
    pub fog: FogOption,

    /// Field of view, in degrees from top to bottom edge of the viewport.
    pub fov_y: PositiveSign<FreeCoordinate>,

    /// Method to use to remap colors to fit within the displayable range.
    pub tone_mapping: ToneMappingOperator,

    /// “Camera exposure” value: a scaling factor from scene luminance to displayed
    /// luminance. Note that the exact interpretation of this depends on the chosen
    /// [`tone_mapping`](ToneMappingOperator).
    pub exposure: ExposureOption,

    /// Proportion of bloom (blurred image) to mix into the original image.
    /// 0.0 is no bloom and 1.0 is no original image.
    pub bloom_intensity: ZeroOne<f32>,

    /// Distance, in unit cubes, from the camera to the farthest visible point.
    ///
    /// TODO: Implement view distance limit (and fog) in raytracer.
    pub view_distance: PositiveSign<FreeCoordinate>,

    /// Style in which to draw the lighting of [`Space`](crate::space::Space)s.
    /// This does not affect the *computation* of lighting.
    pub lighting_display: LightingOption,

    /// Method/fidelity to use for transparency.
    pub transparency: TransparencyOption,

    /// Whether to show the HUD or other UI elements.
    ///
    /// This does not affect UI state or clickability; it purely controls display.
    /// It is intended for the purpose of asking a renderer to produce an image
    /// of the scene without any UI.
    ///
    /// The cursor is not currently considered part of the UI. This may be revisited
    /// later. The “info text” is controlled separately by
    /// [`debug_info_text`](Self::debug_info_text).
    pub show_ui: bool,

    /// Whether to apply antialiasing techniques.
    pub antialiasing: AntialiasingOption,

    /// Draw text overlay showing debug information.
    pub debug_info_text: bool,

    /// What information should be displayed in [`debug_info_text`](Self::debug_info_text).
    //---
    // TODO: It's inelegant that this is counted as part of the graphics options, but perhaps
    // not less so than the other debug options...
    pub debug_info_text_contents: ShowStatus,

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

    /// Causes [`Camera`] to compute a falsified view frustum which is 1/2 the width and height
    /// it should be.
    ///
    /// This may be used to visualize the effect of frustum culling that is performed via
    /// [`Camera::aab_in_view()`].
    pub debug_reduce_view_frustum: bool,
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
        render_method: RenderMethod::Preferred,
        fog: FogOption::None,
        fov_y: ps64(90.),
        // TODO: Change tone mapping default once we have a good implementation.
        tone_mapping: ToneMappingOperator::Clamp,
        exposure: ExposureOption::Fixed(PositiveSign::<f32>::ONE),
        bloom_intensity: zo32(0.),
        view_distance: ps64(200.),
        lighting_display: LightingOption::None,
        transparency: TransparencyOption::Volumetric,
        show_ui: true,
        antialiasing: AntialiasingOption::None,
        debug_info_text: true,
        debug_info_text_contents: ShowStatus::DEFAULT,
        debug_behaviors: false,
        debug_chunk_boxes: false,
        debug_collision_boxes: false,
        debug_light_rays_at_cursor: false,
        debug_reduce_view_frustum: false,
    };

    /// Constrain fields to valid/practical values.
    #[must_use]
    pub fn repair(mut self) -> Self {
        self.fov_y = self.fov_y.clamp(ps64(1.), ps64(189.));
        self.view_distance = self.view_distance.clamp(ps64(1.), ps64(10000.));
        self
    }
}

impl fmt::Debug for GraphicsOptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            render_method,
            fog,
            fov_y,
            tone_mapping,
            exposure,
            bloom_intensity,
            view_distance,
            lighting_display,
            transparency,
            show_ui,
            antialiasing,
            debug_info_text,
            debug_info_text_contents,
            debug_behaviors,
            debug_chunk_boxes,
            debug_collision_boxes,
            debug_light_rays_at_cursor,
            debug_reduce_view_frustum,
        } = self;
        // This custom impl reduces unnecessary text by stripping off NotNan wrappers.
        f.debug_struct("GraphicsOptions")
            .field("render_method", render_method)
            .field("fog", fog)
            .field("fov_y", &fov_y.into_inner())
            .field("tone_mapping", &tone_mapping)
            .field("exposure", &exposure)
            .field("bloom_intensity", &bloom_intensity.into_inner())
            .field("view_distance", &view_distance.into_inner())
            .field("lighting_display", &lighting_display)
            .field("transparency", &transparency)
            .field("show_ui", &show_ui)
            .field("antialiasing", &antialiasing)
            .field("debug_info_text", &debug_info_text)
            .field("debug_info_text_contents", &debug_info_text_contents)
            .field("debug_behaviors", &debug_behaviors)
            .field("debug_chunk_boxes", &debug_chunk_boxes)
            .field("debug_collision_boxes", &debug_collision_boxes)
            .field("debug_light_rays_at_cursor", &debug_light_rays_at_cursor)
            .field("debug_reduce_view_frustum", &debug_reduce_view_frustum)
            .finish()
    }
}

impl Default for GraphicsOptions {
    /// Default graphics options broadly have “everything reasonable” turned on
    /// (they may disable things that are not well-implemented yet).
    ///
    /// TODO: Explain exactly what the default is.
    fn default() -> Self {
        Self {
            render_method: RenderMethod::Preferred,
            fog: FogOption::Abrupt,
            fov_y: ps64(90.),
            // TODO: Change tone mapping default once we have a good implementation.
            tone_mapping: ToneMappingOperator::Clamp,
            exposure: ExposureOption::default(),
            bloom_intensity: zo32(0.125),
            view_distance: ps64(200.),
            lighting_display: LightingOption::Smooth,
            transparency: TransparencyOption::Volumetric,
            show_ui: true,
            antialiasing: AntialiasingOption::default(),
            debug_info_text: true,
            debug_info_text_contents: ShowStatus::DEFAULT,
            debug_behaviors: false,
            debug_chunk_boxes: false,
            debug_collision_boxes: false,
            debug_light_rays_at_cursor: false,
            debug_reduce_view_frustum: false,
        }
    }
}

/// Choices for [`GraphicsOptions::render_method`].
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum RenderMethod {
    /// Use whichever method is presumed to be better for the current situation.
    ///
    /// Currently, this typically means [`RenderMethod::Reference`] for
    /// non-interactive (headless) rendering and [`RenderMethod::Mesh`] for
    /// interactive usage.
    Preferred,

    /// Make triangle meshes of [`Block`]s and of chunks of [`Space`]s and draw them
    /// using the GPU (or software triangle rasterizer as a fallback).
    ///
    /// As of this documentation being written, the available implementation has trouble
    /// with transparent volumes.
    Mesh,

    /// Use the reference implementation of All is Cubes content rendering.
    ///
    /// This means `all_is_cubes_render::raytracer`, a CPU-based raytracer.
    /// It is too slow for high-resolution interactive use, though it does have a potential
    /// advantage in rapidly changing content.
    Reference,
    // TODO: Someday, GpuRaytracing.
}

/// Choices for [`GraphicsOptions::fog`].
///
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
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
///
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
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
            ToneMappingOperator::Clamp => input.clamp(<_>::ONE),
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
///
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Clone, Eq, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum ExposureOption {
    /// Constant exposure; light values in the scene are multiplied by this value
    /// before the tone mapping operator is applied.
    Fixed(PositiveSign<f32>),
    /// Exposure adjusts to compensate for the actual brightness of the scene.
    ///
    /// Note: If [`GraphicsOptions::lighting_display`] is disabled,
    /// then this currently will act as `Fixed(1.0)`.
    Automatic,
}

impl fmt::Debug for ExposureOption {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fixed(value) => write!(f, "Fixed({})", value.into_inner()),
            Self::Automatic => write!(f, "Automatic"),
        }
    }
}

impl ExposureOption {
    pub(crate) fn initial(&self) -> PositiveSign<f32> {
        match *self {
            ExposureOption::Fixed(value) => value,
            ExposureOption::Automatic => PositiveSign::<f32>::ONE,
        }
    }
}

impl Default for ExposureOption {
    fn default() -> Self {
        ExposureOption::Fixed(PositiveSign::<f32>::ONE)
    }
}

/// How to display light in a [`Space`]; part of a [`GraphicsOptions`].
///
#[doc = include_str!("../save/serde-warning.md")]
///
/// [`Space`]: crate::space::Space
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
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
///
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum TransparencyOption {
    /// Conventional transparent surfaces.
    Surface,
    /// Accounts for the thickness of material passed through; colors' alpha values are
    /// interpreted as the opacity of a unit thickness of the material.
    Volumetric,
    /// Alpha above or below the given threshold value will be rounded to fully opaque
    /// or fully transparent, respectively.
    Threshold(ZeroOne<f32>),
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
///
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
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
    // TODO: These functions allow dependents to decide what to do even though the
    // enum is non_exhaustive. Figure out if it really should be non_exhaustive or
    // if we should make these really public.

    /// True if GPU renderers should enable multisampling
    #[doc(hidden)]
    #[mutants::skip] // a test would only reiterate the code
    pub fn is_msaa(&self) -> bool {
        match self {
            Self::None => false,
            Self::IfCheap => true,
            Self::Always => true,
        }
    }

    /// True if renderers for which antialiasing is expensive should do it anyway.
    #[doc(hidden)]
    #[mutants::skip] // a test would only reiterate the code
    pub fn is_strongly_enabled(&self) -> bool {
        match self {
            Self::None => false,
            Self::IfCheap => false,
            Self::Always => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::{rgba_const, zo32, OpacityCategory};
    use pretty_assertions::assert_eq;

    #[test]
    fn debug() {
        let options = GraphicsOptions::default();
        assert_eq!(
            format!("{options:#?}"),
            indoc::indoc! {
                r"GraphicsOptions {
                    render_method: Preferred,
                    fog: Abrupt,
                    fov_y: 90.0,
                    tone_mapping: Clamp,
                    exposure: Fixed(1),
                    bloom_intensity: 0.125,
                    view_distance: 200.0,
                    lighting_display: Smooth,
                    transparency: Volumetric,
                    show_ui: true,
                    antialiasing: None,
                    debug_info_text: true,
                    debug_info_text_contents: ShowStatus(
                        WORLD | STEP | RENDER | CURSOR,
                    ),
                    debug_behaviors: false,
                    debug_chunk_boxes: false,
                    debug_collision_boxes: false,
                    debug_light_rays_at_cursor: false,
                    debug_reduce_view_frustum: false,
                }"
            }
        );
    }

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
                exposure: ExposureOption::Fixed(PositiveSign::<f32>::ONE),
                bloom_intensity: zo32(0.),
                lighting_display: LightingOption::None,
                antialiasing: AntialiasingOption::None,
                ..GraphicsOptions::default()
            }
        )
    }

    #[test]
    fn will_output_alpha() {
        for transparency in &[
            TransparencyOption::Surface,
            TransparencyOption::Volumetric,
            TransparencyOption::Threshold(zo32(0.5)),
        ] {
            assert_eq!(
                transparency.will_output_alpha(),
                transparency
                    .limit_alpha(rgba_const!(1.0, 1.0, 1.0, 0.25))
                    .opacity_category()
                    == OpacityCategory::Partial
            );
        }
    }
}

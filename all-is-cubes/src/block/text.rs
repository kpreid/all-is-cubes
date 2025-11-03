//! Support for [`Primitive::Text`].

#![expect(
    clippy::module_name_repetitions,
    reason = "module is private; https://github.com/rust-lang/rust-clippy/issues/8524"
)]

use alloc::boxed::Box;
use core::iter;
use eg::mono_font::MonoFont;
use embedded_graphics::mono_font::mapping::GlyphMapping;

use arcstr::ArcStr;
use embedded_graphics as eg;
use embedded_graphics::{Drawable as _, prelude::Dimensions as _};
use euclid::vec3;

use crate::block::{self, Block, BlockAttributes, Evoxel, MinEval, Resolution};
use crate::content::palette;
use crate::drawing::{DrawingPlane, rectangle_to_aab};
use crate::math::{
    FaceMap, GridAab, GridCoordinate, GridVector, Gridgid, Rgb, Rgba, Vol, rgba_const,
};
use crate::space::{self, SpaceTransaction};
use crate::universe;

#[cfg(doc)]
use crate::block::{Modifier, Primitive};

use super::Evoxels;

/// A piece of text rendered as voxels.
///
/// Each `Text` contains:
///
/// * A string, as [`ArcStr`].
/// * A [`Font`].
/// * [`Block`]s defining voxel colors and attributes used.
/// * A [`Resolution`] defining the font size.
/// * A bounding box within which the text is positioned (but may overflow).
/// * A [`Positioning`] specifying how the text is positioned within the box.
///
/// To create a block or multiblock group from this, use [`Primitive::Text`].
/// To combine the text with other shapes, use [`Modifier::Composite`].
///
//--
// TODO: Each `Text` instance should memoize glyph layout so that layout work can be shared among
// blocks. We don't really have much to do there yet, though.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Text {
    string: ArcStr,

    font: Font,

    foreground: Block,

    outline: Option<Block>,

    resolution: Resolution,

    /// Voxel-scale bounds in which the text is positioned (not necessarily actual drawing bounds).
    layout_bounds: GridAab,

    positioning: Positioning,

    debug: bool,
}

/// Builder for [`Text`] values.
#[derive(Debug, Clone)]
#[must_use]
pub struct TextBuilder {
    string: ArcStr,

    font: Font,

    foreground: Block,

    outline: Option<Block>,

    resolution: Resolution,

    layout_bounds: Option<GridAab>,

    positioning: Positioning,

    debug: bool,
}

impl Text {
    /// Returns a [`TextBuilder`] which may be used to construct a [`Text`] value with explicit
    /// or default options.
    pub fn builder() -> TextBuilder {
        TextBuilder::default()
    }

    /// Converts this into a [`TextBuilder`] so that it may be modified.
    pub fn into_builder(self) -> TextBuilder {
        let Self {
            string,
            font,
            foreground,
            outline,
            resolution,
            layout_bounds,
            positioning,
            debug,
        } = self;
        TextBuilder {
            string,
            font,
            foreground,
            outline,
            resolution,
            layout_bounds: Some(layout_bounds),
            positioning,
            debug,
        }
    }

    /// Returns the string which this displays.
    pub fn string(&self) -> &ArcStr {
        &self.string
    }
    /// Returns the font which this uses to display the text.
    pub fn font(&self) -> &Font {
        &self.font
    }

    /// Returns the voxel resolution which the text blocks will have.
    pub fn resolution(&self) -> &Font {
        &self.font
    }

    /// Returns the bounding box, within the blocks at the specified resolution, of the text.
    ///
    /// This does not reflect the actual size of the text but the configuration with which this
    /// [`Text`] value was constructed.
    /// The text may overflow this bounding box depending on its length and the
    /// [`positioning()`](Self::positioning).
    pub fn layout_bounds(&self) -> GridAab {
        self.layout_bounds
    }

    /// Returns the [`Positioning`] parameters this uses.
    pub fn positioning(&self) -> Positioning {
        self.positioning
    }

    /// Returns the debug-rendering flag.
    pub fn debug(&self) -> bool {
        self.debug
    }

    /// Returns the bounding box of the text as displayed, in voxels at the
    /// [`resolution()`](Self::resolution).
    ///
    /// This box is in the same units as [`Self::layout_bounds()`] but reflects the actual text
    /// layout rather than the configuration.
    pub fn bounding_voxels(&self) -> GridAab {
        // TODO: Memoize this “layout calculation”.

        let dummy_voxel = Evoxel::from_color(Rgba::BLACK); // could be anything
        self.with_transform_and_drawable(
            match self.outline {
                Some(_) => Brush::Outline {
                    foreground: dummy_voxel,
                    outline: dummy_voxel,
                },
                None => Brush::Plain(dummy_voxel),
            },
            GridVector::zero(),
            |_text_obj, text_aab, _drawing_transform| text_aab,
        )
    }
    /// Returns the bounding box of the text, in blocks — the set of [`Primitive::Text`] offsets
    /// that will render all of it.
    ///
    /// This is identical to [`Self::bounding_voxels()`] scaled down by [`Self::resolution()`].
    pub fn bounding_blocks(&self) -> GridAab {
        self.bounding_voxels().divide(self.resolution.into())
    }

    /// Returns a transaction which places [`Primitive::Text`] blocks containing this text.
    ///
    /// The text lies within the volume [`Self::bounding_blocks()`] transformed by `transform`.
    ///
    /// Each individual block is given to `block_fn` to allow alterations.
    ///
    /// The transaction has no preconditions.
    ///
    /// Panics if `transform` causes coordinate overflow.
    pub fn installation(
        &self,
        transform: Gridgid,
        block_fn: impl Fn(Block) -> Block,
    ) -> SpaceTransaction {
        let dst_to_src_transform = transform.inverse();
        let block_rotation = transform.rotation;
        SpaceTransaction::filling(
            self.bounding_blocks().transform(transform).unwrap(),
            |cube| {
                space::CubeTransaction::replacing(
                    None,
                    Some(block_fn(
                        Block::from_primitive(block::Primitive::Text {
                            text: self.clone(),
                            offset: dst_to_src_transform
                                .transform_cube(cube)
                                .lower_bounds()
                                .to_vector(),
                        })
                        .rotate(block_rotation),
                    )),
                )
            },
        )
    }

    /// Returns a `Block` whose primitive is this text with no offset,
    /// for quickly creating blocks from text that fits in one block.
    pub fn single_block(self) -> Block {
        Block::from_primitive(block::Primitive::Text {
            text: self,
            offset: GridVector::zero(),
        })
    }

    /// Called by [`Primitive::Text`] evaluation to actually produce the voxels for a specific
    /// [`Block`] of text.
    pub(in crate::block) fn evaluate(
        &self,
        block_offset: GridVector,
        filter: &super::EvalFilter<'_>,
    ) -> Result<MinEval, block::InEvalError> {
        if filter.skip_eval {
            // TODO: Once we have a `Handle<FontDef>` or something, this will need to
            // check that before returning.
            return Ok(block::AIR_EVALUATED_MIN); // placeholder value
        }

        // Evaluate blocks making up the brush
        let brush = {
            let _recursion_scope = block::Budget::recurse(&filter.budget)?;

            // TODO: We could save a small amount of work by not building the full
            // `EvaluatedBlock` here and throwing it away.
            let ev_foreground = self.foreground.evaluate_to_evoxel_internal(filter)?;
            match self.outline {
                Some(ref block) => Brush::Outline {
                    foreground: ev_foreground,
                    outline: block.evaluate_to_evoxel_internal(filter)?,
                },
                None => Brush::Plain(ev_foreground),
            }
        };

        self.with_transform_and_drawable(
            brush,
            block_offset,
            |text_obj, text_aab, drawing_transform| {
                let voxels: Evoxels =
                    match text_aab.intersection_cubes(GridAab::for_block(self.resolution)) {
                        Some(bounds_in_this_block) => {
                            let fill = if self.debug {
                                DEBUG_TEXT_BOUNDS_VOXEL
                            } else {
                                Evoxel::AIR
                            };
                            let mut voxels: Vol<Box<[Evoxel]>> =
                                Vol::from_fn(bounds_in_this_block, |_| fill);

                            text_obj
                                .draw(&mut DrawingPlane::new(&mut voxels, drawing_transform))
                                .unwrap();

                            Evoxels::from_many(self.resolution, voxels.map_container(Into::into))
                        }

                        None => Evoxels::from_one(if self.debug {
                            DEBUG_NO_INTERSECTION_VOXEL
                        } else {
                            Evoxel::AIR
                        }),
                    };

                Ok(MinEval::new(
                    BlockAttributes {
                        display_name: self.string.clone(),
                        ..BlockAttributes::default()
                    },
                    voxels,
                ))
            },
        )
    }

    fn thickness(&self) -> GridCoordinate {
        match self.outline {
            Some(_) => 2,
            None => 1,
        }
    }

    fn with_transform_and_drawable<R>(
        &self,
        brush: Brush,
        block_offset: GridVector,
        f: impl FnOnce(
            &'_ eg::text::Text<'_, eg::mono_font::MonoTextStyle<'_, Brush>>,
            GridAab,
            Gridgid,
        ) -> R,
    ) -> R {
        let resolution_g = GridCoordinate::from(self.resolution);
        let Positioning {
            x: positioning_x,
            line_y,
            z: positioning_z,
        } = self.positioning;

        let lb = self.layout_bounds;
        // TODO: The subtractions of 1 here are dubious.
        // I believe they are correct on the principle that embedded-graphics uses
        // "a point labels a pixel" coordinates, so even leftward-extending text
        // *includes* the identified pixel, but I'm not certain about that.
        let layout_offset = vec3(
            match positioning_x {
                PositioningX::Left => lb.lower_bounds().x,
                // 0.75 is a fudge factor that empirically gets the *rounding* behavior we want.
                // though I'm still suspicious that we need to account for the text width too
                PositioningX::Center => libm::round(lb.center().x - 0.75) as GridCoordinate,
                PositioningX::Right => lb.upper_bounds().x - 1,
            },
            match line_y {
                PositioningY::BodyBottom | PositioningY::Baseline => lb.lower_bounds().y,
                PositioningY::BodyMiddle => libm::round(lb.center().y - 0.75) as GridCoordinate,
                PositioningY::BodyTop => lb.upper_bounds().y - 1,
            },
            match positioning_z {
                PositioningZ::Back => lb.lower_bounds().z,
                PositioningZ::Front => lb.upper_bounds().z.saturating_sub(self.thickness()),
            },
        );

        let drawing_transform =
            Gridgid::from_translation(layout_offset - (block_offset * resolution_g))
                * Gridgid::FLIP_Y;

        let character_style = eg::mono_font::MonoTextStyle::new(self.font.eg_font(), brush);
        let text_style = eg::text::TextStyleBuilder::new()
            .alignment(match positioning_x {
                PositioningX::Left => eg::text::Alignment::Left,
                PositioningX::Center => eg::text::Alignment::Center,
                PositioningX::Right => eg::text::Alignment::Right,
            })
            .baseline(match line_y {
                PositioningY::BodyTop => eg::text::Baseline::Top,
                PositioningY::BodyMiddle => eg::text::Baseline::Middle,
                PositioningY::Baseline => eg::text::Baseline::Alphabetic,
                PositioningY::BodyBottom => eg::text::Baseline::Bottom,
            })
            .build();
        let text_obj = &eg::text::Text::with_text_style(
            self.string.as_str(),
            eg::prelude::Point::new(0, 0),
            character_style,
            text_style,
        );
        let text_aab = brush.expand(rectangle_to_aab(
            text_obj.bounding_box(),
            drawing_transform,
            GridAab::ORIGIN_CUBE,
        ));

        f(text_obj, text_aab, drawing_transform)
    }
}

impl universe::VisitHandles for Text {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            string: _,
            font,
            foreground,
            outline,
            resolution: _,
            layout_bounds: _,
            positioning: _,
            debug: _,
        } = self;
        font.visit_handles(visitor);
        foreground.visit_handles(visitor);
        outline.visit_handles(visitor);
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Text {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        // As a temporary measure, restrict GridAab coordinate range to avoid overflows.
        // Eventually we should fix the overflows, but let's do that after we build our own
        // text rendering because that'll be easier. Once that’s done, replace this with a
        // derived implementation.
        //
        // (Another possibility would be to actually restrict `layout_bounds` itself.)
        let layout_bounds = GridAab::checked_from_lower_upper(
            <[i16; 3]>::arbitrary(u)?.map(i32::from),
            <[u16; 3]>::arbitrary(u)?.map(i32::from),
        )
        .map_err(|_volume_error| arbitrary::Error::IncorrectFormat)?;

        Ok(Self {
            // ArcStr doesn't implement Arbitrary
            string: alloc::string::String::arbitrary(u)?.into(),
            font: Font::arbitrary(u)?,
            foreground: Block::arbitrary(u)?,
            outline: Option::<Block>::arbitrary(u)?,
            resolution: Resolution::arbitrary(u)?,
            layout_bounds,
            positioning: Positioning::arbitrary(u)?,
            debug: bool::arbitrary(u)?,
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        // recommended impl from trait documentation
        Self::try_size_hint(depth).unwrap_or_default()
    }

    fn try_size_hint(
        depth: usize,
    ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        // Note that `Text` is recursive because `Text` contains `Block` and `Block` contains
        // `Text`. However, this will still produce a useful, cheap result because
        // `Block::size_hint()` has an explicitly set bound.

        arbitrary::size_hint::try_recursion_guard(depth, |depth| {
            Ok(arbitrary::size_hint::and_all(&[
                alloc::string::String::size_hint(depth),
                Font::size_hint(depth),
                Block::size_hint(depth),
                Option::<Block>::size_hint(depth),
                Resolution::size_hint(depth),
                GridAab::size_hint(depth),
                bool::size_hint(depth),
            ]))
        })
    }
}

impl TextBuilder {
    /// Converts this builder into a [`Text`] value.
    pub fn build(self) -> Text {
        let Self {
            string,
            font,
            foreground,
            outline,
            resolution,
            layout_bounds,
            positioning,
            debug,
        } = self;
        Text {
            string,
            font,
            foreground,
            outline,
            resolution,
            layout_bounds: layout_bounds.unwrap_or(GridAab::for_block(resolution)),
            positioning,
            debug,
        }
    }

    /// Sets the string to be displayed.
    ///
    /// The string may contain newlines, but is not automatically wrapped beyond that.
    ///
    /// The default is the empty string.
    pub fn string(mut self, string: ArcStr) -> Self {
        self.string = string;
        self
    }

    /// Sets the font to use.
    ///
    /// The default is [`Font::System16`].
    pub fn font(mut self, font: Font) -> Self {
        self.font = font;
        self
    }

    /// Sets the “foreground color”, or rather voxel, for the text.
    ///
    /// This block is interpreted as a voxel in the same way as a block in a [`Primitive::Recur`]
    /// space would be. However, the result of evaluating this block not cached. Therefore, it is
    /// highly recommended that the block be stored in a [`block::BlockDef`] (which does cache)
    /// if it is not trivial, particularly if this text is going to span multiple blocks.
    ///
    /// The default value is `Block::from(all_is_cubes_content::palette::ALMOST_BLACK)`.
    pub fn foreground(mut self, foreground: Block) -> Self {
        self.foreground = foreground;
        self
    }

    /// Sets the outline color for the text.
    ///
    /// This appears 1 voxel behind and to the side of the main color; making the overall text
    /// 2 voxels thick.
    ///
    /// See [`Self::foreground()`] for information about how the block is used.
    ///
    /// The default value is [`None`].
    pub fn outline(mut self, outline: Option<Block>) -> Self {
        self.outline = outline;
        self
    }

    /// Sets the voxel resolution to use.
    ///
    /// This affects the size of the text, including its thickness, and the units of
    /// [`layout_bounds`](Self::layout_bounds).
    ///
    /// The default is [16](Resolution::R16).
    pub fn resolution(mut self, resolution: Resolution) -> Self {
        self.resolution = resolution;
        self
    }

    /// Sets the position of the text within (or extending out of) the
    /// [`layout_bounds`](Self::layout_bounds).
    ///
    /// The default is:
    ///
    /// ```rust
    /// # use all_is_cubes::block::text::*;
    /// # let p =
    /// Positioning {
    ///     x: PositioningX::Center,
    ///     line_y: PositioningY::BodyMiddle,
    ///     z: PositioningZ::Back,
    /// }
    /// # ;
    /// # assert_eq!(p, TextBuilder::default().build().positioning());
    /// ```
    pub fn positioning(mut self, positioning: Positioning) -> Self {
        self.positioning = positioning;
        self
    }

    /// Sets the voxel bounding box within which the text is positioned,
    /// as well as the resolution since that determines the coordinate system scale.
    ///
    /// The text might overflow this box; it is currently used only to choose the positioning of the
    /// text and cannot constrain it.
    pub fn layout_bounds(mut self, resolution: Resolution, bounds: GridAab) -> Self {
        self.layout_bounds = Some(bounds);
        self.resolution = resolution;
        self
    }
    /// Sets the debug-rendering flag.
    ///
    /// If true, then the text rendering is modified in an unspecified way which is intended to
    /// assist in diagnosing issues with text layout configuration. In particular, this currently
    /// includes:
    ///
    /// * The region which is in bounds but not filled by a character is filled with a
    ///   semi-transparent marker color instead of being transparent.
    /// * If a [`Primitive::Text`] block's bounds fail to intersect its [`Text`], then the block
    ///   is filled with a marker color.
    pub fn debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
    }
}

impl Default for TextBuilder {
    fn default() -> Self {
        Self {
            string: ArcStr::new(),
            font: Font::System16,
            foreground: block::from_color!(palette::ALMOST_BLACK),
            outline: None,
            resolution: Resolution::R16,
            layout_bounds: None,
            positioning: Positioning {
                x: PositioningX::Center,
                line_y: PositioningY::BodyMiddle,
                z: PositioningZ::Back,
            },
            debug: false,
        }
    }
}

/// A font that may be used with [`Text`] blocks.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Font {
    /// A font whose characteristics are unspecified, other than that it is general-purpose and
    /// has a line height (vertical distance from a point on one line to the corresponding
    /// point of the next line) of 16 voxels.
    ///
    /// This is a placeholder for further improvement in the font system.
    System16,

    #[doc(hidden)]
    // experimental while we figure things out. Probably to be replaced with fonts stored in Universe
    Logo,

    #[doc(hidden)]
    // experimental while we figure things out. Probably to be replaced with fonts stored in Universe
    SmallerBodyText,
}
impl Font {
    /// Do not use. This will be removed if and when we change font renderers.
    #[doc(hidden)]
    pub fn eg_font(&self) -> &MonoFont<'static> {
        use eg::mono_font::iso_8859_1 as f;
        match self {
            Self::System16 => &MonoFont {
                glyph_mapping: &RemapTo8859_1(f::FONT_8X13_BOLD.glyph_mapping),
                ..f::FONT_8X13_BOLD
            },
            Self::Logo => &MonoFont {
                glyph_mapping: &RemapTo8859_1(f::FONT_9X15_BOLD.glyph_mapping),
                ..f::FONT_9X15_BOLD
            },
            Self::SmallerBodyText => &MonoFont {
                glyph_mapping: &RemapTo8859_1(f::FONT_6X10.glyph_mapping),
                ..f::FONT_6X10
            },
        }
    }
}

impl universe::VisitHandles for Font {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        match self {
            Self::System16 | Self::SmallerBodyText | Self::Logo => {}
        }
    }
}

/// How a [`Text`] is to be positioned within a block.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[expect(
    clippy::exhaustive_structs,
    reason = "TODO: probably want to do something else"
)]
pub struct Positioning {
    /// How to place the text horizontally relative to the anchor point.
    pub x: PositioningX,

    // TODO: implement this
    // /// How to place the text's first or last line relative to the anchor point.
    // pub total_y: (),
    /// How to place the characters of the first line relative to the anchor point.
    pub line_y: PositioningY,

    /// How to place the text depthwise.
    ///
    /// For example, 0 means the voxels will be fill the `0..1` range (in front of the
    /// anchor point), and `-1` means they will fill the `-1..0` range (behind the anchor point).
    ///
    /// This is in units of whatever voxel resolution the font itself uses. Therefore, it should
    /// not be used for positioning the text overall,
    /// but rather for voxel-level effects like engraving vs. embossing.
    pub z: PositioningZ,
}

/// How a [`Text`] is to be positioned within the layout bounds, along the X axis (horizontally).
///
/// A component of [`Positioning`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum PositioningX {
    // TODO: Distinguish 'end of graphic' (last bit of ink) from 'nominal character spacing'?
    /// Left (most negative X) end of the line of text is positioned at the left edge of the
    /// layout bounds.
    ///
    /// In the event that RTL text support is added, this is not necessarily the start of the text.
    Left,

    /// Center the text within the layout bounds.
    Center,

    /// Right (most positive X) end of the line of text is positioned at the right edge of the
    /// layout bounds.
    ///
    /// In the event that RTL text support is added, this is not necessarily the end of the text.
    Right,
}

/// How a [`Text`] is to be positioned within the layout bounds, along the Y axis (vertically).
///
/// A component of [`Positioning`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum PositioningY {
    /// The top of a line of text (past which no voxels extend) is aligned with the top edge
    /// of the layout bounds.
    BodyTop,

    /// The text is positioned halfway between `BodyTop` and `BodyBottom`, centered within the
    /// layout bounds.
    /// This may not necessarily visually center the font, but it will leave the most actually
    /// blank margin.
    BodyMiddle,

    /// The bottom edge (of most characters, excluding descenders and accents) is positioned
    /// at the bottom edge of the layout bounds.
    Baseline,

    /// The bottom of a line of text (past which no voxels extend) is aligned with the bottom edge
    /// of the layout bounds.
    BodyBottom,
}

/// How a [`Text`] is to be positioned within the layout bounds, along the Z axis (depth).
///
/// A component of [`Positioning`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum PositioningZ {
    /// Against the back (negative Z) face of the layout bounds.
    Back,

    /// Against the front (positive Z) face of the layout bounds.
    Front,
}

#[cfg(feature = "save")]
mod serialization {
    use crate::block::text;
    use crate::save::schema;

    impl From<&text::Text> for schema::TextSer {
        fn from(value: &text::Text) -> Self {
            let &text::Text {
                ref string,
                ref font,
                ref foreground,
                ref outline,
                resolution,
                layout_bounds,
                positioning,
                debug,
            } = value;
            schema::TextSer::TextV1 {
                string: string.clone(),
                font: font.into(),
                foreground: foreground.clone(),
                outline: outline.clone(),
                resolution,
                layout_bounds,
                positioning: positioning.into(),
                debug,
            }
        }
    }

    impl From<schema::TextSer> for text::Text {
        fn from(value: schema::TextSer) -> Self {
            match value {
                schema::TextSer::TextV1 {
                    string,
                    font,
                    foreground,
                    outline,
                    resolution,
                    layout_bounds,
                    positioning,
                    debug,
                } => text::Text::builder()
                    .string(string)
                    .font(font.into())
                    .foreground(foreground)
                    .outline(outline)
                    .layout_bounds(resolution, layout_bounds)
                    .positioning(positioning.into())
                    .debug(debug)
                    .build(),
            }
        }
    }
}

impl Positioning {
    #[doc(hidden)] // not sure if good idea
    pub const LOW: Self = Positioning {
        x: PositioningX::Left,
        line_y: PositioningY::BodyBottom,
        z: PositioningZ::Back,
    };
}

const DEBUG_NO_INTERSECTION_VOXEL: Evoxel = Evoxel {
    color: rgba_const!(1.0, 0.0, 0.0, 0.5),
    emission: Rgb::ZERO,
    selectable: true,
    collision: block::BlockCollision::None,
};
const DEBUG_TEXT_BOUNDS_VOXEL: Evoxel = Evoxel {
    color: rgba_const!(0.0, 1.0, 0.0, 0.5),
    emission: Rgb::ZERO,
    selectable: true,
    collision: block::BlockCollision::None,
};

/// Type which can be used on a `DrawingPlane` of `Evoxel`s to render text.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Brush {
    Plain(Evoxel),
    Outline { foreground: Evoxel, outline: Evoxel },
}

impl Brush {
    fn expand(&self, aab: GridAab) -> GridAab {
        match self {
            Brush::Plain(_) => aab,
            Brush::Outline { .. } => aab.expand(FaceMap {
                // TODO: This *should* expand bounds left, right, up, and down, but for now, that
                // causes way too much trouble with positioning. As a workaround, pretend those
                // expansions don't exist.
                nx: 0, // should be 1
                ny: 0, // should be 1
                nz: 0,
                px: 0, // should be 1
                py: 0, // should be 1
                pz: 1,
            }),
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (GridVector, Evoxel)> + '_ {
        use itertools::Either::{Left, Right};
        match *self {
            Brush::Plain(foreground) => Left(iter::once((GridVector::zero(), foreground))),
            Brush::Outline {
                foreground,
                outline,
            } => Right(
                [
                    (vec3(0, 0, 1), foreground),
                    (vec3(0, 0, 0), outline),
                    (vec3(-1, 0, 0), outline),
                    (vec3(1, 0, 0), outline),
                    (vec3(0, -1, 0), outline),
                    (vec3(0, 1, 0), outline),
                ]
                .into_iter(),
            ),
        }
    }
}

// Kludge to pretend to have slightly greater character coverage than the fonts actually do:
// remap selected Unicode characters to the `iso_8859_1` subset.
struct RemapTo8859_1<'a>(&'a dyn GlyphMapping);

impl GlyphMapping for RemapTo8859_1<'_> {
    fn index(&self, c: char) -> usize {
        self.0.index(match c {
            '‘' | '’' => '\'',
            '“' | '”' => '"',
            c => c,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Primitive;
    use crate::math::Cube;
    use crate::raytracer::print_space;
    use crate::space::Space;
    use crate::universe::Universe;
    use alloc::string::String;
    use alloc::vec::Vec;
    use arcstr::literal;
    use pretty_assertions::assert_eq;

    /// Convert voxels with z range = 1 to a string for readable comparisons.
    fn plane_to_text(voxels: Vol<&[Evoxel]>) -> Vec<String> {
        fn convert_voxel(v: &Evoxel) -> char {
            if v.color.fully_transparent() {
                '.'
            } else {
                '#'
            }
        }

        let z = voxels.bounds().lower_bounds().z;
        assert_eq!(voxels.bounds().z_range().len(), 1);
        voxels
            .bounds()
            .y_range()
            .rev() // flip Y axis
            .map(|y| {
                voxels
                    .bounds()
                    .x_range()
                    .map(|x| convert_voxel(&voxels[Cube::new(x, y, z)]))
                    .collect::<String>()
            })
            .collect()
    }

    fn single_block_test_case(text: Text) -> (Box<Universe>, Block) {
        // This universe is not really used now except to provide a `ReadTicket`,
        // but I currently expect to add future restrictions on `Block` and `Space` usage
        // that will make it necessary.
        let universe = Universe::new();

        //assert_eq!(text.bounding_blocks(), GridAab::ORIGIN_CUBE);

        let block = Block::from_primitive(Primitive::Text {
            text,
            offset: GridVector::zero(),
        });

        // Print for debugging
        {
            let space = Space::builder(GridAab::ORIGIN_CUBE)
                .read_ticket(universe.read_ticket())
                .filled_with(block.clone())
                .build();
            print_space(&space, [0., 0., 1.]);
        }

        (universe, block)
    }

    #[test]
    fn single_line_text_smoke_test() {
        let text = Text::builder()
            .string(literal!("ab"))
            .font(Font::System16)
            .resolution(Resolution::R16)
            .positioning(Positioning {
                x: PositioningX::Left,
                line_y: PositioningY::BodyBottom,
                z: PositioningZ::Back,
            })
            .build();
        let (universe, block) = single_block_test_case(text.clone());

        let ev = block.evaluate(universe.read_ticket()).unwrap();
        assert_eq!(
            ev.attributes,
            BlockAttributes {
                display_name: arcstr::literal!("ab"),
                ..BlockAttributes::default()
            }
        );
        assert_eq!(
            ev.voxels.bounds(),
            GridAab::from_lower_size([0, 0, 0], [16, 13, 1])
        );
        assert_eq!(ev.voxels.bounds(), text.bounding_voxels());

        assert_eq!(
            plane_to_text(ev.voxels.as_vol_ref()),
            vec![
                "................",
                "........##......",
                "........##......",
                "........##......",
                ".#####..##.###..",
                ".....##.###..##.",
                ".######.##...##.",
                "##...##.##...##.",
                "##...##.##...##.",
                "##..###.###..##.",
                ".###.##.##.###..",
                "................",
                "................",
            ]
        )
    }

    #[test]
    fn multiple_line() {
        let (universe, block) = single_block_test_case(
            Text::builder()
                .resolution(Resolution::R32)
                .string(literal!("abcd\nabcd"))
                .font(Font::System16)
                .positioning(Positioning {
                    x: PositioningX::Left,
                    line_y: PositioningY::BodyTop, // TODO: test case for BodyBottom, which we may want to fix
                    z: PositioningZ::Back,
                })
                .build(),
        );

        assert_eq!(
            plane_to_text(block.evaluate(universe.read_ticket()).unwrap().voxels.as_vol_ref()),
            vec![
                "................................",
                "........##...................##.",
                "........##...................##.",
                "........##...................##.",
                ".#####..##.###...#####...###.##.",
                ".....##.###..##.###..##.##..###.",
                ".######.##...##.##......##...##.",
                "##...##.##...##.##......##...##.",
                "##...##.##...##.##......##...##.",
                "##..###.###..##.###..##.##..###.",
                ".###.##.##.###...#####...###.##.",
                "................................",
                "................................",
                "................................",
                "........##...................##.",
                "........##...................##.",
                "........##...................##.",
                ".#####..##.###...#####...###.##.",
                ".....##.###..##.###..##.##..###.",
                ".######.##...##.##......##...##.",
                "##...##.##...##.##......##...##.",
                "##...##.##...##.##......##...##.",
                "##..###.###..##.###..##.##..###.",
                ".###.##.##.###...#####...###.##.",
                "................................",
                "................................",
            ]
        )
    }

    /// Test that the high-coordinate positioning options correctly meet the
    /// upper corner of the block.
    #[test]
    fn bounding_voxels_of_positioning_high() {
        let text = Text::builder()
            .resolution(Resolution::R32)
            .string(literal!("abc"))
            .font(Font::System16)
            .positioning(Positioning {
                x: PositioningX::Right,
                line_y: PositioningY::BodyTop,
                z: PositioningZ::Front,
            })
            .build();

        // The part we care about precisely is that the upper corner.
        // The lower corner might change when we change the system font metrics.
        assert_eq!(
            text.bounding_voxels(),
            GridAab::from_lower_upper([8, 19, 31], [32, 32, 32])
        );
    }

    /// Test the rounding behavior of text positioning.
    ///
    /// Includes left and right even though only centering is really hairy.
    ///
    /// Note that for odd&even cases, we primarily care about the choice of “round down” vs.
    /// “round up” options in that they shouldn’t *change without notice*.
    ///
    /// * If `odd_font` is true, the string is 27 voxels wide. If false, 48 voxels wide.
    /// * If `odd_bounds` is true, the `layout_bounds` is 15 voxels wide. false, 16 voxels.
    #[rstest::rstest]
    #[case(PositioningX::Left, false, 0..16, 0..48)]
    #[case(PositioningX::Right, false, 0..16, -32..16)]
    #[case(PositioningX::Center, false, 0..16, -16..32)]
    #[case(PositioningX::Center, true, 0..16, -6..21)]
    #[case(PositioningX::Center, false, 0..15, -16..32)]
    #[case(PositioningX::Center, true, 0..15, -6..21)]
    #[case(PositioningX::Center, false, 1..16, -15..33)]
    #[case(PositioningX::Center, true, 1..16, -5..22)]
    fn positioning_x(
        #[case] pos: PositioningX,
        #[case] odd_font: bool,
        #[case] bounds_range: core::ops::Range<i32>,
        #[case] expected: core::ops::Range<i32>,
    ) {
        let text = Text::builder()
            .string(if odd_font {
                // must have an odd number of characters
                literal!("abc")
            } else {
                literal!("abcdef")
            })
            // TODO: when we have custom fonts, use custom fonts instead of depending on properties
            // of fonts with other intents.
            .font(if odd_font { Font::Logo } else { Font::System16 })
            .layout_bounds(
                Resolution::R16,
                GridAab::from_ranges([bounds_range, 0..16, 0..16]),
            )
            .positioning(Positioning {
                x: pos,
                line_y: PositioningY::BodyMiddle,
                z: PositioningZ::Back,
            })
            .build();

        assert_eq!(text.bounding_voxels().x_range(), expected);
    }

    #[test]
    fn no_intersection_with_block() {
        let (universe, block) = single_block_test_case({
            Text::builder()
                .string(literal!("ab"))
                .font(Font::System16)
                .layout_bounds(
                    Resolution::R16,
                    GridAab::from_lower_size([100000, 0, 0], [16, 16, 16]),
                )
                .build()
        });

        let ev = block.evaluate(universe.read_ticket()).unwrap();
        assert_eq!(
            ev.attributes,
            BlockAttributes {
                display_name: arcstr::literal!("ab"),
                ..BlockAttributes::default()
            }
        );
        assert_eq!(ev.resolution(), Resolution::R1);
        assert!(!ev.visible());
    }

    // TODO: test that voxel attributes are as expected
}

//! Support for [`Primitive::Text`] and other text rendering.

#![expect(
    clippy::module_name_repetitions,
    reason = "module is private; https://github.com/rust-lang/rust-clippy/issues/8524"
)]

use alloc::boxed::Box;
use core::{fmt, iter};

use arcstr::ArcStr;
use bevy_platform::sync::OnceLock;
use euclid::vec3;

use crate::block::{self, Block, BlockAttributes, Evoxel, MinEval, Resolution};
use crate::content::palette;
use crate::math::{
    Cube, GridAab, GridCoordinate, GridPoint, GridVector, Gridgid, Rgb, Vol, rgba_const,
};
use crate::space::{self, SpaceTransaction};
use crate::universe;

#[cfg(doc)]
use crate::block::{Modifier, Primitive};

use super::Evoxels;

// -------------------------------------------------------------------------------------------------

mod font;
pub use font::Font;
use font::{FontDecl, glyph_from_binary_image};

mod layout;
use layout::{Layout, compute_layout};

// -------------------------------------------------------------------------------------------------

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
pub struct Text {
    data: TextData,
    layout_cache: OnceLock<Layout>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct TextData {
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

// -------------------------------------------------------------------------------------------------

impl Text {
    /// Returns a [`TextBuilder`] which may be used to construct a [`Text`] value with explicit
    /// or default options.
    pub fn builder() -> TextBuilder {
        TextBuilder::default()
    }

    /// Converts this into a [`TextBuilder`] so that it may be modified.
    pub fn into_builder(self) -> TextBuilder {
        let TextData {
            string,
            font,
            foreground,
            outline,
            resolution,
            layout_bounds,
            positioning,
            debug,
        } = self.data;
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
        &self.data.string
    }
    /// Returns the font which this uses to display the text.
    pub fn font(&self) -> &Font {
        &self.data.font
    }

    /// Returns the voxel resolution which the text blocks will have.
    pub fn resolution(&self) -> Resolution {
        self.data.resolution
    }

    /// Returns the bounding box, within the blocks at the specified resolution, of the text.
    ///
    /// This does not reflect the actual size of the text but the configuration with which this
    /// [`Text`] value was constructed.
    /// The text may overflow this bounding box depending on its length and the
    /// [`positioning()`](Self::positioning).
    pub fn layout_bounds(&self) -> GridAab {
        self.data.layout_bounds
    }

    /// Returns the [`Positioning`] parameters this uses.
    pub fn positioning(&self) -> Positioning {
        self.data.positioning
    }

    /// Returns the debug-rendering flag.
    pub fn debug(&self) -> bool {
        self.data.debug
    }

    /// Returns the bounding box of the text as displayed, in voxels at the
    /// [`resolution()`](Self::resolution).
    ///
    /// This box is in the same units as [`Self::layout_bounds()`] but reflects the actual text
    /// layout rather than the configuration.
    pub fn bounding_voxels(&self) -> GridAab {
        self.get_or_init_layout().bounding_box
    }
    /// Returns the bounding box of the text, in blocks — the set of [`Primitive::Text`] offsets
    /// that will render all of it.
    ///
    /// This is identical to [`Self::bounding_voxels()`] scaled down by [`Self::resolution()`].
    pub fn bounding_blocks(&self) -> GridAab {
        self.bounding_voxels().divide(self.resolution().into())
    }

    /// Returns a transaction which places [`Primitive::Text`] blocks containing this text.
    ///
    /// The text lies within the volume [`Self::bounding_blocks()`] transformed by `transform`.
    ///
    /// Each individual block is given to `block_fn` to allow alterations.
    ///
    /// The transaction has no preconditions.
    ///
    /// # Panics
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

        let &Layout {
            bounding_box: text_aab,
            ref glyphs,
            z: layout_z,
        } = self.get_or_init_layout();

        // Apply `block_offset` to the result of the layout.
        // TODO: handle overflow
        let block_offset_in_voxels = (-block_offset) * GridCoordinate::from(self.data.resolution);
        let aab_in_block_voxel_space = text_aab.translate(block_offset_in_voxels);

        // Check whether the text intersects the volume of this block.
        // If it doesn’t, we can skip consulting the glyphs at all.
        // TODO: We should also have a way to look up only the intersecting glyphs
        // so that a fragment of a large text is handled efficiently.
        let voxels: Evoxels = match aab_in_block_voxel_space
            .intersection_cubes(GridAab::for_block(self.data.resolution))
        {
            Some(bounds_in_this_block) => {
                // Evaluate blocks making up the brush.
                let brush: Brush<Evoxel> = {
                    let _recursion_scope = block::Budget::recurse(&filter.budget)?;
                    let evaluated_foreground =
                        self.data.foreground.evaluate_to_evoxel_internal(filter)?;
                    match self.data.outline {
                        Some(ref block) => Brush::Outline {
                            foreground: evaluated_foreground,
                            outline: block.evaluate_to_evoxel_internal(filter)?,
                        },
                        None => Brush::Plain(evaluated_foreground),
                    }
                };

                let background = if self.data.debug {
                    DEBUG_TEXT_BOUNDS_VOXEL
                } else {
                    Evoxel::AIR
                };
                let mut voxels: Vol<Box<[Evoxel]>> =
                    Vol::from_fn(bounds_in_this_block, |_| background);

                let decl = self.data.font.font_decl();
                let pixels = decl.binary_image();

                for glyph in glyphs.iter() {
                    let glyph_position_3d: GridPoint =
                        glyph.position.extend(layout_z).cast_unit() + block_offset_in_voxels;
                    glyph_from_binary_image(pixels, decl, glyph.glyph_index).for_each(
                        |position_in_glyph| {
                            let position_in_block_of_voxel = glyph_position_3d
                                + vec3(position_in_glyph.x, -position_in_glyph.y, 0);
                            for (offset, evoxel) in brush.iter() {
                                if let Some(vox) =
                                    voxels.get_mut(position_in_block_of_voxel + offset)
                                {
                                    *vox = evoxel;
                                }
                            }
                        },
                    );
                }

                Evoxels::from_many(self.data.resolution, voxels.map_container(Into::into))
            }

            None => Evoxels::from_one(if self.data.debug {
                DEBUG_NO_INTERSECTION_VOXEL
            } else {
                Evoxel::AIR
            }),
        };

        Ok(MinEval::new(
            BlockAttributes {
                // TODO: putting the *entire* string in the name is dubious in general.
                display_name: self.data.string.clone(),
                ..BlockAttributes::default()
            },
            voxels,
        ))
    }

    /// Draw text voxels directly to blocks in a space (making large text)
    ///
    /// TODO: This is quickly slapped together for the `LargeText` widget, and may not be good
    /// general API.
    #[doc(hidden)]
    pub fn draw_voxels_to_transaction(&self, txn: &mut SpaceTransaction, transform: Gridgid) {
        let &Layout {
            bounding_box: _,
            ref glyphs,
            z: layout_z,
        } = self.get_or_init_layout();

        let decl = self.data.font.font_decl();
        let pixels = decl.binary_image();

        let brush: Brush<&Block> = match self.data.outline {
            Some(ref outline) => Brush::Outline {
                foreground: &self.data.foreground,
                outline,
            },
            None => Brush::Plain(&self.data.foreground),
        };

        for glyph in glyphs.iter() {
            glyph_from_binary_image(pixels, decl, glyph.glyph_index).for_each(
                |position_in_glyph| {
                    for (brush_offset, brush_block) in brush.iter() {
                        let transformed_position = transform.transform_cube(Cube::from(
                            glyph.position.extend(layout_z).cast_unit()
                                + vec3(position_in_glyph.x, -position_in_glyph.y, 0)
                                + brush_offset,
                        ));
                        txn.at(transformed_position).overwrite(brush_block.clone());
                    }
                },
            );
        }
    }

    fn get_or_init_layout(&self) -> &Layout {
        self.layout_cache.get_or_init(|| {
            compute_layout(
                &self.data.string,
                self.data.font.font_decl(),
                self.data.outline.is_some(),
                self.data.layout_bounds,
                self.data.positioning,
            )
        })
    }
}

impl From<TextData> for Text {
    fn from(data: TextData) -> Self {
        Self {
            data,
            layout_cache: OnceLock::new(),
        }
    }
}

impl Clone for Text {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            layout_cache: self.layout_cache.clone(),
        }
    }
}

impl PartialEq for Text {
    fn eq(&self, other: &Self) -> bool {
        let Self {
            data,
            layout_cache: _, // layout_cache is derived from data
        } = self;
        *data == other.data
    }
}
impl Eq for Text {}

impl core::hash::Hash for Text {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        let Self {
            data,
            layout_cache: _, // layout_cache is derived from data
        } = self;
        data.hash(state);
    }
}

impl universe::VisitHandles for Text {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            data:
                TextData {
                    string: _,
                    font,
                    foreground,
                    outline,
                    resolution: _,
                    layout_bounds: _,
                    positioning: _,
                    debug: _,
                },
            layout_cache: _,
        } = self;
        font.visit_handles(visitor);
        foreground.visit_handles(visitor);
        outline.visit_handles(visitor);
    }
}

#[expect(clippy::missing_fields_in_debug)]
impl fmt::Debug for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let TextData {
            string,
            font,
            foreground,
            outline,
            resolution,
            layout_bounds,
            positioning,
            debug,
        } = &self.data;
        f.debug_struct("Text")
            .field("string", string)
            .field("font", font)
            .field("foreground", foreground)
            .field("outline", outline)
            .field("resolution", resolution)
            .field("layout_bounds", layout_bounds)
            .field("positioning", positioning)
            .field("debug", debug)
            .finish()
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

        Ok(Self::from(TextData {
            // ArcStr doesn't implement Arbitrary
            string: alloc::string::String::arbitrary(u)?.into(),
            font: Font::arbitrary(u)?,
            foreground: Block::arbitrary(u)?,
            outline: Option::<Block>::arbitrary(u)?,
            resolution: Resolution::arbitrary(u)?,
            layout_bounds,
            positioning: Positioning::arbitrary(u)?,
            debug: bool::arbitrary(u)?,
        }))
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
        Text::from(TextData {
            string,
            font,
            foreground,
            outline,
            resolution,
            layout_bounds: layout_bounds.unwrap_or(GridAab::for_block(resolution)),
            positioning,
            debug,
        })
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

/// How a [`Text`] is to be positioned within a block.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[expect(
    clippy::exhaustive_structs,
    reason = "TODO: probably want to do something else"
)]
pub struct Positioning {
    /// How to place the text horizontally relative to the layout bounds.
    pub x: PositioningX,

    // TODO: implement this
    // /// How to place the text's first or last line relative to the layout bounds.
    // pub total_y: (),
    /// How to place the characters of the first line relative to the layout bounds.
    pub line_y: PositioningY,

    /// How to place the text depthwise relative to the layout bounds.
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

impl Positioning {
    #[doc(hidden)] // not sure if good idea
    pub const LOW: Self = Positioning {
        x: PositioningX::Left,
        line_y: PositioningY::BodyBottom,
        z: PositioningZ::Back,
    };
}

// -------------------------------------------------------------------------------------------------

#[cfg(feature = "save")]
mod serialization {
    use crate::block::text;
    use crate::save::schema;

    impl From<&text::Text> for schema::TextSer {
        fn from(value: &text::Text) -> Self {
            let &text::Text {
                data:
                    text::TextData {
                        ref string,
                        ref font,
                        ref foreground,
                        ref outline,
                        resolution,
                        layout_bounds,
                        positioning,
                        debug,
                    },
                layout_cache: _,
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

// -------------------------------------------------------------------------------------------------

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

/// Specifies how a text glyph is painted into 3D space.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Brush<V> {
    Plain(V),
    Outline { foreground: V, outline: V },
}

// -------------------------------------------------------------------------------------------------

impl<V> Brush<V> {
    pub(crate) fn iter(&self) -> impl Iterator<Item = (GridVector, V)> + '_
    where
        V: Copy,
    {
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

// No `mod tests`; tests are located in the `test-aic` package.

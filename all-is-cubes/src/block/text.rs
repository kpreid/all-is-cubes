//! Support for [`Primitive::Text`].

use alloc::boxed::Box;

use arcstr::ArcStr;
use embedded_graphics as eg;
use embedded_graphics::{prelude::Dimensions as _, Drawable as _};
use euclid::vec3;

use crate::block::{self, Block, BlockAttributes, EvalBlockError, Evoxel, MinEval, Resolution};
use crate::content::palette;
use crate::drawing::{rectangle_to_aab, DrawingPlane};
use crate::math::{GridAab, GridCoordinate, GridVector, Gridgid, Rgb, Vol};
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

    resolution: Resolution,

    /// Voxel-scale bounds in which the text is positioned (not necessarily actual drawing bounds).
    layout_bounds: GridAab,

    positioning: Positioning,
}

impl Text {
    /// Create a new [`Text`] value containing the given string.
    ///
    /// TODO: these arguments are incomplete and unstable.
    pub fn new(string: ArcStr, font: Font, positioning: Positioning) -> Self {
        let resolution = Resolution::R16;
        Self {
            string,
            font,
            resolution,
            layout_bounds: GridAab::for_block(resolution),
            positioning,
        }
    }

    /// Returns the string which this displays.
    pub fn string(&self) -> &ArcStr {
        &self.string
    }

    #[allow(missing_docs)]
    pub fn set_string(&mut self, value: ArcStr) {
        self.string = value;
    }

    /// Returns the font which this uses to display the text.
    pub fn font(&self) -> &Font {
        &self.font
    }

    #[allow(missing_docs)]
    pub fn set_font(&mut self, value: Font) {
        self.font = value;
    }

    /// Returns the voxel resolution which the text blocks will have.
    pub fn resolution(&self) -> &Font {
        &self.font
    }

    /// Returns the bounding box, within the blocks at the specified resolution, of the text.
    ///
    /// The text may overflow this bounding box depending on the length and positioning.
    pub fn layout_bounds(&self) -> GridAab {
        self.layout_bounds
    }

    /// Set the bounds within which the text is positioned, and the resolution of their coordinates
    /// and the text voxels.
    ///
    /// The text may overflow this bounding box depending on the length and positioning.

    pub fn set_layout_bounds(&mut self, resolution: Resolution, layout_bounds: GridAab) {
        self.resolution = resolution;
        self.layout_bounds = layout_bounds;
    }

    /// Returns the [`Positioning`] parameters this uses.
    pub fn positioning(&self) -> Positioning {
        self.positioning
    }

    #[allow(missing_docs)]
    pub fn set_positioning(&mut self, value: Positioning) {
        self.positioning = value;
    }

    /// Returns the bounding box of the text, in blocks — the set of [`Primitive::Text`] offsets
    /// that will render all of it.
    pub fn bounding_blocks(&self) -> GridAab {
        self.with_transform_and_drawable(
            GridVector::zero(),
            |_text_obj, text_aab, _drawing_transform| text_aab.divide(self.resolution.into()),
        )
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

    pub(crate) fn evaluate(
        &self,
        block_offset: GridVector,
        _depth: u8,
        filter: &super::EvalFilter,
    ) -> Result<MinEval, EvalBlockError> {
        if filter.skip_eval {
            // TODO: Once we have a `URef<FontDef>` or something, this will need to
            // check  that before returning.
            return Ok(block::AIR_EVALUATED_MIN); // placeholder value
        }

        self.with_transform_and_drawable(block_offset, |text_obj, text_aab, drawing_transform| {
            let mut voxels: Vol<Box<[Evoxel]>> = Vol::from_fn(
                text_aab
                    .intersection(GridAab::for_block(self.resolution))
                    .unwrap_or(GridAab::ORIGIN_EMPTY),
                |_| Evoxel::AIR,
            );

            text_obj
                .draw(&mut DrawingPlane::new(&mut voxels, drawing_transform))
                .unwrap();

            Ok(MinEval {
                voxels: Evoxels::Many(self.resolution, voxels.map_container(Into::into)),
                attributes: BlockAttributes {
                    display_name: self.string.clone(),
                    ..BlockAttributes::default()
                },
            })
        })
    }

    fn with_transform_and_drawable<R>(
        &self,
        block_offset: GridVector,
        f: impl FnOnce(
            &'_ eg::text::Text<'_, eg::mono_font::MonoTextStyle<'_, Evoxel>>,
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
        let layout_offset = vec3(
            match positioning_x {
                PositioningX::Left => lb.lower_bounds().x,
                PositioningX::Center => lb.center().x as GridCoordinate,
                PositioningX::Right => lb.upper_bounds().x,
            },
            match line_y {
                PositioningY::BodyBottom | PositioningY::Baseline => lb.lower_bounds().y,
                PositioningY::BodyMiddle => lb.center().y as GridCoordinate,
                PositioningY::BodyTop => lb.upper_bounds().y,
            },
            match positioning_z {
                PositioningZ::Back => lb.lower_bounds().z,
                PositioningZ::Front => lb.upper_bounds().z - 1,
            },
        );

        let drawing_transform =
            Gridgid::from_translation(layout_offset - (block_offset * resolution_g))
                * Gridgid::FLIP_Y;

        let character_style = eg::mono_font::MonoTextStyle::new(
            self.font.eg_font(),
            Evoxel {
                color: palette::ALMOST_BLACK.with_alpha_one(),
                selectable: false,
                collision: block::BlockCollision::Hard,
                emission: Rgb::ZERO,
            },
        );
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
        let text_aab = rectangle_to_aab(
            text_obj.bounding_box(),
            drawing_transform,
            GridAab::ORIGIN_CUBE,
        );

        f(text_obj, text_aab, drawing_transform)
    }
}

impl universe::VisitRefs for Text {
    fn visit_refs(&self, visitor: &mut dyn universe::RefVisitor) {
        let Self {
            string: _,
            font,
            resolution: _,
            layout_bounds: _,
            positioning: _,
        } = self;
        font.visit_refs(visitor);
    }
}

/// A font that may be used with [`Text`] blocks.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Font {
    /// A font whose characteristics are unspecified, other than that it is general-purpose and
    /// has a line height (vertical distance from a point on one line to the corresponding
    /// point of the next line) of 16 voxels.
    ///
    /// This is a placeholder for further improvement in the font system.
    System16,
}
impl Font {
    fn eg_font(&self) -> &eg::mono_font::MonoFont<'static> {
        match self {
            //Self::System16 => &eg::mono_font::iso_8859_1::FONT_9X15,
            Self::System16 => &eg::mono_font::iso_8859_1::FONT_8X13_BOLD,
        }
    }
}

impl universe::VisitRefs for Font {
    fn visit_refs(&self, _: &mut dyn universe::RefVisitor) {
        match self {
            Self::System16 => {}
        }
    }
}

/// How a [`Text`] is to be positioned within a block.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)] // TODO: probably want to do something else
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
#[non_exhaustive]
pub enum PositioningZ {
    /// Against the back (negative Z) face of the layout bounds.
    Back,

    /// Against the front (positive Z) face of the layout bounds.
    Front,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Block, Primitive};
    use crate::math::GridVector;
    use crate::raytracer::print_space;
    use crate::space::Space;
    use alloc::string::String;
    use alloc::vec::Vec;
    use euclid::point3;
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
                    .map(|x| convert_voxel(&voxels[point3(x, y, z)]))
                    .collect::<String>()
            })
            .collect()
    }

    #[test]
    fn text_smoke_test() {
        let text = Text::new(
            arcstr::literal!("ab"),
            Font::System16,
            Positioning {
                x: PositioningX::Left,
                line_y: PositioningY::BodyBottom,
                z: PositioningZ::Back,
            },
        );

        assert_eq!(text.bounding_blocks(), GridAab::ORIGIN_CUBE);

        let block = Block::from_primitive(Primitive::Text {
            text,
            offset: GridVector::zero(),
        });

        // Print for debugging
        {
            let space = Space::builder(GridAab::ORIGIN_CUBE)
                .filled_with(block.clone())
                .build();
            print_space(&space, [0., 0., 1.]);
        }

        let ev = block.evaluate().unwrap();
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
}

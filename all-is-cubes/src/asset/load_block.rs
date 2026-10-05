//! Expressing block definitions as constant data structures rather than
//! Rust functions that build non-constant data structures.
//!
//! # Motivation
//!
//! Because these data structures are fully constructible in `const` context, the code can be
//! guaranteed to be compiled into constant values rather than machine code that constructs those
//! values. This typically results in the executable being smaller.
//!
//! Eventually, we hope that we will also be able to keep these as data files rather than making
//! them part of the source code, but that may not turn out to be feasible or desirable.

#![expect(
    clippy::exhaustive_structs,
    missing_debug_implementations,
    reason = "these types are intended to be written as constants, only"
)]
#![expect(missing_docs, clippy::missing_errors_doc, reason = "TODO")]

use alloc::format;
use alloc::vec::Vec;

use crate::asset;
use crate::block::{self, Resolution};
use crate::camera::imgref_size;
use crate::drawing::VoxelBrush;
use crate::euclid::vec3;
use crate::linking::InGenError;
use crate::math::{Cube, GridAab, GridCoordinate, GridRotation, Rgb, Rgba, Srgba8};
use crate::space;
use crate::universe::{ReadTicket, UniverseTransaction};

#[cfg(doc)]
use crate::block::Atom;

// -------------------------------------------------------------------------------------------------
// Const-compatible “schema” data structures

/// Const-constructible data which a [`block::Block`] can be built from.
///
/// # Example
///
// TODO: make a runnable example using a suitable png file
/// ```no_run
/// # fn main() -> Result<(), all_is_cubes::linking::InGenError> {
/// use all_is_cubes::{
///     arcstr::literal,
///     asset,
///     block,
///     math::GridRotation,
///     universe::UniverseTransaction,
/// };
///
/// let mut txn = UniverseTransaction::default();
/// let block: block::Block = const {
///     asset::Block {
///         primitive: asset::PrimitiveOrSuch::Image {
///             image: asset::include_image!("load_image_test.png"),
///             rotation: GridRotation::RXZY,
///             expansion: asset::Expansion::Extrude(&[0..2]),
///             visible: asset::Vox::DEFAULT,
///             invisible: asset::Vox::DENOTES_AIR,
///         },
///         modifiers: &[block::Modifier::SetAttribute(
///             block::SetAttribute::DisplayName(literal!("Example Block")),
///         )],
///     }
/// }
/// .load(&mut txn)?;
/// # Ok(()) }
/// ```
///
/// Placing the [`asset::Block`] expression in a `const {}` block ensures that it will be
/// compiled into constant data rather than code that constructs it,
/// which is usually more compact.
#[expect(missing_docs, reason = "TODO")]
pub struct Block {
    pub primitive: PrimitiveOrSuch,
    pub modifiers: &'static [block::Modifier],
}

impl Block {
    /// Turns the static [`asset::Block`] data into a regular [`block::Block`].
    #[inline(never)] // don't duplicate *any* of this logic, to keep binary size down
    pub fn load(self, txn: &mut UniverseTransaction) -> Result<block::Block, InGenError> {
        Context { txn }.build_block(self)
    }
}

/// Specifies an [`asset::Block`]’s primitive (and possibly some modifiers).
/// Differs from [`block::Primitive`] in that it does not contain the voxel data, or a handle
/// to the voxel data, but instead a procedure for obtaining or computing the data.
#[non_exhaustive]
pub enum PrimitiveOrSuch {
    /// Use a single [`block::Atom`].
    Atom(block::Atom),

    /// Use the image file that was provided separately.
    ///
    /// The resolution of the block is taken from the image.
    Image {
        image: &'static asset::LazyImage,

        /// After the image is expanded into a 3D shape, rotate or reflect it this way.
        ///
        /// In many cases, this should be [`GridRotation::RXyZ`] in order to convert from
        /// Y-down coordinates to Y-up.
        rotation: GridRotation,

        /// How to expand the 2D image into a 3D voxel shape.
        expansion: Expansion,

        /// Voxel properties to use for image pixels whose alpha is not 0.
        visible: Vox,

        /// Voxel properties to use for image pixels whose alpha is 0.
        invisible: Vox,
    },
}

/// How to expand a 2D image into a 3D voxel shape in a [`asset::PrimitiveOrSuch::Image`].
#[non_exhaustive]
pub enum Expansion {
    /// Extrude the image on the depth (Z before rotation) axis, possibly discontiguously,
    /// within the given ranges.
    ///
    /// The image must be square, and its side length must be some [`Resolution`].
    // TODO: switch to core::range::Range when the range syntax for it is stable
    Extrude(&'static [core::ops::Range<GridCoordinate>]),

    /// Treat the image as a series of slices on the depth (Z before rotation) axis.
    /// Slices are arranged along the vertical axis,
    /// so the image must overall have dimensions
    /// (<var>width</var>, <var>`slice_height`</var> × <var>slice count</var>)
    /// <var>slice count</var> is the image’s height divided by <var>`slice_height`</var>.
    Stack { slice_height: u16 },
}

/// Specifies the properties of each voxel in a block produced by
/// [`asset::PrimitiveOrSuch::Image`],
/// other than the color taken from the image.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Vox {
    /// As per [`Atom::collision`].
    // TODO: support specifying emission mapping (instead of color or duplicated, and scale factor)
    pub collision: block::BlockCollision,

    /// If `Some`, then pretend the pixel color is the given color, ignoring all components
    /// including alpha.
    pub replace_color: Option<Rgba>,
}

impl Vox {
    /// The values which are also the values used by `Block as From<Rgba>`.
    pub const DEFAULT: Self = Self {
        collision: block::BlockCollision::Hard,
        replace_color: None,
    };

    /// When these atom attributes are specified, [`block::AIR`] is used instead of a newly
    /// defined block, if the color is transparent.
    pub const DENOTES_AIR: Self = Self {
        collision: block::Evoxel::AIR.collision,
        replace_color: None,
    };
}

/// Const-constructible data which a [`space::Space`] can be built from.
///
/// This is primarily intended to be used for constructing multiple blocks that share a space.
#[non_exhaustive]
pub enum Space {
    Image {
        image: &'static asset::LazyImage,

        /// After the image is expanded into a 3D shape, rotate or reflect it this way.
        ///
        /// In many cases, this should be [`GridRotation::RXyZ`] in order to convert from
        /// Y-down coordinates to Y-up.
        rotation: GridRotation,

        /// How to expand the 2D image into a 3D voxel shape.
        expansion: Expansion,

        /// Voxel properties to use for image pixels whose alpha is not 0.
        visible: Vox,

        /// Voxel properties to use for image pixels whose alpha is 0.
        invisible: Vox,
    },
}

impl Space {
    /// Turns the static [`asset::Space`] data into a regular [`space::Space`].
    #[inline(never)] // don't duplicate *any* of this logic, to keep binary size down
    pub fn load(self, txn: &mut UniverseTransaction) -> Result<space::Space, InGenError> {
        Context { txn }.build_space(self, None)
    }
}

// -------------------------------------------------------------------------------------------------
// Conversion innards

/// Temporary structure for the state of a [`Block::load()`] or [`Space::load()`] operation.
struct Context<'a> {
    /// The transaction into which any needed blocks or spaces will be inserted.
    txn: &'a mut UniverseTransaction,
}

impl Context<'_> {
    fn build_block(&mut self, input: Block) -> Result<block::Block, InGenError> {
        let Block {
            primitive,
            modifiers,
        } = input;

        let mut block = self.build_primitive(primitive)?;
        for modifier in modifiers {
            block = block.with_modifier(modifier.clone());
        }

        Ok(block)
    }

    fn build_primitive(&mut self, input: PrimitiveOrSuch) -> Result<block::Block, InGenError> {
        // Stub ticket is OK because all blocks used have no indirection.
        let read_ticket = ReadTicket::stub();

        Ok(match input {
            PrimitiveOrSuch::Atom(atom) => block::Block::from(atom),

            PrimitiveOrSuch::Image {
                image,
                rotation,
                expansion,
                visible,
                invisible,
            } => {
                let path = image.path();
                let [image_width, _image_height] = imgref_size(image).into();
                let Ok(resolution) = Resolution::try_from(image_width) else {
                    return Err(InGenError::Other(
                        format!(
                            "image “{path}” has width {image_width}, \
                                which is not a valid block resolution"
                        )
                        .into(),
                    ));
                };
                let space = self.build_space(
                    Space::Image {
                        image,
                        rotation,
                        expansion,
                        visible,
                        invisible,
                    },
                    Some(resolution),
                )?;

                let block_builder = block::Block::builder().read_ticket(read_ticket);

                let space_handle = self.txn.insert_anonymous(space);

                block_builder.voxels_handle(resolution, space_handle).build()
            }
        })
    }

    fn build_space(
        &mut self,
        input: Space,
        constrain_to_block: Option<Resolution>,
    ) -> Result<space::Space, InGenError> {
        // not using txn
        let _ = self;

        // Stub ticket is OK because all blocks used have no indirection.
        let read_ticket = ReadTicket::stub();

        Ok(match input {
            Space::Image {
                image,
                rotation,
                expansion,
                visible,
                invisible,
            } => {
                let path = image.path();
                let [image_width, image_height] = imgref_size(image).into();

                let pixel_color_to_voxel = |srgba_color: Srgba8| -> Option<block::Block> {
                    let is_invisible = srgba_color[3] == 0;
                    let voxel_config @ &Vox {
                        collision,
                        replace_color,
                    } = if is_invisible { &invisible } else { &visible };

                    if is_invisible && *voxel_config == Vox::DENOTES_AIR {
                        None
                    } else {
                        Some(block::Block::from(block::Atom {
                            color: replace_color.unwrap_or_else(|| Rgba::from_srgb8(srgba_color)),
                            emission: Rgb::ZERO,
                            collision,
                        }))
                    }
                };

                match expansion {
                    Expansion::Extrude(extrusion) => {
                        if let Some(resolution) = constrain_to_block
                            && u32::from(resolution) != image_height
                        {
                            return Err(InGenError::Other(
                                format!(
                                    "image “{path}” has height {image_height}, \
                                    which is not the same as its width {image_width}"
                                )
                                .into(),
                            ));
                        }

                        // TODO: polishing: make bad data not allocate unbounded memory
                        let extrusion_cubes: Vec<Cube> = extrusion
                            .iter()
                            .cloned()
                            .flatten()
                            .map(|z| {
                                Cube::from(rotation.transform_vector(vec3(0, 0, z)).to_point())
                            })
                            .collect();

                        if let Some(resolution) = constrain_to_block
                            && let Some(brush_bounds) = extrusion_cubes
                                .iter()
                                .copied()
                                .map(Cube::grid_aab)
                                .reduce(GridAab::union_cubes)
                            && !GridAab::for_block(resolution).contains_box(brush_bounds)
                        {
                            return Err(InGenError::Other(
                                format!(
                                    "extrusion bounds {brush_bounds:?} \
                                        exceeds block resolution {resolution}"
                                )
                                .into(),
                            ));
                        }

                        asset::space_from_image_raw(
                            read_ticket,
                            image.as_ref(),
                            rotation,
                            &mut |pixel: Srgba8| {
                                if let Some(block) = pixel_color_to_voxel(pixel) {
                                    VoxelBrush::new(extrusion_cubes.iter().map(|&cube| {
                                        (cube.lower_bounds().to_vector(), block.clone())
                                    }))
                                } else {
                                    VoxelBrush::EMPTY_REF.clone()
                                }
                            },
                            &mut asset::pixel_to_voxel::flat_2d_to_3d,
                        )
                    }
                    Expansion::Stack { slice_height } => {
                        if !image_height.is_multiple_of(u32::from(slice_height)) {
                            return Err(InGenError::Other(
                                format!(
                                    "image “{path}” has height {image_height}, \
                                        which is not a multiple of the slice height {slice_height}"
                                )
                                .into(),
                            ));
                        }

                        let slice_height = i32::from(slice_height);
                        asset::space_from_image_raw(
                            read_ticket,
                            image.as_ref(),
                            rotation,
                            &mut |pixel| {
                                if let Some(block) = pixel_color_to_voxel(pixel) {
                                    VoxelBrush::single(block)
                                } else {
                                    VoxelBrush::EMPTY_REF.clone()
                                }
                            },
                            &mut |p| {
                                let p = p.to_i32();
                                Cube::new(
                                    p.x,
                                    p.y.rem_euclid(slice_height),
                                    p.y.div_euclid(slice_height),
                                )
                            },
                        )
                    }
                }?
            }
        })
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use alloc::sync::Arc;

    use crate::asset;
    use crate::block::{self, Resolution::R2};
    use crate::linking::InGenError;
    use crate::math::{GridAab, GridRotation, Rgb, Rgba, Vol};
    use crate::universe::UniverseTransaction;
    use crate::util::ErrorChain;

    fn pretty_unwrap<T>(result: Result<T, InGenError>) -> T {
        match result {
            Ok(value) => value,
            Err(e) => panic!("{}", ErrorChain(&e)),
        }
    }

    #[test]
    fn atom() {
        const ATOM: block::Atom = block::Atom {
            color: Rgba::new(1.0, 0.0, 1.0, 1.0),
            emission: Rgb::new(1.0, 2.0, 3.0),
            collision: block::BlockCollision::None,
        };
        const ROTATE: block::Modifier = block::Modifier::Rotate(GridRotation::RZYX);
        const INPUT: asset::Block = asset::Block {
            primitive: asset::PrimitiveOrSuch::Atom(ATOM),
            modifiers: &[ROTATE],
        };

        let txn = &mut UniverseTransaction::default();
        let block = pretty_unwrap(INPUT.load(txn));

        assert_eq!(block, block::Block::from(ATOM).with_modifier(ROTATE));
        assert!(txn.is_empty());
    }

    const IMAGE_2X2: &asset::LazyImage = asset::include_image!("load_block/test_2x2_0rgb.png");

    #[macro_rules_attribute::apply(crate::util::cartesian_product_test)]
    fn image_simple_extrusion(
        #[case(visible = asset::Vox::DEFAULT)]
        #[case(invisible = asset::Vox::DENOTES_AIR)]
        invisible: asset::Vox,
    ) {
        let config = asset::Block {
            primitive: asset::PrimitiveOrSuch::Image {
                image: IMAGE_2X2,
                rotation: GridRotation::IDENTITY,
                expansion: asset::Expansion::Extrude(&[0..2]),
                visible: asset::Vox::DEFAULT,
                invisible,
            },
            modifiers: &[],
        };

        let txn = &mut UniverseTransaction::default();
        let loaded_block = pretty_unwrap(config.load(txn));

        // this branch matches the special case in the code under test
        let invisible_voxel = if invisible == asset::Vox::DENOTES_AIR {
            block::Evoxel::AIR
        } else {
            block::Evoxel::from_color(Rgba::TRANSPARENT)
        };
        let evaluated = loaded_block.evaluate(txn.read_ticket()).unwrap();
        assert_eq!(
            block::EvoxelsEq::from(evaluated.voxels()),
            block::EvoxelsEq::from(block::Evoxels::from_paletted(
                R2,
                Arc::new([
                    invisible_voxel,
                    // Note that by using `from_color()` here, we test the documented claim that
                    // `Vox::DEFAULT` is equivalent to `from_color()`.
                    block::Evoxel::from_color(Rgba::new(0., 1., 0., 1.)),
                    block::Evoxel::from_color(Rgba::new(1., 0., 0., 1.)),
                    block::Evoxel::from_color(Rgba::new(0., 0., 1., 1.)),
                ]),
                Vol::from_elements(GridAab::for_block(R2), [0, 0, 1, 1, 2, 2, 3, 3].as_slice())
                    .unwrap()
            ))
        );
    }

    #[test]
    fn extrusion_out_of_range() {
        let config = asset::Block {
            primitive: asset::PrimitiveOrSuch::Image {
                image: IMAGE_2X2,
                rotation: GridRotation::IDENTITY,
                expansion: asset::Expansion::Extrude(&[0..3]),
                visible: asset::Vox::DEFAULT,
                invisible: asset::Vox::DEFAULT,
            },
            modifiers: &[],
        };

        let txn = &mut UniverseTransaction::default();
        let error = config.load(txn).unwrap_err();

        // TODO: have a proper error type and assert what we got here?
        std::println!("{}", ErrorChain(&error));
    }
}

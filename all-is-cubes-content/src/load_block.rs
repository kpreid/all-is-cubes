//! Experimental module to express block definitions as constant data structures rather than
//! Rust functions that build non-constant data structures.
//!
//! Eventually, we hope that these data structures will become able to be stored as simple data
//! files, but for the moment, they are still written as Rust code, particularly to avoid the
//! overhead of a parser that is (currently) only used to load hardcoded data.
//!
//! # Rationale
//!
//! Actual and planned advantages:
//!
//! * Eliminate costs of having distinct, nontrivial Rust code for each block definition.
//! * Instead of having both code defining the block and `.png`s for the voxels, have the
//!   rest of the definition live next to the `.png`s.
//!   (This is not implemented, and will need to be done using a proc-macro or `include!` abuse.)
//! * Public and usable as a tool downstream, eventually.
//!
//! Costs/disadvantages:
//!
//! * Another parallel(ish) set of data structures that aren’t just `Block`.
//!
//! If the experiment is successful, then this should likely be promoted out of
//! `all-is-cubes-content` into a public module of `all-is-cubes`, so that it can be used without
//! bringing in the demo content.

use alloc::format;
use alloc::vec::Vec;

use all_is_cubes::block::{self, Resolution};
use all_is_cubes::content::load_image::{PngAdapter, block_from_image};
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::euclid::{Point2D, point2, vec3};
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{Cube, GridAab, GridCoordinate, GridRotation, Rgb, Rgba};
use all_is_cubes::universe::{ReadTicket, UniverseTransaction};

// for convenience, incorporate key items from of `load_image`
pub use all_is_cubes::content::load_image::{LazyImage, include_image};

#[cfg(doc)]
use crate::load_block; // self, for documentation

// -------------------------------------------------------------------------------------------------
// Const-compatible “schema” data structures

/// Const-constructible data which a [`block::Block`] can be built from.
pub struct Block {
    pub primitive: PrimitiveOrSuch,
    pub modifiers: &'static [block::Modifier],
}

impl Block {
    /// Entry point to the [`load_block`] system.
    /// Call this to turn the static [`load_block::Block`] data into a regular
    /// [`all_is_cubes::block::Block`].
    #[inline(never)] // don't duplicate *any* of this logic, to keep binary size down
    pub fn load(self, txn: &mut UniverseTransaction) -> Result<block::Block, InGenError> {
        Context { txn }.build_block(self)
    }
}

/// Specifies the block’s primitive (and possibly some modifiers).
/// Differs from [`block::Primitive`] in that it does not contain the voxel data, or a handle
/// to the voxel data, but instead a procedure for obtaining or computing the data.
pub enum PrimitiveOrSuch {
    /// Use a single [`block::Atom`].
    Atom(block::Atom),

    /// Use the image file that was provided separately.
    ///
    /// The resolution of the block is taken from the image.
    Image {
        image: &'static LazyImage,

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

/// How to expand a 2D image into a 3D voxel shape.
pub enum Expansion {
    /// Extrude the image on the depth (Z before rotation) axis, possibly discontiguously,
    /// within the given ranges.
    ///
    /// The image must be square, and its side length must be some [`Resolution`].
    // TODO: switch to core::range::Range when the range syntax for it is stable
    Extrude(&'static [core::ops::Range<GridCoordinate>]),

    /// Treat the image as a series of slices on the depth (Z before rotation) axis.
    /// Each slice is assumed to be square, and slices are arranged along the vertical axis,
    /// so the image must overall have dimensions
    /// (<var>width</var>, <var>width</var> × <var>slice count</var>).
    Stack,
}

/// Specifies the properties of each voxel in the block, except for the color taken from the
/// image.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Vox {
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

// -------------------------------------------------------------------------------------------------
// Conversion innards

/// Temporary structure for the state of a [`Block::load()`] operation.
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
                let [image_width, image_height] = image.size().into();
                let Ok(resolution) = Resolution::try_from(image_width) else {
                    return Err(InGenError::Other(
                        format!(
                            "image “{path}” has width {image_width}, \
                                which is not a valid block resolution"
                        )
                        .into(),
                    ));
                };
                let full_block_bounds = GridAab::for_block(resolution);

                let pixel_color_to_voxel = |srgba_color: [u8; 4]| -> Option<block::Block> {
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

                // TODO: Make these different modes share more of their logic.
                match expansion {
                    Expansion::Extrude(extrusion) => {
                        if u32::from(resolution) != image_height {
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

                        if let Some(brush_bounds) = extrusion_cubes
                            .iter()
                            .copied()
                            .map(Cube::grid_aab)
                            .reduce(GridAab::union_cubes)
                            && !full_block_bounds.contains_box(brush_bounds)
                        {
                            return Err(InGenError::Other(
                                format!(
                                    "extrusion bounds {brush_bounds:?} \
                                        exceeds block resolution {resolution}"
                                )
                                .into(),
                            ));
                        }

                        // Actually build the block.
                        // Stub ticket is OK because all blocks used have no indirection.
                        block_from_image(ReadTicket::stub(), image, rotation, &|pixel: [u8; 4]| {
                            if let Some(block) = pixel_color_to_voxel(pixel) {
                                VoxelBrush::new(
                                    extrusion_cubes.iter().map(|&cube| {
                                        (cube.lower_bounds().to_vector(), block.clone())
                                    }),
                                )
                            } else {
                                VoxelBrush::EMPTY_REF.clone()
                            }
                        })?
                        .build_txn(self.txn)
                    }
                    Expansion::Stack => {
                        let expected_image_height = u32::from(resolution).pow(2);
                        if image_height != expected_image_height {
                            return Err(InGenError::Other(
                                format!(
                                    "image “{path}” has height {image_height}, \
                                        but should have the width squared, {expected_image_height}"
                                )
                                .into(),
                            ));
                        }

                        let transform =
                            rotation.inverse().to_positive_octant_transform(resolution.into());

                        let adapter = PngAdapter::adapt(image, &|pixel| {
                            if let Some(block) = pixel_color_to_voxel(pixel) {
                                VoxelBrush::single(block)
                            } else {
                                VoxelBrush::EMPTY_REF.clone()
                            }
                        });

                        // TODO: dubious whether we should be using voxels_fn rather than starting
                        // from the image pixels. This way we get voxels_fn()'s empty space
                        // shrinking, but arguably that should be done some other way.
                        block::Block::builder()
                            .read_ticket(ReadTicket::stub())
                            .voxels_fn(resolution, |cube| {
                                let cube = transform.transform_cube(cube).lower_bounds();
                                let image_point: Point2D<i32, ()> =
                                    point2(cube.x, cube.y + i32::from(resolution) * cube.z);

                                // TODO: make the adapter able to work with single blocks always,
                                // instead of brushes.
                                adapter
                                    .get_brush(image_point.x, image_point.y)
                                    .origin_block()
                                    .unwrap_or(&block::AIR)
                            })?
                            .build_txn(self.txn)
                    }
                }
            }
        })
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use alloc::sync::Arc;

    use all_is_cubes::block::{self, Resolution::R2};
    use all_is_cubes::linking::InGenError;
    use all_is_cubes::math::{GridAab, GridRotation, Rgb, Rgba, Vol};
    use all_is_cubes::universe::UniverseTransaction;
    use all_is_cubes::util::ErrorChain;

    use crate::load_block as lb;
    use crate::load_image::{LazyImage, include_image};

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
        const INPUT: lb::Block = lb::Block {
            primitive: lb::PrimitiveOrSuch::Atom(ATOM),
            modifiers: &[ROTATE],
        };

        let txn = &mut UniverseTransaction::default();
        let block = pretty_unwrap(INPUT.load(txn));

        assert_eq!(block, block::Block::from(ATOM).with_modifier(ROTATE));
        assert!(txn.is_empty());
    }

    const IMAGE_2X2: &LazyImage = include_image!("load_block/test_2x2_0rgb.png");

    #[macro_rules_attribute::apply(all_is_cubes::util::cartesian_product_test)]
    fn image_simple_extrusion(
        #[case(visible = lb::Vox::DEFAULT)]
        #[case(invisible = lb::Vox::DENOTES_AIR)]
        invisible: lb::Vox,
    ) {
        let config = lb::Block {
            primitive: lb::PrimitiveOrSuch::Image {
                image: IMAGE_2X2,
                rotation: GridRotation::IDENTITY,
                expansion: lb::Expansion::Extrude(&[0..2]),
                visible: lb::Vox::DEFAULT,
                invisible,
            },
            modifiers: &[],
        };

        let txn = &mut UniverseTransaction::default();
        let loaded_block = pretty_unwrap(config.load(txn));

        // this branch matches the special case in the code under test
        let invisible_voxel = if invisible == lb::Vox::DENOTES_AIR {
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
        let config = lb::Block {
            primitive: lb::PrimitiveOrSuch::Image {
                image: IMAGE_2X2,
                rotation: GridRotation::IDENTITY,
                expansion: lb::Expansion::Extrude(&[0..3]),
                visible: lb::Vox::DEFAULT,
                invisible: lb::Vox::DEFAULT,
            },
            modifiers: &[],
        };

        let txn = &mut UniverseTransaction::default();
        let error = config.load(txn).unwrap_err();

        // TODO: have a proper error type and assert what we got here?
        std::println!("{}", ErrorChain(&error));
    }
}

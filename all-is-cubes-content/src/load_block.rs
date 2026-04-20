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

use all_is_cubes::euclid::vec3;
use alloc::vec::Vec;

use all_is_cubes::block::{self, Resolution};
use all_is_cubes::content::load_image::{LazyImage, block_from_image};
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{Cube, GridAab, GridCoordinate, GridRotation, Rgb, Rgba};
use all_is_cubes::universe::{ReadTicket, UniverseTransaction};

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
    Image {
        image: &'static LazyImage,

        rotation: GridRotation,

        /// Specifies how the image is extruded to the depth axis, as a list of ranges.
        extrusion: &'static [core::range::Range<GridCoordinate>],

        /// Voxel properties to use for image pixels whose alpha is not 0.
        visible: Vox,

        /// Voxel properties to use for image pixels whose alpha is 0.
        invisible: Vox,
    },
}

/// Specifies the properties of each voxel in the block, except for the color taken from the
/// image.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Vox {
    // TODO: support specifying emission mapping (instead of color or duplicated, and scale factor)
    pub collision: block::BlockCollision,
}

impl Vox {
    /// The values which are also the values used by `Block as From<Rgba>`.
    pub const DEFAULT: Self = Self {
        collision: block::BlockCollision::Hard,
    };

    /// When these atom attributes are specified, [`block::AIR`] is used instead of a newly
    /// defined block, if the color is transparent.
    pub const DENOTES_AIR: Self = Self {
        collision: block::Evoxel::AIR.collision,
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
                extrusion,
                visible,
                invisible,
            } => {
                let path = image.path();
                let [image_width, image_height] = image.size().into();
                let Ok(resolution) = Resolution::try_from(image_width) else {
                    return Err(InGenError::Other(format!(
                        "image “{path}” has width {image_width}, which is not a valid block resolution"
                    ).into()));
                };
                let full_block_bounds = GridAab::for_block(resolution);

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
                    .copied()
                    .flatten()
                    .map(|z| Cube::from(rotation.transform_vector(vec3(0, 0, z)).to_point()))
                    .collect();

                if let Some(brush_bounds) =
                    extrusion_cubes.iter().copied().map(Cube::grid_aab).reduce(GridAab::union_cubes)
                    && !full_block_bounds.contains_box(brush_bounds)
                {
                    return Err(InGenError::Other(
                        format!(
                            "extrusion bounds {brush_bounds:?} exceeds block resolution {resolution}"
                        )
                        .into(),
                    ));
                }

                // Actually build the block.
                // Stub ticket is OK because all blocks used have no indirection.
                block_from_image(ReadTicket::stub(), image, rotation, &|pixel: [u8; 4]| {
                    let is_invisible = pixel[3] == 0;
                    let voxel_config @ &Vox { collision } =
                        if is_invisible { &invisible } else { &visible };

                    // If the provided `invisible` value is equivalent to what `AIR` would be,
                    // then make the brush empty to save work and also to use `AIR` in those places.
                    // TODO: This special rule is a little bit inelegant.
                    // Figure out how to make `Vox` more expressive in a way that makes this more regular.
                    if is_invisible && *voxel_config == Vox::DENOTES_AIR {
                        VoxelBrush::EMPTY_REF.clone()
                    } else {
                        let atom = block::Block::from(block::Atom {
                            color: Rgba::from_srgb8(pixel),
                            emission: Rgb::ZERO,
                            collision,
                        });

                        VoxelBrush::new(
                            extrusion_cubes
                                .iter()
                                .map(|&cube| (cube.lower_bounds().to_vector(), atom.clone())),
                        )
                    }
                })?
                .build_txn(self.txn)
            }
        })
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
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

    #[rstest::rstest]
    fn image_simple_extrusion(
        #[values(lb::Vox::DEFAULT, lb::Vox::DENOTES_AIR)] invisible: lb::Vox,
    ) {
        let config = lb::Block {
            primitive: lb::PrimitiveOrSuch::Image {
                image: IMAGE_2X2,
                rotation: GridRotation::IDENTITY,
                extrusion: &[0..2],
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
            evaluated.voxels().as_vol_ref(),
            Vol::from_elements(
                GridAab::for_block(R2),
                [
                    invisible_voxel,
                    invisible_voxel,
                    // Note that by using `from_color()` here, we test the documented claim that
                    // `Vox::DEFAULT` is equivalent to `from_color()`.
                    block::Evoxel::from_color(Rgba::new(0., 1., 0., 1.)),
                    block::Evoxel::from_color(Rgba::new(0., 1., 0., 1.)),
                    block::Evoxel::from_color(Rgba::new(1., 0., 0., 1.)),
                    block::Evoxel::from_color(Rgba::new(1., 0., 0., 1.)),
                    block::Evoxel::from_color(Rgba::new(0., 0., 1., 1.)),
                    block::Evoxel::from_color(Rgba::new(0., 0., 1., 1.)),
                ]
                .as_slice()
            )
            .unwrap()
        );
    }

    #[test]
    fn extrusion_out_of_range() {
        let config = lb::Block {
            primitive: lb::PrimitiveOrSuch::Image {
                image: IMAGE_2X2,
                rotation: GridRotation::IDENTITY,
                extrusion: &[0..3],
                visible: lb::Vox::DEFAULT,
                invisible: lb::Vox::DEFAULT,
            },
            modifiers: &[],
        };

        let txn = &mut UniverseTransaction::default();
        let error = config.load(txn).unwrap_err();

        // TODO: have a proper error type and assert what we got here?
        println!("{}", ErrorChain(&error));
    }
}

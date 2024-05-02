//! Built-in “game content”: basic shapes and colors used in the UI and tests.
//!
//! This module is private; the public interface to this stuff is the separate
//! `all-is-cubes-content` crate.

use alloc::string::ToString;
use alloc::vec::Vec;

use embedded_graphics::mono_font::iso_8859_1::FONT_9X15_BOLD;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::prelude::{Drawable, Point};
use embedded_graphics::text::Alignment;
use embedded_graphics::text::Baseline;
use embedded_graphics::text::Text;
use embedded_graphics::text::TextStyleBuilder;

use crate::arcstr::{literal, ArcStr};
use crate::block::{Block, Resolution, Resolution::R16, RotationPlacementRule};
use crate::color_block;
use crate::inv::{Slot, Tool};
use crate::math::{rgb_const, Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, Rgb, Rgba};
use crate::raycast::Raycaster;
use crate::space::{SetCubeError, Space};
use crate::transaction::Transactional as _;
use crate::universe::{Universe, UniverseTransaction};

mod draw_box;
#[doc(hidden)] // public for exhibit testing
pub use draw_box::*;
#[doc(hidden)] // TODO: make public with better API
pub mod load_image;
pub mod palette;
#[doc(hidden)]
pub mod testing;

/// Generate a set of distinct [`Primitive::Atom`] blocks for use in tests.
/// They will have distinct colors and names, and all other attributes default.
/// They will be fully opaque.
///
/// ```
/// # use all_is_cubes::block::{Block, Resolution};
/// # use all_is_cubes::content::make_some_blocks; // hide because wrong import path
/// #
/// let blocks: [Block; 3] = make_some_blocks();
/// assert_ne!(blocks[0], blocks[1]);
/// assert_ne!(blocks[0], blocks[2]);
/// assert_ne!(blocks[1], blocks[2]);
/// assert_eq!(blocks[0].evaluate().unwrap().resolution(), Resolution::R1);
/// ```
///
/// [`Primitive::Atom`]: crate::block::Primitive::Atom
pub fn make_some_blocks<const COUNT: usize>() -> [Block; COUNT] {
    core::array::from_fn(|i| make_one_block(i, COUNT))
}

#[inline(never)] // discourage unnecessarily repeated code
fn make_one_block(i: usize, n: usize) -> Block {
    Block::builder()
        .display_name(i.to_string())
        .color(color_for_make_blocks(i, n))
        .build()
}

/// Generate a set of distinct [`Primitive::Recur`] blocks for use in tests.
/// They will have distinct appearances and names, and all other attributes default.
/// They will be fully opaque.
///
/// ```
/// # use all_is_cubes::block::{Block, Resolution};
/// # use all_is_cubes::content::make_some_voxel_blocks; // hide because wrong import path
/// # use all_is_cubes::universe::Universe;
/// #
/// let mut universe = Universe::new();
/// let blocks: [Block; 3] = make_some_voxel_blocks(&mut universe);
/// assert_ne!(blocks[0], blocks[1]);
/// assert_ne!(blocks[0], blocks[2]);
/// assert_ne!(blocks[1], blocks[2]);
/// assert_eq!(blocks[0].evaluate().unwrap().resolution(), Resolution::R16);
/// ```
///
/// [`Primitive::Recur`]: crate::block::Primitive::Recur
pub fn make_some_voxel_blocks<const COUNT: usize>(universe: &mut Universe) -> [Block; COUNT] {
    universe
        .transact(|txn, _| Ok(make_some_voxel_blocks_txn(txn)))
        .unwrap()
}

#[doc(hidden)] // TODO: make this replace the other version once we've confirmed that `&mut SomeTransaction` is the direction we want to go for composing worldgen transactions
pub fn make_some_voxel_blocks_txn<const COUNT: usize>(
    transaction: &mut UniverseTransaction,
) -> [Block; COUNT] {
    core::array::from_fn(|i| make_one_voxel_block(transaction, i, COUNT))
}

#[inline(never)] // discourage unnecessarily repeated code
fn make_one_voxel_block(transaction: &mut UniverseTransaction, i: usize, n: usize) -> Block {
    let resolution = R16;
    let color = color_for_make_blocks(i, n);
    let mut block_space = Space::for_block(resolution)
        .filled_with(Block::from(color))
        .build();
    axes(&mut block_space).unwrap();
    for face in Face6::ALL {
        Text::with_text_style(
            &i.to_string(),
            Point::new(i32::from(resolution) / 2, i32::from(resolution) / 2),
            MonoTextStyle::new(&FONT_9X15_BOLD, palette::ALMOST_BLACK),
            TextStyleBuilder::new()
                .baseline(Baseline::Middle)
                .alignment(Alignment::Center)
                .build(),
        )
        .draw(
            &mut block_space.draw_target(face.face_transform(GridCoordinate::from(resolution) - 1)),
        )
        .unwrap();
    }
    Block::builder()
        .display_name(i.to_string())
        .voxels_handle(resolution, transaction.insert_anonymous(block_space))
        .build()
}

fn color_for_make_blocks(i: usize, n: usize) -> Rgba {
    let luminance = if n > 1 {
        i as f32 / (n - 1) as f32
    } else {
        0.5
    };
    Rgba::new(luminance, luminance, luminance, 1.0)
}

/// Generate a block which fills some fraction of its cube volume, from the bottom (−Y) up.
///
/// (This function exists because of a variety of tests of recursive blocks needing this
/// pattern.)
///
/// TODO: Allow caller-provided colors/pattern.
/// TODO: Consider writing the size on the faces.
#[doc(hidden)] // exported for all-is-cubes-content usage, not reexport
pub fn make_slab(
    universe: &mut Universe,
    numerator: GridCoordinate,
    denominator: Resolution,
) -> Block {
    universe
        .transact(|txn, _| Ok(make_slab_txn(txn, numerator, denominator)))
        .unwrap()
}

#[doc(hidden)] // exported for all-is-cubes-content usage, not reexport
pub fn make_slab_txn(
    txn: &mut UniverseTransaction,
    numerator: GridCoordinate,
    denominator: Resolution,
) -> Block {
    let voxel_palette = [
        color_block!(palette::PLANK),
        Block::from(palette::PLANK * 1.06),
    ];
    let bounds = GridAab::from_lower_size(
        [0, 0, 0],
        [denominator.to_grid(), numerator, denominator.to_grid()],
    );

    let mut space = Space::builder(bounds).build();
    // Checkerboard pattern
    space
        .fill(space.bounds(), |cube| {
            Some(&voxel_palette[(cube.x + cube.y + cube.z).rem_euclid(2) as usize])
        })
        .unwrap();

    Block::builder()
        .display_name(format!("Slab {numerator}/{denominator}"))
        .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
        .voxels_handle(denominator, txn.insert_anonymous(space))
        .build()
}

/// Draw the Space's axes as lines of blocks centered on (0, 0, 0).
///
/// ```
/// use all_is_cubes::block::AIR;
/// use all_is_cubes::math::GridAab;
/// use all_is_cubes::space::Space;
/// use all_is_cubes::content::axes;
///
/// let mut space = Space::empty(GridAab::from_lower_upper([-10, -10, -10], [11, 11, 11]));
/// axes(&mut space);
///
/// assert_ne!(space[[10, 0, 0]], AIR);
/// assert_ne!(space[[0, 10, 0]], AIR);
/// assert_ne!(space[[0, 0, 10]], AIR);
/// assert_ne!(space[[-10, 0, 0]], AIR);
/// assert_ne!(space[[0, -10, 0]], AIR);
/// assert_ne!(space[[0, 0, -10]], AIR);
/// ```
pub fn axes(space: &mut Space) -> Result<(), SetCubeError> {
    const DIR_NAMES: FaceMap<ArcStr> = FaceMap {
        nx: literal!("x"),
        ny: literal!("y"),
        nz: literal!("z"),
        px: literal!("X"),
        py: literal!("Y"),
        pz: literal!("Z"),
    };

    for face in Face6::ALL {
        let axis = face.axis();
        let direction = face.normal_vector::<GridCoordinate, ()>()[axis];
        let raycaster = Raycaster::new([0.5, 0.5, 0.5], face.normal_vector::<FreeCoordinate, _>())
            .within(space.bounds());
        for step in raycaster {
            let i = step.cube_ahead().lower_bounds()[axis] * direction; // always positive
            let (color, display_name): (Rgb, ArcStr) = if i.rem_euclid(2) == 0 {
                (axis.color(), i.rem_euclid(10).to_string().into())
            } else {
                if direction > 0 {
                    (rgb_const!(1.0, 1.0, 1.0), DIR_NAMES[face].clone())
                } else {
                    (rgb_const!(0.0, 0.0, 0.0), DIR_NAMES[face].clone())
                }
            };
            space.set(
                step.cube_ahead(),
                Block::builder()
                    .display_name(display_name)
                    .color(color.with_alpha_one())
                    .light_emission(axis.color() * 3.0)
                    .build(),
            )?;
        }
    }
    Ok(())
}

/// A set of inventory items to give character free movement and modification of
/// everything in the universe. (For the moment, actually just the current space.)
///
/// TODO: ideally `flying` wouldn't be an explicit parameter but determined based on
/// the same inputs as choose the spawn position.
pub fn free_editing_starter_inventory(flying: bool) -> Vec<Slot> {
    vec![
        Slot::one(Tool::RemoveBlock { keep: true }),
        Slot::one(Tool::Jetpack { active: flying }),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Atom, BlockAttributes, BlockCollision};

    #[test]
    fn make_some_blocks_0() {
        assert_eq!(make_some_blocks::<0>(), []);
    }

    #[test]
    fn make_some_blocks_1() {
        // Should succeed even though the normal range would be division-by-zero.
        assert_eq!(
            make_some_blocks::<1>(),
            [Block::from(Atom {
                attributes: BlockAttributes {
                    display_name: "0".into(),
                    ..BlockAttributes::default()
                },
                color: Rgba::new(0.5, 0.5, 0.5, 1.0),
                emission: Rgb::ZERO,
                collision: BlockCollision::Hard,
            })]
        );
    }

    #[test]
    fn make_some_blocks_2() {
        assert_eq!(
            make_some_blocks::<2>(),
            [
                Block::from(Atom {
                    attributes: BlockAttributes {
                        display_name: "0".into(),
                        ..BlockAttributes::default()
                    },
                    color: Rgba::new(0.0, 0.0, 0.0, 1.0),
                    emission: Rgb::ZERO,
                    collision: BlockCollision::Hard,
                }),
                Block::from(Atom {
                    attributes: BlockAttributes {
                        display_name: "1".into(),
                        ..BlockAttributes::default()
                    },
                    color: Rgba::new(1.0, 1.0, 1.0, 1.0),
                    emission: Rgb::ZERO,
                    collision: BlockCollision::Hard,
                })
            ]
        );
    }

    #[test]
    fn make_some_blocks_multiple_call_equality() {
        assert_eq!(make_some_blocks::<3>(), make_some_blocks::<3>());

        // Note: If we ever get "functional" procedural generation blocks, make_some_voxel_blocks should use it and this will change.
        let universe = &mut Universe::new();
        assert_ne!(
            make_some_voxel_blocks::<3>(universe),
            make_some_voxel_blocks::<3>(universe)
        );
    }
}

use std::mem::size_of;

use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::block::{
    self, Block, BlockAttributes, BlockCollision, Cost, EvaluatedBlock, Evoxel, Evoxels,
    Resolution::*, AIR,
};
use crate::math::{Cube, FaceMap, GridAab, Rgb, Rgba, Vol};
use crate::universe::Universe;

#[test]
fn evaluated_block_debug_simple() {
    let ev = color_block!(Rgba::WHITE).evaluate().unwrap();

    // not testing the one-line version because it'll be not too surprising
    assert_eq!(
        format!("{ev:#?}\n"),
        indoc! {"
                EvaluatedBlock {
                    color: Rgba(1.0, 1.0, 1.0, 1.0),
                    opaque: {all: true},
                    visible: true,
                    uniform_collision: Some(Hard),
                    resolution: 1,
                    voxel: Evoxel {
                        color: Rgba(1.0, 1.0, 1.0, 1.0),
                        emission: Rgb(0.0, 0.0, 0.0),
                        selectable: true,
                        collision: Hard,
                    },
                    voxel_opacity_mask: Some(GridAab(0..1, 0..1, 0..1)),
                    cost: Cost {
                        components: 1,
                        voxels: 0,
                        recursion: 0,
                    },
                }
            "}
    );
}

#[test]
fn evaluated_block_debug_complex() {
    let mut universe = Universe::new();
    let voxel = Block::builder()
        .color(Rgba::WHITE)
        .light_emission(Rgb::new(1.0, 2.0, 3.0))
        .build();
    let ev = Block::builder()
        .display_name("hello")
        .voxels_fn(R2, |p| {
            if p == Cube::new(1, 1, 1) {
                &AIR
            } else {
                &voxel
            }
        })
        .unwrap()
        .build_into(&mut universe)
        .evaluate()
        .unwrap();

    assert_eq!(
        format!("{ev:#?}\n"),
        indoc! {"
                EvaluatedBlock {
                    attributes: BlockAttributes {
                        display_name: \"hello\",
                    },
                    color: Rgba(1.0, 1.0, 1.0, 1.0),
                    light_emission: Rgb(1.0, 2.0, 3.0),
                    opaque: {−all: true, +all: false},
                    visible: true,
                    uniform_collision: None,
                    resolution: 2,
                    voxels: GridAab(0..2, 0..2, 0..2),
                    voxel_opacity_mask: Some(GridAab(0..2, 0..2, 0..2)),
                    cost: Cost {
                        components: 1,
                        voxels: 8,
                        recursion: 0,
                    },
                }
            "}
    );
}

/// `Evoxel`s are stored in large quantity, so we should think carefully any time we
/// might make it bigger. Or maybe even try to make it smaller.
#[test]
fn evoxel_size() {
    assert_eq!(
        size_of::<Evoxel>(),
        (4 + 3) * size_of::<f32>() // colors
                + 2 // flags
                + 2 // padding
    )
}

#[test]
fn visible_or_animated() {
    #[allow(clippy::needless_pass_by_value)]
    fn va(block: Block) -> bool {
        block.evaluate().unwrap().visible_or_animated()
    }
    assert!(!va(AIR));
    assert!(!va(Block::builder().color(Rgba::TRANSPARENT).build()));
    assert!(va(Block::builder().color(Rgba::WHITE).build()));
    assert!(va(Block::builder()
        .color(Rgba::TRANSPARENT)
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .build()));
}

#[test]
fn from_voxels_zero_bounds() {
    let attributes = BlockAttributes::default();
    let resolution = R4;
    let bounds = GridAab::from_lower_size([1, 2, 3], [0, 0, 0]);
    assert_eq!(
        EvaluatedBlock::from_voxels(
            attributes.clone(),
            Evoxels::Many(resolution, Vol::from_fn(bounds, |_| unreachable!())),
            Cost::ZERO
        ),
        EvaluatedBlock {
            attributes,
            color: Rgba::TRANSPARENT,
            face_colors: FaceMap::repeat(Rgba::TRANSPARENT),
            light_emission: Rgb::ZERO,
            voxels: Evoxels::Many(resolution, Vol::from_fn(bounds, |_| unreachable!())),
            opaque: FaceMap::repeat(false),
            visible: false,
            uniform_collision: Some(BlockCollision::None),
            voxel_opacity_mask: None,
            cost: Cost::ZERO, // TODO wrong
        }
    );
}

#[test]
fn solid_block_equivalent_at_any_resolution() {
    let mut attributes = BlockAttributes::default();
    attributes.display_name = "foo".into();

    for color in [
        Rgba::BLACK,
        Rgba::WHITE,
        Rgba::TRANSPARENT,
        Rgba::new(0.0, 0.5, 1.0, 0.5),
    ] {
        let voxel = Evoxel::from_color(color);
        let ev_one =
            EvaluatedBlock::from_voxels(attributes.clone(), Evoxels::One(voxel), Cost::ZERO);
        let ev_many = EvaluatedBlock::from_voxels(
            attributes.clone(),
            Evoxels::Many(R2, Vol::from_fn(GridAab::for_block(R2), |_| voxel)),
            Cost::ZERO,
        );

        // Check that they are identical except for the voxel data
        assert_eq!(
            EvaluatedBlock {
                voxels: ev_one.voxels.clone(),
                voxel_opacity_mask: ev_one.voxel_opacity_mask.clone(),
                ..ev_many
            },
            ev_one,
            "Input color {color:?}"
        );
    }
}

/// Test that interior color is hidden by surface color.
///
/// TODO: This test is irregular because it bypasses constructing a `Block`, but
/// this is convenient, but it doesn't match other tests in `crate::block`. What style
/// should we use?
#[test]
fn overall_color_ignores_interior() {
    let resolution = R8;
    let outer_bounds = GridAab::for_block(resolution);
    let inner_bounds = outer_bounds.expand(FaceMap::repeat(-1));
    let outer_color = Rgba::new(1.0, 0.0, 0.0, 1.0);
    let inner_color = Rgba::new(0.0, 1.0, 0.0, 1.0);
    let voxels = Evoxels::Many(
        resolution,
        Vol::from_fn(outer_bounds, |p| {
            Evoxel::from_color(if inner_bounds.contains_cube(p) {
                inner_color
            } else {
                outer_color
            })
        }),
    );

    // The inner_color should be ignored because it is not visible.
    let ev = EvaluatedBlock::from_voxels(BlockAttributes::default(), voxels, Cost::ZERO);

    assert_eq!(ev.color, outer_color);
}

#[test]
fn opacity_as_category() {
    for color in [
        Rgba::BLACK,
        Rgba::WHITE,
        Rgba::TRANSPARENT,
        Rgba::new(0.0, 0.5, 1.0, 0.5),
    ] {
        assert_eq!(
            Block::from(color).evaluate().unwrap().opacity_as_category(),
            color.opacity_category(),
            "Input color {color:?}"
        );
    }
}

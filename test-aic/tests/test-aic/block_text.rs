use all_is_cubes::arcstr::literal;
use pretty_assertions::assert_eq;

use all_is_cubes::block::text::{
    Font, Positioning, PositioningX, PositioningY, PositioningZ, Text,
};
use all_is_cubes::block::{self, Block, Primitive, Resolution};
use all_is_cubes::math::{Cube, GridAab, GridVector, Vol, range_len};
use all_is_cubes::space::Space;
use all_is_cubes::universe::Universe;
use all_is_cubes_render::raytracer::print_space;

/// Convert voxels with z range = 1 to a string for readable comparisons.
fn plane_to_text(voxels: Vol<&[block::Evoxel]>) -> Vec<String> {
    fn convert_voxel(v: &block::Evoxel) -> char {
        if v.color.fully_transparent() {
            '.'
        } else {
            '#'
        }
    }

    let z = voxels.bounds().lower_bounds().z;
    assert_eq!(range_len(voxels.bounds().z_range()), 1);
    voxels
        .bounds()
        .y_range()
        .into_iter()
        .rev() // flip Y axis
        .map(|y| {
            voxels
                .bounds()
                .x_range()
                .into_iter()
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
        print_space(&space.read(), [0., 0., 1.]);
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
        *ev.attributes(),
        Block::builder().display_name(literal!("ab")).build_attributes(),
    );
    assert_eq!(
        ev.voxels().bounds(),
        GridAab::from_lower_size([0, 0, 0], [16, 13, 1])
    );
    assert_eq!(ev.voxels().bounds(), text.bounding_voxels());

    assert_eq!(
        plane_to_text(ev.voxels().as_vol_ref()),
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
        plane_to_text(block.evaluate(universe.read_ticket()).unwrap().voxels().as_vol_ref()),
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
    #[case] bounds_range: core::range::Range<i32>,
    #[case] expected: core::range::Range<i32>,
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
        *ev.attributes(),
        Block::builder().display_name(literal!("ab")).build_attributes()
    );
    assert_eq!(ev.resolution(), Resolution::R1);
    assert!(!ev.visible());
}

// TODO: test that Evoxel attributes are as expected

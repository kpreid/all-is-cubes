use all_is_cubes::inv;

use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Modifier::Inventory",
    subtitle: "",
    placement: Placement::Surface,
)]
fn INVENTORY(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    let mut space = Space::empty(GridAab::from_lower_size([0, 0, 0], [4, 3, 1]));

    let tray = Block::builder()
        .display_name("Tray")
        .voxels_fn(R16, |cube| {
            // tray shape
            if cube.y == 0 || cube.y == 1 && alg::square_radius(R16, cube)[0] == 8 {
                const { &block::from_color!(palette::STEEL) }
            } else {
                &AIR
            }
        })?
        .inventory_config(inv::InvInBlock::new(
            9,
            R4,
            R16,
            vec![
                inv::IconRow::new(0..3, point3(1, 1, 1), vec3(5, 0, 0)),
                inv::IconRow::new(3..6, point3(1, 1, 6), vec3(5, 0, 0)),
                inv::IconRow::new(6..9, point3(1, 1, 11), vec3(5, 0, 0)),
            ],
        ))
        .build_txn(&mut txn);

    let (straight_pipe_block, elbow_pipe_block) = make_pipe_blocks(&mut txn);
    let clockwise_in_xy_plane = GridRotation::RyXZ;

    // for purposes of "what is the primitive" we define the elbow pipe as NZ-to-NX
    // but for demo building purposes it is easier if we say it is in the XY plane, NY-to-NX
    let elbow_pipe_block = elbow_pipe_block.rotate(GridRotation::RXZy);

    let pipe_with_item_1 = elbow_pipe_block
        .evaluate()
        .unwrap()
        .with_inventory([inv::Tool::Block(block::from_color!(Rgb::UNIFORM_LUMINANCE_RED)).into()]);
    let pipe_with_item_2 = elbow_pipe_block
        .evaluate()
        .unwrap()
        .with_inventory([inv::Tool::Block(block::from_color!(Rgb::UNIFORM_LUMINANCE_BLUE)).into()]);
    let empty_pipe = straight_pipe_block.evaluate().unwrap().with_inventory([]);
    let empty_elbow_pipe = elbow_pipe_block.evaluate().unwrap().with_inventory([]);

    stack(
        &mut space,
        [0, 0, 0],
        [
            pedestal,
            &tray.evaluate().unwrap().with_inventory([
                inv::Tool::Block(demo_blocks[DemoBlocks::ExhibitBackground].clone()).into(),
                inv::Tool::Block(block::from_color!(Rgb::UNIFORM_LUMINANCE_RED)).into(),
                inv::Tool::Block(block::from_color!(Rgb::UNIFORM_LUMINANCE_GREEN)).into(),
                inv::Tool::Block(block::from_color!(Rgb::UNIFORM_LUMINANCE_BLUE)).into(),
                inv::Tool::Block(demo_blocks[DemoBlocks::Lamp(true)].clone()).into(),
            ]),
        ],
    )?;
    stack(
        &mut space,
        [1, 0, 0],
        [pedestal, &tray.evaluate().unwrap().with_inventory([])],
    )?;
    stack(
        &mut space,
        [2, 0, 0],
        [
            &pipe_with_item_1.rotate(clockwise_in_xy_plane),
            &empty_pipe.clone().rotate(GridRotation::RXZY),
            &pipe_with_item_2
                .clone()
                .rotate(clockwise_in_xy_plane * clockwise_in_xy_plane),
        ],
    )?;

    stack(
        &mut space,
        [3, 0, 0],
        [
            &empty_elbow_pipe.clone(),
            &empty_pipe.clone().rotate(GridRotation::RXZy),
            &empty_elbow_pipe.rotate(clockwise_in_xy_plane.inverse()),
        ],
    )?;

    Ok((space, txn))
}

fn make_pipe_blocks(txn: &mut ExhibitTransaction) -> (Block, Block) {
    // TODO: Move this to `DemoBlocks`?

    let pipe_corner_material = block::from_color!(0.6, 0.2, 0.2);
    let pipe_side_pattern_material = block::from_color!(0.3, 0.1, 0.1);
    let pipe_side_glass = block::from_color!(0.4, 0.4, 0.4, 0.2);
    let pipe_box_style = BoxStyle::from_fn(|part| {
        if part.is_corner() || part.is_edge() {
            Some(pipe_corner_material.clone())
        } else if part.is_face() && !part.is_on_face(Face6::NZ) && !part.is_on_face(Face6::PZ) {
            // replaced in actual use
            Some(block::from_color!(0.1, 0.1, 0.1))
        } else {
            None
        }
    });

    let resolution = R32;
    let pipe_scale = R4;
    let pipe_slots = inv::Ix::from(pipe_scale) * 2 - 1;
    let straight_pipe_box = GridAab::from_lower_upper([11, 11, 0], [21, 21, 32]);

    let straight_voxels_fn = |cube| {
        let Some(part) = BoxPart::from_cube(straight_pipe_box, cube) else {
            return &AIR;
        };
        if part.is_face() && !part.is_on_face(Face6::NZ) && !part.is_on_face(Face6::PZ) {
            // Sides of the pipe get an arrow pattern
            let xy_diamond_radius = (cube.x * 2 - 31).abs().min((cube.y * 2 - 31).abs());
            if (cube.z + xy_diamond_radius + 2).rem_euclid(32) < 12 {
                &pipe_side_pattern_material
            } else {
                &pipe_side_glass
            }
        } else {
            pipe_box_style[part].as_ref().unwrap_or(&AIR)
        }
    };

    let straight_pipe = Block::builder()
        .display_name("Pipe")
        .voxels_fn(resolution, straight_voxels_fn)
        .unwrap()
        .inventory_config(inv::InvInBlock::new(
            pipe_slots,
            pipe_scale,
            resolution,
            // pipe slots overlap by half to allow a smoothish movement
            vec![inv::IconRow::new(
                0..pipe_slots,
                point3(12, 12, 0),
                vec3(0, 0, 4),
            )],
        ))
        .tick_action(block::TickAction {
            operation: Operation::Alt(
                [
                    Operation::MoveInventory {
                        transfer_into_adjacent: Some(Face6::PZ),
                    },
                    // This turns failures to move into do-nothing successes.
                    // TODO: there should be a simple empty operation.
                    Operation::Neighbors([].into()),
                ]
                .into(),
            ),
            schedule: time::Schedule::from_period(NonZero::new(6).unwrap()),
        })
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .build_txn(txn);

    let elbow_pipe = Block::builder()
        .display_name("Pipe")
        .voxels_fn(resolution, |mut cube| {
            // Bevel-join the pipe so that it connects NZ to NX instead of PZ.
            if cube.x < cube.z {
                (cube.x, cube.z) = (cube.z, 31 - cube.x);
            }
            straight_voxels_fn(cube)
        })
        .unwrap()
        .inventory_config(inv::InvInBlock::new(
            pipe_slots,
            pipe_scale,
            R32,
            // pipe slots overlap by half to allow a smoothish movement
            vec![
                inv::IconRow::new(0..(pipe_slots / 2), point3(12, 12, 0), vec3(0, 0, 4)),
                inv::IconRow::new(
                    (pipe_slots / 2)..pipe_slots,
                    point3(12, 12, 12),
                    vec3(-4, 0, 0),
                ),
            ],
        ))
        .tick_action(block::TickAction {
            operation: Operation::Alt(
                [
                    Operation::MoveInventory {
                        transfer_into_adjacent: Some(Face6::NX),
                    },
                    // This turns failures to move into do-nothing successes.
                    // TODO: there should be a simple empty operation.
                    Operation::Neighbors([].into()),
                ]
                .into(),
            ),
            schedule: time::Schedule::from_period(NonZero::new(6).unwrap()),
        })
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .build_txn(txn);

    (straight_pipe, elbow_pipe)
}

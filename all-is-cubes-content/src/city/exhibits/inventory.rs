use hashbrown::HashMap;
use itertools::Itertools;

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

    let mut space = Space::empty(GridAab::from_lower_size([0, 0, -1], [4, 3, 2]));

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

    let pipe_table = prepare_pipes([
        // TODO: Should prepare_pipes() automatically fix the lack of inventories?
        PipeData {
            block: straight_pipe_block
                .evaluate(txn.read_ticket())
                .unwrap()
                .with_inventory([]),
            from_face: Face6::NZ,
            to_face: Face6::PZ,
        },
        PipeData {
            block: elbow_pipe_block
                .evaluate(txn.read_ticket())
                .unwrap()
                .with_inventory([]),
            from_face: Face6::NZ,
            to_face: Face6::NX,
        },
    ]);

    space.mutate(txn.read_ticket(), |m| {
        stack(
            m,
            [0, 0, 0],
            [
                pedestal,
                &tray.evaluate(txn.read_ticket()).unwrap().with_inventory([
                    inv::Tool::Block(demo_blocks[DemoBlocks::ExhibitBackground].clone()).into(),
                    inv::Tool::Block(block::from_color!(Rgb::UNIFORM_LUMINANCE_RED)).into(),
                    inv::Tool::Block(block::from_color!(Rgb::UNIFORM_LUMINANCE_GREEN)).into(),
                    inv::Tool::Block(block::from_color!(Rgb::UNIFORM_LUMINANCE_BLUE)).into(),
                    inv::Tool::Block(demo_blocks[DemoBlocks::Lamp(true)].clone()).into(),
                ]),
            ],
        )?;
        stack(
            m,
            [1, 0, 0],
            [
                pedestal,
                &tray.evaluate(txn.read_ticket()).unwrap().with_inventory([]),
            ],
        )?;

        for (cube, block) in fit_pipes(
            &pipe_table,
            [
                [2, 0, 0],
                [2, 1, 0],
                [2, 2, 0],
                [2, 2, -1],
                [2, 1, -1],
                [1, 1, -1],
                [1, 0, -1],
                [2, 0, -1],
                [3, 0, -1],
                [3, 1, -1],
                [3, 2, -1],
                [3, 2, 0],
                [3, 1, 0],
                [3, 0, 0],
                // wrap around to join the start
                [2, 0, 0],
                [2, 1, 0],
            ]
            .map(Cube::from),
        ) {
            m.set(cube, block)?;
        }

        // Place some items in the loop of pipe
        {
            let pipe_with_item_1 = straight_pipe_block
                .evaluate(txn.read_ticket())
                .unwrap()
                .with_inventory([
                    inv::Tool::Block(block::from_color!(Rgb::UNIFORM_LUMINANCE_RED)).into(),
                ]);
            let pipe_with_item_2 = straight_pipe_block
                .evaluate(txn.read_ticket())
                .unwrap()
                .with_inventory([inv::Tool::Block(block::from_color!(
                    Rgb::UNIFORM_LUMINANCE_BLUE
                ))
                .into()]);
            m.set([2, 1, 0], pipe_with_item_1.rotate(GridRotation::RXzY))?;
            m.set([3, 1, 0], pipe_with_item_2.rotate(GridRotation::RXZy))?;
        }

        Ok::<(), InGenError>(())
    })?;

    Ok((space, txn))
}

struct PipeData {
    block: Block,
    from_face: Face6,
    to_face: Face6,
}

// Assumed to be complete
type PipeTable = HashMap<(Face6, Face6), Block>;

/// Take a set of [`PipeData`] and expand it with rotations of those pipes.
fn prepare_pipes(pipes: impl IntoIterator<Item = PipeData>) -> PipeTable {
    let mut table: HashMap<(Face6, Face6), Block> = HashMap::new();
    for pipe in pipes {
        // TODO: Allow caller — or the individual pipes — to rank preferred and dispreferred
        // rotations more finely than "prefer IDENTITY".
        for rotation in GridRotation::ALL_BUT_REFLECTIONS {
            let faces = (
                rotation.transform(pipe.from_face),
                rotation.transform(pipe.to_face),
            );
            if rotation == GridRotation::IDENTITY {
                // Unconditionally insert.
                table.insert(faces, pipe.block.clone().rotate(rotation));
            } else {
                // Insert only if we don't have one already.
                table
                    .entry(faces)
                    .or_insert_with(|| pipe.block.clone().rotate(rotation));
            }
        }
    }
    table
}

/// Given a set of `pipes`, rotate and place them so that they follow the `path`.
///
/// The first and last elements of `path` are left unaltered and indicate the starting and
/// ending faces the pipes should connect to.
///
/// `pipes` should contain a straight pipe and a right-angle pipe.
/// If `pipes` contains multiple elements with the same angle, the one which does not require
/// rotation will be preferred.
///
/// Panics if any element of `path` is not adjacent to the previous element.
/// Panics if `pipes` does not contain a straight pipe and a right-angle pipe and one is needed.
#[track_caller]
fn fit_pipes(
    pipes: &PipeTable,
    path: impl IntoIterator<Item = Cube>,
) -> impl Iterator<Item = (Cube, &Block)> {
    path.into_iter()
        .tuple_windows()
        .map(|(cube_behind, cube_here, cube_ahead)| {
            let face_behind = Face6::try_from(cube_behind - cube_here).expect("invalid path");
            let face_ahead = Face6::try_from(cube_ahead - cube_here).expect("invalid path");

            (cube_here, &pipes[&(face_behind, face_ahead)])
        })
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

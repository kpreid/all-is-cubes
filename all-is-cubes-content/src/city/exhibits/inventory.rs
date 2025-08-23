use super::prelude::*;
use crate::pipe;

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

    let (straight_pipe_block, elbow_pipe_block) = pipe::make_pipe_blocks(&mut txn);

    let kit = pipe::Kit::new_with_rotations([
        // TODO: Should prepare_pipes() automatically fix the lack of inventories?
        pipe::Descriptor {
            block: straight_pipe_block.evaluate(txn.read_ticket()).unwrap().with_inventory([]),
            from_face: Face6::NZ,
            to_face: Face6::PZ,
        },
        pipe::Descriptor {
            block: elbow_pipe_block.evaluate(txn.read_ticket()).unwrap().with_inventory([]),
            from_face: Face6::NZ,
            to_face: Face6::NX,
        },
    ]);

    space.mutate(ctx.universe.read_ticket().with_transaction(&txn), |m| {
        stack(
            m,
            [0, 0, 0],
            [
                pedestal,
                &tray.evaluate(txn.read_ticket()).unwrap().with_inventory([
                    inv::Tool::Block(demo_blocks[DemoBlocks::ExhibitBackground].clone()).into(),
                    inv::Tool::Block(block::from_color!(Rgb01::UNIFORM_LUMINANCE_RED)).into(),
                    inv::Tool::Block(block::from_color!(Rgb01::UNIFORM_LUMINANCE_GREEN)).into(),
                    inv::Tool::Block(block::from_color!(Rgb01::UNIFORM_LUMINANCE_BLUE)).into(),
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

        for (cube, block) in kit.fit(
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
            let pipe_with_item_1 =
                straight_pipe_block.evaluate(txn.read_ticket()).unwrap().with_inventory([
                    inv::Tool::Block(block::from_color!(Rgb01::UNIFORM_LUMINANCE_RED)).into(),
                ]);
            let pipe_with_item_2 =
                straight_pipe_block.evaluate(txn.read_ticket()).unwrap().with_inventory([
                    inv::Tool::Block(block::from_color!(Rgb01::UNIFORM_LUMINANCE_BLUE)).into(),
                ]);
            m.set([2, 1, 0], pipe_with_item_1.rotate(GridRotation::RXzY))?;
            m.set([3, 1, 0], pipe_with_item_2.rotate(GridRotation::RXZy))?;
        }

        Ok::<(), InGenError>(())
    })?;

    Ok((space, txn))
}

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

    let mut space = Space::empty(GridAab::from_lower_size([0, 0, 0], [4, 2, 1]));

    let inventory_display_block = Block::builder()
        .display_name("Has some inventory")
        .voxels_fn(R16, |cube| {
            // tray shape
            if cube.y == 0 || cube.y == 1 && alg::square_radius(R16, cube)[0] == 8 {
                const { &color_block!(palette::STEEL) }
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

    let has_items_block = inventory_display_block.evaluate().unwrap().with_inventory([
        inv::Tool::Block(demo_blocks[DemoBlocks::ExhibitBackground].clone()).into(),
        inv::Tool::Block(color_block!(Rgb::UNIFORM_LUMINANCE_RED)).into(),
        inv::Tool::Block(color_block!(Rgb::UNIFORM_LUMINANCE_GREEN)).into(),
        inv::Tool::Block(color_block!(Rgb::UNIFORM_LUMINANCE_BLUE)).into(),
        inv::Tool::Block(demo_blocks[DemoBlocks::Lamp(true)].clone()).into(),
    ]);

    stack(&mut space, [0, 0, 0], [pedestal, &has_items_block])?;

    Ok((space, txn))
}

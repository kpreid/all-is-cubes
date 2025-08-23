use super::prelude::*;

#[exhibit(
    name: "Miscellaneous",
    subtitle: "blocks",
    placement: Placement::Surface,
)]
fn MISC_BLOCKS(ctx: Context<'_>) {
    let txn = ExhibitTransaction::default();
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;

    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
        .read_ticket(ctx.universe.read_ticket())
        .build_and_mutate(|m| {
            stack(m, [0, 0, 0], [demo_blocks[DemoBlocks::Crate].clone()])?;
            stack(m, [1, 0, 0], [demo_blocks[DemoBlocks::Greebly].clone()])?;
            Ok(())
        })?;

    Ok((space, txn))
}

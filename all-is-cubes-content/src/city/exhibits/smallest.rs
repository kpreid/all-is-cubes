use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "World's Smallest Voxel",
    subtitle: "1/128th the length of a standard block",
    placement: Placement::Surface,
)]
fn SMALLEST(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    let resolution = R128;
    assert_eq!(
        resolution,
        Resolution::MAX,
        "need to update the exhibit info"
    );
    let rg = GridCoordinate::from(resolution);

    let block_space = Space::builder(GridAab::from_lower_size([rg / 2, 0, rg / 2], [1, 1, 1]))
        .read_ticket(ctx.universe.read_ticket())
        .filled_with(block::from_color!(palette::ALMOST_BLACK))
        .build();

    let mut exhibit_space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 2, 1])).build();
    exhibit_space.mutate(ctx.universe.read_ticket(), |m| {
        stack(
            m,
            [0, 0, 0],
            [
                pedestal,
                &Block::builder()
                    .display_name("World's Smallest Voxel")
                    .voxels_handle(resolution, txn.insert_anonymous(block_space))
                    .build(),
            ],
        )
    })?;

    Ok((exhibit_space, txn))
}

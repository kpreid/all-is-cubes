use crate::alg::voronoi_pattern;

use super::prelude::*;

#[exhibit(
    name: "Block Destruction",
    subtitle: "Animation prototype",
    placement: Placement::Surface,
)]
fn DESTRUCTION(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();

    let width: u16 = 7;

    let footprint = GridAab::from_lower_size([0, 0, 0], [width.into(), 3, 1]);
    let mut space = Space::builder(footprint).build();
    space.mutate(ctx.universe.read_ticket(), |m| {
        let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(ctx.universe)?;
        let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
        let pedestal = &demo_blocks[DemoBlocks::Pedestal];
        let block_to_destroy = &landscape_blocks[LandscapeBlocks::Grass];

        let mut next_mask = None;
        for stage in 0i32..width.into() {
            let mask = generate_destruction_mask(
                &mut txn,
                R16,
                (f64::from(stage) + 0.5) / f64::from(width),
                next_mask,
            )?;
            next_mask = Some(mask.clone());

            let destroyed = block_to_destroy
                .clone()
                .with_modifier(Composite::new(mask, CompositeOperator::In).reversed());

            stack(m, GridPoint::new(stage, 0, 0), [pedestal, &destroyed])?;
        }
        Ok::<(), InGenError>(())
    })?;

    Ok((space, txn))
}

fn generate_destruction_mask(
    txn: &mut ExhibitTransaction,
    resolution: Resolution,
    fraction: f64,
    next_mask: Option<Block>,
) -> Result<Block, InGenError> {
    let solid = block::from_color!(Rgba::WHITE);
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(3887829);
    let points: [_; 32] = core::array::from_fn(|_| {
        let free_point = Cube::ORIGIN.aab().random_point(&mut rng);
        (
            free_point,
            if free_point.y > fraction {
                AIR
            } else {
                solid.clone()
            },
        )
    });
    let pattern = voronoi_pattern(resolution, false, points.as_slice());

    Ok(Block::builder()
        .voxels_fn(resolution, pattern)?
        .activation_action(next_mask.map(Operation::Become))
        .build_txn(txn))
}

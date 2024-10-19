use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Modifier::Move",
    subtitle: "Stationary but not animated cases.",
    placement: Placement::Surface,
)]
fn MOVED_BLOCKS(_: Context<'_>) {
    let mut txn = ExhibitTransaction::default();
    let mut space = Space::empty(GridAab::from_lower_upper([0, 0, -3], [16, 2, 3]));

    let blocks: [Block; 16] = make_some_voxel_blocks_txn(&mut txn);
    for x in 0..8 {
        for z in 0..2 {
            let i = x + z * 8;
            let distance = (i * 16).try_into().unwrap();
            let block = &blocks[i as usize];
            let [move_out, move_in] = Move::new(Face6::PY, distance, 0).to_paired();
            // TODO: Move should be able to spawn a "tail" on its own when animated?
            space.set(
                [x * 2, 0, (1 - z) * 2],
                block.clone().with_modifier(move_out),
            )?;
            space.set(
                [x * 2, 1, (1 - z) * 2],
                block.clone().with_modifier(move_in),
            )?;

            // Horizontal
            let [move_out, move_in] = Move::new(Face6::PZ, distance, 0).to_paired();
            space.set([i, 0, -2], block.clone().with_modifier(move_out))?;
            space.set([i, 0, -1], block.clone().with_modifier(move_in))?;
        }
    }
    Ok((space, txn))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Projectile",
    subtitle: "click me to launch",
    placement: Placement::Surface,
)]
fn PROJECTILE(ctx: Context<'_>) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let txn = ExhibitTransaction::default();

    let projectile = &demo_blocks[DemoBlocks::Projectile];

    let moving_in = projectile
        .clone()
        .with_modifier(Move::new(Face6::NY, 256, -32));

    // TODO: make the launcher block visibly contain and launch the projectile.
    // This will require getting `Move` tick actions to cooperate with `Composite`.
    let launcher = Block::builder()
        .display_name(literal!("Launcher"))
        .color(Rgb::UNIFORM_LUMINANCE_RED.with_alpha(ps32(1.0)))
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .activation_action(Operation::Neighbors(
            [
                // TODO: Instead of `DestroyTo`, we should have an operation that only
                // succeeds if there is room to enter empty space here (if the destination
                // is AIR, for now).
                (Cube::new(0, 1, 0), Operation::DestroyTo(moving_in)),
            ]
            .into(),
        ))
        .build();

    let space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(launcher)
        .build();

    Ok((space, txn))
}

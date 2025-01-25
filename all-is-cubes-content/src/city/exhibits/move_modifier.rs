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
            let [move_out, move_in] = Move::new(Face6::PY, distance, 0).into_paired();
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
            let [move_out, move_in] = Move::new(Face6::PZ, distance, 0).into_paired();
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
    let launch_automatically = false;

    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let txn = ExhibitTransaction::default();

    let projectile = &demo_blocks[DemoBlocks::Projectile];

    let [move_in, _move_out] = Move::new(Face6::NY, 256, -32).into_paired();
    let projectile_moving_in = projectile.clone().with_modifier(move_in);
    // let projectile_moving_out = projectile.clone().with_modifier(move_out);

    let launch_operation = Operation::Neighbors(
        [
            // TODO: We want to animate the projectile exiting;
            // currently, if we do this, the launcher doesn't work any more,
            // probably because it is a composite with a left-over Air block.
            // Improve modifier and compositing semantics so this works.
            //
            // (
            //     Cube::new(0, 0, 0),
            //     Operation::AddModifiers(Arc::new([Composite::new(
            //         projectile_moving_out,
            //         CompositeOperator::Over,
            //     )
            //     .reversed()
            //     .into()])),
            // ),

            // TODO: Instead of `DestroyTo`, we should have an operation that only
            // succeeds if there is room to enter empty space here (if the destination
            // is AIR, for now).
            (
                Cube::new(0, 1, 0),
                Operation::DestroyTo(projectile_moving_in),
            ),
        ]
        .into(),
    );

    let tick_action = launch_automatically.then(|| block::TickAction {
        operation: launch_operation.clone(),
        schedule: time::Schedule::from_period(NonZero::new(60).unwrap()),
    });

    let launcher = Block::builder()
        .display_name(literal!("Launcher"))
        .color(Rgb::UNIFORM_LUMINANCE_RED.with_alpha(zo32(1.0)))
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .activation_action(launch_operation)
        .tick_action(tick_action)
        .build();

    let space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(launcher)
        .build();

    Ok((space, txn))
}

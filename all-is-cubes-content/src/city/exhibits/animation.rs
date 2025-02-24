use crate::AnimatedVoxels;

use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Animation",
    subtitle: "Blocks whose definition is animated",
    placement: Placement::Surface,
)]
fn ANIMATION(ctx: Context<'_>) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;

    let footprint = GridAab::from_lower_size([0, 0, -1], [5, 2, 3]);
    let mut space = Space::empty(footprint);
    let mut txn = ExhibitTransaction::default();

    let sweep_block = {
        let resolution = R8;
        let mut block_space = Space::for_block(resolution).build();
        // The length of this pattern is set so that the block will sometimes be fully opaque and sometimes be invisible.
        let fills = [
            AIR,
            AIR,
            AIR,
            AIR,
            AIR,
            block::from_color!(0.0, 0.3, 0.0),
            block::from_color!(0.0, 0.7, 0.0),
            block::from_color!(0.0, 1.0, 0.0),
            block::from_color!(0.0, 0.7, 0.7),
            block::from_color!(0.0, 0.3, 1.0),
        ];
        let repeats_per_fill = 6;
        SpaceTransaction::add_behavior(
            block_space.bounds(),
            AnimatedVoxels::new(move |p, frame| {
                let n = fills.len() as GridCoordinate * repeats_per_fill;
                let location_offset = p.x + p.y + p.z;
                let time_offset = (frame as GridCoordinate).rem_euclid(n);
                let value = location_offset.wrapping_sub(time_offset);
                fills[value
                    .div_euclid(repeats_per_fill)
                    .rem_euclid(fills.len() as GridCoordinate) as usize]
                    .clone()
            }),
        )
        .execute(&mut block_space, &mut transaction::no_outputs)?;
        Block::builder()
            .animation_hint(block::AnimationHint::redefinition(
                block::AnimationChange::Shape,
            ))
            .voxels_handle(resolution, txn.insert_anonymous(block_space))
            .build()
    };

    let fire_block = {
        let fire_resolution = R8;
        Block::builder()
            .animation_hint(block::AnimationHint::redefinition(
                block::AnimationChange::Shape,
            ))
            .voxels_handle(fire_resolution, {
                let fire_bounds = GridAab::for_block(fire_resolution);
                let mut space = Space::for_block(fire_resolution).build();
                space.set([0, 0, 0], block::from_color!(Rgb::ONE))?; // placeholder for not fully transparent so first pass lighting is better
                SpaceTransaction::add_behavior(fire_bounds, Fire::new(fire_bounds))
                    .execute(&mut space, &mut transaction::no_outputs)
                    .unwrap();
                txn.insert_anonymous(space)
            })
            .build()
    };

    space.set([0, 0, 0], sweep_block)?;
    space.fill_uniform(
        GridAab::from_lower_upper([2, 0, -1], [5, 1, 2]),
        &fire_block,
    )?;
    space.set([3, 0, 0], &demo_blocks[DemoBlocks::Road])?; // in middle of fire
    space.set([1, 1, -1], &demo_blocks[DemoBlocks::Clock])?;

    Ok((space, txn))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Operation::Become",
    subtitle: "",
    placement: Placement::Surface,
)]
fn BECOME(ctx: Context<'_>) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    let mut space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 2, 3])).build();
    for (state, z) in [(false, 0), (true, 2)] {
        stack(
            &mut space,
            [0, 0, z],
            [pedestal, &demo_blocks[DemoBlocks::BecomeBlinker(state)]],
        )?;
    }

    Ok((space, ExhibitTransaction::default()))
}

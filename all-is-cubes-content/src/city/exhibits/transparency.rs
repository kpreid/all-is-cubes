use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Full Block Transparency",
    subtitle:
        "Test depth sorting and blending.\n\
        Lighting of volumes still needs work.",
    placement: Placement::Surface,
)]
fn TRANSPARENCY_WHOLE_BLOCK(ctx: Context<'_>) {
    let colors = [
        rgb01!(1.0, 0.5, 0.5),
        rgb01!(0.5, 1.0, 0.5),
        rgb01!(0.5, 0.5, 1.0),
        rgb01!(0.9, 0.9, 0.9),
    ];
    let alphas = [0.25, 0.5, 0.75, 0.95].map(zo32);
    let windowpane = GridAab::from_lower_upper([-1, 0, 3], [2, alphas.len() as GridCoordinate, 4]);

    let space = Space::builder(GridAab::from_lower_size([-3, 0, -3], [7, 5, 7]))
        .read_ticket(ctx.universe.read_ticket())
        .build_and_mutate(|m| {
            for (rot, color) in Face6::PY.clockwise().iterate().zip(&colors) {
                m.fill(
                    windowpane
                        .transform(rot.to_positive_octant_transform(1))
                        .unwrap(),
                    |Cube { y, .. }| Some(Block::from(color.with_alpha(alphas[y as usize]))),
                )?;
            }

            Ok(())
        })?;
    Ok((space, ExhibitTransaction::default()))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Smaller Block Transparency",
    subtitle:
        "Large but not full blocks.",
    placement: Placement::Surface,
)]
fn TRANSPARENCY_SHRUNKEN_BLOCK(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();

    // TODO: add some more variety of shape variations and overlaps

    let slab_1 = Block::builder()
        .voxels_fn(R2, |cube| {
            if cube.x >= 1 {
                block::from_color!(0.9, 0.9, 1.0, 0.99)
            } else {
                AIR
            }
        })?
        .build_txn(&mut txn);
    let slab_2 = Block::builder()
        .voxels_fn(R2, |cube| {
            if cube.x < 1 {
                block::from_color!(0.05, 0.05, 0.05, 0.99)
            } else {
                AIR
            }
        })?
        .build_txn(&mut txn);

    let space = Space::builder(GridAab::from_lower_size([-3, 0, -3], [7, 5, 7]))
        .read_ticket(ctx.universe.read_ticket())
        .build_and_mutate(|m| {
            for rot in Face6::PY.clockwise().iterate() {
                let region = GridAab::from_lower_upper([-1, 0, 3], [2, 4, 4]);
                m.fill(
                    region
                        .transform(rot.to_positive_octant_transform(1))
                        .unwrap(),
                    |Cube { x, y, z, .. }| {
                        Some(
                            [&slab_1, &slab_2]
                                [x.wrapping_add(y).wrapping_add(z).rem_euclid(2) as usize],
                        )
                    },
                )?;
            }

            Ok(())
        })?;

    Ok((space, txn))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Complex Voxel Transparency",
    subtitle:
        "",
    placement: Placement::Surface,
)]
fn TRANSPARENCY_GLASS_AND_WATER(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();

    let footprint = GridAab::from_lower_size([0, 0, 0], [7, 4, 7]);
    let pool = GridAab::from_lower_size([1, 0, 1], [5, 2, 5]);
    let mut space = Space::empty(footprint);

    let water_voxel = Block::builder()
        .color(rgba_const!(0.02, 0.04, 0.9, 0.5))
        .collision(BlockCollision::None)
        .build();
    let water_surface_voxel = Block::builder()
        .color(rgba_const!(0.5, 0.5, 0.72, 0.8))
        .collision(BlockCollision::None)
        .build();

    let water_surface_block = Block::builder()
        .voxels_fn(R8, |p| match p.y {
            0..4 => &water_voxel,
            4 => &water_surface_voxel,
            _ => &AIR,
        })?
        .build_txn(&mut txn);

    let window_block = {
        let window_pane_resolution = R32;
        let depth = 3;
        let window_frame_block = block::from_color!(palette::ALMOST_BLACK);
        let window_glass_surface_block = block::from_color!(0.5, 0.72, 0.5, 0.6);
        let window_glass_inner_block = block::from_color!(0.7, 0.72, 0.7, 0.05);
        let upper = GridCoordinate::from(window_pane_resolution) - 1;

        Block::builder()
            .rotation_rule(RotationPlacementRule::Attach { by: Face6::NZ })
            .voxels_fn(window_pane_resolution, |p| {
                if p.z >= depth {
                    return &AIR;
                }
                if p.x == 0 || p.y == 0 || p.x == upper || p.y == upper {
                    return &window_frame_block;
                }
                if p.z == depth - 1 || p.z == 0 {
                    if p.x == 1 || p.y == 1 || p.x == upper - 1 || p.y == upper - 1 {
                        &window_frame_block
                    } else {
                        &window_glass_surface_block
                    }
                } else {
                    &window_glass_inner_block
                }
            })?
            .build_txn(&mut txn)
    };

    space.mutate(ctx.universe.read_ticket(), |m| {
        // TODO: This use of `four_walls()` can be replaced with a `BoxStyle`.
        for wall in four_walls(pool.expand(FaceMap::symmetric([1, 0, 1]))) {
            m.fill_uniform(
                wall.bounds_excluding_corners,
                &window_block.clone().rotate(
                    GridRotation::from_to(Face6::PX, wall.counterclockwise_direction, Face6::PY)
                        .unwrap(),
                ),
            )?;
        }

        m.fill_uniform(
            pool.abut(Face6::NY, 0).unwrap().abut(Face6::PY, 1).unwrap(),
            &water_voxel,
        )?;
        m.fill_uniform(pool.abut(Face6::PY, -1).unwrap(), &water_surface_block)?;

        let [floater] = make_some_voxel_blocks_txn(&mut txn);
        m.set([3, 1, 3], floater)?;

        Ok::<(), InGenError>(())
    })?;

    Ok((space, txn))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Simple Transparent Voxels",
    subtitle: "",
    placement: Placement::Surface,
)]
fn TRANSPARENCY_VOX(ctx: Context<'_>) {
    // The previous voxel transparency exhibit proved too hard to reach conclusions from.
    // This one has blatantly obvious geometry and strong but not 100% opacity.

    let mut txn = ExhibitTransaction::default();

    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    let resolution = R4;
    let block = Block::builder()
        .voxels_fn(resolution, |cube| {
            if cube.lower_bounds().rem_euclid(&Size3D::splat(2)) == point3(0, 0, 0) {
                block::from_color!(1.0, 0.0, 0.0, 0.9)
            } else if cube.lower_bounds().rem_euclid(&Size3D::splat(2)) == point3(1, 1, 1) {
                block::from_color!(0.0, 0.0, 1.0, 0.9)
            } else {
                block::from_color!(1.0, 1.0, 1.0, 0.12)
            }
        })?
        .build_txn(&mut txn);

    let exhibit_space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 2, 1]))
        .read_ticket(ctx.universe.read_ticket())
        .build_and_mutate(|m| stack(m, [0, 0, 0], [pedestal, &block]))?;

    Ok((exhibit_space, txn))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Swimming Pool",
    subtitle: "Transparent blocks that can be passed through",
    placement: Placement::Surface,
)]
fn SWIMMING_POOL(ctx: Context<'_>) {
    let width = 6;
    let depth = 6;
    let water_area = GridAab::from_lower_upper([0, -depth, 0], [width, 0, width]);
    let mut space = Space::empty(water_area);
    space.mutate(ctx.universe.read_ticket(), |m| {
        m.fill_uniform(
            water_area,
            &Block::builder()
                .display_name("Not entirely unlike water")
                .color(Rgba::new(0.96, 0.96, 1.0, 0.1))
                .collision(BlockCollision::None)
                .build(),
        )
    })?;
    Ok((space, ExhibitTransaction::default()))
}

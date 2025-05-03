use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Trees",
    subtitle: "",
    placement: Placement::Surface,
)]
fn TREES(ctx: Context<'_>) {
    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(ctx.universe)?;
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(128947981240);

    let n_x = 4;
    let n_z = 4;
    let spacing_x = 6;
    let spacing_z = 6;
    let bounds = GridAab::from_lower_upper(
        [-2, -1, -2],
        [(n_x - 1) * spacing_x + 3, 20, (n_z - 1) * spacing_z + 3],
    );
    let mut space = Space::builder(bounds).build();

    space
        .mutate(ctx.universe.read_ticket(), |m| {
            // Grassy floor
            m.fill_uniform(
                bounds.abut(Face6::NY, -1).unwrap(),
                &landscape_blocks[LandscapeBlocks::Grass],
            )?;

            for ix in 0..n_x {
                for iz in 0..n_z {
                    let origin = Cube::new(ix * spacing_x, 0, iz * spacing_z);
                    tree::make_tree(
                        &landscape_blocks,
                        &mut rng,
                        origin,
                        GridAab::single_cube(origin).expand(FaceMap {
                            nx: 2,
                            ny: 0,
                            nz: 2,
                            px: 2,
                            py: u32::try_from(ix + iz * 2).unwrap(),
                            pz: 2,
                        }),
                    )
                    .execute_m(m)?;
                }
            }

            // exhibit of leaves growth stages for debugging
            for (i, g) in (0i32..).zip(tree::TreeGrowth::exhaust()) {
                m.set(
                    [i * 2, 0, bounds.lower_bounds().z],
                    &landscape_blocks[LandscapeBlocks::Leaves(g)],
                )?;
            }

            Ok::<(), InGenError>(())
        })
        .unwrap();
    Ok((space, ExhibitTransaction::default()))
}

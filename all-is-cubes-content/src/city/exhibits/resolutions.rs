use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Resolutions",
    subtitle:
        "Voxel blocks can be subdivided into\n\
        powers of 2 from 2 to 128.",
    placement: Placement::Surface,
)]
fn RESOLUTIONS(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();

    let footprint = GridAab::from_lower_size([0, 0, 0], [5, 3, 3]);
    let mut space = Space::empty(footprint);

    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    space.mutate(ctx.universe.read_ticket(), |m| {
        for (i, &resolution) in (0i32..).zip([R1, R2, R4, R8, R16, R32].iter()) {
            let example = Block::builder()
                .voxels_fn(resolution, |p| {
                    if p.x + p.y + p.z >= GridCoordinate::from(resolution) {
                        return AIR.clone();
                    }
                    let rescale = if resolution > R8 { 4 } else { 1 };
                    let color = Rgb::from(
                        p.lower_bounds()
                            .to_vector()
                            .map(|s| {
                                ps32(
                                    (s / GridCoordinate::from(rescale)) as f32
                                        / f32::from(u16::from(resolution) / rescale - 1).max(1.),
                                )
                            })
                            .cast_unit(),
                    );
                    Block::from(color)
                })?
                .build_txn(&mut txn);
            let label = text::Text::builder()
                .resolution(R32)
                .string(arcstr::format!("{resolution}"))
                .font(text::Font::SmallerBodyText)
                .foreground(demo_blocks[DemoBlocks::LabelTextVoxel].clone())
                .positioning(text::Positioning::LOW)
                .build()
                .single_block();
            stack(
                m,
                GridPoint::new(i.rem_euclid(3) * 2, 0, i.div_euclid(3) * 2),
                [pedestal, &example, &label],
            )?;
        }
        Ok::<(), InGenError>(())
    })?;

    Ok((space, txn))
}

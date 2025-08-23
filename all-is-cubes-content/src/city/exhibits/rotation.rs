use super::prelude::*;

#[exhibit(
    name: "Rotations",
    subtitle: "Rotated blocks and GridRotation::from_to()",
    placement: Placement::Surface,
)]
fn ROTATIONS(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let mut space = Space::empty(GridAab::from_lower_size([-2, 0, -2], [5, 5, 5]));

    let [_, central_block] = make_some_voxel_blocks_txn(&mut txn);
    let pointing_block = &demo_blocks[DemoBlocks::Arrow];

    let center = GridPoint::new(0, 0, 0);

    space.mutate(ctx.universe.read_ticket(), |m| {
        m.set(center, central_block)?;

        let mut place_rotated_arrow =
            |pos: GridPoint, rot: GridRotation| -> Result<(), InGenError> {
                stack(
                    m,
                    pos,
                    [
                        &pointing_block.clone().rotate(rot),
                        &text::Text::builder()
                            .string(arcstr::format!("{rot:?}"))
                            .font(text::Font::SmallerBodyText)
                            .foreground(demo_blocks[DemoBlocks::LabelTextVoxel].clone())
                            .resolution(R32)
                            .positioning(text::Positioning::LOW)
                            .build()
                            .single_block(),
                    ],
                )?;
                Ok(())
            };

        for face in [Face6::PX, Face6::PZ, Face6::NX, Face6::NZ] {
            place_rotated_arrow(
                center + face.vector(2),
                GridRotation::from_to(Face6::NZ, face.opposite(), Face6::PY).unwrap(),
            )?;
        }
        Ok::<(), InGenError>(())
    })?;

    Ok((space, txn))
}

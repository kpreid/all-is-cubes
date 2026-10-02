use super::prelude::*;

#[exhibit(
    name: "Ambient Sound",
    subtitle: "",
    placement: Placement::Underground,
)]
fn AMBIENT_SOUND(ctx: Context<'_>) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let mut txn = ExhibitTransaction::default();

    let section_size = GridSize::new(6, 5, 4);

    // --- Blocks ---

    // TODO: make blocks that look speaker-like and maybe are labeled with the frequency.
    let mut ambient_blocks = make_some_voxel_blocks_txn::<{ sound::Band::COUNT }>(&mut txn);
    for (i, block) in ambient_blocks.iter_mut().enumerate() {
        let frequency = sound::Band::from_index(i).center_frequency();
        block.modifiers_mut().push(
            block::SetAttribute::AmbientSound(Arc::new(sound::Ambient::noise_at_frequency(
                ps32(0.2 + 40.0 / frequency),
                frequency,
            )))
            .into(),
        );
    }

    // --- Space ---

    let section_count = sound::Band::COUNT as i32;
    let walking_space = 3;
    let interior = GridAab::from_lower_size(
        [0, -1, 0],
        [
            section_size.width + walking_space,
            section_size.height,
            section_size.depth * section_count.cast_unsigned(),
        ],
    );
    let mut space = Space::builder(interior).read_ticket(ctx.universe.read_ticket()).build();

    space.mutate(ctx.universe.read_ticket(), |m| -> Result<(), InGenError> {
        // walking floor
        m.fill_uniform(
            m.bounds().abut(Face::NY, -1).err_is_unreachable(),
            &demo_blocks[DemoBlocks::ExhibitBackground],
        )?;

        for section_index in 0..section_count {
            let section_lower_bound = GridPoint::new(
                0,
                -1,
                (section_count - 1 - section_index) * section_size.depth.cast_signed(),
            );
            let section_box = GridAab::from_lower_size(section_lower_bound, section_size);

            let box_style = BoxStyle::from_fn(|part| {
                if part == BoxPart::INTERIOR || part == BoxPart::face(Face::PX) {
                    None
                } else {
                    Some(ambient_blocks[section_index as usize].clone())
                }
            });

            box_style.create_box(section_box).execute_m(m)?;
        }
        Ok(())
    })?;

    Ok((space, txn))
}

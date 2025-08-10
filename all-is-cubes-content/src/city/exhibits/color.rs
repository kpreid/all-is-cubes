use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Colors",
    subtitle: "RGB cube of 5 linear color steps",
    placement: Placement::Surface,
)]
fn COLORS(ctx: Context<'_>) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;

    let gradient_resolution = 5;
    let space = Space::builder(GridAab::from_lower_size(
        [0, 0, 0],
        [
            gradient_resolution * 2 - 1,
            gradient_resolution * 2,
            gradient_resolution * 2 - 1,
        ],
    ))
    .read_ticket(ctx.universe.read_ticket())
    .build_and_mutate(|m| {
        m.fill_all(|p| {
            let color_point = p.lower_bounds() / 2;
            let part_of_grid: [GridCoordinate; 3] =
                p.lower_bounds().to_vector().map(|s| s.rem_euclid(2)).into();
            let color = Rgb::from(
                color_point
                    .to_vector()
                    .map(|s| {
                        PositiveSign::<f32>::new_strict(s as f32 / (gradient_resolution - 1) as f32)
                    })
                    .cast_unit(),
            );
            let color_srgb = color.with_alpha_one().to_srgb8();
            let description = arcstr::format!(
                "Linear\n  {:0.2}\n  {:0.2}\n  {:0.2}\nsRGB\n  #{:02x}{:02x}{:02x}",
                color.red(),
                color.green(),
                color.blue(),
                color_srgb[0],
                color_srgb[1],
                color_srgb[2]
            );
            match part_of_grid {
                [0, 0, 0] => Some(
                    Block::builder()
                        .display_name(description)
                        .color(color.with_alpha_one())
                        .build(),
                ),
                [0, 1, 0] => Some({
                    text::Text::builder()
                        .string(description)
                        .font(text::Font::SmallerBodyText)
                        .foreground(demo_blocks[DemoBlocks::LabelTextVoxel].clone())
                        .resolution(R64)
                        .positioning(text::Positioning {
                            line_y: text::PositioningY::BodyTop,
                            ..text::Positioning::LOW
                        })
                        .build()
                        .single_block()
                        .rotate(GridRotation::RXzY)
                }),
                _ => None,
            }
        })
    })?;

    Ok((space, ExhibitTransaction::default()))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Colored Lights",
    subtitle: "RGBCMY lights in an enclosed room",
    placement: Placement::Surface,
)]
fn COLOR_LIGHTS(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();

    let room_width = 11;
    let room_length = 16;
    let room_height = 7;
    let separator_width = 4; // less than room_width/2
    let brightness = 1.0;

    let interior = GridAab::from_lower_size(
        [0, 0, 0],
        Size3D::new(room_width, room_height, room_length).to_u32(),
    );
    let mut space = Space::empty(interior.expand(FaceMap::splat(1)));

    fn normalize(color: Rgb) -> Rgb {
        color * color.luminance().recip()
    }

    let light_colors = [
        rgb_const!(1.0, 0.0, 0.0),
        rgb_const!(1.0, 1.0, 0.0),
        rgb_const!(0.0, 1.0, 0.0),
        rgb_const!(0.0, 1.0, 1.0),
        rgb_const!(0.0, 0.0, 1.0),
        rgb_const!(1.0, 0.0, 1.0),
    ];
    let surface_colors = [
        rgb_const!(1.0, 0.0, 0.0),
        rgb_const!(1.0, 1.0, 0.0),
        rgb_const!(0.0, 1.0, 0.0),
        rgb_const!(0.0, 1.0, 1.0),
        rgb_const!(0.0, 0.0, 1.0),
        rgb_const!(1.0, 0.0, 1.0),
        rgb_const!(0.25, 0.25, 0.25),
        rgb_const!(0.75, 0.75, 0.75),
        rgb_const!(1.0, 1.0, 1.0),
    ];

    // Room wall block with test card
    let wall_color_block = block::from_color!(0.5, 0.5, 0.5, 1.0);
    let wall_resolution = R16;
    let wall_resolution_g = GridCoordinate::from(wall_resolution);
    let wall_block = {
        let colors_as_blocks: Vec<Block> =
            surface_colors.iter().copied().map(Block::from).collect();
        let wall_block_space = Space::for_block(wall_resolution)
            .read_ticket(ctx.universe.read_ticket())
            .filled_with(wall_color_block.clone())
            .build_and_mutate(|m| {
                for rotation in [
                    GridRotation::IDENTITY,
                    GridRotation::CLOCKWISE,
                    GridRotation::CLOCKWISE * GridRotation::CLOCKWISE,
                    GridRotation::COUNTERCLOCKWISE,
                ] {
                    let transform = rotation.to_positive_octant_transform(wall_resolution_g)
                        * Gridgid::from_translation([4, 4, wall_resolution_g - 1]);

                    for (i, swatch_block) in (0i32..).zip(colors_as_blocks.iter()) {
                        m.fill_uniform(
                            GridAab::from_lower_size(
                                [i.rem_euclid(3) * 3, i.div_euclid(3) * 3, 0],
                                [2, 2, 1],
                            )
                            .transform(transform)
                            .unwrap(),
                            swatch_block,
                        )?;
                    }
                }
                Ok(())
            })?;

        Block::builder()
            .display_name("Color room wall")
            .voxels_handle(wall_resolution, txn.insert_anonymous(wall_block_space))
            .build()
    };

    // Wall corner
    let corner = Block::builder()
        .display_name("Color room wall corner")
        .rotation_rule(RotationPlacementRule::Attach { by: Face6::NZ }) // TODO: more specific
        .voxels_fn(wall_resolution, |p| {
            if p.x.pow(2) + p.z.pow(2) < GridCoordinate::from(wall_resolution).pow(2) {
                &wall_color_block
            } else {
                &AIR
            }
        })?
        .build_txn(&mut txn);

    // Construct room.
    BoxStyle::from_whole_blocks_for_walls(
        Some(wall_block.clone()),
        Some(wall_block.clone()),
        Some(wall_block.clone()),
        Some(corner.rotate(GridRotation::RxYz)),
    )
    .create_box(interior.expand(FaceMap::splat(1)))
    .execute(
        &mut space,
        ctx.universe.read_ticket(),
        &mut transaction::no_outputs,
    )?;

    space.mutate(ctx.universe.read_ticket(), |m| {
        // Separators between floors
        let floor_sep_size = Size3D::new(separator_width, 1, room_length).to_u32();
        m.fill_uniform(
            GridAab::from_lower_size([0, room_height / 2, 0], floor_sep_size),
            &wall_block,
        )?;
        m.fill_uniform(
            GridAab::from_lower_size(
                [room_width - separator_width, room_height / 2, 0],
                floor_sep_size,
            ),
            &wall_block,
        )?;

        // Entrance door
        m.fill_uniform(
            GridAab::from_lower_size([room_width / 2 - 1, 0, room_length], [3, 2, 1]),
            &AIR,
        )?;

        // Place lights and horizontal separators
        for (i, color) in light_colors.iter().copied().enumerate() {
            let z = (i as GridCoordinate) * (room_length - 1)
                / (light_colors.len() as GridCoordinate - 1);
            let on_low_side = i.is_multiple_of(2);
            let p = GridPoint::new(if on_low_side { 1 } else { room_width - 2 }, 0, z);
            m.set(
                p,
                Block::builder()
                    .display_name("Colored light with colored surface")
                    .color(color.with_alpha_one())
                    .light_emission(normalize(color) * brightness)
                    .build(),
            )?;
            m.set(
                p + GridVector::new(0, room_height - 1, 0),
                Block::builder()
                    .display_name("Colored light with white surface")
                    .color(Rgba::WHITE)
                    .light_emission(normalize(color) * brightness)
                    .build(),
            )?;

            // Separator between different light areas
            let wall_size = Size3D::new(separator_width, room_height, 1).to_u32();
            if on_low_side {
                m.fill_uniform(
                    GridAab::from_lower_size([room_width - separator_width, 0, z], wall_size),
                    &wall_block,
                )?;
            } else {
                m.fill_uniform(GridAab::from_lower_size([0, 0, z], wall_size), &wall_block)?;
            }
        }
        Ok::<(), InGenError>(())
    })?;

    // TODO: Add an RGBCMY section, and also a color-temperature section (or maybe different buildings)
    // sRGB white is D65, or approximately 6500 K.

    Ok((space, txn))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Colored Reflections",
    subtitle: "Light colored by surface reflections",
    placement: Placement::Underground,
)]
fn COLORED_BOUNCE(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();

    let interior_radius = 3i32;
    let wall_thickness = 3u32;
    let total_radius = interior_radius.saturating_add_unsigned(wall_thickness);
    let brightness = 50.0;

    // --- Blocks ---

    let reflecting_block = {
        let rbbs = BoxStyle::from_whole_blocks_for_walls(
            Some(rgba_const!(1.0, 0.0, 0.0, 1.0).into()),
            Some(rgba_const!(0.0, 1.0, 0.0, 1.0).into()),
            Some(rgba_const!(0.0, 0.0, 1.0, 1.0).into()),
            Some(palette::ALMOST_BLACK.into()),
        );
        let rbbounds = GridAab::for_block(R32);
        Block::builder()
            .voxels_fn(R32, |cube| rbbs.cube_at(rbbounds, cube).unwrap_or(&AIR))?
            .build_txn(&mut txn)
    };

    let light_block = Block::builder()
        .color(Rgba::WHITE)
        .light_emission(Rgb::ONE * brightness)
        .build();

    let wall_block = block::from_color!(0.25, 0.25, 0.25); // fairly absorbing

    // --- Space ---

    let interior = GridAab::from_lower_size(
        GridPoint::splat(-interior_radius),
        GridSize::splat(u32::try_from(interior_radius).unwrap() * 2 + 1),
    );
    let space = Space::builder(interior.expand(FaceMap::splat(wall_thickness)))
        .read_ticket(ctx.universe.read_ticket())
        .build_and_mutate(|m| {
            // Thick walls + interior cavity
            m.fill_all_uniform(&wall_block).unwrap();
            m.fill_uniform(interior, &AIR).unwrap();

            // Dig pockets for lights to be in
            for dir in Face6::ALL {
                let far_end =
                    GridAab::ORIGIN_CUBE.translate(dir.normal_vector() * (total_radius - 1));
                m.fill_uniform(GridAab::ORIGIN_CUBE.union_box(far_end), &AIR)
                    .unwrap();
                m.fill_uniform(far_end, &light_block).unwrap();
            }

            // Central reflecting block
            m.fill_uniform(
                GridAab::ORIGIN_CUBE.expand(FaceMap::splat(1)),
                &reflecting_block,
            )?;

            // Hole to look in through
            m.fill_uniform(
                GridAab::from_lower_upper([2, 0, interior_radius + 1], [3, 2, total_radius + 1]),
                &AIR,
            )
            .unwrap();
            Ok(())
        })?;

    Ok((space, txn))
}

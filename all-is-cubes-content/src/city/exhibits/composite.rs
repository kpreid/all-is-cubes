use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Modifier::Composite",
    subtitle: "",
    placement: Placement::Surface,
)]
fn COMPOSITE(ctx: Context<'_>) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;

    let sources = [
        &demo_blocks[DemoBlocks::Lamp(true)],
        &demo_blocks[DemoBlocks::Arrow],
        &demo_blocks[DemoBlocks::Signboard],
    ];
    let destinations = [
        &demo_blocks[DemoBlocks::ExhibitBackground],
        &demo_blocks[DemoBlocks::GlassBlock],
        &demo_blocks[DemoBlocks::LamppostBase],
    ];
    let operators = [
        CompositeOperator::Over,
        CompositeOperator::In,
        CompositeOperator::Out,
        CompositeOperator::Atop,
    ];

    let mut space = Space::empty(GridAab::from_lower_upper(
        [0, 0, 0],
        [
            destinations.len() as GridCoordinate * 2,
            operators.len() as GridCoordinate * 3,
            sources.len() as GridCoordinate * 2,
        ],
    ));
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    for (di, destination) in destinations.into_iter().enumerate() {
        for (si, source) in sources.into_iter().enumerate() {
            for (oi, operator) in operators.into_iter().enumerate() {
                let composite = destination.clone().with_modifier(Composite::new(
                    source.clone().rotate(GridRotation::CLOCKWISE),
                    operator,
                ));

                let label_str = arcstr::format!(
                    "{s}\n{operator:?}\n{d}",
                    s = source.evaluate().unwrap().attributes().display_name,
                    d = destination.evaluate().unwrap().attributes().display_name
                );
                let label = text::Text::builder()
                    .string(label_str)
                    .resolution(R64)
                    .font(text::Font::SmallerBodyText)
                    .foreground(demo_blocks[DemoBlocks::LabelTextVoxel].clone())
                    .positioning(text::Positioning {
                        // TODO: this should be "last line at the bottom" but that isn't implemented
                        line_y: text::PositioningY::BodyTop,
                        ..text::Positioning::LOW
                    })
                    .build()
                    .single_block();

                stack(
                    &mut space,
                    GridPoint::new(
                        di as GridCoordinate * 2,
                        oi as GridCoordinate * 3,
                        si as GridCoordinate * 2,
                    ),
                    [if oi == 0 { pedestal } else { &AIR }, &composite, &label],
                )?;
            }
        }
    }
    Ok((space, ExhibitTransaction::default()))
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Dashed outline boxes",
    subtitle: "",
    placement: Placement::Surface,
)]
fn DASHED_BOXES(_: Context<'_>) {
    let mut txn = ExhibitTransaction::default();

    let color = Rgb::new(1.0, 0.5, 0.5);
    let brush = Block::from(color);
    let corner_brush = Block::from(color * 0.6);
    let line_segment = Block::builder()
        .display_name("Dashed Box Segment")
        .voxels_fn(R16, |p| {
            let zmod = p.z.rem_euclid(4);
            if p.x == 0 && p.y == 0 && zmod > 0 && zmod < 3 {
                &brush
            } else {
                &AIR
            }
        })?
        .build_txn(&mut txn);
    let corner = Block::builder()
        .display_name("Dashed Box Corner")
        .voxels_fn(R16, |p| {
            if p.x < 2 && p.z < 2 && p.y < 2 {
                &corner_brush
            } else {
                &AIR
            }
        })?
        .build_txn(&mut txn);
    let style = crate::BoxStyle::from_composited_corner_and_edge(corner, line_segment);

    let mut space = Space::empty_positive(7, 3, 3);
    // Unit sized box
    style
        .create_box(GridAab::from_lower_size([0, 0, 1], [1, 1, 1]))
        .execute(&mut space, &mut transaction::no_outputs)?;
    // Tall box
    style
        .create_box(GridAab::from_lower_size([2, 0, 1], [1, 3, 1]))
        .execute(&mut space, &mut transaction::no_outputs)?;
    // Large box
    style
        .create_box(GridAab::from_lower_size([4, 0, 0], [3, 3, 3]))
        .execute(&mut space, &mut transaction::no_outputs)?;

    Ok((space, txn))
}
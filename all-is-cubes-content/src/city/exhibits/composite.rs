use super::prelude::*;

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
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    let space = Space::builder(GridAab::from_lower_upper(
        [0, 0, 0],
        [
            destinations.len() as GridCoordinate * 2,
            operators.len() as GridCoordinate * 3,
            sources.len() as GridCoordinate * 2,
        ],
    ))
    .read_ticket(ctx.universe.read_ticket())
    .build_and_mutate(|m| {
        for (di, destination) in (0i32..).zip(destinations) {
            for (si, source) in (0i32..).zip(sources) {
                for (oi, operator) in (0i32..).zip(operators) {
                    let composite = destination.clone().with_modifier(Composite::new(
                        source.clone().rotate(Face6::PY.clockwise()),
                        operator,
                    ));

                    let label_str = arcstr::format!(
                        "{s}\n{operator:?}\n{d}",
                        s = source
                            .evaluate(ctx.universe.read_ticket())
                            .unwrap()
                            .attributes()
                            .display_name,
                        d = destination
                            .evaluate(ctx.universe.read_ticket())
                            .unwrap()
                            .attributes()
                            .display_name
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
                        m,
                        GridPoint::new(di * 2, oi * 3, si * 2),
                        [if oi == 0 { pedestal } else { &AIR }, &composite, &label],
                    )?;
                }
            }
        }
        Ok(())
    })?;
    Ok((space, ExhibitTransaction::default()))
}

#[exhibit(
    name: "Dashed outline boxes",
    subtitle: "",
    placement: Placement::Surface,
)]
fn DASHED_BOXES(_: Context<'_>) {
    let mut txn = ExhibitTransaction::default();

    let color = Rgb01::new(1.0, 0.5, 0.5);
    let brush = Block::from(color);
    let corner_brush = Block::from(color * zo32(0.6));
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
    let style = BoxStyle::from_composited_corner_and_edge(corner, line_segment);

    let mut space = Space::empty_positive(7, 3, 3);
    for one_box in const {
        [
            // Unit sized box
            GridAab::tiny(point3(0, 0, 1), size3(1, 1, 1)),
            // Tall box
            GridAab::tiny(point3(2, 0, 1), size3(1, 3, 1)),
            // Large box
            GridAab::tiny(point3(4, 0, 0), size3(3, 3, 3)),
        ]
    } {
        style.create_box(one_box).execute(
            &mut space,
            ReadTicket::stub(),
            &mut transaction::no_outputs,
        )?;
    }

    Ok((space, txn))
}

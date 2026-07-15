use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Primitive::Text",
    subtitle: "",
    placement: Placement::SurfaceWithBackWall,
)]
fn TEXT(ctx: Context<'_>) {
    let foreground_block = block::from_color!(palette::HUD_TEXT_FILL);
    let outline_block = block::from_color!(palette::HUD_TEXT_STROKE);

    struct Texhibit {
        text: block::Text,
        f: Box<dyn Fn(Block) -> Block>,
        offset: GridVector,
    }

    let debug = true;

    let texts = [
        Texhibit {
            text: block::Text::builder()
                .debug(debug)
                .string(literal!("right back"))
                .positioning(text::Positioning {
                    x: text::PositioningX::Right,
                    line_y: text::PositioningY::BodyBottom,
                    z: text::PositioningZ::Back,
                })
                .build(),
            f: Box::new(identity),
            offset: vec3(0, 0, 0),
        },
        Texhibit {
            text: block::Text::builder()
                .debug(debug)
                .string(literal!("left front"))
                .positioning(text::Positioning {
                    x: text::PositioningX::Left,
                    line_y: text::PositioningY::BodyBottom,
                    z: text::PositioningZ::Front,
                })
                .build(),
            f: Box::new(identity),
            offset: vec3(0, 1, 0),
        },
        {
            let material_block =
                Composite::new(block::from_color!(palette::STEEL), CompositeOperator::Over)
                    .reversed()
                    .compose_or_replace(
                        block::from_color!(palette::PLANK).with_modifier(Move::new(
                            Face::NZ,
                            R32,
                            1,
                            0,
                        )),
                    );
            let op = Composite::new(material_block, CompositeOperator::Out);
            Texhibit {
                text: block::Text::builder()
                    .string(literal!("engraved"))
                    .resolution(R32)
                    .positioning(text::Positioning {
                        x: text::PositioningX::Center,
                        line_y: text::PositioningY::BodyMiddle,
                        z: text::PositioningZ::Front,
                    })
                    .build(),
                f: Box::new(move |text_block| op.clone().compose_or_replace(text_block)),
                offset: vec3(0, 2, 0),
            }
        },
        Texhibit {
            text: block::Text::builder()
                .debug(debug)
                .string(literal!("left back outline"))
                .foreground(foreground_block)
                .outline(Some(outline_block))
                .positioning(text::Positioning {
                    x: text::PositioningX::Left,
                    line_y: text::PositioningY::BodyBottom,
                    z: text::PositioningZ::Back,
                })
                .build(),
            f: Box::new(identity),
            offset: vec3(0, 3, 0),
        },
        Texhibit {
            text: block::Text::builder()
                .debug(debug)
                .string(literal!("weird vert bounds"))
                .layout_bounds(R16, GridAab::from_lower_upper([0, 16, 0], [64, 64, 64]))
                // .foreground(foreground_block)
                // .outline(Some(outline_block))
                .positioning(text::Positioning {
                    x: text::PositioningX::Left,
                    line_y: text::PositioningY::BodyMiddle,
                    z: text::PositioningZ::Back,
                })
                .build(),
            f: Box::new(identity),
            offset: vec3(0, 4, 0),
        },
    ];

    let bounds_for_text = texts
        .iter()
        .map(|ex| {
            ex.text
                .measure(ReadTicket::stub())
                .unwrap()
                .rendering_bounding_blocks()
                .translate(ex.offset)
        })
        .reduce(|a, b| a.union_box(b))
        .unwrap();

    let mut space = Space::builder(bounds_for_text).build();

    // TODO: detect collisions
    for Texhibit { text, f, offset } in texts {
        text.installation(
            ctx.universe.read_ticket(),
            Gridgid::from_translation(offset),
            f,
        )
        .unwrap()
        .execute(&mut space, ReadTicket::stub(), &mut transaction::no_outputs)
        .unwrap();
    }

    Ok((space, ExhibitTransaction::default()))
}

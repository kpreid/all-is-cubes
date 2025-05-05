use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Primitive::Text",
    subtitle: "",
    placement: Placement::Surface,
)]
fn TEXT(_: Context<'_>) {
    use all_is_cubes::block::text;

    let foreground_block = block::from_color!(palette::HUD_TEXT_FILL);
    let outline_block = block::from_color!(palette::HUD_TEXT_STROKE);

    struct Texhibit {
        text: text::Text,
        f: Box<dyn Fn(Block) -> Block>,
        offset: GridVector,
    }

    let texts = [
        Texhibit {
            text: text::Text::builder()
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
            text: text::Text::builder()
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
            let op = Composite::new(
                block::from_color!(palette::MENU_BACK),
                CompositeOperator::Out,
            );
            Texhibit {
                text: text::Text::builder()
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
            text: text::Text::builder()
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
            text: text::Text::builder()
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
        .map(|ex| ex.text.bounding_blocks().translate(ex.offset))
        .reduce(|a, b| a.union_box(b))
        .unwrap();

    let mut space = Space::builder(bounds_for_text).build();

    // TODO: detect collisions
    for Texhibit { text, f, offset } in texts {
        text.installation(Gridgid::from_translation(offset), f)
            .execute(&mut space, ReadTicket::stub(), &mut transaction::no_outputs)
            .unwrap();
    }

    Ok((space, ExhibitTransaction::default()))
}

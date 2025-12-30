//! Conversion between the types in [`super::schema`] and those used in
//! normal operation.

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use super::schema;

mod behavior {
    use super::*;
    use crate::behavior::{Behavior, BehaviorSet, BehaviorSetTransaction, Host, Persistence};
    use crate::transaction::{Merge as _, Transactional as _};

    // TODO: Stop serializing H::Attachment directly or document that it has to be stable.

    impl<H> Serialize for BehaviorSet<H>
    where
        H: Host<Attachment: Serialize>,
    {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            schema::BehaviorSetSer::BehaviorSetV1 {
                behaviors: self
                    .iter()
                    .filter_map(|entry| {
                        let Persistence(behavior) = entry.behavior.persistence()?;
                        Some(schema::BehaviorSetEntryV1Ser {
                            attachment: entry.attachment.clone(),
                            behavior,
                        })
                    })
                    .collect::<Vec<schema::BehaviorSetEntryV1Ser<H::Attachment>>>(),
            }
            .serialize(serializer)
        }
    }

    impl<'de, H> Deserialize<'de> for BehaviorSet<H>
    where
        H: Host<Attachment: serde::de::DeserializeOwned>,
    {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            match schema::BehaviorSetSer::<H::Attachment>::deserialize(deserializer)? {
                schema::BehaviorSetSer::BehaviorSetV1 { behaviors } => {
                    let mut set = BehaviorSet::new();
                    set.transact(|txn, _| {
                        #[allow(
                            unreachable_patterns,
                            reason = "remove this when BehaviorV1Ser is nonempty"
                        )]
                        for schema::BehaviorSetEntryV1Ser {
                            behavior,
                            attachment,
                        } in behaviors
                        {
                            txn.merge_from(BehaviorSetTransaction::insert(
                                attachment,
                                behavior.into(),
                            ))?;
                        }
                        Ok(())
                    })
                    .expect("BehaviorSet execute failure");
                    Ok(set)
                }
            }
        }
    }

    impl<A> From<schema::BehaviorV1Ser> for Arc<dyn Behavior<A>> {
        fn from(value: schema::BehaviorV1Ser) -> Self {
            match value {}
        }
    }
}

mod block {
    use super::*;
    use crate::block::{
        AnimationChange, AnimationHint, Atom, Block, BlockCollision, Composite, Modifier, Move,
        PlacementAction, Primitive, Quote, RotationPlacementRule, TickAction, Zoom, text,
    };
    use crate::math::{Rgb, Rgba};
    use crate::tag;
    use schema::{BlockSer, ModifierSer};

    impl Serialize for Block {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            BlockSer::BlockV1 {
                primitive: match self.primitive() {
                    Primitive::Indirect(definition) => schema::PrimitiveSer::IndirectV1 {
                        definition: definition.clone(),
                    },
                    &Primitive::Atom(Atom {
                        color,
                        emission,
                        collision,
                    }) => schema::PrimitiveSer::AtomV1 {
                        color: color.into(),
                        light_emission: emission.into(),
                        collision: collision.into(),
                    },
                    &Primitive::Recur {
                        ref space,
                        offset,
                        resolution,
                    } => schema::PrimitiveSer::RecurV1 {
                        space: space.clone(),
                        offset: offset.into(),
                        resolution,
                    },
                    &Primitive::Air => schema::PrimitiveSer::AirV1,
                    &Primitive::Text { ref text, offset } => {
                        schema::PrimitiveSer::TextPrimitiveV1 {
                            text: text.into(),
                            offset: offset.into(),
                        }
                    }
                    &Primitive::Raw { .. } => {
                        return Err(serde::ser::Error::custom("cannot serialize Primitive::Raw"));
                    }
                },
                modifiers: self.modifiers().iter().map(ModifierSer::from).collect(),
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Block {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            Ok(match BlockSer::deserialize(deserializer)? {
                BlockSer::BlockV1 {
                    primitive,
                    modifiers,
                } => {
                    let mut block = Block::from_primitive(primitive_from_schema(primitive));
                    block.modifiers_mut().extend(modifiers.into_iter().map(Modifier::from));
                    block
                }
            })
        }
    }

    fn primitive_from_schema(value: schema::PrimitiveSer) -> Primitive {
        match value {
            schema::PrimitiveSer::IndirectV1 { definition } => Primitive::Indirect(definition),
            schema::PrimitiveSer::AtomV1 {
                color,
                light_emission: emission,
                collision,
            } => Primitive::Atom(Atom {
                color: Rgba::from(color),
                emission: Rgb::from(emission),
                collision: collision.into(),
            }),
            schema::PrimitiveSer::RecurV1 {
                space,
                offset,
                resolution,
            } => Primitive::Recur {
                space,
                offset: offset.into(),
                resolution,
            },
            schema::PrimitiveSer::AirV1 => Primitive::Air,
            schema::PrimitiveSer::TextPrimitiveV1 { text, offset } => Primitive::Text {
                text: text.into(),
                offset: offset.into(),
            },
        }
    }

    impl From<BlockCollision> for schema::BlockCollisionSer {
        fn from(value: BlockCollision) -> Self {
            use schema::BlockCollisionSer as S;
            match value {
                BlockCollision::None => S::NoneV1,
                BlockCollision::Hard => S::HardV1,
            }
        }
    }

    impl From<schema::BlockCollisionSer> for BlockCollision {
        fn from(value: schema::BlockCollisionSer) -> Self {
            use schema::BlockCollisionSer as S;
            match value {
                S::NoneV1 => BlockCollision::None,
                S::HardV1 => BlockCollision::Hard,
            }
        }
    }

    impl From<RotationPlacementRule> for schema::RotationPlacementRuleSer {
        fn from(value: RotationPlacementRule) -> Self {
            use schema::RotationPlacementRuleSer as S;
            match value {
                RotationPlacementRule::Never => S::NeverV1,
                RotationPlacementRule::Attach { by } => S::AttachV1 { by },
            }
        }
    }

    impl From<schema::RotationPlacementRuleSer> for RotationPlacementRule {
        fn from(value: schema::RotationPlacementRuleSer) -> Self {
            use schema::RotationPlacementRuleSer as S;
            match value {
                S::NeverV1 => RotationPlacementRule::Never,
                S::AttachV1 { by } => RotationPlacementRule::Attach { by },
            }
        }
    }

    impl From<AnimationHint> for schema::AnimationHintSerV1 {
        fn from(value: AnimationHint) -> Self {
            let AnimationHint {
                redefinition,
                replacement,
            } = value;
            schema::AnimationHintSerV1 {
                redefinition: redefinition.into(),
                replacement: replacement.into(),
            }
        }
    }

    impl From<schema::AnimationHintSerV1> for AnimationHint {
        fn from(value: schema::AnimationHintSerV1) -> Self {
            let schema::AnimationHintSerV1 {
                redefinition,
                replacement,
            } = value;
            AnimationHint {
                redefinition: redefinition.into(),
                replacement: replacement.into(),
            }
        }
    }

    impl From<AnimationChange> for schema::AnimationChangeV1Ser {
        fn from(value: AnimationChange) -> Self {
            use schema::AnimationChangeV1Ser as S;
            match value {
                AnimationChange::None => S::None,
                AnimationChange::ColorSameCategory => S::ColorSameCategory,
                AnimationChange::Shape => S::Shape,
            }
        }
    }

    impl From<schema::AnimationChangeV1Ser> for AnimationChange {
        fn from(value: schema::AnimationChangeV1Ser) -> Self {
            use schema::AnimationChangeV1Ser as S;
            match value {
                S::None => AnimationChange::None,
                S::ColorSameCategory => AnimationChange::ColorSameCategory,
                S::Shape => AnimationChange::Shape,
            }
        }
    }

    impl<'a> From<&'a Modifier> for ModifierSer<'a> {
        fn from(value: &'a Modifier) -> Self {
            use crate::block::SetAttribute as A;

            match *value {
                Modifier::SetAttribute(ref attribute) => match *attribute {
                    A::DisplayName(ref attr_value) => ModifierSer::DisplayNameV1 {
                        display_name: attr_value.clone(),
                    },
                    A::Selectable(selectable) => ModifierSer::SelectableV1 { selectable },
                    A::Inventory(ref attr_value) => {
                        ModifierSer::InvInBlockV1((&**attr_value).into())
                    }
                    A::AmbientSound(ref attr_value) => {
                        ModifierSer::AmbientSoundV1((&**attr_value).into())
                    }
                    A::RotationRule(attr_value) => ModifierSer::RotationRuleV1 {
                        rotation_rule: attr_value.into(),
                    },
                    A::PlacementAction(ref attr_value) => ModifierSer::PlacementActionV1 {
                        placement_action: attr_value.as_deref().cloned().map(
                            |PlacementAction {
                                 operation,
                                 in_front,
                             }| schema::PlacementActionSer {
                                operation,
                                in_front,
                            },
                        ),
                    },
                    A::TickAction(ref attr_value) => ModifierSer::TickActionV1 {
                        tick_action: attr_value.as_deref().cloned().map(|ta| {
                            schema::TickActionSerV1 {
                                operation: ta.operation,
                                schedule: ta.schedule,
                            }
                        }),
                    },
                    A::ActivationAction(ref attr_value) => ModifierSer::ActivationActionV1 {
                        activation_action: attr_value.as_deref().cloned(),
                    },
                    A::AnimationHint(attr_value) => ModifierSer::AnimationHintV1(attr_value.into()),
                },
                Modifier::Tag(tag::Be(ref tag)) => ModifierSer::TagV1 { tag: tag.clone() },
                Modifier::Quote(Quote { suppress_ambient }) => {
                    ModifierSer::QuoteV1 { suppress_ambient }
                }
                Modifier::Rotate(rotation) => ModifierSer::RotateV1 { rotation },
                Modifier::Composite(Composite {
                    ref source,
                    operator,
                    reverse,
                    disassemblable,
                }) => ModifierSer::CompositeV1 {
                    source: source.clone(),
                    operator,
                    reverse,
                    disassemblable,
                },
                Modifier::Zoom(ref m) => m.to_serial_schema(),
                Modifier::Move(ref m) => ModifierSer::Move(m.into()),
                Modifier::Inventory(ref inventory) => ModifierSer::BlockInventoryV1 {
                    inventory: Cow::Borrowed(inventory),
                },
            }
        }
    }

    impl From<ModifierSer<'_>> for Modifier {
        fn from(value: ModifierSer<'_>) -> Self {
            use crate::block::SetAttribute as A;
            let ms = Modifier::SetAttribute;

            match value {
                ModifierSer::TagV1 { tag } => Modifier::Tag(tag::Be(tag)),
                ModifierSer::QuoteV1 { suppress_ambient } => {
                    Modifier::Quote(Quote { suppress_ambient })
                }
                ModifierSer::RotateV1 { rotation } => Modifier::Rotate(rotation),
                ModifierSer::CompositeV1 {
                    source,
                    operator,
                    reverse,
                    disassemblable,
                } => Modifier::Composite(Composite {
                    source,
                    operator,
                    reverse,
                    disassemblable,
                }),
                ModifierSer::ZoomV1 { scale, offset } => {
                    Modifier::Zoom(Zoom::new(scale, offset.map(i32::from).into()))
                }
                ModifierSer::Move(m) => Modifier::Move(m.into()),
                ModifierSer::BlockInventoryV1 { inventory } => {
                    Modifier::Inventory(inventory.into_owned())
                }

                // Attributes
                ModifierSer::DisplayNameV1 { display_name } => ms(A::DisplayName(display_name)),
                ModifierSer::SelectableV1 { selectable } => ms(A::Selectable(selectable)),
                ModifierSer::InvInBlockV1(attr_value) => {
                    ms(A::Inventory(Arc::new(attr_value.into())))
                }
                ModifierSer::AmbientSoundV1(attr_value) => {
                    ms(A::AmbientSound(Arc::new(attr_value.into())))
                }
                ModifierSer::RotationRuleV1 { rotation_rule } => {
                    ms(A::RotationRule(rotation_rule.into()))
                }
                ModifierSer::PlacementActionV1 { placement_action } => {
                    ms(A::PlacementAction(placement_action.map(
                        |schema::PlacementActionSer {
                             operation,
                             in_front,
                         }| {
                            Arc::new(PlacementAction {
                                operation,
                                in_front,
                            })
                        },
                    )))
                }
                ModifierSer::TickActionV1 { tick_action } => ms(A::TickAction(tick_action.map(
                    |schema::TickActionSerV1 {
                         operation,
                         schedule,
                     }| {
                        Arc::new(TickAction {
                            operation,
                            schedule,
                        })
                    },
                ))),
                ModifierSer::ActivationActionV1 { activation_action } => {
                    ms(A::ActivationAction(activation_action.map(Arc::new)))
                }
                ModifierSer::AnimationHintV1(attr_value) => ms(A::AnimationHint(attr_value.into())),
            }
        }
    }

    impl From<&Move> for schema::MoveSerV1 {
        fn from(value: &Move) -> Self {
            let &Move {
                direction,
                distance,
                velocity,
                schedule,
            } = value;

            schema::MoveSerV1 {
                direction,
                distance,
                velocity,
                schedule,
            }
        }
    }

    impl From<schema::MoveSerV1> for Move {
        fn from(value: schema::MoveSerV1) -> Self {
            let schema::MoveSerV1 {
                direction,
                distance,
                velocity,
                schedule,
            } = value;

            Move {
                direction,
                distance,
                velocity,
                schedule,
            }
        }
    }

    // Since `Text` has private fields, its `From` impls are in its module.

    impl From<&text::Font> for schema::FontSer {
        fn from(value: &text::Font) -> Self {
            match value {
                text::Font::System16 => schema::FontSer::System16V1,
                text::Font::Logo => schema::FontSer::LogoV1,
                text::Font::SmallerBodyText => schema::FontSer::UnstableSmallerBodyTextV1,
            }
        }
    }

    impl From<schema::FontSer> for text::Font {
        fn from(value: schema::FontSer) -> Self {
            match value {
                schema::FontSer::System16V1 => text::Font::System16,
                schema::FontSer::LogoV1 => text::Font::Logo,
                schema::FontSer::UnstableSmallerBodyTextV1 => text::Font::SmallerBodyText,
            }
        }
    }

    impl From<text::Positioning> for schema::PositioningSerV1 {
        fn from(value: text::Positioning) -> Self {
            let text::Positioning { x, line_y, z } = value;
            schema::PositioningSerV1 {
                x: x.into(),
                line_y: line_y.into(),
                z: z.into(),
            }
        }
    }

    impl From<schema::PositioningSerV1> for text::Positioning {
        fn from(value: schema::PositioningSerV1) -> Self {
            let schema::PositioningSerV1 { x, line_y, z } = value;
            text::Positioning {
                x: x.into(),
                line_y: line_y.into(),
                z: z.into(),
            }
        }
    }

    impl From<text::PositioningX> for schema::PositioningXSer {
        fn from(value: text::PositioningX) -> Self {
            match value {
                text::PositioningX::Left => schema::PositioningXSer::LeftV1,
                text::PositioningX::Center => schema::PositioningXSer::CenterV1,
                text::PositioningX::Right => schema::PositioningXSer::RightV1,
            }
        }
    }

    impl From<schema::PositioningXSer> for text::PositioningX {
        fn from(value: schema::PositioningXSer) -> Self {
            match value {
                schema::PositioningXSer::LeftV1 => text::PositioningX::Left,
                schema::PositioningXSer::CenterV1 => text::PositioningX::Center,
                schema::PositioningXSer::RightV1 => text::PositioningX::Right,
            }
        }
    }

    impl From<text::PositioningY> for schema::PositioningYSer {
        fn from(value: text::PositioningY) -> Self {
            match value {
                text::PositioningY::BodyTop => schema::PositioningYSer::BodyTopV1,
                text::PositioningY::BodyMiddle => schema::PositioningYSer::BodyMiddleV1,
                text::PositioningY::Baseline => schema::PositioningYSer::BaselineV1,
                text::PositioningY::BodyBottom => schema::PositioningYSer::BodyBottomV1,
            }
        }
    }

    impl From<schema::PositioningYSer> for text::PositioningY {
        fn from(value: schema::PositioningYSer) -> Self {
            match value {
                schema::PositioningYSer::BodyTopV1 => text::PositioningY::BodyTop,
                schema::PositioningYSer::BodyMiddleV1 => text::PositioningY::BodyMiddle,
                schema::PositioningYSer::BaselineV1 => text::PositioningY::Baseline,
                schema::PositioningYSer::BodyBottomV1 => text::PositioningY::BodyBottom,
            }
        }
    }

    impl From<text::PositioningZ> for schema::PositioningZSer {
        fn from(value: text::PositioningZ) -> Self {
            match value {
                text::PositioningZ::Front => schema::PositioningZSer::FrontV1,
                text::PositioningZ::Back => schema::PositioningZSer::BackV1,
            }
        }
    }

    impl From<schema::PositioningZSer> for text::PositioningZ {
        fn from(value: schema::PositioningZSer) -> Self {
            match value {
                schema::PositioningZSer::FrontV1 => text::PositioningZ::Front,
                schema::PositioningZSer::BackV1 => text::PositioningZ::Back,
            }
        }
    }
}

// `character::Character` and `character::Spawn` serialization are inside their module
// for the sake of private fields.

mod fluff {
    use super::*;
    use crate::fluff::Fluff;

    impl Serialize for Fluff {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match self {
                // Currently, all of the available values in `Fluff` are placeholders.
                Fluff::Gone
                | Fluff::Beep
                | Fluff::Happened
                | Fluff::BlockFault { .. }
                | Fluff::PlaceBlockGeneric
                | Fluff::BlockImpact { .. } => schema::FluffSer::GoneV1,
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Fluff {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            Ok(match schema::FluffSer::deserialize(deserializer)? {
                schema::FluffSer::GoneV1 => Fluff::Gone,
            })
        }
    }
}

mod inv {
    use super::*;
    use crate::inv::{self, Inventory, Slot, Tool};

    impl Serialize for Inventory {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            schema::InventorySer::InventoryV1 {
                slots: self.slots.iter().map(|slot| slot.into()).collect(),
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Inventory {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            match schema::InventorySer::deserialize(deserializer)? {
                schema::InventorySer::InventoryV1 { slots } => Ok(Inventory {
                    slots: slots.into_iter().map(|s| s.into()).collect(),
                }),
            }
        }
    }

    impl From<Option<schema::InvStackSer>> for Slot {
        fn from(slot: Option<schema::InvStackSer>) -> Self {
            match slot {
                Some(schema::InvStackSer { count, item }) => Slot::Stack(count, item),
                None => Slot::Empty,
            }
        }
    }

    impl From<&Slot> for Option<schema::InvStackSer> {
        fn from(slot: &Slot) -> Self {
            match *slot {
                Slot::Empty => None,
                Slot::Stack(count, ref item) => Some(schema::InvStackSer {
                    count,
                    item: item.clone(),
                }),
            }
        }
    }

    impl Serialize for Tool {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match *self {
                Tool::Activate => schema::ToolSer::ActivateV1 {},
                Tool::RemoveBlock { keep } => schema::ToolSer::RemoveBlockV1 { keep },
                Tool::Block(ref block) => schema::ToolSer::BlockV1 {
                    block: block.clone(),
                },
                Tool::InfiniteBlocks(ref block) => schema::ToolSer::InfiniteBlocksV1 {
                    block: block.clone(),
                },
                Tool::CopyFromSpace => schema::ToolSer::CopyFromSpaceV1 {},
                Tool::EditBlock => schema::ToolSer::EditBlockV1 {},
                Tool::PushPull => schema::ToolSer::PushPullV1 {},
                Tool::Jetpack { active } => schema::ToolSer::JetpackV1 { active },
                Tool::Custom { ref op, ref icon } => schema::ToolSer::CustomV1 {
                    op: op.clone(),
                    icon: icon.clone(),
                },
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Tool {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            Ok(match schema::ToolSer::deserialize(deserializer)? {
                schema::ToolSer::ActivateV1 {} => Tool::Activate,
                schema::ToolSer::RemoveBlockV1 { keep } => Tool::RemoveBlock { keep },
                schema::ToolSer::BlockV1 { block } => Tool::Block(block),
                schema::ToolSer::InfiniteBlocksV1 { block } => Tool::InfiniteBlocks(block),
                schema::ToolSer::CopyFromSpaceV1 {} => Tool::CopyFromSpace,
                schema::ToolSer::EditBlockV1 {} => Tool::EditBlock,
                schema::ToolSer::PushPullV1 {} => Tool::PushPull,
                schema::ToolSer::JetpackV1 { active } => Tool::Jetpack { active },
                schema::ToolSer::CustomV1 { op, icon } => Tool::Custom { op, icon },
            })
        }
    }

    impl From<&inv::InvInBlock> for schema::InvInBlockSerV1 {
        fn from(value: &inv::InvInBlock) -> Self {
            let inv::InvInBlock {
                size,
                icon_scale,
                icon_resolution,
                ref icon_rows,
            } = *value;
            schema::InvInBlockSerV1 {
                size,
                icon_scale,
                icon_resolution,
                icon_rows: icon_rows.iter().map(schema::IconRowSerV1::from).collect(),
            }
        }
    }

    impl From<schema::InvInBlockSerV1> for inv::InvInBlock {
        fn from(value: schema::InvInBlockSerV1) -> Self {
            let schema::InvInBlockSerV1 {
                size,
                icon_scale,
                icon_resolution,
                icon_rows,
            } = value;
            inv::InvInBlock {
                size,
                icon_scale,
                icon_resolution,
                icon_rows: icon_rows.into_iter().map(inv::IconRow::from).collect(),
            }
        }
    }

    impl From<&inv::IconRow> for schema::IconRowSerV1 {
        fn from(value: &inv::IconRow) -> Self {
            let inv::IconRow {
                first_slot,
                count,
                origin,
                stride,
            } = *value;
            schema::IconRowSerV1 {
                first_slot,
                count,
                origin: origin.into(),
                stride: stride.into(),
            }
        }
    }

    impl From<schema::IconRowSerV1> for inv::IconRow {
        fn from(value: schema::IconRowSerV1) -> Self {
            let schema::IconRowSerV1 {
                first_slot,
                count,
                origin,
                stride,
            } = value;
            inv::IconRow {
                first_slot,
                count,
                origin: origin.into(),
                stride: stride.into(),
            }
        }
    }
}

mod op {
    use super::*;
    use crate::op;

    impl Serialize for op::Operation {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match self {
                op::Operation::Alt(ops) => schema::OperationSer::AltV1 {
                    ops: Cow::Borrowed(&ops[..]),
                },
                op::Operation::Become(block) => schema::OperationSer::BecomeV1 {
                    block: block.clone(),
                },
                op::Operation::DestroyTo(block) => schema::OperationSer::DestroyToV1 {
                    block: block.clone(),
                },
                &op::Operation::Replace {
                    ref old,
                    ref new,
                    conserved,
                    optional,
                } => schema::OperationSer::ReplaceV1 {
                    old: old.clone(),
                    new: new.clone(),
                    conserved,
                    optional,
                },

                op::Operation::AddModifiers(modifiers) => schema::OperationSer::AddModifiersV1 {
                    modifiers: modifiers.iter().map(schema::ModifierSer::from).collect(),
                },
                op::Operation::StartMove(m) => schema::OperationSer::StartMoveV1(m.into()),
                &op::Operation::MoveInventory {
                    transfer_into_adjacent,
                } => schema::OperationSer::MoveInventoryV1 {
                    transfer_into_adjacent,
                },
                &op::Operation::TakeInventory { destroy_if_empty } => {
                    schema::OperationSer::TakeInventoryV1 { destroy_if_empty }
                }
                op::Operation::Neighbors(neighbors) => schema::OperationSer::NeighborsV1 {
                    // TODO: arrange to be able to borrow here
                    neighbors: Cow::Owned(
                        neighbors
                            .iter()
                            .map(|&(offset, ref op)| (offset.into(), op.clone()))
                            .collect(),
                    ),
                },
                op::Operation::AndFluff { fluff, and } => schema::OperationSer::AndFluffV1 {
                    fluff: fluff.clone(),
                    and: and.clone(),
                },
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for op::Operation {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            Ok(match schema::OperationSer::deserialize(deserializer)? {
                schema::OperationSer::AltV1 { ops } => op::Operation::Alt(ops.into()),
                schema::OperationSer::BecomeV1 { block } => op::Operation::Become(block),
                schema::OperationSer::DestroyToV1 { block } => op::Operation::DestroyTo(block),
                schema::OperationSer::ReplaceV1 {
                    old,
                    new,
                    conserved,
                    optional,
                } => op::Operation::Replace {
                    old,
                    new,
                    conserved,
                    optional,
                },
                schema::OperationSer::AddModifiersV1 { modifiers } => op::Operation::AddModifiers(
                    cow_into_iter(modifiers).map(crate::block::Modifier::from).collect(),
                ),
                schema::OperationSer::StartMoveV1(m) => op::Operation::StartMove(m.into()),
                schema::OperationSer::MoveInventoryV1 {
                    transfer_into_adjacent,
                } => op::Operation::MoveInventory {
                    transfer_into_adjacent,
                },
                schema::OperationSer::TakeInventoryV1 { destroy_if_empty } => {
                    op::Operation::TakeInventory { destroy_if_empty }
                }
                schema::OperationSer::NeighborsV1 { neighbors } => op::Operation::Neighbors(
                    cow_into_iter(neighbors).map(|(offset, op)| (offset.into(), op)).collect(),
                ),
                schema::OperationSer::AndFluffV1 { fluff, and } => {
                    op::Operation::AndFluff { fluff, and }
                }
            })
        }
    }
}

mod sound {
    use super::*;
    use crate::sound;

    impl Serialize for sound::SoundDef {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            schema::SoundDefSer::SynthesizedSoundV1 {
                duration: self.duration,
                frequency: self.frequency,
                amplitude: self.amplitude,
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for sound::SoundDef {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            match schema::SoundDefSer::deserialize(deserializer)? {
                schema::SoundDefSer::SynthesizedSoundV1 {
                    duration,
                    frequency,
                    amplitude,
                } => Ok(sound::SoundDef {
                    duration,
                    frequency,
                    amplitude,
                }),
            }
        }
    }

    impl From<&sound::Ambient> for schema::AmbientSoundSerV1 {
        fn from(value: &sound::Ambient) -> Self {
            let &sound::Ambient {
                noise_bands: sound::Spectrum(noise_bands),
            } = value;
            schema::AmbientSoundSerV1 { noise_bands }
        }
    }

    impl From<schema::AmbientSoundSerV1> for sound::Ambient {
        fn from(value: schema::AmbientSoundSerV1) -> Self {
            let schema::AmbientSoundSerV1 { noise_bands } = value;
            Self {
                noise_bands: sound::Spectrum(noise_bands),
            }
        }
    }
}

mod space {
    use super::*;
    use crate::math::{Rgb, Vol};
    use crate::save::compress::{GzSerde, Leu16};
    use crate::space::{self, LightPhysics, Sky, Space, SpacePhysics};

    impl Serialize for Space {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            self.read().serialize(serializer)
        }
    }

    impl Serialize for space::Read<'_> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            schema::SpaceSer::SpaceV1 {
                bounds: self.bounds(),
                physics: self.physics().into(),
                blocks: self.block_data().iter().map(|bd| bd.block().clone()).collect(),
                contents: GzSerde(Cow::Owned(
                    self.extract(self.bounds(), |e| Leu16::from(e.block_index())).into_elements(),
                )),
                light: if matches!(self.physics().light, LightPhysics::None) {
                    None
                } else {
                    Some(GzSerde(Cow::Owned(
                        self.extract(self.bounds(), |e| {
                            let mut light = schema::LightSerV1::from(e.light());
                            if self.in_light_update_queue(e.cube()) {
                                light.status = schema::LightStatusSerV1::Uninitialized
                            }
                            light
                        })
                        .into_elements(),
                    )))
                },
                behaviors: Cow::Borrowed(self.behaviors()),
                spawn: Cow::Borrowed(self.spawn()),
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Space {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            match schema::SpaceSer::deserialize(deserializer)? {
                schema::SpaceSer::SpaceV1 {
                    bounds,
                    physics,
                    blocks,
                    contents: GzSerde(contents),
                    light,
                    behaviors,
                    spawn,
                } => {
                    // Convert data representations
                    let contents = Vol::from_elements(
                        bounds,
                        Vec::from(contents)
                            .into_iter()
                            .map(space::BlockIndex::from)
                            .collect::<Box<[_]>>(),
                    )
                    .map_err(serde::de::Error::custom)?;
                    let light = light
                        .map(|GzSerde(data)| {
                            Vol::from_elements(
                                bounds,
                                Vec::from(data)
                                    .into_iter()
                                    .map(space::PackedLight::from)
                                    .collect::<Box<[_]>>(),
                            )
                        })
                        .transpose()
                        .map_err(serde::de::Error::custom)?;

                    let space = Space::builder(bounds)
                        .physics(physics.into())
                        // .read_ticket(todo!("TODO(ecs)"))
                        .palette_and_contents(blocks, contents, light)
                        .map_err(serde::de::Error::custom)?
                        .behaviors(behaviors.into_owned())
                        .spawn(spawn.into_owned())
                        .build();

                    Ok(space)
                }
            }
        }
    }

    impl From<&SpacePhysics> for schema::SpacePhysicsSerV1 {
        fn from(value: &SpacePhysics) -> Self {
            let &SpacePhysics {
                gravity,
                ref sky,
                ref light,
            } = value;
            Self {
                gravity: gravity.into(),
                sky: sky.into(),
                light: light.into(),
            }
        }
    }

    impl From<schema::SpacePhysicsSerV1> for SpacePhysics {
        fn from(value: schema::SpacePhysicsSerV1) -> Self {
            let schema::SpacePhysicsSerV1 {
                gravity,
                sky,
                light,
            } = value;
            Self {
                gravity: gravity.into(),
                sky: sky.into(),
                light: light.into(),
            }
        }
    }

    impl From<&Sky> for schema::SkySer {
        fn from(value: &Sky) -> Self {
            use schema::SkySer as S;
            match value {
                &Sky::Uniform(color) => S::UniformV1 {
                    color: color.into(),
                },
                &Sky::Octants(colors) => S::OctantsV1 {
                    colors: colors.map(Rgb::into),
                },
            }
        }
    }

    impl From<schema::SkySer> for Sky {
        fn from(value: schema::SkySer) -> Self {
            use schema::SkySer as S;
            match value {
                S::UniformV1 { color } => Sky::Uniform(color.into()),
                S::OctantsV1 { colors } => Sky::Octants(colors.map(Rgb::from)),
            }
        }
    }

    impl From<&LightPhysics> for schema::LightPhysicsSerV1 {
        fn from(value: &LightPhysics) -> Self {
            use schema::LightPhysicsSerV1 as S;
            match value {
                LightPhysics::None => S::NoneV1,
                &LightPhysics::Rays { maximum_distance } => S::RaysV1 { maximum_distance },
            }
        }
    }

    impl From<schema::LightPhysicsSerV1> for LightPhysics {
        fn from(value: schema::LightPhysicsSerV1) -> Self {
            use schema::LightPhysicsSerV1 as S;
            match value {
                S::NoneV1 => LightPhysics::None,
                S::RaysV1 { maximum_distance } => LightPhysics::Rays { maximum_distance },
            }
        }
    }
}

mod tag {
    use super::*;
    use crate::tag::{Tag, TagDef};

    impl Serialize for Tag {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match self {
                Tag::Handle(handle) => schema::TagSer::TagHandleV1 {
                    handle: handle.clone(),
                },
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Tag {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            match schema::TagSer::deserialize(deserializer)? {
                schema::TagSer::TagHandleV1 { handle } => Ok(Tag::Handle(handle)),
            }
        }
    }
    impl Serialize for TagDef {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let TagDef = *self;
            schema::TagDefSer::TagDefV1 {}.serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for TagDef {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            match schema::TagDefSer::deserialize(deserializer)? {
                schema::TagDefSer::TagDefV1 {} => Ok(TagDef),
            }
        }
    }
}

mod time {
    use super::*;
    use crate::time;

    impl Serialize for time::Schedule {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            schema::ScheduleSer::ScheduleV1 {
                period: self.to_period().unwrap(),
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for time::Schedule {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            Ok(match schema::ScheduleSer::deserialize(deserializer)? {
                schema::ScheduleSer::ScheduleV1 { period } => time::Schedule::from_period(period),
            })
        }
    }
}

mod universe {
    use super::*;
    use crate::block::BlockDef;
    use crate::save::schema::MemberEntrySer;
    use crate::time;
    use crate::universe::{
        self, AnyHandle, Handle, HandleSet, Name, PartialUniverse, Universe, tl,
    };
    use schema::{HandleSer, MemberDe, NameSer};

    impl From<&BlockDef> for schema::MemberSer<'_> {
        fn from(block_def: &BlockDef) -> Self {
            schema::MemberSer::Block {
                value: block_def.block().clone(),
            }
        }
    }

    impl Serialize for PartialUniverse<'_> {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            let &Self {
                read_ticket,
                ref handles,
            } = self;

            let members = handles
                .iter()
                .map(|handle: &AnyHandle| match handle {
                    AnyHandle::BlockDef(member_handle) => {
                        let name = member_handle.name();
                        let member_repr = schema::MemberSer::from(
                            member_handle.read(read_ticket).map_err(|e| {
                                serde::ser::Error::custom(format!(
                                    "Failed to read universe member {name}: {e}"
                                ))
                            })?,
                        );
                        Ok(MemberEntrySer {
                            name: member_handle.name(),
                            value: member_repr,
                        })
                    }
                    AnyHandle::Character(member_handle) => Ok(MemberEntrySer {
                        name: member_handle.name(),
                        value: schema::MemberSer::Character {
                            value: schema::SerializeHandle(read_ticket, member_handle.clone()),
                        },
                    }),
                    AnyHandle::SoundDef(member_handle) => Ok(MemberEntrySer {
                        name: member_handle.name(),
                        value: schema::MemberSer::Sound {
                            value: schema::SerializeHandle(read_ticket, member_handle.clone()),
                        },
                    }),
                    AnyHandle::Space(member_handle) => Ok(MemberEntrySer {
                        name: member_handle.name(),
                        value: schema::MemberSer::Space {
                            value: schema::SerializeHandle(read_ticket, member_handle.clone()),
                        },
                    }),
                    AnyHandle::TagDef(member_handle) => Ok(MemberEntrySer {
                        name: member_handle.name(),
                        value: schema::MemberSer::Tag {
                            value: schema::SerializeHandle(read_ticket, member_handle.clone()),
                        },
                    }),
                })
                .collect::<Result<Vec<MemberEntrySer<schema::MemberSer<'_>>>, S::Error>>()?;
            schema::UniverseSer::UniverseV1 { members }.serialize(serializer)
        }
    }

    impl Serialize for Universe {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            // TODO: Serialize resources such as `Clock`, possibly by giving them unique Handles.
            PartialUniverse {
                handles: HandleSet::all_of(self),
                read_ticket: self.read_ticket(),
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Universe {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            let (data, mut universe) = {
                let scope = tl::Scope::install(tl::Context {
                    purpose: tl::Purpose::Deserialization,
                    universe: Universe::new(),
                });
                let data = schema::UniverseDe::deserialize(deserializer)?;
                let universe = scope.take(tl::Purpose::Deserialization).universe;
                (data, universe)
            };

            match data {
                schema::UniverseDe::UniverseV1 { members } => {
                    for MemberEntrySer { name, value } in members {
                        match value {
                            MemberDe::Block { value: block } => universe.insert_deserialized(
                                name,
                                Box::new(BlockDef::new(universe.read_ticket(), block)),
                            ),
                            MemberDe::Character { value } => {
                                universe.insert_deserialized(name, value)
                            }
                            MemberDe::Sound { value } => universe.insert_deserialized(name, value),
                            MemberDe::Space { value } => universe.insert_deserialized(name, value),
                            MemberDe::Tag { value } => universe.insert_deserialized(name, value),
                        }
                        .map_err(serde::de::Error::custom)?;
                    }
                }
            }

            universe.validate_deserialized_members().map_err(serde::de::Error::custom)?;

            // Perform a paused step to let things do re-initialization,
            // such as `Space` block evaluation, without actually causing any in-game time
            // to pass.
            universe.step(true, time::Deadline::Asap);

            Ok(*universe)
        }
    }

    impl<T: 'static> Serialize for Handle<T> {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            HandleSer::HandleV1 { name: self.name() }.serialize(serializer)
        }
    }

    impl<'de, T: universe::UniverseMember + 'static> Deserialize<'de> for Handle<T> {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            match HandleSer::deserialize(deserializer)? {
                HandleSer::HandleV1 { name } => {
                    match tl::get_from_context(tl::Purpose::Deserialization, {
                        let name = name.clone();
                        |context| context.universe.get_or_insert_deserializing(name)
                    }) {
                        Some(result) => result.map_err(serde::de::Error::custom),
                        None => {
                            // If there is no `Universe` context, use a “gone” handle.
                            // I am unsure whether this has any practical application, but it
                            // is at least useful in deserialization tests.
                            Ok(Handle::new_gone(name))
                        }
                    }
                }
            }
        }
    }

    impl Serialize for Name {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            match self {
                Name::Specific(s) => NameSer::Specific(s.clone()),
                &Name::Anonym(number) => NameSer::Anonym(number),
                Name::Pending => {
                    return Err(serde::ser::Error::custom(
                        "cannot serialize a pending Handle",
                    ));
                }
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Name {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            Ok(match NameSer::deserialize(deserializer)? {
                NameSer::Specific(s) => Name::Specific(s),
                NameSer::Anonym(n) => Name::Anonym(n),
            })
        }
    }

    impl<'ticket, T> Serialize for schema::SerializeHandle<'ticket, T>
    where
        // Each `Read` implementation must serialize to the serialization of the member.
        T: universe::UniverseMember<Read<'ticket>: Serialize>,
    {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let &schema::SerializeHandle(read_ticket, ref handle) = self;
            let read = handle.read(read_ticket).map_err(|e| {
                serde::ser::Error::custom(format!(
                    "Failed to read universe member {name}: {e}",
                    name = handle.name()
                ))
            })?;
            read.serialize(serializer)
        }
    }
}

/// Create an efficient iterator of owned elements from a [`Cow`].
///
/// `cow.into_owned().into_iter()` creates a redundant `Vec`, and
/// `cow.iter().cloned()` redundantly clones owned items;
/// this function avoids both.
fn cow_into_iter<T: Clone>(cow: Cow<'_, [T]>) -> impl Iterator<Item = T> + '_ {
    use itertools::Either;
    match cow {
        Cow::Borrowed(slice) => Either::Left(slice.iter().cloned()),
        Cow::Owned(vec) => Either::Right(vec.into_iter()),
    }
}

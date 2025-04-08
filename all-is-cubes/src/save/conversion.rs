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
        AnimationChange, AnimationHint, Atom, Block, BlockAttributes, BlockCollision, Composite,
        Modifier, Move, PlacementAction, Primitive, Quote, RotationPlacementRule, TickAction, Zoom,
        text,
    };
    use crate::math::{Rgb, Rgba};
    use crate::tag;
    use schema::{BlockSer, ModifierSer};

    impl Serialize for Block {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            BlockSer::BlockV1 {
                primitive: schema::PrimitiveSer::from(self.primitive()),
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
                    let (primitive, attributes) = primitive_from_schema(primitive);
                    let mut block = Block::from_primitive(primitive);

                    let attr_mod_iter = attributes
                        .filter(|a| a != BlockAttributes::DEFAULT_REF)
                        .map(Modifier::from)
                        .into_iter();
                    let general_mod_iter = modifiers.into_iter().map(Modifier::from);

                    block
                        .modifiers_mut()
                        .extend(attr_mod_iter.chain(general_mod_iter));
                    block
                }
            })
        }
    }

    impl<'a> From<&'a Primitive> for schema::PrimitiveSer {
        fn from(value: &'a Primitive) -> Self {
            match value {
                Primitive::Indirect(definition) => schema::PrimitiveSer::IndirectV1 {
                    definition: definition.clone(),
                },
                &Primitive::Atom(Atom {
                    color,
                    emission,
                    collision,
                }) => schema::PrimitiveSer::AtomV1 {
                    // attributes on the primitive are no longer used, but still supported
                    // by deserialization.
                    attributes: BlockAttributes::DEFAULT_REF.into(),
                    color: color.into(),
                    light_emission: emission.into(),
                    collision: collision.into(),
                },
                &Primitive::Recur {
                    ref space,
                    offset,
                    resolution,
                } => schema::PrimitiveSer::RecurV1 {
                    // attributes on the primitive are no longer used, but still supported
                    // by deserialization.
                    attributes: BlockAttributes::DEFAULT_REF.into(),
                    space: space.clone(),
                    offset: offset.into(),
                    resolution,
                },
                &Primitive::Air => schema::PrimitiveSer::AirV1,
                &Primitive::Text { ref text, offset } => schema::PrimitiveSer::TextPrimitiveV1 {
                    text: text.into(),
                    offset: offset.into(),
                },
            }
        }
    }

    fn primitive_from_schema(value: schema::PrimitiveSer) -> (Primitive, Option<BlockAttributes>) {
        match value {
            schema::PrimitiveSer::IndirectV1 { definition } => {
                (Primitive::Indirect(definition), None)
            }
            schema::PrimitiveSer::AtomV1 {
                attributes,
                color,
                light_emission: emission,
                collision,
            } => (
                Primitive::Atom(Atom {
                    color: Rgba::from(color),
                    emission: Rgb::from(emission),
                    collision: collision.into(),
                }),
                Some(attributes.into()),
            ),
            schema::PrimitiveSer::RecurV1 {
                attributes,
                space,
                offset,
                resolution,
            } => (
                Primitive::Recur {
                    space,
                    offset: offset.into(),
                    resolution,
                },
                Some(attributes.into()),
            ),
            schema::PrimitiveSer::AirV1 => (Primitive::Air, None),
            schema::PrimitiveSer::TextPrimitiveV1 { text, offset } => (
                Primitive::Text {
                    text: text.into(),
                    offset: offset.into(),
                },
                None,
            ),
        }
    }

    impl<'a> From<&'a BlockAttributes> for schema::BlockAttributesV1Ser {
        fn from(value: &'a BlockAttributes) -> Self {
            let &BlockAttributes {
                ref display_name,
                selectable,
                ref inventory,
                rotation_rule,
                ref placement_action,
                ref tick_action,
                ref activation_action,
                animation_hint,
            } = value;
            schema::BlockAttributesV1Ser {
                display_name: display_name.clone(),
                selectable,
                inventory: inventory.into(),
                rotation_rule: rotation_rule.into(),
                placement_action: placement_action.as_ref().map(
                    |&PlacementAction {
                         ref operation,
                         in_front,
                     }| schema::PlacementActionSer {
                        operation: operation.clone(),
                        in_front,
                    },
                ),
                tick_action: tick_action.as_ref().map(
                    |&TickAction {
                         ref operation,
                         schedule,
                     }| schema::TickActionSer {
                        operation: operation.clone(),
                        schedule,
                    },
                ),
                activation_action: activation_action.clone(),
                animation_hint: animation_hint.into(),
            }
        }
    }

    impl From<schema::BlockAttributesV1Ser> for BlockAttributes {
        fn from(value: schema::BlockAttributesV1Ser) -> Self {
            let schema::BlockAttributesV1Ser {
                display_name,
                selectable,
                inventory,
                rotation_rule,
                placement_action,
                tick_action,
                activation_action,
                animation_hint,
            } = value;
            Self {
                display_name,
                selectable,
                inventory: inventory.into(),
                rotation_rule: rotation_rule.into(),
                placement_action: placement_action.map(
                    |schema::PlacementActionSer {
                         operation,
                         in_front,
                     }| PlacementAction {
                        operation,
                        in_front,
                    },
                ),
                tick_action: tick_action.map(
                    |schema::TickActionSer {
                         operation,
                         schedule,
                     }| {
                        TickAction {
                            operation,
                            schedule,
                        }
                    },
                ),
                activation_action,
                animation_hint: animation_hint.into(),
            }
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

    impl From<AnimationHint> for schema::AnimationHintSer {
        fn from(value: AnimationHint) -> Self {
            let AnimationHint {
                redefinition,
                replacement,
            } = value;
            schema::AnimationHintSer::AnimationHintV1 {
                redefinition: redefinition.into(),
                replacement: replacement.into(),
            }
        }
    }

    impl From<schema::AnimationHintSer> for AnimationHint {
        fn from(value: schema::AnimationHintSer) -> Self {
            use schema::AnimationHintSer as S;
            match value {
                S::AnimationHintV1 {
                    redefinition,
                    replacement,
                } => AnimationHint {
                    redefinition: redefinition.into(),
                    replacement: replacement.into(),
                },
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
            match *value {
                Modifier::Attributes(ref attributes) => ModifierSer::AttributesV1 {
                    attributes: (&**attributes).into(),
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
            match value {
                ModifierSer::AttributesV1 { attributes } => {
                    Modifier::Attributes(Arc::new(attributes.into()))
                }
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
            }
        }
    }

    impl From<&Move> for schema::MoveSer {
        fn from(value: &Move) -> Self {
            let &Move {
                direction,
                distance,
                velocity,
                schedule,
            } = value;

            schema::MoveSer::MoveV1 {
                direction,
                distance,
                velocity,
                schedule,
            }
        }
    }

    impl From<schema::MoveSer> for Move {
        fn from(value: schema::MoveSer) -> Self {
            match value {
                schema::MoveSer::MoveV1 {
                    direction,
                    distance,
                    velocity,
                    schedule,
                } => Move {
                    direction,
                    distance,
                    velocity,
                    schedule,
                },
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

    impl From<&inv::InvInBlock> for schema::InvInBlockSer {
        fn from(value: &inv::InvInBlock) -> Self {
            let inv::InvInBlock {
                size,
                icon_scale,
                icon_resolution,
                ref icon_rows,
            } = *value;
            schema::InvInBlockSer::InvInBlockV1 {
                size,
                icon_scale,
                icon_resolution,
                icon_rows: icon_rows.iter().map(schema::IconRowSerV1::from).collect(),
            }
        }
    }

    impl From<schema::InvInBlockSer> for inv::InvInBlock {
        fn from(value: schema::InvInBlockSer) -> Self {
            match value {
                schema::InvInBlockSer::InvInBlockV1 {
                    size,
                    icon_scale,
                    icon_resolution,
                    icon_rows,
                } => inv::InvInBlock {
                    size,
                    icon_scale,
                    icon_resolution,
                    icon_rows: icon_rows.into_iter().map(inv::IconRow::from).collect(),
                },
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
                op::Operation::StartMove(m) => {
                    schema::OperationSer::StartMoveV1 { modifier: m.into() }
                }
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
                    cow_into_iter(modifiers)
                        .map(crate::block::Modifier::from)
                        .collect(),
                ),
                schema::OperationSer::StartMoveV1 { modifier: m } => {
                    op::Operation::StartMove(m.into())
                }
                schema::OperationSer::MoveInventoryV1 {
                    transfer_into_adjacent,
                } => op::Operation::MoveInventory {
                    transfer_into_adjacent,
                },
                schema::OperationSer::TakeInventoryV1 { destroy_if_empty } => {
                    op::Operation::TakeInventory { destroy_if_empty }
                }
                schema::OperationSer::NeighborsV1 { neighbors } => op::Operation::Neighbors(
                    cow_into_iter(neighbors)
                        .map(|(offset, op)| (offset.into(), op))
                        .collect(),
                ),
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
            schema::SpaceSer::SpaceV1 {
                bounds: self.bounds(),
                physics: self.physics().into(),
                blocks: self
                    .block_data()
                    .iter()
                    .map(|bd| bd.block().clone())
                    .collect(),
                contents: GzSerde(Cow::Owned(
                    self.extract(self.bounds(), |e| Leu16::from(e.block_index()))
                        .into_elements(),
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
    use crate::character::Character;
    use crate::save::schema::MemberEntrySer;
    use crate::sound::SoundDef;
    use crate::space::Space;
    use crate::tag::TagDef;
    use crate::time;
    use crate::universe::{self, Handle, Name, PartialUniverse, ReadGuard, Universe};
    use core::cell::RefCell;
    use schema::{HandleSer, MemberDe, NameSer};

    impl From<&BlockDef> for schema::MemberSer {
        fn from(block_def: &BlockDef) -> Self {
            schema::MemberSer::Block {
                value: block_def.block().clone(),
            }
        }
    }

    impl Serialize for PartialUniverse {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            let Self {
                blocks,
                characters,
                sounds,
                spaces,
                tags,
            } = self;

            let blocks = blocks.iter().map(|member_handle: &Handle<BlockDef>| {
                let name = member_handle.name();
                let read_guard: ReadGuard<BlockDef> = member_handle.read().map_err(|e| {
                    serde::ser::Error::custom(format!("Failed to read universe member {name}: {e}"))
                })?;
                let member_repr = schema::MemberSer::from(&*read_guard);
                Ok(MemberEntrySer {
                    name: member_handle.name(),
                    value: member_repr,
                })
            });
            let characters = characters.iter().map(|member_handle: &Handle<Character>| {
                Ok(MemberEntrySer {
                    name: member_handle.name(),
                    value: schema::MemberSer::Character {
                        value: schema::SerializeHandle(member_handle.clone()),
                    },
                })
            });
            let sounds = sounds.iter().map(|member_handle: &Handle<SoundDef>| {
                Ok(MemberEntrySer {
                    name: member_handle.name(),
                    value: schema::MemberSer::Sound {
                        value: schema::SerializeHandle(member_handle.clone()),
                    },
                })
            });
            let spaces = spaces.iter().map(|member_handle: &Handle<Space>| {
                Ok(MemberEntrySer {
                    name: member_handle.name(),
                    value: schema::MemberSer::Space {
                        value: schema::SerializeHandle(member_handle.clone()),
                    },
                })
            });
            let tags = tags.iter().map(|member_handle: &Handle<TagDef>| {
                Ok(MemberEntrySer {
                    name: member_handle.name(),
                    value: schema::MemberSer::Tag {
                        value: schema::SerializeHandle(member_handle.clone()),
                    },
                })
            });

            schema::UniverseSer::UniverseV1 {
                members: blocks
                    .chain(characters)
                    .chain(sounds)
                    .chain(spaces)
                    .chain(tags)
                    .collect::<Result<Vec<MemberEntrySer<schema::MemberSer>>, S::Error>>()?,
            }
            .serialize(serializer)
        }
    }

    impl Serialize for Universe {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            PartialUniverse::all_of(self).serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Universe {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            let (data, mut universe) = {
                let scope = ContextScope::install(DeContext {
                    universe: Universe::new(),
                });
                let data = schema::UniverseDe::deserialize(deserializer)?;
                let universe = scope.take().universe;
                (data, universe)
            };

            match data {
                schema::UniverseDe::UniverseV1 { members } => {
                    for MemberEntrySer { name, value } in members {
                        match value {
                            MemberDe::Block { value: block } => {
                                universe.insert_deserialized(name, BlockDef::new(block))
                            }
                            MemberDe::Character { value } => {
                                universe.insert_deserialized(name, value)
                            }
                            MemberDe::Sound { value } => universe.insert_deserialized(name, value),
                            MemberDe::Space { value } => universe.insert_deserialized(name, value),
                            MemberDe::Tag { value } => universe.insert_deserialized(name, value),
                        }
                        .expect("insertion from deserialization failed");
                    }
                }
            }

            universe
                .fix_deserialized_handles()
                .map_err(serde::de::Error::custom)?;

            // Perform a paused step to let things do re-initialization,
            // such as `Space` block evaluation, without actually causing any in-game time
            // to pass.
            universe.step(true, time::DeadlineNt::Asap);

            Ok(universe)
        }
    }

    impl<T: 'static> Serialize for Handle<T> {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            HandleSer::HandleV1 { name: self.name() }.serialize(serializer)
        }
    }

    impl<'de, T: 'static> Deserialize<'de> for Handle<T>
    where
        Universe: universe::UniverseTable<T, Table = universe::Storage<T>>,
    {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            match HandleSer::deserialize(deserializer)? {
                HandleSer::HandleV1 { name } => {
                    HANDLE_DESERIALIZATION_CONTEXT.with(|context_cell| -> Result<Self, D::Error> {
                        match Option::as_mut(&mut context_cell.borrow_mut()) {
                            Some(context) => context
                                .universe
                                .get_or_insert_deserializing(name)
                                .map_err(serde::de::Error::custom),
                            None => {
                                // If there is no `Universe` context, use a “gone” handle.
                                // I am unsure whether this has any practical application, but it
                                // is at least useful in deserialization tests.
                                Ok(Handle::new_gone(name))
                            }
                        }
                    })
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

    impl<T: Serialize + 'static> Serialize for schema::SerializeHandle<T> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let handle: &Handle<T> = &self.0;
            let read_guard: ReadGuard<T> = handle.read().map_err(|e| {
                serde::ser::Error::custom(format!(
                    "Failed to read universe member {name}: {e}",
                    name = handle.name()
                ))
            })?;
            let value: &T = &read_guard;
            value.serialize(serializer)
        }
    }

    std::thread_local! {
        /// Thread-local state used to communicate from [`Universe`] deserialization to
        /// [`Handle`] deserialization so that the [`Handle`]`] points to a member of that
        /// [`Universe`].
        ///
        /// If [`None`], no [`Universe`] deserialization is currently occurring.
        ///
        /// TODO: Find an alternative not dependent on external state. Perhaps
        /// serde::DeserializeSeed will do, or if necessary we can modify Handle to support
        /// modification after construction.
        static HANDLE_DESERIALIZATION_CONTEXT: RefCell<Option<DeContext>> = const {
            RefCell::new(None)
        };
    }

    struct DeContext {
        universe: Universe,
    }

    struct ContextScope;

    impl ContextScope {
        fn install(new_context: DeContext) -> Self {
            HANDLE_DESERIALIZATION_CONTEXT.with_borrow_mut(|tl| {
                assert!(tl.is_none(), "cannot nest Universe deserialization");
                *tl = Some(new_context);
            });
            Self
        }

        /// Uninstall and retrieve the context. Panics if none present.
        fn take(self) -> DeContext {
            let context = HANDLE_DESERIALIZATION_CONTEXT.with_borrow_mut(|tl| {
                tl.take()
                    .expect("something went wrong with HANDLE_DESERIALIZATION_CONTEXT")
            });
            core::mem::forget(self); // don't run Drop
            context
        }
    }

    impl Drop for ContextScope {
        fn drop(&mut self) {
            HANDLE_DESERIALIZATION_CONTEXT.with(|handle_context| {
                let mut handle_context = handle_context.borrow_mut();
                assert!(
                    handle_context.is_some(),
                    "something went wrong with HANDLE_DESERIALIZATION_CONTEXT"
                );
                *handle_context = None;
            });
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

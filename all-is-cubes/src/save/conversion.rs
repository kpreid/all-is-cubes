//! Conversion between the types in [`super::schema`] and those used in
//! normal operation.

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use super::schema;

/// Implements [`Serialize`] and [`Deserialize`] for `$library_type` using the conversions
/// * `TryFrom<$schema_type> for $library_type`
/// * `From<&$library_type> for $schema_type`
#[allow(unused)] // TODO: use this
macro_rules! impl_serde_via_schema_by_ref {
    ($library_type:ty, $schema_type:ty) => {
        impl ::serde::Serialize for $library_type {
            fn serialize<S: ::serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                // Construct wrapper by reference, unlike #[serde(into)]
                let schema_form: $schema_type = <$schema_type as From<&$library_type>>::from(self);
                <$schema_type as ::serde::Serialize>::serialize(&schema_form, serializer)
            }
        }
        impl<'de> ::serde::Deserialize<'de> for $library_type {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: ::serde::Deserializer<'de>,
            {
                // This is basically `#[serde(try_from = $schema_type)]`.

                let schema_form: $schema_type =
                    <$schema_type as ::serde::Deserialize<'de>>::deserialize(deserializer)?;
                // TODO: Don't convert error here
                <$library_type as std::convert::TryFrom<$schema_type>>::try_from(schema_form)
                    .map_err(serde::de::Error::custom)
            }
        }
    };
}

mod block {
    use super::*;
    use crate::block::{
        AnimationChange, AnimationHint, Atom, Block, BlockAttributes, BlockCollision, Composite,
        Modifier, Move, Primitive, Quote, RotationPlacementRule, Zoom,
    };
    use crate::math::Rgba;
    use schema::{BlockSer, ModifierSer};

    impl Serialize for Block {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            BlockSer::BlockV1 {
                primitive: schema::PrimitiveSer::from(self.primitive()),
                modifiers: self
                    .modifiers()
                    .iter()
                    .map(schema::ModifierSer::from)
                    .collect(),
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
                    let mut block = Block::from_primitive(primitive.into());
                    block
                        .modifiers_mut()
                        .extend(modifiers.into_iter().map(Modifier::from));
                    block
                }
            })
        }
    }

    impl From<&Primitive> for schema::PrimitiveSer {
        fn from(value: &Primitive) -> Self {
            match value {
                Primitive::Indirect(definition) => schema::PrimitiveSer::IndirectV1 {
                    definition: definition.clone(),
                },
                &Primitive::Atom(Atom {
                    ref attributes,
                    color,
                    collision,
                }) => schema::PrimitiveSer::AtomV1 {
                    color: color.into(),
                    attributes: attributes.into(),
                    collision: collision.into(),
                },
                &Primitive::Recur {
                    ref attributes,
                    ref space,
                    offset,
                    resolution,
                } => schema::PrimitiveSer::RecurV1 {
                    attributes: attributes.into(),
                    space: space.clone(),
                    offset: offset.into(),
                    resolution,
                },
                Primitive::Air => schema::PrimitiveSer::AirV1,
            }
        }
    }

    impl From<schema::PrimitiveSer> for Primitive {
        fn from(value: schema::PrimitiveSer) -> Self {
            match value {
                schema::PrimitiveSer::IndirectV1 { definition } => Primitive::Indirect(definition),
                schema::PrimitiveSer::AtomV1 {
                    attributes,
                    color,
                    collision,
                } => Primitive::Atom(Atom {
                    attributes: BlockAttributes::from(attributes),
                    color: Rgba::from(color),
                    collision: collision.into(),
                }),
                schema::PrimitiveSer::RecurV1 {
                    attributes,
                    space,
                    offset,
                    resolution,
                } => Primitive::Recur {
                    attributes: attributes.into(),
                    space,
                    offset: offset.into(),
                    resolution,
                },
                schema::PrimitiveSer::AirV1 => Primitive::Air,
            }
        }
    }

    impl From<&BlockAttributes> for schema::BlockAttributesV1Ser {
        fn from(value: &BlockAttributes) -> Self {
            let &BlockAttributes {
                ref display_name,
                selectable,
                rotation_rule,
                light_emission,
                tick_action: _, // TODO: serialize tick_action once it is cleaner
                animation_hint,
            } = value;
            schema::BlockAttributesV1Ser {
                display_name: display_name.to_string(),
                selectable,
                rotation_rule: rotation_rule.into(),
                light_emission: light_emission.into(),
                animation_hint: animation_hint.into(),
            }
        }
    }

    impl From<schema::BlockAttributesV1Ser> for BlockAttributes {
        fn from(value: schema::BlockAttributesV1Ser) -> Self {
            // TODO: implement deserializing all attributes
            let schema::BlockAttributesV1Ser {
                display_name,
                selectable,
                rotation_rule,
                light_emission,
                animation_hint,
            } = value;
            Self {
                display_name: display_name.into(),
                selectable,
                rotation_rule: rotation_rule.into(),
                light_emission: light_emission.into(),
                tick_action: None,
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

    impl From<&Modifier> for ModifierSer {
        fn from(value: &Modifier) -> Self {
            match *value {
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
                Modifier::Move(Move {
                    direction,
                    distance,
                    velocity,
                }) => ModifierSer::MoveV1 {
                    direction,
                    distance,
                    velocity,
                },
            }
        }
    }

    impl From<schema::ModifierSer> for Modifier {
        fn from(value: schema::ModifierSer) -> Self {
            match value {
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
                ModifierSer::MoveV1 {
                    direction,
                    distance,
                    velocity,
                } => Modifier::Move(Move::new(direction, distance, velocity)),
            }
        }
    }
}

// `character::Character` serialization is inside its module for the sake of private fields.

mod math {
    use super::*;
    use crate::math::{Aab, GridAab};

    impl Serialize for Aab {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            schema::AabSer {
                lower: self.lower_bounds_p().into(),
                upper: self.upper_bounds_p().into(),
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Aab {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            let schema::AabSer { lower, upper } = schema::AabSer::deserialize(deserializer)?;
            Aab::checked_from_lower_upper(lower.into(), upper.into())
                .ok_or_else(|| serde::de::Error::custom("invalid AAB"))
        }
    }

    impl Serialize for GridAab {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            schema::GridAabSer {
                lower: self.lower_bounds().into(),
                upper: self.upper_bounds().into(),
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for GridAab {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            let schema::GridAabSer { lower, upper } =
                schema::GridAabSer::deserialize(deserializer)?;
            GridAab::checked_from_lower_upper(lower, upper).map_err(serde::de::Error::custom)
        }
    }
}

mod inv {
    use super::*;
    use crate::inv::{EphemeralOpaque, Inventory, Slot, Tool};

    impl Serialize for Inventory {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            schema::InventorySer::InventoryV1 {
                slots: self
                    .slots
                    .iter()
                    .map(|slot| match *slot {
                        crate::inv::Slot::Empty => None,
                        crate::inv::Slot::Stack(count, ref item) => Some(schema::InvStackSer {
                            count,
                            item: item.clone(),
                        }),
                    })
                    .collect(),
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
                    slots: slots
                        .into_iter()
                        .map(|slot| match slot {
                            Some(schema::InvStackSer { count, item }) => Slot::Stack(count, item),
                            None => Slot::Empty,
                        })
                        .collect(),
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
                Tool::ExternalAction {
                    function: _,
                    ref icon,
                } => schema::ToolSer::ExternalActionV1 { icon: icon.clone() },
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
                schema::ToolSer::ExternalActionV1 { icon } => Tool::ExternalAction {
                    function: EphemeralOpaque(None),
                    icon,
                },
            })
        }
    }
}

mod space {
    use super::*;
    use crate::math::GridArray;
    use crate::save::compress::{GzSerde, Leu16};
    use crate::space::{self, LightPhysics, Space, SpacePhysics};
    use std::borrow::Cow;

    impl Serialize for Space {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            // TODO: more efficient serialization without extract() and with some kind of compression
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
                        .into_elements()
                        .into(),
                )),
                light: if matches!(self.physics().light, space::LightPhysics::None) {
                    None
                } else {
                    Some(GzSerde(Cow::Owned(
                        self.extract(self.bounds(), |e| schema::LightSerV1::from(e.light()))
                            .into_elements()
                            .into(),
                    )))
                },
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
                } => {
                    // Convert data representations
                    let contents = GridArray::from_elements(
                        bounds,
                        Vec::from(contents)
                            .into_iter()
                            .map(space::BlockIndex::from)
                            .collect::<Box<[_]>>(),
                    )
                    .map_err(serde::de::Error::custom)?;
                    let light = light
                        .map(|GzSerde(data)| {
                            GridArray::from_elements(
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
                        .palette_and_contents(&mut blocks.into_iter(), contents, light)
                        .map_err(serde::de::Error::custom)?
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
                sky_color,
                ref light,
            } = value;
            Self {
                gravity: gravity.into(),
                sky_color: sky_color.into(),
                light: light.into(),
            }
        }
    }

    impl From<schema::SpacePhysicsSerV1> for SpacePhysics {
        fn from(value: schema::SpacePhysicsSerV1) -> Self {
            let schema::SpacePhysicsSerV1 {
                gravity,
                sky_color,
                light,
            } = value;
            Self {
                gravity: gravity.into(),
                sky_color: sky_color.into(),
                light: light.into(),
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

mod universe {
    use super::*;
    use crate::block::{Block, BlockDef};
    use crate::character::Character;
    use crate::save::schema::MemberEntrySer;
    use crate::space::Space;
    use crate::universe::{self, Name, PartialUniverse, UBorrow, URef, Universe};
    use schema::{MemberDe, NameSer, URefSer};
    use std::cell::RefCell;

    impl From<&BlockDef> for schema::MemberSer {
        fn from(block_def: &BlockDef) -> Self {
            let block: &Block = block_def;
            schema::MemberSer::Block {
                value: block.clone(),
            }
        }
    }

    impl Serialize for PartialUniverse {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            let Self {
                blocks,
                characters,
                spaces,
            } = self;

            let blocks = blocks.iter().map(|member_ref: &URef<BlockDef>| {
                let name = member_ref.name();
                let read_guard: UBorrow<BlockDef> = member_ref.read().map_err(|e| {
                    serde::ser::Error::custom(format!("Failed to read universe member {name}: {e}"))
                })?;
                let member_repr = schema::MemberSer::from(&*read_guard);
                Ok(schema::MemberEntrySer {
                    name: member_ref.name(),
                    value: member_repr,
                })
            });
            let characters = characters.iter().map(|member_ref: &URef<Character>| {
                Ok(schema::MemberEntrySer {
                    name: member_ref.name(),
                    value: schema::MemberSer::Character {
                        value: schema::SerializeRef(member_ref.clone()),
                    },
                })
            });
            let spaces = spaces.iter().map(|member_ref: &URef<Space>| {
                Ok(schema::MemberEntrySer {
                    name: member_ref.name(),
                    value: schema::MemberSer::Space {
                        value: schema::SerializeRef(member_ref.clone()),
                    },
                })
            });

            schema::UniverseSer::UniverseV1 {
                members: blocks
                    .chain(characters)
                    .chain(spaces)
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
                    for schema::MemberEntrySer { name, value } in members {
                        match value {
                            MemberDe::Block { value: block } => universe
                                .insert_deserialized(name, BlockDef::new(block))
                                .map(|_| ()),
                            MemberDe::Character { value: character } => {
                                universe.insert_deserialized(name, character).map(|_| ())
                            }
                            MemberDe::Space { value: space } => {
                                universe.insert_deserialized(name, space).map(|_| ())
                            }
                        }
                        .expect("insertion from deserialization failed");
                    }
                }
            }

            universe
                .fix_deserialized_refs()
                .map_err(serde::de::Error::custom)?;

            // Perform a paused step to let things do re-initialization,
            // such as `Space` block evaluation, without actually causing any in-game time
            // to pass.
            universe.step(
                crate::time::Tick::from_seconds(0.0).pause(),
                instant::Instant::now(),
            );

            Ok(universe)
        }
    }

    impl<T: 'static> Serialize for URef<T> {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            URefSer::URefV1 { name: self.name() }.serialize(serializer)
        }
    }

    impl<'de, T: 'static> Deserialize<'de> for URef<T>
    where
        Universe: universe::UniverseTable<T, Table = universe::Storage<T>>,
    {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            match URefSer::deserialize(deserializer)? {
                URefSer::URefV1 { name } => {
                    UREF_DESERIALIZATION_CONTEXT.with(|context| -> Result<Self, D::Error> {
                        let mut context_refcell_guard = context.borrow_mut();

                        match context_refcell_guard.as_mut() {
                            Some(context) => context
                                .universe
                                .get_or_insert_deserializing(name)
                                .map_err(serde::de::Error::custom),
                            None => {
                                // If there is no `Universe` context, use a “gone” reference.
                                // I am unsure whether this has any practical application, but it
                                // is at least useful in deserialization tests.
                                Ok(URef::new_gone(name))
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
                    return Err(serde::ser::Error::custom("cannot serialize a pending URef"))
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

    impl<T: Serialize + 'static> Serialize for schema::SerializeRef<T> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let uref: &URef<T> = &self.0;
            let read_guard: UBorrow<T> = uref.read().map_err(|e| {
                serde::ser::Error::custom(format!(
                    "Failed to read universe member {name}: {e}",
                    name = uref.name()
                ))
            })?;
            let value: &T = &read_guard;
            value.serialize(serializer)
        }
    }

    thread_local! {
        /// Thread-local state used to communicate from [`Universe`] deserialization to
        /// [`URef`] deserialization so that the [`URef`] points to a member of that [`Universe`].
        ///
        /// If [`None`], no [`Universe`] deserialization is currently occurring.
        ///
        /// TODO: Find an alternative not dependent on external state. Perhaps
        /// serde::DeserializeSeed will do, or if necessary we can modify URef to support
        /// modification after construction.
        static UREF_DESERIALIZATION_CONTEXT: RefCell<Option<DeContext>> = RefCell::new(None);
    }

    struct DeContext {
        universe: Universe,
    }

    struct ContextScope;

    impl ContextScope {
        fn install(new_context: DeContext) -> Self {
            UREF_DESERIALIZATION_CONTEXT.with(|tl| {
                let mut tl = tl.borrow_mut();
                assert!(tl.is_none(), "cannot nest Universe deserialization");
                *tl = Some(new_context);
            });
            Self
        }

        /// Uninstall and retrieve the context. Panics if none present.
        fn take(self) -> DeContext {
            let context = UREF_DESERIALIZATION_CONTEXT.with(|tl| {
                tl.borrow_mut()
                    .take()
                    .expect("something went wrong with UREF_DESERIALIZATION_CONTEXT")
            });
            std::mem::forget(self); // don't run Drop
            context
        }
    }

    impl Drop for ContextScope {
        fn drop(&mut self) {
            UREF_DESERIALIZATION_CONTEXT.with(|uref_context| {
                let mut uref_context = uref_context.borrow_mut();
                assert!(
                    uref_context.is_some(),
                    "something went wrong with UREF_DESERIALIZATION_CONTEXT"
                );
                *uref_context = None;
            });
        }
    }
}

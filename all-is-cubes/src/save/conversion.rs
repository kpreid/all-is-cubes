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
    use crate::block::{Block, BlockAttributes, Composite, Modifier, Move, Primitive, Quote, Zoom};
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
                Primitive::Indirect(_) => todo!(),
                &Primitive::Atom(ref attributes, color) => schema::PrimitiveSer::AtomV1 {
                    color: color.into(),
                    attributes: attributes.into(),
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
                schema::PrimitiveSer::AirV1 => Primitive::Air,
                schema::PrimitiveSer::AtomV1 { attributes, color } => {
                    Primitive::Atom(BlockAttributes::from(attributes), Rgba::from(color))
                }
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
            }
        }
    }

    impl From<&BlockAttributes> for schema::BlockAttributesV1Ser {
        fn from(value: &BlockAttributes) -> Self {
            let &BlockAttributes {
                // TODO: implement serializing all attributes
                ref display_name,
                selectable,
                collision: _,
                rotation_rule: _,
                light_emission,
                tick_action: _,
                animation_hint: _,
            } = value;
            schema::BlockAttributesV1Ser {
                display_name: display_name.to_string(),
                selectable,
                light_emission: light_emission.into(),
            }
        }
    }

    impl From<schema::BlockAttributesV1Ser> for BlockAttributes {
        fn from(value: schema::BlockAttributesV1Ser) -> Self {
            // TODO: implement deserializing all attributes
            let schema::BlockAttributesV1Ser {
                display_name,
                selectable,
                light_emission,
            } = value;
            Self {
                display_name: display_name.into(),
                selectable,
                light_emission: light_emission.into(),
                ..Default::default()
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

mod universe {
    use super::*;
    use crate::block::{Block, BlockDef};
    use crate::save::schema::MemberEntrySer;
    use crate::universe::{Name, UBorrow, URef, Universe, UniverseIndex};
    use schema::{AnyUniverseMemberSer, NameSer, URefSer};

    impl From<&BlockDef> for schema::AnyUniverseMemberSer {
        fn from(block_def: &BlockDef) -> Self {
            let block: &Block = block_def;
            schema::AnyUniverseMemberSer::BlockDef(block.clone())
        }
    }

    impl Serialize for Universe {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            schema::UniverseSer::UniverseV1 {
                members: self
                    .iter_by_type()
                    .map(|(name, member_ref): (Name, URef<BlockDef>)| -> Result<schema::MemberEntrySer, S::Error> {
                        let read_guard: UBorrow<BlockDef> = member_ref.read().map_err(|e| {
                            serde::ser::Error::custom(format!(
                                "Failed to read universe member {name}: {e}"
                            ))
                        })?;
                        let member_repr = schema::AnyUniverseMemberSer::from(&*read_guard);
                        Ok(schema::MemberEntrySer { name, value: member_repr })
                    })
                    .collect::<Result<Vec<MemberEntrySer>, S::Error>>()?,
            }
            .serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Universe {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            let data = schema::UniverseSer::deserialize(deserializer)?;
            let mut universe = Universe::new();
            match data {
                // TODO: Instead of new_gone(), this needs to be a named ref that can be
                // hooked up to its definition.
                schema::UniverseSer::UniverseV1 { members } => {
                    for schema::MemberEntrySer { name, value } in members {
                        match value {
                            AnyUniverseMemberSer::BlockDef(block) => {
                                universe.insert(name, BlockDef::new(block))
                            }
                        }
                        .expect("insertion from deserialization failed");
                    }
                }
            }
            Ok(universe)
        }
    }

    impl<T: 'static> Serialize for URef<T> {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            URefSer::URefV1 { name: self.name() }.serialize(serializer)
        }
    }

    impl<'de, T: 'static> Deserialize<'de> for URef<T> {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            Ok(match URefSer::deserialize(deserializer)? {
                // TODO: Instead of new_gone(), this needs to be a named ref that can be
                // hooked up to its definition.
                URefSer::URefV1 { name } => URef::new_gone(name),
            })
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
}

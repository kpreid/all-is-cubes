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

mod universe {
    use super::*;
    use crate::universe::{Name, URef};
    use schema::{NameSer, URefSer};

    impl<T: 'static> Serialize for URef<T> {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            URefSer::URefV1 {
                name: match self.name() {
                    Name::Specific(s) => NameSer::Specific(s),
                    Name::Anonym(n) => NameSer::Anonym(n),
                    Name::Pending => {
                        return Err(serde::ser::Error::custom("cannot serialize a pending URef"))
                    }
                },
            }
            .serialize(serializer)
        }
    }

    impl<'de, T: 'static> Deserialize<'de> for URef<T> {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            Ok(match URefSer::deserialize(deserializer)? {
                // TODO: Instead of new_gone(), this needs to be a named ref that can be
                // hooked up to its definition.
                URefSer::URefV1 { name } => URef::new_gone(match name {
                    NameSer::Specific(s) => Name::Specific(s),
                    NameSer::Anonym(n) => Name::Anonym(n),
                }),
            })
        }
    }
}

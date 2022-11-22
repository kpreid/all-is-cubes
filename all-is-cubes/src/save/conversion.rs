//! Conversion between the types in [`super::schema`] and those used in
//! normal operation.

#![allow(unused)] // TODO: use this

/// Implements [`Serialize`] and [`Deserialize`] for `$library_type` using the conversions
/// * `TryFrom<$schema_type> for $library_type`
/// * `From<&$library_type> for $schema_type`
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
pub(crate) use impl_serde_via_schema_by_ref;

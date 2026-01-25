use core::fmt;

use serde::de::IntoDeserializer;
use serde::ser::Impossible;

use all_is_cubes::arcstr::{self, ArcStr, literal};
use all_is_cubes::math;
use all_is_cubes_render::camera;

// -------------------------------------------------------------------------------------------------

/// Serialization and deserialization of settings value types.
///
/// Implementations should have the following properties:
///
/// * `serialize()` is infallible.
/// * `Self::deserialize(value.serialize()) == Ok(value)`
///
/// They are not required to be “self-describing” (deserialization as a different type may produce
/// nonsense).
pub(crate) trait StringForm: Sized + 'static {
    fn serialize(&self) -> ArcStr;
    /// The error should be a message describing the parse error details.
    /// It does not need to include the value.
    fn deserialize(value: &str) -> Result<Self, DeserializeError>;
}

// -------------------------------------------------------------------------------------------------

impl StringForm for ArcStr {
    fn serialize(&self) -> ArcStr {
        ArcStr::clone(self)
    }

    fn deserialize(value: &str) -> Result<Self, DeserializeError> {
        Ok(ArcStr::from(value))
    }
}

// -------------------------------------------------------------------------------------------------

/// Adapter from [`serde::Serialize`] to [`StringForm`].
///
/// The value serialized must serialize as a single string or unit variant.
struct SettingSerializer;

#[mutants::skip]
impl serde::Serializer for SettingSerializer {
    type Ok = ArcStr;
    type Error = SerializeError;

    type SerializeSeq = Impossible<Self::Ok, Self::Error>;
    type SerializeTuple = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleVariant = Impossible<Self::Ok, Self::Error>;
    type SerializeMap = Impossible<Self::Ok, Self::Error>;
    type SerializeStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeStructVariant = Impossible<Self::Ok, Self::Error>;

    // TODO: replace panics with error values

    fn serialize_bool(self, _: bool) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a bool"
        )))
    }

    fn serialize_i8(self, _: i8) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an integer"
        )))
    }

    fn serialize_i16(self, _: i16) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an integer"
        )))
    }

    fn serialize_i32(self, _: i32) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an integer"
        )))
    }

    fn serialize_i64(self, _: i64) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an integer"
        )))
    }

    fn serialize_u8(self, _: u8) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an integer"
        )))
    }

    fn serialize_u16(self, _: u16) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an integer"
        )))
    }

    fn serialize_u32(self, _: u32) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an integer"
        )))
    }

    fn serialize_u64(self, _: u64) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an integer"
        )))
    }

    fn serialize_f32(self, _: f32) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a number"
        )))
    }

    fn serialize_f64(self, _: f64) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a number"
        )))
    }

    fn serialize_char(self, _: char) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a char"
        )))
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(ArcStr::from(v))
    }

    fn serialize_bytes(self, _: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not bytes"
        )))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an option"
        )))
    }

    fn serialize_some<T>(self, _: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        Err(SerializeError(literal!(
            "value must serialize as a string, not an option"
        )))
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a unit"
        )))
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(ArcStr::from(name))
    }

    fn serialize_unit_variant(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(ArcStr::from(variant))
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a non-unit variant"
        )))
    }

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a sequence"
        )))
    }

    fn serialize_tuple(self, _: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a tuple"
        )))
    }

    fn serialize_tuple_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a struct"
        )))
    }

    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a non-unit variant"
        )))
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a map"
        )))
    }

    fn serialize_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a struct"
        )))
    }

    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(SerializeError(literal!(
            "value must serialize as a string, not a non-unit variant"
        )))
    }

    fn collect_str<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + fmt::Display,
    {
        Ok(arcstr::format!("{value}"))
    }

    fn is_human_readable(&self) -> bool {
        true
    }
}

/// Error produced during `serde` serialization of a setting.
///
/// This should never happen unless a [`TypedKey`] was defined incorrectly;
/// it is an error type and not a panic purely because this is an easy option to have available
/// and may slightly improve panic messages.
#[derive(Debug, PartialEq)]
struct SerializeError(pub ArcStr);

impl fmt::Display for SerializeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(&self.0)
    }
}

impl core::error::Error for SerializeError {}

impl serde::ser::Error for SerializeError {
    #[track_caller]
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Self(arcstr::format!("{msg}"))
    }
}

/// Error produced during deserialization, which will be translated to a [`ParseError`]
/// for public use.
///
/// The error message should describe the parse/deserialization error, without repeating the value.
#[derive(Debug, PartialEq)]
pub(in crate::settings) struct DeserializeError(pub ArcStr);

impl fmt::Display for DeserializeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl core::error::Error for DeserializeError {}

impl serde::de::Error for DeserializeError {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Self(arcstr::format!("{msg}"))
    }
}

// -------------------------------------------------------------------------------------------------

macro_rules! impl_string_form_via_from_str_and_display {
    ($type:ty) => {
        impl $crate::settings::serialize::StringForm for $type {
            fn serialize(&self) -> ::all_is_cubes::arcstr::ArcStr {
                ::all_is_cubes::arcstr::format!("{self}")
            }

            fn deserialize(
                value: &str,
            ) -> Result<Self, $crate::settings::serialize::DeserializeError> {
                value.parse::<Self>().map_err(|e| {
                    // Bad practice: this discards the error source().
                    // But it is unlikely there is any.
                    DeserializeError(::all_is_cubes::arcstr::format!("{e}"))
                })
            }
        }
    };
}
pub(super) use impl_string_form_via_from_str_and_display;

impl_string_form_via_from_str_and_display!(bool);
impl_string_form_via_from_str_and_display!(i8);
impl_string_form_via_from_str_and_display!(i16);
impl_string_form_via_from_str_and_display!(i32);
impl_string_form_via_from_str_and_display!(i64);
impl_string_form_via_from_str_and_display!(i128);
impl_string_form_via_from_str_and_display!(u8);
impl_string_form_via_from_str_and_display!(u16);
impl_string_form_via_from_str_and_display!(u32);
impl_string_form_via_from_str_and_display!(u64);
impl_string_form_via_from_str_and_display!(u128);
impl_string_form_via_from_str_and_display!(math::PositiveSign<f32>);
impl_string_form_via_from_str_and_display!(math::PositiveSign<f64>);
impl_string_form_via_from_str_and_display!(math::ZeroOne<f32>);
impl_string_form_via_from_str_and_display!(math::ZeroOne<f64>);

// -------------------------------------------------------------------------------------------------

macro_rules! impl_string_form_via_serde {
    ($type:ty) => {
        impl StringForm for $type {
            fn serialize(&self) -> ArcStr {
                match ::serde::Serialize::serialize(
                    self,
                    $crate::settings::serialize::SettingSerializer
                ) {
                    Ok(string) => string,
                    Err(e) => {
                        panic!(
                            "setting serialization should never fail; \
                             setting type {type} has a bug: {e}",
                            type = ::core::stringify!($type),
                        )
                    }
                }
            }

            fn deserialize(
                value: &str
            ) -> Result<Self, $crate::settings::serialize::DeserializeError> {
                <Self as ::serde::Deserialize>::deserialize(value.into_deserializer())
            }
        }
    };
}

// Note: This list must contain only values that can serialize to a single string, such as
// fieldless enums. But even then, consider using `impl_string_form_via_from_str_and_display`
// when possible instead, since that involves less complex generic code than using `serde`.
impl_string_form_via_serde!(all_is_cubes::util::ShowStatus);
impl_string_form_via_serde!(camera::AntialiasingOption);
impl_string_form_via_serde!(camera::FogOption);
impl_string_form_via_serde!(camera::LightingOption);
impl_string_form_via_serde!(camera::RenderMethod);
impl_string_form_via_serde!(camera::ToneMappingOperator);

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::math::ps64;

    fn assert_round_trip_value<T>(value: &T, expected_string: &str)
    where
        T: PartialEq + fmt::Debug + StringForm,
    {
        let actual_string = value.serialize();
        assert_eq!(
            actual_string, expected_string,
            "actual string != expected string"
        );
        assert_eq!(
            &T::deserialize(&actual_string)
                .expect("failed to deserialize (but serialized string was as expected)"),
            value,
            "roundtripped value not as expected"
        );
    }

    #[test]
    fn positive_sign_serialization() {
        assert_round_trip_value(&ps64(1.0), "1");
        assert_round_trip_value(&ps64(1.5), "1.5");
        assert_round_trip_value(&ps64(1e10), "10000000000");
        assert_round_trip_value(&ps64(f64::INFINITY), "inf");

        assert_eq!(
            math::PositiveSign::<f64>::deserialize("1e10"),
            Ok(ps64(1e10)),
        );
        assert_eq!(
            math::PositiveSign::<f64>::deserialize("NaN"),
            Err(DeserializeError(literal!("value was NaN")))
        );
    }

    #[test]
    fn enum_serialization() {
        #[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
        enum TestEnum {
            Foo,
            Bar,
        }
        impl_string_form_via_serde!(TestEnum);

        assert_round_trip_value(&TestEnum::Foo, "Foo");
        assert_round_trip_value(&TestEnum::Bar, "Bar");

        // Invalid value
        assert_eq!(
            <TestEnum as StringForm>::deserialize("Baz"),
            // Note: this error message is provided by serde, not our code
            Err(DeserializeError(literal!(
                "unknown variant `Baz`, expected `Foo` or `Bar`"
            )))
        );
    }
}

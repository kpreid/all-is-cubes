use alloc::borrow::{Cow, ToOwned};
use alloc::vec::Vec;
use core::fmt;
use std::io::{self, Write as _};

use serde::de::Error as _;

/// A slice of `T` which, when serialized, will be compressed in the gzip format.
///
/// To ensure portability, `T` should be endianness-independent.
///
/// Furthermore, if the serde serializer
/// [`is_human_readable()`](serde::Serializer::is_human_readable), then the compressed
/// bytes will be base64 encoded, in the hopes of producing a more compact textual result.
/// Otherwise, they will be serialized identically to a `Vec<u8>`.
///
/// The serialized format includes a version tag for the compression choices.
pub(crate) struct GzSerde<'a, T: 'static>(pub Cow<'a, [T]>)
where
    [T]: ToOwned;

impl<T: bytemuck::NoUninit> serde::Serialize for GzSerde<'_, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let uncompressed_bytes = bytemuck::must_cast_slice::<T, u8>(self.0.as_ref());

        let compression = flate2::Compression::fast();

        if serializer.is_human_readable() {
            let mut gz_encoder = flate2::GzBuilder::new().write(
                base64::write::EncoderStringWriter::new(&BASE64_ENGINE),
                compression,
            );
            gz_encoder.write_all(uncompressed_bytes).unwrap();
            let b64_encoder = gz_encoder.finish().unwrap();
            let b64string = b64_encoder.into_inner();

            GzSerdeInternal::Base64Gzip(Cow::Borrowed(b64string.as_str())).serialize(serializer)
        } else {
            let mut gz_encoder = flate2::GzBuilder::new().write(Vec::<u8>::new(), compression);
            gz_encoder.write_all(uncompressed_bytes).unwrap();
            let compressed_bytes = gz_encoder.finish().unwrap();

            GzSerdeInternal::Gzip(Cow::Borrowed(compressed_bytes.as_slice())).serialize(serializer)
        }
    }
}

impl<'de, T: bytemuck::CheckedBitPattern> serde::Deserialize<'de> for GzSerde<'_, T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        match GzSerdeInternal::deserialize(deserializer)? {
            GzSerdeInternal::Base64Gzip(b64string) => {
                deserialize_common(flate2::bufread::GzDecoder::new(io::BufReader::new(
                    base64::read::DecoderReader::new(
                        io::Cursor::new(b64string.as_bytes()),
                        &BASE64_ENGINE,
                    ),
                )))
                .map_err(|e| D::Error::custom(format!("invalid base64+gzip data: {e}")))
            }
            GzSerdeInternal::Gzip(gzip_bytes) => {
                deserialize_common(flate2::bufread::GzDecoder::new(io::Cursor::new(gzip_bytes)))
                    .map_err(|e| D::Error::custom(format!("invalid gzip data: {e}")))
            }
        }
    }
}

fn deserialize_common<T: bytemuck::CheckedBitPattern>(
    mut r: impl io::Read,
) -> Result<GzSerde<'static, T>, io::Error> {
    let mut uncompressed = Vec::new();
    r.read_to_end(&mut uncompressed)?;
    Ok(GzSerde(Cow::Owned(
        bytemuck::checked::try_cast_slice::<u8, T>(&uncompressed)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?
            .to_owned(),
    )))
}

impl<T> fmt::Debug for GzSerde<'_, T>
where
    [T]: ToOwned,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Truncate because this is typically very long and not interesting.
        // TODO: tune length
        if self.0.len() > 16 {
            write!(f, "GzSerde({:?}...)", &self.0[..16])
        } else {
            write!(f, "GzSerde({:?})", &self.0[..])
        }
    }
}

const BASE64_ENGINE: base64::engine::GeneralPurpose =
    base64::engine::general_purpose::STANDARD_NO_PAD;

#[derive(serde::Serialize, serde::Deserialize)]
enum GzSerdeInternal<'a> {
    Base64Gzip(Cow<'a, str>),
    Gzip(Cow<'a, [u8]>),
}

/// u16, but in guaranteed little-endian, unaligned representation.
#[derive(Copy, Clone, Debug, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub(crate) struct Leu16([u8; 2]);

impl From<u16> for Leu16 {
    fn from(value: u16) -> Self {
        Self(value.to_le_bytes())
    }
}
impl From<Leu16> for u16 {
    fn from(value: Leu16) -> Self {
        u16::from_le_bytes(value.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[track_caller]
    fn assert_round_trip<T>(value: &[T], expected_base64: &str)
    where
        T: bytemuck::Pod + Eq + fmt::Debug,
    {
        let json_value =
            serde_json::to_value(GzSerde(Cow::Borrowed(value))).expect("failed to serialize");

        assert_eq!(
            json_value,
            json!({ "Base64Gzip": expected_base64 }),
            "serialized str != expected str"
        );
        let deserialized =
            serde_json::from_value::<GzSerde<'_, T>>(json_value).expect("failed to deserialize");
        assert_eq!(
            &deserialized.0[..],
            value,
            "roundtripped value not as expected"
        );

        // TODO: test non-human-readable output
    }

    #[test]
    fn empty() {
        // output is non-empty because it includes a gzip header
        assert_round_trip::<[u8; 2]>(&[], "H4sIAAAAAAAE/wMAAAAAAAAAAAA");
    }

    #[test]
    fn nonempty() {
        assert_round_trip::<[u8; 2]>(
            &[[1, 2], [3, 4], [5, 6]],
            "H4sIAAAAAAAE/2NkYmZhZQMAJHf2gQYAAAA",
        );
    }

    #[test]
    fn proof_of_compression() {
        assert_round_trip::<[u8; 2]>(
            &vec![[123, 45]; 10000],
            "H4sIAAAAAAAE/+3QAQ0AAAiAsEQmtLyzBvtIwHdEgAABAgQIECBAgAABAgQI1AX8ESBAgAABAgQIECBAgAABAn0BhwQIECBAgAABAgQIECBAgEBfwCEBAgQIECBAgAABAgQIECDQF3BIgAABAgQIECBAgAABAgRe4ADS7V+aIE4AAA",
        );
    }
}

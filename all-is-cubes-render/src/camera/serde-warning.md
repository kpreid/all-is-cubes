# Serialization stability warning

This type implements [`serde::Serialize`] and [`serde::Deserialize`], but serialization
support is still experimental (as is the game data model in general). We do not guarantee that future versions of `all-is-cubes`
will be able to deserialize data which is serialized by this version.

Additionally, the serialization schema is designed with `serde_json` in mind. It is not
guaranteed that using a different data format crate, which may use a different subset of
the information exposed via [`serde::Serialize`], will produce stable results.


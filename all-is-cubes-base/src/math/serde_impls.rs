#![allow(clippy::missing_inline_in_public_items)]

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::math::{self, Aab, Cube, GridAab, GridCoordinate};

#[derive(Debug, Deserialize, Serialize)]
struct AabSer {
    // This one isn't an explicitly versioned enum because I expect we'll not need to change it
    lower: [f64; 3],
    upper: [f64; 3],
}

#[derive(Debug, Deserialize, Serialize)]
struct GridAabSer {
    // This one isn't an explicitly versioned enum because I expect we'll not need to change it
    lower: [GridCoordinate; 3],
    upper: [GridCoordinate; 3],
}

impl Serialize for Cube {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let &Cube { x, y, z } = self;

        [x, y, z].serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Cube {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let [x, y, z] = <[i32; 3]>::deserialize(deserializer)?;
        Ok(Cube::new(x, y, z))
    }
}

impl Serialize for Aab {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        AabSer {
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
        let AabSer { lower, upper } = AabSer::deserialize(deserializer)?;
        Aab::checked_from_lower_upper(lower.into(), upper.into())
            .ok_or_else(|| serde::de::Error::custom("invalid AAB"))
    }
}

impl Serialize for GridAab {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        GridAabSer {
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
        let GridAabSer { lower, upper } = GridAabSer::deserialize(deserializer)?;
        GridAab::checked_from_lower_upper(lower, upper).map_err(serde::de::Error::custom)
    }
}

impl<T: Serialize> Serialize for math::PositiveSign<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_ref().serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for math::PositiveSign<T>
where
    Self: TryFrom<T, Error: core::error::Error>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Self::try_from(T::deserialize(deserializer)?).map_err(serde::de::Error::custom)
    }
}

impl<T: Serialize> Serialize for math::ZeroOne<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_ref().serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for math::ZeroOne<T>
where
    Self: TryFrom<T, Error: core::error::Error>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Self::try_from(T::deserialize(deserializer)?).map_err(serde::de::Error::custom)
    }
}

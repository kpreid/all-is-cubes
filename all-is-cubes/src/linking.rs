// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Storing and accessing definitions of standard blocks in a [`Universe`].
//!
//! An enum implementing [`BlockModule`] defines a set of names, and
//! [`BlockProvider`] assists in ensuring that all of those names are defined
//! and storing or retrieving their block values in a specific [`Universe`].
//!
//! In the future this mechanism may grow to become a dynamic linker/dependency injector
//! by becoming aware of dependencies between “modules”. For now, it's just enough to
//! solve bootstrapping needs.

use std::collections::HashMap;
use std::error::Error;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::ops::Index;
use strum::IntoEnumIterator;

use crate::block::{Block, BlockDef};
use crate::space::SetCubeError;
use crate::universe::{InsertError, Name, URef, Universe, UniverseIndex};

fn name_in_module<E: BlockModule>(key: &E) -> Name {
    Name::from(format!("{}/{}", E::namespace(), key).as_str())
}

// TODO: document
pub trait DefaultProvision {
    fn default(self) -> Block;
}

// TODO: consider replacing Display (presumed derived by strum) with a special trait
pub trait BlockModule: Display + IntoEnumIterator + Eq + Hash + Clone {
    fn namespace() -> &'static str;
}

#[derive(Clone, Debug)]
pub struct BlockProvider<E> {
    /// Guaranteed to contain an entry for every variant of `E` if `E`'s
    /// `strum::IntoEnumIterator` implementation is accurate.
    map: HashMap<E, Block>,
}

impl<E> Default for BlockProvider<E>
where
    E: DefaultProvision + IntoEnumIterator + Eq + Hash + Clone,
{
    fn default() -> Self {
        Self {
            map: E::iter()
                .map(|key| {
                    let block = DefaultProvision::default(key.clone());
                    (key, block)
                })
                .collect(),
        }
    }
}

impl<E> BlockProvider<E>
where
    E: BlockModule,
{
    /// Constructs a `BlockProvider` with block definitions computed by the given function.
    pub fn new<F, B>(mut definer: F) -> Result<Self, GenError>
    where
        F: FnMut(E) -> Result<B, InGenError>,
        B: Into<Block>,
    {
        let mut map = HashMap::new();
        for key in E::iter() {
            let block: Block = definer(key.clone())
                .map_err(|e| GenError::failure(e, name_in_module(&key)))?
                .into();
            map.insert(key, block);
        }
        Ok(Self { map })
    }

    pub fn install(&self, universe: &mut Universe) -> Result<BlockProvider<E>, InsertError> {
        for key in E::iter() {
            // TODO: the &* mess should not be required
            universe.insert(name_in_module(&key), BlockDef::new(self[key].clone()))?;
        }
        Ok(Self::using(universe).expect("failed to retrieve names we just inserted??"))
    }

    pub fn using(universe: &Universe) -> Result<BlockProvider<E>, ProviderError>
    where
        E: Eq + Hash + Display,
    {
        let mut found: HashMap<E, URef<BlockDef>> = HashMap::new();
        let mut missing = Vec::new();
        for key in E::iter() {
            let name = name_in_module(&key);
            if let Some(uref) = universe.get(&name) {
                found.insert(key, uref);
            } else {
                missing.push(name);
            }
        }
        if !missing.is_empty() {
            return Err(ProviderError {
                missing: missing.into(),
            });
        }
        Ok(BlockProvider {
            map: E::iter()
                .map(|key| {
                    let block = Block::Indirect(found.remove(&key).unwrap());
                    (key, block)
                })
                .collect(),
        })
    }
}

impl<E: Eq + Hash> Index<E> for BlockProvider<E> {
    type Output = Block;

    fn index(&self, index: E) -> &Self::Output {
        &self.map[&index]
    }
}

#[derive(Clone, Debug, Eq, thiserror::Error, PartialEq)]
#[error("missing block definitions: {missing:?}")] // TODO: use Name's Display within the list
pub struct ProviderError {
    missing: Box<[Name]>,
}

/// An error resulting from “world generation”: failure to calculate/create/place objects
/// (due to bad parameters or unforeseen edge cases), failure to successfully store them
/// in or retrieve them from a [`Universe`], et cetera.
#[derive(Debug, thiserror::Error)]
pub struct GenError {
    // TODO: Replace box with enum for common cases
    #[source]
    detail: InGenError,
    for_object: Option<Name>,
}

impl GenError {
    pub fn failure(error: impl Into<InGenError>, object: Name) -> Self {
        Self {
            detail: error.into(),
            for_object: Some(object),
        }
    }
}

impl fmt::Display for GenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.detail)?;
        if let Some(name) = &self.for_object {
            write!(f, "\nwhile setting up {}", name)?;
        }
        Ok(())
    }
}

impl From<InsertError> for GenError {
    // TODO: Maybe InsertError should just be a variant of GenError?
    fn from(error: InsertError) -> Self {
        GenError {
            for_object: match &error {
                InsertError::AlreadyExists(name) => Some(name.clone()),
            },
            detail: error.into(),
        }
    }
}

/// Aggregation of types of errors that might occur in “world generation”.
///
/// This is distinct from [`GenError`] in that this type is returned from functions
/// _responsible for generation,_ and that type is returned from functions that
/// _manage_ generation — that invoke the first kind and (usually) store its result
/// in the [`Universe`]. This separation is intended to encourage more precise
/// attribution of the source of the error despite implicit conversions, because a
/// “nested” [`GenError`] will be obligated to be wrapped in `InGenError` rather than
/// mistakenly taken as the same level.
///
/// TODO: Work this into a coherent set of error cases rather than purely
/// "I saw one of these once, so add it".
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum InGenError {
    /// Generic error container for unusual situations.
    #[error(transparent)]
    Other(Box<dyn Error>),

    /// Something else needed to be generated and that failed.
    #[error(transparent)]
    Gen(Box<GenError>),

    /// Failed to insert the generated items in the [`Universe`].
    #[error(transparent)]
    Insert(#[from] InsertError),

    /// Failed to find a needed dependency.
    // TODO: Any special handling? Phrase this as "missing dependency"?
    #[error(transparent)]
    Provider(#[from] ProviderError),

    /// Failed during [`Space`](crate::space::Space) manipulation.
    // TODO: Break apart `SetCubeError::EvalBlock` to its contents?
    #[error(transparent)]
    SetCube(#[from] SetCubeError),
}

impl InGenError {
    /// Convert an arbitrary error to `InGenError`.
    pub fn other<E: Error + 'static>(error: E) -> Self {
        Self::Other(Box::new(error))
    }
}

impl From<GenError> for InGenError {
    fn from(error: GenError) -> Self {
        // We need to box this to avoid an unboxed recursive type.
        InGenError::Gen(Box::new(error))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::space::Grid;

    #[test]
    fn gen_error_message() {
        let e = GenError::failure(
            SetCubeError::OutOfBounds {
                modification: Grid::for_block(1),
                space_bounds: Grid::for_block(3),
            },
            "x".into(),
        );
        assert_eq!(
            e.to_string(),
            "Grid(0..1, 0..1, 0..1) is outside of the bounds Grid(0..3, 0..3, 0..3)\nwhile setting up 'x'"
        );
    }

    #[test]
    #[allow(clippy::try_err)]
    fn gen_error_composition() {
        // TODO: this isn't the greatest example situation
        fn a() -> Result<(), GenError> {
            b().map_err(|e| GenError::failure(e, "x".into()))?;
            Ok(())
        }
        fn b() -> Result<(), InGenError> {
            Err(SetCubeError::OutOfBounds {
                modification: Grid::for_block(1),
                space_bounds: Grid::for_block(1),
            })?;
            Ok(())
        }
        let r = a();
        assert!(
            matches!(
                r,
                Err(GenError {
                    detail: InGenError::SetCube(_),
                    for_object: Some(Name::Specific(_)),
                })
            ),
            "got error: {:?}",
            r
        );
    }
}

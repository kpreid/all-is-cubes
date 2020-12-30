// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Storing and accessing definitions of standard blocks in a [`Universe`].
//!
//! An enum implementing [`BlockModule`] defines a set of names, and
//! [`BlockProvider`] assists in ensuring that all of those names are defined
//! and storing or retrieving their block values in a specific [`Universe`].
//!
//! In the future this mechanism may grow to become a dynamic linker/dependency injector
//! by becoming aware of dependencies between “modules”. For now, it's just enough to
//! solve bootstrapping needs.

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Index;
use strum::IntoEnumIterator;

use crate::block::{Block, BlockDef};
use crate::universe::{InsertError, Name, URef, Universe, UniverseIndex};

fn name_in_module<E: BlockModule>(key: &E) -> Name {
    Name::from(format!("{}/{}", E::namespace(), key).as_str())
}

// TODO: document
pub trait DefaultProvision {
    fn default(self) -> Cow<'static, Block>;
}

// TODO: consider replacing Display (presumed derived by strum) with a special trait
pub trait BlockModule: Display + IntoEnumIterator + Eq + Hash + Clone {
    fn namespace() -> &'static str;
}

#[derive(Clone, Debug)]
pub struct BlockProvider<E> {
    /// Guaranteed to contain an entry for every variant of `E` if `E`'s
    /// `strum::IntoEnumIterator` implementation is accurate.
    map: HashMap<E, Cow<'static, Block>>,
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
    E: IntoEnumIterator + Eq + Hash + Clone,
{
    /// Constructs a `BlockProvider` with block definitions computed by the given function.
    pub fn new<F, B>(mut definer: F) -> Self
    where
        F: FnMut(E) -> B,
        B: Into<Cow<'static, Block>>,
    {
        Self {
            map: E::iter()
                .map(|key| {
                    let block: Cow<'static, Block> = definer(key.clone()).into();
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
    pub fn install(&self, universe: &mut Universe) -> Result<BlockProvider<E>, InsertError> {
        for key in E::iter() {
            // TODO: the &* mess should not be required
            universe.insert(
                name_in_module(&key),
                BlockDef::new(self[key].clone().into_owned()),
            )?;
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
                    let block = Cow::Owned(Block::Indirect(found.remove(&key).unwrap()));
                    (key, block)
                })
                .collect(),
        })
    }
}

impl<E: Eq + Hash> Index<E> for BlockProvider<E> {
    type Output = Cow<'static, Block>;

    fn index(&self, index: E) -> &Self::Output {
        &self.map[&index]
    }
}

#[derive(Clone, Debug, Eq, thiserror::Error, PartialEq)]
#[error("missing block definitions: {missing:?}")] // TODO: use Name's Display within the list
pub struct ProviderError {
    missing: Box<[Name]>,
}

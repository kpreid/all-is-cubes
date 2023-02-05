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

use exhaust::Exhaust;

use crate::block::{Block, BlockDef, Primitive};
use crate::space::SetCubeError;
use crate::transaction::ExecuteError;
use crate::universe::{InsertError, Name, URef, Universe, UniverseIndex};
use crate::util::YieldProgress;

fn name_in_module<E: BlockModule>(key: &E) -> Name {
    Name::from(format!("{ns}/{key}", ns = E::namespace()).as_str())
}

/// Allows the use of [`BlockProvider::default`] to construct a [`BlockProvider`]
/// using this type as its set of keys. [`Self::default`] will be called once for
/// each value of [`Self`].
///
/// See [`BlockModule`] for related expectations.
pub trait DefaultProvision {
    /// Returns the default block value to use for the given key. This will typically
    /// have to be a [`Primitive::Atom`] block.
    fn default(self) -> Block;
}

/// Types whose values identify blocks in a set of related blocks, which may be
/// stored in a [`BlockProvider`] or under specific names in a [`Universe`].
///
/// The names of the [`Universe`]'s corresponding [`BlockDef`]s are formed by
/// combining the [`namespace()`](Self::namespace) and `self.to_string()` (the
/// [`Display`] trait implementation).
///
/// Implement this trait for an enum, then use the functions of
/// [`BlockProvider`] to work with the described set of blocks.
///
/// TODO: consider replacing Display with a separate method so as not to presume its meaning
pub trait BlockModule: Exhaust + Display + Eq + Hash + Clone {
    /// A namespace for the members of this module; currently, this should be a
    /// `/`-separated path with no trailing slash, but (TODO:) we should have a
    /// more rigorous namespace scheme for [`Name`]s in future versions.
    fn namespace() -> &'static str;
}

/// TODO: document
#[derive(Clone, Debug)]
pub struct BlockProvider<E> {
    /// Guaranteed to contain an entry for every variant of `E` if `E`'s
    /// [`Exhaust`] implementation is accurate.
    map: HashMap<E, Block>,
}

impl<E> Default for BlockProvider<E>
where
    E: DefaultProvision + Exhaust + Eq + Hash + Clone,
{
    fn default() -> Self {
        Self {
            map: E::exhaust()
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
    ///
    /// This is an async function for the sake of cancellation and optional cooperative
    /// multitasking. It may be blocked on from a synchronous context.
    pub async fn new<F, B>(progress: YieldProgress, mut definer: F) -> Result<Self, GenError>
    where
        F: FnMut(E) -> Result<B, InGenError>,
        B: Into<Block>,
    {
        let count = E::exhaust().count();
        let mut map = HashMap::new();
        for (key, progress) in E::exhaust().zip(progress.split_evenly(count)) {
            let block: Block = definer(key.clone())
                .map_err(|e| GenError::failure(e, name_in_module(&key)))?
                .into();
            map.insert(key, block);
            progress.finish().await;
        }
        Ok(Self { map })
    }

    /// Add the block definitions stored in this [`BlockProvider`] into `universe` as
    /// [`BlockDef`]s, returning a new [`BlockProvider`] whose blocks refer to those
    /// definitions (via [`Primitive::Indirect`]).
    ///
    /// TODO: Migrate this to operate via `UniverseTransaction` instead.
    pub fn install(&self, universe: &mut Universe) -> Result<BlockProvider<E>, InsertError> {
        for key in E::exhaust() {
            // TODO: the &* mess should not be required
            universe.insert(name_in_module(&key), BlockDef::new(self[key].clone()))?;
        }
        Ok(Self::using(universe).expect("failed to retrieve names we just inserted??"))
    }

    /// Obtain the definitions of `E`'s blocks from `universe`, returning a new
    /// [`BlockProvider`] whose blocks refer to those definitions (via
    /// [`Primitive::Indirect`]).
    ///
    /// Returns an error if any of the blocks are not defined in that universe.
    pub fn using(universe: &Universe) -> Result<BlockProvider<E>, ProviderError>
    where
        E: Eq + Hash + Display,
    {
        let mut found: HashMap<E, URef<BlockDef>> = HashMap::new();
        let mut missing = Vec::new();
        for key in E::exhaust() {
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
            map: E::exhaust()
                .map(|key| {
                    let block =
                        Block::from_primitive(Primitive::Indirect(found.remove(&key).unwrap()));
                    (key, block)
                })
                .collect(),
        })
    }

    /// Iterate over the entire contents of this.
    pub fn iter(&self) -> impl Iterator<Item = (E, &Block)> + Send
    where
        E: Sync,
        <E as Exhaust>::Iter: Send,
    {
        E::exhaust().map(|key| {
            let block: &Block = &self.map[&key];
            (key, block)
        })
    }
}

impl<E: Eq + Hash> Index<E> for BlockProvider<E> {
    type Output = Block;

    fn index(&self, index: E) -> &Self::Output {
        &self.map[&index]
    }
}

/// Error when a [`BlockProvider`] could not be created because the definitions of some
/// of its blocks are missing.
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
    /// Wrap an error, that occurred while creating an object, as a [`GenError`] which also
    /// names the object.
    pub fn failure(error: impl Into<InGenError>, object: Name) -> Self {
        Self {
            detail: error.into(),
            for_object: Some(object),
        }
    }
}

impl fmt::Display for GenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Don't include `detail` because that's our `Error::source()`.
        // The assumption is that the cause chain will be walked when printing an error.
        if let Some(name) = &self.for_object {
            write!(f, "An error occurred while generating object {name}")?;
        } else {
            write!(f, "An error occurred while generating an object")?;
        }
        Ok(())
    }
}

impl From<InsertError> for GenError {
    // TODO: Maybe InsertError should just be a variant of GenError?
    fn from(error: InsertError) -> Self {
        GenError {
            for_object: Some(error.name.clone()),
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
    Other(Box<dyn Error + Send + Sync>),

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

    /// Failed during a transaction.
    // TODO: This isn't very coherent; we're just aggregating various errors
    #[error(transparent)]
    Transaction(#[from] ExecuteError),
}

impl InGenError {
    /// Convert an arbitrary error to `InGenError`.
    pub fn other<E: Error + Send + Sync + 'static>(error: E) -> Self {
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
    use crate::block::Resolution::*;
    use crate::math::GridAab;
    use crate::util::assert_send_sync;

    #[test]
    fn errors_are_send_sync() {
        assert_send_sync::<GenError>();
        assert_send_sync::<InGenError>();
    }

    #[test]
    fn gen_error_message() {
        let set_cube_error = SetCubeError::OutOfBounds {
            modification: GridAab::for_block(R1),
            space_bounds: GridAab::for_block(R4),
        };
        let e = GenError::failure(set_cube_error.clone(), "x".into());
        assert_eq!(
            e.to_string(),
            "An error occurred while generating object 'x'",
        );
        let source = Error::source(&e)
            .expect("has source")
            .downcast_ref::<InGenError>()
            .expect("is InGenError");
        assert_eq!(source.to_string(), set_cube_error.to_string());
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
                modification: GridAab::for_block(R1),
                space_bounds: GridAab::for_block(R1),
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
            "got error: {r:?}"
        );
    }
}

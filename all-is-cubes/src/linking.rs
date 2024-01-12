//! Storing and accessing definitions of standard blocks in a [`Universe`].
//!
//! An enum implementing [`BlockModule`] defines a set of names, and
//! [`BlockProvider`] assists in ensuring that all of those names are defined
//! and storing or retrieving their block values in a specific [`Universe`].
//!
//! In the future this mechanism may grow to become a dynamic linker/dependency injector
//! by becoming aware of dependencies between “modules”. For now, it's just enough to
//! solve bootstrapping needs.

use alloc::boxed::Box;
use alloc::vec::Vec;
use core::fmt;
use core::hash::Hash;
use core::ops::Index;

use exhaust::Exhaust;
use hashbrown::HashMap as HbHashMap;

use crate::block::{Block, BlockDef};
use crate::space::SetCubeError;
use crate::transaction::ExecuteError;
use crate::universe::{InsertError, Name, URef, Universe, UniverseTransaction};
use crate::util::{ErrorIfStd, YieldProgress};

#[cfg(doc)]
use crate::block::Primitive;

fn name_in_module<E: BlockModule>(key: &E) -> Name {
    Name::from(format!("{ns}/{key}", ns = E::namespace()))
}

/// Allows the use of [`Provider::default`] to construct a [`Provider`]
/// using this type as its set of keys. [`Self::module_default()`] will be called once for
/// each value of [`Self`].
///
/// See [`BlockModule`] for related expectations.
pub trait DefaultProvision<T> {
    /// Returns the default value to use for the given key.
    fn module_default(self) -> T;
}

/// Types whose values identify blocks in a set of related blocks, which may be
/// stored in a [`BlockProvider`] or under specific names in a [`Universe`].
///
/// The names of the [`Universe`]'s corresponding [`BlockDef`]s are formed by
/// combining the [`namespace()`](Self::namespace) and `self.to_string()` (the
/// [`Display`](fmt::Display) trait implementation).
///
/// Implement this trait for an enum, then use the functions of
/// [`BlockProvider`] to work with the described set of blocks.
///
/// TODO: consider replacing Display with a separate method so as not to presume its meaning
pub trait BlockModule: Exhaust + fmt::Debug + fmt::Display + Eq + Hash + Clone {
    /// A namespace for the members of this module; currently, this should be a
    /// `/`-separated path with no trailing slash, but (TODO:) we should have a
    /// more rigorous namespace scheme for [`Name`]s in future versions.
    fn namespace() -> &'static str;
}

/// An instance of a [`BlockModule`]; a container of a `Block` for every possible `E`.
///
/// TODO: Deprecate and remove this alias.
pub type BlockProvider<E> = Provider<E, Block>;

/// Key-value container of a `V` value for every possible `E`.
#[derive(Clone, Debug)]
pub struct Provider<E, V> {
    /// Guaranteed to contain an entry for every variant of `E` if `E`'s
    /// [`Exhaust`] implementation is accurate.
    map: HbHashMap<E, V>,
}

impl<E, V> Default for Provider<E, V>
where
    E: DefaultProvision<V> + Exhaust + Eq + Hash + Clone,
{
    fn default() -> Self {
        Self {
            map: E::exhaust()
                .map(|key| {
                    let value = DefaultProvision::module_default(key.clone());
                    (key, value)
                })
                .collect(),
        }
    }
}

impl<E, V> Provider<E, V>
where
    E: BlockModule,
{
    /// Constructs a `Provider` with values computed by the given function.
    ///
    /// This is an async function for the sake of cancellation and optional cooperative
    /// multitasking. It may be blocked on from a synchronous context.
    pub async fn new<F>(progress: YieldProgress, mut definer: F) -> Result<Self, GenError>
    where
        F: FnMut(E) -> Result<V, InGenError>,
    {
        let count = E::exhaust().count();
        let mut map = HbHashMap::with_capacity(count);
        for (key, progress) in E::exhaust().zip(progress.split_evenly(count)) {
            let value: V =
                definer(key.clone()).map_err(|e| GenError::failure(e, name_in_module(&key)))?;
            map.insert(key, value);
            progress.finish().await;
        }
        Ok(Self { map })
    }
}

// TODO: Generalize these to non-blocks however makes sense
impl<E: BlockModule> Provider<E, Block> {
    /// Add the block definitions stored in this [`BlockProvider`] into `universe` as
    /// [`BlockDef`]s, returning a new [`BlockProvider`] whose blocks refer to those
    /// definitions (via [`Primitive::Indirect`]).
    pub fn install(&self, txn: &mut UniverseTransaction) -> Result<Self, InsertError> {
        let mut map = HbHashMap::new();
        for key in E::exhaust() {
            let block_def_ref =
                URef::new_pending(name_in_module(&key), BlockDef::new(self[&key].clone()));
            txn.insert_mut(block_def_ref.clone())?;
            map.insert(key, Block::from(block_def_ref));
        }
        Ok(Self { map })
    }

    /// Obtain the definitions of `E`'s blocks from `universe`, returning a new
    /// [`BlockProvider`] whose blocks refer to those definitions (via
    /// [`Primitive::Indirect`]).
    ///
    /// Returns an error if any of the blocks are not defined in that universe.
    pub fn using(universe: &Universe) -> Result<Self, ProviderError>
    where
        E: Eq + Hash + fmt::Display,
    {
        let mut found: HbHashMap<E, URef<BlockDef>> = HbHashMap::new();
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
        Ok(Provider {
            map: E::exhaust()
                .map(|key| {
                    let block = Block::from(found.remove(&key).unwrap());
                    (key, block)
                })
                .collect(),
        })
    }
}

/// These methods do not require `E` to be a [`BlockModule`].
impl<E: Exhaust + fmt::Debug + Clone + Eq + Hash, V> Provider<E, V> {
    /// Alternative to [`Self::new()`] which is neither async nor fallible.
    fn new_sync<F>(mut definer: F) -> Self
    where
        F: FnMut(E) -> V,
    {
        Provider {
            map: E::exhaust()
                .map(|key| (key.clone(), definer(key)))
                .collect(),
        }
    }

    /// Create another [`Provider`] with different keys that map into a subset of
    /// this provider's keys.
    ///
    /// TODO: add a test
    #[must_use]
    pub fn subset<K>(&self, function: impl Fn(K) -> E) -> Provider<K, V>
    where
        K: Exhaust + fmt::Debug + Clone + Eq + Hash,
        V: Clone,
    {
        Provider::new_sync(|key: K| self[function(key)].clone())
    }

    /// Create another [`Provider`] with a modification to each value.
    #[must_use]
    pub fn map<V2>(&self, mut function: impl FnMut(&E, &V) -> V2) -> Provider<E, V2> {
        Provider {
            map: self
                .map
                .iter()
                .map(|(key, value)| (key.clone(), function(key, value)))
                .collect(),
        }
    }

    /// Iterate over the entire contents of this.
    pub fn iter(&self) -> ModuleIter<'_, E, V> {
        ModuleIter {
            key_iter: E::exhaust(),
            map: &self.map,
        }
    }

    #[cfg(test)]
    fn consistency_check(&self) {
        use hashbrown::HashSet;
        let expected_keys: HashSet<E> = E::exhaust().collect();
        let actual_keys: HashSet<E> = self.map.keys().cloned().collect();
        assert_eq!(
            expected_keys, actual_keys,
            "Provider keys are not as expected"
        );
    }
}

impl<E: Eq + Hash, V: PartialEq> PartialEq for Provider<E, V> {
    fn eq(&self, other: &Self) -> bool {
        let Self { map } = self;
        *map == other.map
    }
}
impl<E: Eq + Hash, V: PartialEq> Eq for Provider<E, V> {}

impl<E: Eq + Hash, V> Index<E> for Provider<E, V> {
    type Output = V;

    fn index(&self, index: E) -> &Self::Output {
        &self.map[&index]
    }
}
impl<E: Eq + Hash, V> Index<&E> for Provider<E, V> {
    type Output = V;

    fn index(&self, index: &E) -> &Self::Output {
        &self.map[index]
    }
}

impl<'provider, E: Exhaust + fmt::Debug + Clone + Eq + Hash, V> IntoIterator
    for &'provider Provider<E, V>
{
    type Item = (E, &'provider V);
    type IntoIter = ModuleIter<'provider, E, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Iterator returned by [`Provider::iter()`].
#[allow(missing_debug_implementations)]
pub struct ModuleIter<'provider, E: Exhaust, V> {
    /// Using the `Exhaust` iterator instead of the `HashMap` iterator guarantees a deterministic
    /// iteration order. (We don't currently publicly promise that, though.)
    key_iter: <E as Exhaust>::Iter,
    map: &'provider HbHashMap<E, V>,
}

impl<'provider, E: Exhaust + Eq + Hash, V> Iterator for ModuleIter<'provider, E, V> {
    type Item = (E, &'provider V);

    fn next(&mut self) -> Option<Self::Item> {
        self.key_iter.next().map(|key| {
            let value: &V = &self.map[&key];
            (key, value)
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.key_iter.size_hint()
    }
}

impl<E: Exhaust + Eq + Hash, V> ExactSizeIterator for ModuleIter<'_, E, V> where
    E::Iter: ExactSizeIterator
{
}

/// Error when a [`Provider`] could not be created because the definitions of some
/// of its members are missing.
#[derive(Clone, Debug, Eq, displaydoc::Display, PartialEq)]
#[displaydoc("module definitions missing from universe: {missing:?}")] // TODO: use Name's Display within the list
pub struct ProviderError {
    missing: Box<[Name]>,
}

#[cfg(feature = "std")]
impl std::error::Error for ProviderError {}

/// An error resulting from “world generation”: failure to calculate/create/place objects
/// (due to bad parameters or unforeseen edge cases), failure to successfully store them
/// in or retrieve them from a [`Universe`], et cetera.
#[derive(Debug)]
pub struct GenError {
    #[cfg_attr(not(feature = "std"), allow(dead_code))]
    detail: InGenError,
    for_object: Option<Name>,
}

#[cfg(feature = "std")]
impl std::error::Error for GenError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.detail)
    }
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

impl From<ExecuteError> for GenError {
    // TODO: Ideally, this works only for `UniverseTransaction` errors, which relate to
    // specific members, but we don't have a static distinction between different transactions'
    // errors yet.
    fn from(error: ExecuteError) -> Self {
        GenError {
            for_object: None,
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
#[derive(Debug)]
#[non_exhaustive]
pub enum InGenError {
    /// Generic error container for unusual situations.
    Other(Box<dyn ErrorIfStd + Send + Sync>),

    /// Something else needed to be generated and that failed.
    Gen(Box<GenError>),

    /// Failed to insert the generated items in the [`Universe`].
    Insert(InsertError),

    /// Failed to find a needed dependency.
    // TODO: Any special handling? Phrase this as "missing dependency"?
    Provider(ProviderError),

    /// Failed during [`Space`](crate::space::Space) manipulation.
    SetCube(SetCubeError),

    /// Failed during a transaction.
    // TODO: This isn't very coherent; we're just aggregating various errors
    Transaction(ExecuteError),
}

impl InGenError {
    /// Convert an arbitrary error to `InGenError`.
    #[cfg_attr(not(feature = "std"), doc(hidden))]
    pub fn other<E: ErrorIfStd + Send + Sync + 'static>(error: E) -> Self {
        Self::Other(Box::new(error))
    }
}

#[cfg(feature = "std")]
impl std::error::Error for InGenError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            InGenError::Other(e) => e.source(),
            InGenError::Gen(e) => e.source(),
            InGenError::Insert(e) => e.source(),
            InGenError::Provider(e) => e.source(),
            InGenError::SetCube(e) => e.source(),
            InGenError::Transaction(e) => e.source(),
        }
    }
}

impl fmt::Display for InGenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InGenError::Other(e) => e.fmt(f),
            InGenError::Gen(e) => e.fmt(f),
            InGenError::Insert(e) => e.fmt(f),
            InGenError::Provider(e) => e.fmt(f),
            InGenError::SetCube(e) => e.fmt(f),
            InGenError::Transaction(e) => e.fmt(f),
        }
    }
}

impl From<GenError> for InGenError {
    fn from(error: GenError) -> Self {
        // We need to box this to avoid an unboxed recursive type.
        InGenError::Gen(Box::new(error))
    }
}
impl From<InsertError> for InGenError {
    fn from(error: InsertError) -> Self {
        InGenError::Insert(error)
    }
}
impl From<ProviderError> for InGenError {
    fn from(error: ProviderError) -> Self {
        InGenError::Provider(error)
    }
}
impl From<SetCubeError> for InGenError {
    fn from(error: SetCubeError) -> Self {
        InGenError::SetCube(error)
    }
}
impl From<ExecuteError> for InGenError {
    fn from(error: ExecuteError) -> Self {
        InGenError::Transaction(error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Quote, Resolution::*};
    use crate::content::make_some_blocks;
    use crate::math::GridAab;
    use crate::transaction::{self, Transaction as _};
    use crate::util::assert_send_sync;
    use alloc::string::ToString;
    use std::error::Error;

    #[derive(Exhaust, Clone, Debug, Eq, Hash, PartialEq)]
    enum Key {
        A,
        B,
        C,
    }
    impl fmt::Display for Key {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{self:?}")
        }
    }
    impl BlockModule for Key {
        fn namespace() -> &'static str {
            "test-key"
        }
    }

    fn test_provider() -> ([Block; 3], BlockProvider<Key>) {
        let blocks = make_some_blocks();
        let provider = BlockProvider::new_sync(|k: Key| match k {
            Key::A => blocks[0].clone(),
            Key::B => blocks[1].clone(),
            Key::C => blocks[2].clone(),
        });
        provider.consistency_check();

        (blocks, provider)
    }

    #[test]
    fn provider_install() {
        let mut universe = Universe::new();
        let (_, provider) = test_provider();

        let mut txn = UniverseTransaction::default();
        let installed = provider.install(&mut txn).unwrap();
        txn.execute(&mut universe, &mut transaction::no_outputs)
            .unwrap();

        assert_eq!(installed, BlockProvider::using(&universe).unwrap());
    }

    #[test]
    fn provider_subset() {
        let (_, p1) = test_provider();
        let p2 = p1.subset(|x: bool| if x { Key::A } else { Key::B });
        p2.consistency_check();
        assert_eq!(p1[Key::A], p2[true]);
        assert_eq!(p1[Key::B], p2[false]);
    }

    #[test]
    fn provider_map() {
        let (_, p1) = test_provider();
        let p2 = p1.map(|_, block| block.clone().with_modifier(Quote::default()));
        p2.consistency_check();
        assert_eq!(
            p1[Key::A].clone().with_modifier(Quote::default()),
            p2[Key::A],
        );
    }

    #[test]
    fn errors_are_send_sync() {
        assert_send_sync::<GenError>();
        assert_send_sync::<InGenError>();
    }

    #[test]
    #[cfg(feature = "std")] // Error::source only exists on std
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

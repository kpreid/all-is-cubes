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
use core::error::Error;
use core::fmt;
use core::hash::Hash;
use core::ops::Index;

use exhaust::Exhaust;
use hashbrown::HashMap as HbHashMap;

use crate::block::{self, Block, BlockDef};
use crate::space::{self, SetCubeError, SpaceTransaction};
use crate::transaction::ExecuteError;
use crate::universe::{
    self, Handle, InsertError, Name, ReadTicket, Universe, UniverseTransaction, VisitHandles,
};
use crate::util::YieldProgress;

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
    /// multitasking. It may be blocked on from a synchronous context (but if that is the
    /// only use, consider calling [`Provider::new_sync()`] instead).
    pub async fn new<F>(progress: YieldProgress, mut definer: F) -> Result<Self, GenError>
    where
        F: FnMut(E) -> Result<V, InGenError>,
    {
        let count = E::exhaust().count();
        let mut map = HbHashMap::with_capacity(count);
        #[expect(
            clippy::shadow_unrelated,
            reason = "https://github.com/rust-lang/rust-clippy/issues/11827"
        )]
        for (key, progress) in E::exhaust().zip(progress.split_evenly(count)) {
            match definer(key.clone()) {
                Ok(value) => {
                    map.insert(key, value);
                    progress.finish().await;
                }
                Err(e) => return Err(GenError::failure(e, name_in_module(&key))),
            }
        }
        Ok(Self { map })
    }
}

// TODO: Generalize these to non-blocks however makes sense
impl<E: BlockModule> Provider<E, Block> {
    /// Constructs a [`BlockProvider`] with [`BlockDef`]s whose block values are computed by the
    /// given function.
    ///
    /// The module itself is passed to `definer`, which may be used to create
    /// relationships among the blocks (e.g. one having the behavior of turning into another).
    /// Attempting to read those handles will necessarily fail.
    //---
    // TODO: This has to exist, but is an awkward shape and block-specific.
    // Figure out what the actually good API looks like.
    pub async fn new_installed_cyclic<F>(
        progress: YieldProgress,
        txn: &mut UniverseTransaction,
        mut definer: F,
    ) -> Result<Self, GenError>
    where
        F: FnMut(&Provider<E, Block>, &mut UniverseTransaction, E) -> Result<Block, InGenError>,
    {
        let module: Self = Self::new_sync(|key| {
            let block_def_handle: Handle<BlockDef> = txn
                .insert_without_value(name_in_module(&key))
                .expect("module failed to produce distinct keys");
            Block::from(block_def_handle)
        });

        let count = module.map.len();
        #[expect(
            clippy::shadow_unrelated,
            reason = "https://github.com/rust-lang/rust-clippy/issues/11827"
        )]
        for (key, progress) in E::exhaust().zip(progress.split_evenly(count)) {
            match definer(&module, txn, key.clone()) {
                Ok(block_value) => {
                    let block::Primitive::Indirect(block_def_handle) = module[key].primitive()
                    else {
                        unreachable!()
                    };
                    txn.set_pending_value(
                        block_def_handle,
                        BlockDef::new(
                            txn.read_ticket().expect_may_fail(), // TODO: should caller provide ticket to rest of universe which we merge somehow? or should we stop expecting successful evaluations while building modules/txns, entirely
                            block_value,
                        ),
                    );
                    progress.finish().await;
                }
                Err(e) => return Err(GenError::failure(e, name_in_module(&key))),
            }
        }
        Ok(module)
    }

    /// Add the block definitions stored in this [`BlockProvider`] into `universe` as
    /// [`BlockDef`]s, returning a new [`BlockProvider`] whose blocks refer to those
    /// definitions (via [`Primitive::Indirect`]).
    ///
    /// The given `read_ticket` should be sufficient for evaluating the blocks
    /// in `self`.
    pub fn install(
        &self,
        read_ticket: ReadTicket<'_>,
        txn: &mut UniverseTransaction,
    ) -> Result<Self, InsertError> {
        // The non-generic part of the code.
        #[inline(never)]
        fn create_block_def_and_indirect(
            read_ticket: ReadTicket<'_>,
            txn: &mut UniverseTransaction,
            name: Name,
            block: &Block,
        ) -> Result<Block, InsertError> {
            let value = BlockDef::new(read_ticket.with_transaction(txn), block.clone());
            let block_def_handle = txn.insert_mut(name, value)?;
            let indirect_block = Block::from(block_def_handle);
            Ok(indirect_block)
        }

        let mut map = HbHashMap::with_capacity(self.map.len());
        for key in E::exhaust() {
            let indirect_block =
                create_block_def_and_indirect(read_ticket, txn, name_in_module(&key), &self[&key])?;
            map.insert(key, indirect_block);
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
        let mut found: HbHashMap<E, Handle<BlockDef>> = HbHashMap::new();
        let mut missing = Vec::new();
        for key in E::exhaust() {
            let name = name_in_module(&key);
            if let Some(handle) = universe.get(&name) {
                found.insert(key, handle);
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
            map: found
                .into_iter()
                .map(|(key, handle)| {
                    let block = Block::from(handle);
                    (key, block)
                })
                .collect(),
        })
    }
}

/// These methods do not require `E` to be a [`BlockModule`].
impl<E: Exhaust + fmt::Debug + Clone + Eq + Hash, V> Provider<E, V> {
    /// Alternative to [`Self::new()`] which is neither async nor fallible.
    pub fn new_sync<F>(mut definer: F) -> Self
    where
        F: FnMut(E) -> V,
    {
        Provider {
            map: E::exhaust().map(|key| (key.clone(), definer(key))).collect(),
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

impl<E: Eq + Hash + VisitHandles, V: VisitHandles> VisitHandles for Provider<E, V> {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self { map } = self;
        for (key, value) in map {
            key.visit_handles(visitor);
            value.visit_handles(visitor);
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Iterator returned by [`Provider::iter()`].
#[expect(missing_debug_implementations)]
pub struct ModuleIter<'provider, E: Exhaust, V> {
    /// Using the `Exhaust` iterator instead of the `HashMap` iterator guarantees a deterministic
    /// iteration order. (We don't currently publicly promise that, though.)
    key_iter: exhaust::Iter<E>,
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

impl<E, V> ExactSizeIterator for ModuleIter<'_, E, V>
where
    E: Exhaust + Eq + Hash,
    exhaust::Iter<E>: ExactSizeIterator,
{
}

/// Error when a [`Provider`] could not be created because the definitions of some
/// of its members are missing.
#[derive(Clone, Debug, Eq, displaydoc::Display, PartialEq)]
#[displaydoc("module definitions missing from universe: {missing:?}")] // TODO: use Name's Display within the list
pub struct ProviderError {
    missing: Box<[Name]>,
}

impl Error for ProviderError {}

/// An error resulting from “world generation”.
///
/// May be failure to calculate/create/place objects
/// (due to bad parameters or unforeseen edge cases),
/// failure to successfully store them in or retrieve them from a [`Universe`],
/// et cetera.
///
/// A [`GenError`] contains an [`InGenError`] and additionally specifies which universe
/// member was to be generated but failed.
#[derive(Debug)]
pub struct GenError {
    detail: InGenError,
    for_object: Option<Name>,
}

impl Error for GenError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
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

impl From<ExecuteError<UniverseTransaction>> for GenError {
    // TODO: Ideally, this works only for `UniverseTransaction` errors, which relate to
    // specific members, but we don't have a static distinction between different transactions'
    // errors yet.
    fn from(error: ExecuteError<UniverseTransaction>) -> Self {
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
    Other(Box<dyn Error + Send + Sync>),

    /// Something else needed to be generated and that failed.
    Gen(Box<GenError>), // boxed due to being a recursive type

    /// Failed to insert the generated items in the [`Universe`].
    Insert(InsertError),

    /// Failed to find a needed dependency.
    // TODO: Any special handling? Phrase this as "missing dependency"?
    Provider(ProviderError),

    /// Failed during [`Space`](crate::space::Space) construction.
    Space(space::builder::Error),

    /// Failed during [`Space`](crate::space::Space) manipulation.
    SetCube(SetCubeError),

    /// Failed during a transaction executed as part of generation.
    UniverseTransaction(ExecuteError<UniverseTransaction>),

    /// Failed during a transaction executed as part of generation.
    SpaceTransaction(ExecuteError<SpaceTransaction>),
}

impl InGenError {
    /// Convert an arbitrary error to `InGenError`.
    #[cfg_attr(not(feature = "std"), doc(hidden))]
    pub fn other<E: Error + Send + Sync + 'static>(error: E) -> Self {
        Self::Other(Box::new(error))
    }
}

impl Error for InGenError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            InGenError::Other(e) => e.source(),
            InGenError::Gen(e) => e.source(),
            InGenError::Insert(e) => e.source(),
            InGenError::Provider(e) => e.source(),
            InGenError::Space(e) => e.source(),
            InGenError::SetCube(e) => e.source(),
            InGenError::UniverseTransaction(e) => e.source(),
            InGenError::SpaceTransaction(e) => e.source(),
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
            InGenError::Space(e) => e.fmt(f),
            InGenError::SetCube(e) => e.fmt(f),
            InGenError::UniverseTransaction(e) => e.fmt(f),
            InGenError::SpaceTransaction(e) => e.fmt(f),
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
impl From<space::builder::Error> for InGenError {
    fn from(error: space::builder::Error) -> Self {
        InGenError::Space(error)
    }
}
impl From<SetCubeError> for InGenError {
    fn from(error: SetCubeError) -> Self {
        InGenError::SetCube(error)
    }
}
impl From<crate::content::load_image::BlockFromImageError> for InGenError {
    fn from(error: crate::content::load_image::BlockFromImageError) -> Self {
        // TODO: give this its own variant?
        InGenError::Other(Box::new(error))
    }
}
impl From<ExecuteError<UniverseTransaction>> for InGenError {
    fn from(error: ExecuteError<UniverseTransaction>) -> Self {
        InGenError::UniverseTransaction(error)
    }
}
impl From<ExecuteError<SpaceTransaction>> for InGenError {
    fn from(error: ExecuteError<SpaceTransaction>) -> Self {
        InGenError::SpaceTransaction(error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{AIR, Quote, Resolution::*};
    use crate::content::make_some_blocks;
    use crate::math::GridAab;
    use crate::transaction::Transactional as _;
    use crate::util::assert_conditional_send_sync;

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

        // TODO: double-unwrap in this case is a bad sign (InsertError != UniverseConflict)
        let installed = universe
            .transact(|txn, u| Ok(provider.install(u.read_ticket(), txn)))
            .unwrap()
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
    fn provider_eq() {
        let (_, p1) = test_provider();
        let (_, p2) = test_provider();
        assert_eq!(p1, p2);
        assert_ne!(
            p1,
            p2.map(|key, block| if *key == Key::B { AIR } else { block.clone() })
        );
    }

    #[test]
    fn errors_are_send_sync() {
        assert_conditional_send_sync::<GenError>();
        assert_conditional_send_sync::<InGenError>();
    }

    #[test]
    fn gen_error_message() {
        use alloc::string::ToString;

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
    #[expect(clippy::try_err)]
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

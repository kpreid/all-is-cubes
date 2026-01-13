//! Module defining various traits and impls relating to [`Universe`] containing different
//! types of members.
//!
//! This module is not public and that is part of the protection of several items
//! inside it (the public-in-private trick).

use alloc::boxed::Box;
use core::any::Any;
use core::{fmt, hash};

use bevy_ecs::prelude as ecs;
use bevy_ecs::query::QueryData;

use crate::block::BlockDef;
use crate::character::Character;
use crate::sound::SoundDef;
use crate::space::Space;
use crate::tag::TagDef;
use crate::transaction;
use crate::universe::{
    self, ErasedHandle, Handle, InsertError, Name, Universe, handle::HandlePtr, universe_txn as ut,
};

// -------------------------------------------------------------------------------------------------
// Traits

/// Not-externally-implementable supertrait for [`UniverseMember`] to make it sealed and hide
/// implementation details.
pub(crate) trait SealedMember: Sized {
    /// Components to spawn when inserting a member of this type into a universe.
    type Bundle: ecs::Bundle;

    type ReadQueryData: bevy_ecs::query::ReadOnlyQueryData + Clone;

    /// Register `VisitableComponents` and anything else needed.
    fn register_all_member_components(world: &mut ecs::World);

    /// Constructs `Self::Read` from a value that has not yet been inserted into the
    /// [`Universe`].
    fn read_from_standalone(value: &Self) -> Self::Read<'_>
    where
        Self: UniverseMember;

    /// Constructs `Self::Read` from query data.
    fn read_from_query<'r>(
        data: <Self::ReadQueryData as QueryData>::Item<'r, '_>,
    ) -> Self::Read<'r>
    where
        Self: UniverseMember;

    /// Constructs `Self::Read` from an [`ecs::EntityRef`].
    /// This may be used when queries are not feasible or not needed.
    ///
    /// Returns [`None`] when the entity does not have the components it should have.
    /// No other validation is guaranteed to be performed.
    #[expect(dead_code, reason = "intermittently useful in tests")]
    fn read_from_entity_ref(entity: ecs::EntityRef<'_>) -> Option<Self::Read<'_>>
    where
        Self: UniverseMember;

    /// Converts `Self` (the form independent of a universe) into a bundle to be part of a
    /// newly spawned entity.
    fn into_bundle(value: Box<Self>) -> Self::Bundle;
}

/// Trait for every type which can be a named member of a universe and be referred to by
/// [`Handle`]s.
///
/// This trait provides no operations itself, but is a bound on functions of [`Universe`],
/// [`Handle`], and [`UniverseTransaction`][ut::UniverseTransaction].
//---
// TODO: Give this trait a better name. Some errors refer to this concept as "object", and
// in a purely abstract architecture sense it’s a sort of “entity” but that conflicts with
// “is a bevy_ecs entity”.
#[expect(private_bounds)]
pub trait UniverseMember: Sized + 'static + fmt::Debug + SealedMember + MemberBoilerplate {
    /// Type returned by [`Handle::<T>::read()`][Handle::read()] which is the way to read the
    /// `T` value after it has been inserted into the [`Universe`].
    //---
    // For serialization, this type must serialize to the universe member type’s serialization
    // schema. (The simplest way to do this is for it to be `&T`, for example.)
    type Read<'ticket>: Clone;
}

/// Trait for operations on [`Handle`]s and queries that must be implemented for each member type.
/// This trait is implemented by macro and not customized for each member type.
pub(in crate::universe) trait MemberBoilerplate: Sized {
    /// Generic constructor for [`AnyHandle`].
    fn into_any_handle(handle: Handle<Self>) -> AnyHandle;

    /// Generic constructor for [`AnyPending`].
    fn into_any_pending(handle: Handle<Self>, value: Option<Box<Self>>) -> AnyPending;

    fn member_read_query_state(
        queries: &MemberReadQueryStates,
    ) -> &ecs::QueryState<Self::ReadQueryData>
    where
        Self: UniverseMember;

    fn member_mutation_query_state(
        queries: &mut MemberWriteQueryStates,
    ) -> &mut ecs::QueryState<<<Self as universe::Transactional>::Transaction as universe::TransactionOnEcs>::WriteQueryData>
    where
        Self: UniverseMember + transaction::Transactional<Transaction: universe::TransactionOnEcs>;

    /// Fetch the query for for this member tyype from [`MemberReadQueries`].
    /// May fail if such a query was not provided.
    fn member_read_query<'q, 'w, 's>(
        queries: &'q MemberReadQueries<'w, 's>,
    ) -> Option<&'q ecs::Query<'w, 's, Self::ReadQueryData>>
    where
        Self: UniverseMember;
}

// -------------------------------------------------------------------------------------------------

/// Use this macro to implement [`UniverseMember`] and related traits for any member type whose only
/// ECS component (that are read through [`Handle`]s) is itself.
///
/// TODO(ecs): Remove all uses of this so that public types are not component types.
macro_rules! impl_universe_member_for_single_component_type {
    ($member_type:path) => {
        impl $crate::universe::SealedMember for $member_type {
            type Bundle = (Self,);
            type ReadQueryData = &'static Self;

            fn register_all_member_components(world: &mut ::bevy_ecs::world::World) {
                $crate::universe::VisitableComponents::register::<$member_type>(world);
            }

            fn read_from_standalone(
                value: &Self,
            ) -> <Self as $crate::universe::UniverseMember>::Read<'_> {
                value
            }

            fn read_from_query<'r>(
                data: <Self::ReadQueryData as ::bevy_ecs::query::QueryData>::Item<'r, '_>,
            ) -> <Self as $crate::universe::UniverseMember>::Read<'r> {
                data
            }

            fn read_from_entity_ref(
                entity: ::bevy_ecs::world::EntityRef<'_>,
            ) -> Option<<Self as $crate::universe::UniverseMember>::Read<'_>> {
                entity.get::<$member_type>()
            }

            fn into_bundle(value: ::alloc::boxed::Box<Self>) -> Self::Bundle {
                (*value,)
            }
        }

        impl $crate::universe::UniverseMember for $member_type {
            // TODO(ecs): when we have multiple components, this will need to be defined
            // separately for each member type.
            type Read<'ticket> = &'ticket $member_type;
        }
    };
}
pub(crate) use impl_universe_member_for_single_component_type;

/// Generates boilerplate impls for a specific Universe member type.
macro_rules! impl_universe_for_member {
    ($member_type:ident, $table:ident) => {
        impl MemberBoilerplate for $member_type {
            fn into_any_handle(handle: Handle<Self>) -> AnyHandle {
                AnyHandle::$member_type(handle)
            }

            fn into_any_pending(handle: Handle<Self>, value: Option<Box<Self>>) -> AnyPending {
                AnyPending::$member_type { handle, value }
            }

            fn member_read_query_state(
                queries: &MemberReadQueryStates,
            ) -> &ecs::QueryState<<Self as SealedMember>::ReadQueryData> {
                &queries.$table
            }

            fn member_mutation_query_state(
                queries: &mut MemberWriteQueryStates,
            ) -> &mut ecs::QueryState<<<Self as universe::Transactional>::Transaction as universe::TransactionOnEcs>::WriteQueryData> {
                &mut queries.$table
            }

            fn member_read_query<'q, 'w, 's>(
                queries: &'q MemberReadQueries<'w, 's>,
            ) -> Option<&'q ecs::Query<'w, 's, <Self as SealedMember>::ReadQueryData>>
            {
                queries.$table.as_ref()
            }
        }


        impl ut::UTransactional for $member_type {
            fn bind(
                target: Handle<Self>,
                transaction: Self::Transaction,
            ) -> ut::UniverseTransaction {
                ut::UniverseTransaction::from(AnyTransaction::$member_type(
                    ut::TransactionInUniverse {
                        target,
                        transaction,
                    },
                ))
            }
        }
    };
}

/// Generates data structures which cover all universe member types.
macro_rules! member_enums_and_impls {
    ( $( ($member_type:ident, $table_name:ident), )* ) => {
        impl Universe {
            /// Iterate over all members of this universe.
            ///
            /// The iteration order is currently not guaranteed.
            pub fn iter(&self) -> impl Iterator<Item = AnyHandle> {
                core::iter::empty()
                $(
                    .chain(self.iter_by_type::<$member_type>()
                        .map(|(_name, handle)| AnyHandle::$member_type(handle)))
                )*
            }

            pub(in crate::universe) fn register_all_member_components(world: &mut ecs::World) {
                $(
                    <$member_type as SealedMember>::register_all_member_components(world);
                )*
            }
        }

        /// Holds any one of the concrete [`Handle<T>`](Handle) types that can be in a [`Universe`].
        ///
        /// See also [`ErasedHandle`], which is implemented by `Handle`s rather than owning one.
        /// This type dereferences to `dyn ErasedHandle` to provide all the operations that
        /// trait does.
        #[derive(Clone, Debug)]
        #[non_exhaustive]
        #[allow(missing_docs, reason = "variant meanings are obvious from name")]
        pub enum AnyHandle {
            $( $member_type(Handle<$member_type>), )*
        }

        impl AnyHandle {
            /// For debugging — not guaranteed to be stable.
            #[doc(hidden)] // TODO: not great API, but used by all-is-cubes-port
            pub fn member_type_name(&self) -> &'static str {
                 match self {
                    $( AnyHandle::$member_type(_) => {
                        core::any::type_name::<$member_type>()
                    } )*
                }
            }

            /// Used as part of checking whether a deserialized Universe contains bad handles.
            #[cfg(feature = "save")]
            pub(crate) fn not_still_deserializing(
                &self,
                read_ticket: $crate::universe::ReadTicket<'_>,
            ) -> bool {
                match self {
                    $( AnyHandle::$member_type(h) => {
                        !matches!(
                            h.read(read_ticket),
                            Err(handle_error) if matches!(
                                handle_error.kind,
                                $crate::universe::HandleErrorKind::NotReady,
                            )
                        )
                    } )*
                }
            }

            /// For deleting members.
            pub(in crate::universe) fn set_state_to_gone(
                &self,
                reason: $crate::universe::GoneReason,
            ) -> () {
                match self {
                    $( AnyHandle::$member_type(handle) => handle.set_state_to_gone(reason), )*
                }
            }

            pub(in crate::universe) fn has_strong_handles(
                &self,
            ) -> bool {
                match self {
                    $( AnyHandle::$member_type(handle) => handle.has_strong_handles(), )*
                }
            }
        }

        impl core::ops::Deref for AnyHandle {
            type Target = dyn ErasedHandle;

            fn deref(&self) -> &Self::Target {
                match self {
                    $( Self::$member_type(r) => r, )*
                }
            }
        }

        $( impl_universe_for_member!($member_type, $table_name); )*

        /// Polymorphic container for [`TransactionInUniverse`] which is
        /// used to store transactions in a [`UniverseTransaction`].
        #[derive(Clone, Default, PartialEq)]
        #[non_exhaustive]
        pub(in crate::universe) enum AnyTransaction {
            #[default]
            Noop,
            $( $member_type(ut::TransactionInUniverse<$member_type>), )*
        }

        impl AnyTransaction {
            pub(in crate::universe) fn target_erased(&self) -> Option<&dyn ErasedHandle> {
                use AnyTransaction::*;
                match self {
                    Noop => None,
                    $( $member_type(t) => Some(&t.target), )*
                }
            }

            /// Returns the transaction out of the [`TransactionInUniverse`] wrapper.
            pub(in crate::universe) fn transaction_as_debug(&self) -> &dyn fmt::Debug {
                use AnyTransaction::*;
                match self {
                    Noop => &"AnyTransaction::Noop",
                    $( Self::$member_type(t) => &t.transaction, )*
                }
            }
        }

        impl transaction::Transaction for AnyTransaction {
            type Target = Universe;
            type Context<'a> = ();
            type CommitCheck = ut::AnyTransactionCheck;
            type Output = transaction::NoOutput;
            type Mismatch = AnyTransactionMismatch;

            fn check(
                &self,
                universe: &Universe, (): Self::Context<'_>
            ) -> Result<Self::CommitCheck, Self::Mismatch> {
                Ok::<Self::CommitCheck, Self::Mismatch>(match self {
                    Self::Noop => Box::new(()),
                    $(
                        Self::$member_type(t) => Box::new(t.check(universe, ())
                            .map_err(AnyTransactionMismatch::$member_type)?),
                    )*
                })
            }

            fn commit(
                self,
                universe: &mut Universe,
                check: Self::CommitCheck,
                outputs: &mut dyn FnMut(Self::Output),
            ) -> Result<(), transaction::CommitError> {
                match self {
                    Self::Noop => Ok(()),
                    $( Self::$member_type(t) => ut::anytxn_commit_helper(t, universe, check, outputs), )*
                }
            }
        }

        impl transaction::Merge for AnyTransaction {
            type MergeCheck = ut::AnyTransactionCheck;
            type Conflict = AnyTransactionConflict;

            fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
                match (self, other) {
                    (Self::Noop, _) => Ok(Box::new(())),
                    (_, Self::Noop) => Ok(Box::new(())),
                    $( (Self::$member_type(t1), Self::$member_type(t2)) => {
                        let check =
                            t1.check_merge(t2)
                                .map_err(AnyTransactionConflict::$member_type)?;
                        Ok(Box::new(check))
                    } )*
                    (_, _) => Err(AnyTransactionConflict::TypeMismatch),
                }
            }

            fn commit_merge(&mut self, other: Self, check: Self::MergeCheck) {
                match (self, other) {
                    (_t1, Self::Noop) => {},
                    (t1 @ Self::Noop, t2) => *t1 = t2,
                    $( (Self::$member_type(t1), Self::$member_type(t2)) => {
                        ut::anytxn_merge_helper(t1, t2, check)
                    } )*
                    (_, _) => panic!("Mismatched transaction target types"),
                }
            }
        }

        /// [`AnyTransaction`] precondition errors.
        #[derive(Clone, Debug, Eq, PartialEq)]
        #[non_exhaustive]
        pub(in crate::universe) enum AnyTransactionMismatch {
            $(
                $member_type(
                    <
                        <$member_type as transaction::Transactional>::Transaction
                        as transaction::Transaction
                    >::Mismatch
                ),
            )*
        }

        /// [`AnyTransaction`] conflict errors.
        #[derive(Clone, Debug, Eq, PartialEq)]
        #[non_exhaustive]
        pub(in crate::universe) enum AnyTransactionConflict {
            TypeMismatch,
            $(
                $member_type(
                    <
                        <$member_type as transaction::Transactional>::Transaction
                        as transaction::Merge
                    >::Conflict
                ),
            )*
        }

            impl core::error::Error for AnyTransactionMismatch {
                fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
                    match self {
                        $( Self::$member_type(e) => Some(e), )*
                    }
                }
            }

            impl core::error::Error for AnyTransactionConflict {
                fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
                    match self {
                        Self::TypeMismatch => None,
                        $( Self::$member_type(e) => Some(e), )*
                    }
                }
            }

        impl fmt::Display for AnyTransactionMismatch {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $( Self::$member_type(e) => e.fmt(f), )*
                }
            }
        }
        impl fmt::Display for AnyTransactionConflict {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Self::TypeMismatch => write!(f, "Mismatched transaction target types"),
                    $( Self::$member_type(e) => e.fmt(f), )*
                }
            }
        }

        /// Holds a pending handle and its value, to be inserted into a universe.
        ///
        /// There is no validation that the handle is in fact pending.
        /// This type is used only within [`crate::universe::universe_txn::MemberTxn`].
        pub(in crate::universe) enum AnyPending {
            $(
                $member_type {
                    handle: Handle<$member_type>,

                    /// Handle’s value, if it has been provided yet.
                    /// Insertion always fails if `None`.
                    ///
                    /// Boxed so that the size of the enum isn't the size of the largest member type
                    value: Option<Box<$member_type>>,
                },
            )*
        }

        impl AnyPending {
            pub(crate) fn handle(&self) -> &dyn ErasedHandle {
                match self {
                    $( AnyPending::$member_type { handle, .. } => handle, )*
                }
            }

            /// Returns whether this does not yet belong to a universe and can start
            /// doing so. Used by [`MemberTxn::check()`].
            ///
            /// See [`Handle::check_upgrade_pending()`] for more information.
            pub(crate) fn check_upgrade_pending(
                &self,
                read_ticket_for_self: $crate::universe::ReadTicket<'_>,
                universe_id: $crate::universe::UniverseId,
            ) -> Result<(), $crate::universe::InsertError> {
                match self {
                    $(
                        Self::$member_type { handle, value: _ } => {
                            handle.check_upgrade_pending(read_ticket_for_self, universe_id)
                        }
                    )*
                }
            }

            /// Returns a reference to a `dyn Any` whose underlying type is the owned
            /// `Option<Box<T>>`.
            pub(crate) fn value_as_any_option_box(&self) -> &dyn Any {
                match self {
                    $(
                        Self::$member_type { handle: _, value } => value,
                    )*
                }
            }
            /// Returns a reference to a `dyn Any` whose underlying type is the owned
            /// `Option<Box<T>>`.
            pub(crate) fn value_as_mut_any_option_box(&mut self) -> &mut dyn Any {
                match self {
                    $(
                        Self::$member_type { handle: _, value } => value,
                    )*
                }
            }

            /// Used by [`MemberTxn::commit()`] to perform inserts.
            pub(crate) fn insert_pending_into_universe(
                self,
                universe: &mut Universe
            ) -> Result<(), InsertError> {
                match self {
                    $(
                        AnyPending::$member_type { handle, value } => {
                            if let Some(value) = value {
                                handle.insert_pending_into_universe(universe, value)
                            } else {
                                Err(InsertError {
                                    name: handle.name(),
                                    kind: $crate::universe::InsertErrorKind::ValueMissing,
                                })
                            }

                        }
                    )*
                }
            }
        }

        impl fmt::Debug for AnyPending {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:?} = ", self.handle())?;
                match self {
                    $(
                        Self::$member_type { handle: _, value: Some(value) } => {
                            value.fmt(f)?;
                        },
                    )*
                    $( Self::$member_type { handle: _, value: None } )|* => {
                        write!(f, "<value not set>")?;
                    }
                }
                Ok(())
            }
        }

        impl PartialEq for AnyPending {
            fn eq(&self, rhs: &Self) -> bool {
                match (self, rhs) {
                    $(
                        (
                            AnyPending::$member_type { handle: h1, value: _ },
                            AnyPending::$member_type { handle: h2, value: _ },
                        ) => {
                            // It should be impossible for two `AnyPending`s with the same handle
                            // to exist, and the value may not implement `PartialEq`, so we don't.
                            h1 == h2
                        }
                    )*
                    (_, _) => false,
                }
            }
        }
        impl Eq for AnyPending {}

        /// Queries for reading members, sometimes used by [`Handle::read()`].
        /// Contains one `QueryState` per member type, whose `QueryData` is that member's
        /// `UniverseMember::Read`.
        #[derive(ecs::FromWorld)]
        #[macro_rules_attribute::derive($crate::universe::ecs_details::derive_manual_query_bundle!)]
        pub(crate) struct MemberReadQueryStates {
            $(
                pub(in crate::universe) $table_name:
                    ::bevy_ecs::query::QueryState<
                        <$member_type as universe::SealedMember>::ReadQueryData
                    >,
            )*
        }

        impl MemberReadQueryStates {
        #[expect(dead_code, reason = "TODO(ecs): going to use this in systems")]
            pub fn query_manual<'w, 's>(
                &'s self,
                world: &'w ecs::World,
            ) -> MemberReadQueries<'w, 's> {
                MemberReadQueries {
                    universe_id: *world.resource(),
                    $(
                        $table_name:
                            Some(self.$table_name.query_manual(world)),
                    )*
                }
            }
        }

        /// Queries for mutating members, used by transaction commits.
        /// Contains one `QueryState` per member type, whose `QueryData` is that member's
        /// `TransactionOnEcs::WriteQueryData`.
        #[derive(ecs::FromWorld)]
        #[macro_rules_attribute::derive($crate::universe::ecs_details::derive_manual_query_bundle!)]
        pub(in crate::universe) struct MemberWriteQueryStates {
            $(
                pub(in crate::universe) $table_name:
                    ::bevy_ecs::query::QueryState<
                        <
                            <$member_type as universe::Transactional>::Transaction
                            as universe::TransactionOnEcs
                        >::WriteQueryData
                    >,
            )*
        }

        /// Queries for reading members, sometimes used by [`Handle::read()`].
        /// Contains one optional `Query` per member type, whose `QueryData` is that member's
        /// `UniverseMember::Read`.
        ///
        /// Each query is optional so as to enable constructing this with a variety of different
        /// amounts of access from different systems.
        ///
        /// To obtain this, either:
        ///
        /// * Use [`AllMemberReadQueries`] as a system parameter, or
        /// * Call [`MemberReadQueries::query_manuql()`].
        pub(crate) struct MemberReadQueries<'w, 's> {
            pub(crate) universe_id: universe::UniverseId,
            $(
                pub(crate) $table_name:
                    ::core::option::Option<::bevy_ecs::prelude::Query<'w, 's,
                        <$member_type as universe::SealedMember>::ReadQueryData
                    >>,
            )*
        }

        /// System parameter that constructs a [`MemberReadQueries`].
        #[derive(bevy_ecs::system::SystemParam)]
        pub(crate) struct AllMemberReadQueries<'w, 's> {
            pub(crate) universe_id: ecs::Res<'w, universe::UniverseId>,
            $(
                pub(crate) $table_name:
                    ::bevy_ecs::prelude::Query<'w, 's,
                        <$member_type as universe::SealedMember>::ReadQueryData
                    >,
            )*
        }

        impl<'w, 's> AllMemberReadQueries<'w, 's> {
            pub fn get(self) -> MemberReadQueries<'w, 's> {
                MemberReadQueries {
                    universe_id: *self.universe_id,
                    $(
                        $table_name: Some(self.$table_name),
                    )*
                }
            }
        }
    }
}

// This macro does not handle everything.
// To add another type, it is also necessary to update:
//    Universe::delete
//    Universe::gc
//    Universe::step (if the members do anything on step)
//    struct PartialUniverse
//    impl Serialize for PartialUniverse
//    save::schema::MemberSer
//    transaction::universe_txn::*
// TODO(ecs): The above comment is stale.
member_enums_and_impls!(
    (BlockDef, blocks),
    (Character, characters),
    (SoundDef, sounds),
    (Space, spaces),
    (TagDef, tags),
);

// -------------------------------------------------------------------------------------------------

impl AnyHandle {
    /// Downcast to a specific `Handle<T>` type.
    pub fn downcast_ref<T: 'static>(&self) -> Option<&Handle<T>> {
        let handle: &dyn ErasedHandle = self.as_ref();
        let handle: &dyn Any = handle;
        handle.downcast_ref()
    }

    /// Downcast to a specific `Handle<T>` type.
    pub fn downcast<T: 'static>(self) -> Result<Handle<T>, AnyHandle> {
        // TODO: implement this more efficiently, without the clone
        // (note that `Box`ing to use `Box<dyn Any>` downcasting is *not* more efficient)
        match self.downcast_ref() {
            Some(handle) => Ok(handle.clone()),
            None => Err(self),
        }
    }
}

impl AsRef<dyn ErasedHandle> for AnyHandle {
    fn as_ref(&self) -> &dyn ErasedHandle {
        &**self
    }
}
impl core::borrow::Borrow<dyn ErasedHandle> for AnyHandle {
    fn borrow(&self) -> &dyn ErasedHandle {
        &**self
    }
}

impl HandlePtr for AnyHandle {
    fn as_erased_shared_pointer(&self) -> *const () {
        let r: &dyn ErasedHandle = &**self;
        r.as_erased_shared_pointer()
    }
}

impl ErasedHandle for AnyHandle {
    fn name(&self) -> Name {
        let r: &dyn ErasedHandle = &**self;
        r.name()
    }

    fn universe_id(&self) -> Option<super::UniverseId> {
        let r: &dyn ErasedHandle = &**self;
        r.universe_id()
    }

    fn as_entity(
        &self,
        expected_universe: super::UniverseId,
    ) -> Result<ecs::Entity, universe::handle::HandleError> {
        let r: &dyn ErasedHandle = &**self;
        r.as_entity(expected_universe)
    }

    fn to_any_handle(&self) -> AnyHandle {
        self.clone()
    }
}

impl PartialEq for AnyHandle {
    fn eq(&self, other: &AnyHandle) -> bool {
        self.as_erased_shared_pointer() == other.as_erased_shared_pointer()
    }
}
impl Eq for AnyHandle {}
impl hash::Hash for AnyHandle {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        // Must agree with `Hash for Handle` and `Hash for dyn ErasedHandle`
        self.as_erased_shared_pointer().hash(state);
    }
}

impl PartialEq<dyn ErasedHandle> for AnyHandle {
    fn eq(&self, other: &dyn ErasedHandle) -> bool {
        self.as_erased_shared_pointer() == other.as_erased_shared_pointer()
    }
}
impl hashbrown::Equivalent<AnyHandle> for dyn ErasedHandle {
    fn equivalent(&self, other: &AnyHandle) -> bool {
        self.as_erased_shared_pointer() == other.as_erased_shared_pointer()
    }
}

impl<T> PartialEq<Handle<T>> for AnyHandle
where
    Handle<T>: HandlePtr,
{
    fn eq(&self, other: &Handle<T>) -> bool {
        self.as_erased_shared_pointer() == other.as_erased_shared_pointer()
    }
}
impl<T> PartialEq<AnyHandle> for Handle<T>
where
    Handle<T>: HandlePtr,
{
    fn eq(&self, other: &AnyHandle) -> bool {
        *other == *self
    }
}

// TODO: we can get rid of this if we arrange to have a version of `MemberReadQueryStates` that carries `Query` instead of `QueryState`
impl bevy_ecs::system::ExclusiveSystemParam for &mut MemberReadQueryStates {
    type State = MemberReadQueryStates;
    type Item<'s> = &'s mut MemberReadQueryStates;

    fn init(world: &mut ecs::World, _: &mut bevy_ecs::system::SystemMeta) -> Self::State {
        <MemberReadQueryStates as bevy_ecs::world::FromWorld>::from_world(world)
    }

    fn get_param<'s>(
        state: &'s mut Self::State,
        _: &bevy_ecs::system::SystemMeta,
    ) -> Self::Item<'s> {
        state
    }
}

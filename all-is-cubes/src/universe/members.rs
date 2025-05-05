//! Module defining various traits and impls relating to [`Universe`] containing different
//! types of members.
//!
//! This module is not public and that is part of the protection of several items
//! inside it (the public-in-private trick).

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::string::ToString;
use alloc::vec::Vec;
use core::fmt;

use crate::block::BlockDef;
use crate::character::Character;
use crate::sound::SoundDef;
use crate::space::Space;
use crate::tag::TagDef;
use crate::transaction;
use crate::universe::{
    ErasedHandle, Handle, HandleSet, InsertError, Name, ReadTicket, RootHandle, Universe,
    UniverseIter, universe_txn as ut,
};
use crate::util::Refmt as _;

/// A `BTreeMap` is used to ensure that the iteration order is deterministic across
/// runs/versions.
pub(crate) type Storage<T> = BTreeMap<Name, RootHandle<T>>;

/// Trait for every type which can be a named member of a universe.
///
/// This trait is also public-in-private and thus prevents anything bounded by it from
/// being called or implemented by an unsupported type (“sealing”). However, because of
/// that, it also cannot mention anything we don't also want to make public or
/// public-in-private.
#[doc(hidden)]
#[expect(unnameable_types)]
pub trait UniverseMember: Sized + 'static + fmt::Debug {
    /// Generic constructor for [`AnyHandle`].
    fn into_any_handle(r: Handle<Self>) -> AnyHandle;
}

/// For each type `T` that can be a member of a [`Universe`], this trait is implemented,
/// `impl UniverseTable<T> for Universe`, in order to provide the collection of members
/// of that type.
///
/// This trait is not public and is used only within the implementation of [`Universe`];
/// thus, the `Table` associated type does not need to be a public type.
///
/// TODO: Implement this on [`UniverseTables`] instead.
pub(crate) trait UniverseTable<T> {
    type Table;

    fn table(&self) -> &Self::Table;

    fn table_mut(&mut self) -> &mut Self::Table;
}

/// Trait implemented by [`Universe`] once for each type of object that can be stored in a
/// [`Universe`], that permits lookups of that type.
///
/// This trait must be public(-in-private) so it can be a bound on public methods.
/// It could be just public, but it's cleaner to not require importing it everywhere.
#[doc(hidden)]
#[expect(unnameable_types)]
pub trait UniverseOps<T>
where
    T: UniverseMember,
{
    // Internal: Implementations of this are in the [`members`] module.

    fn get(&self, name: &Name) -> Option<Handle<T>>;

    fn iter_by_type(&self) -> UniverseIter<'_, T>;
}

/// Trait implemented by [`HandleSet`] once for each type of object that can be
/// stored in a [`Universe`], that permits lookups of that type.
///
/// This trait must be public(-in-private) so it can be a bound on public methods.
/// It could be just public, but it's cleaner to not require importing it everywhere.
#[doc(hidden)]
#[expect(unnameable_types)]
pub trait HandleSetOps<T>
where
    T: UniverseMember,
{
    fn from_set(members: impl IntoIterator<Item = Handle<T>>) -> Self
    where
        Self: Sized;
}

// Helper functions to implement `UniverseOps` without putting everything
// in the macro body.
pub(super) fn ops_get<T>(this: &Universe, name: &Name) -> Option<Handle<T>>
where
    Universe: UniverseTable<T, Table = Storage<T>>,
{
    <Universe as UniverseTable<T>>::table(this)
        .get(name)
        .map(RootHandle::downgrade)
}

/// Generates impls for a specific Universe member type.
macro_rules! impl_universe_for_member {
    ($member_type:ident, $table:ident) => {
        impl UniverseMember for $member_type {
            fn into_any_handle(handle: Handle<$member_type>) -> AnyHandle {
                AnyHandle::$member_type(handle)
            }
        }

        impl UniverseTable<$member_type> for Universe {
            type Table = Storage<$member_type>;

            fn table(&self) -> &Storage<$member_type> {
                &self.tables.$table
            }
            fn table_mut(&mut self) -> &mut Storage<$member_type> {
                &mut self.tables.$table
            }
        }

        impl UniverseTable<$member_type> for HandleSet {
            type Table = Vec<Handle<$member_type>>;

            fn table(&self) -> &Self::Table {
                &self.$table
            }
            fn table_mut(&mut self) -> &mut Self::Table {
                &mut self.$table
            }
        }

        impl UniverseOps<$member_type> for Universe {
            fn get(&self, name: &Name) -> Option<Handle<$member_type>> {
                ops_get(self, name)
            }
            fn iter_by_type(&self) -> UniverseIter<'_, $member_type> {
                UniverseIter(UniverseTable::<$member_type>::table(self).iter())
            }
        }

        impl HandleSetOps<$member_type> for HandleSet {
            fn from_set(members: impl IntoIterator<Item = Handle<$member_type>>) -> Self {
                // TODO: enforce exactly one universe id
                let mut new_self = Self::default();
                UniverseTable::<$member_type>::table_mut(&mut new_self).extend(members);
                new_self
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
        /// Sub-structure of [`Universe`] that actually stores the members.
        #[derive(Debug, Default)]
        pub(super) struct UniverseTables {
            $( pub(crate) $table_name: Storage<$member_type>, )*
        }

        impl UniverseTables {
            /// Called by Universe as part of its `Debug`
            pub(crate) fn fmt_members(&self, ds: &mut fmt::DebugStruct<'_, '_>) {
                $( fmt_members_of_type::<$member_type>(&self.$table_name, ds); )*
            }

            pub(crate) fn get_any(&self, name: &Name) -> Option<Box<dyn ErasedHandle>> {
                $(
                    if let Some(root_handle) = self.$table_name.get(name) {
                        return Some(Box::new(root_handle.downgrade()));
                    }
                )*
                None
            }
        }

        /// Holds any one of the concrete [`Handle<T>`](Handle) types that can be in a [`Universe`].
        ///
        /// See also [`ErasedHandle`], which is implemented by `Handle`s rather than owning one.
        /// This type dereferences to `dyn ErasedHandle` to provide all the operations that
        /// trait does.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        #[non_exhaustive]
        #[allow(missing_docs, reason = "variant meanings are obvious from name")]
        pub enum AnyHandle {
            $( $member_type(Handle<$member_type>), )*
        }

        impl AnyHandle {
            /// Returns whether this [`Handle`] does not yet belong to a universe and can start
            /// doing so. Used by [`MemberTxn::check`].
            ///
            /// See [`Handle::check_upgrade_pending`] for more information.
            pub(crate) fn check_upgrade_pending(
                &self,
                universe_id: $crate::universe::UniverseId,
            ) -> Result<(), $crate::universe::InsertError> {
                match self {
                    $( Self::$member_type(handle) => handle.check_upgrade_pending(universe_id), )*
                }
            }

            /// Used by [`MemberTxn::commit()`] to perform inserts.
            pub(crate) fn insert_and_upgrade_pending(
                &self,
                universe: &mut Universe,
            ) -> Result<(), InsertError> {
                match self {
                    $( AnyHandle::$member_type(pending_handle) => {
                        ut::any_handle_insert_and_upgrade_pending(universe, pending_handle)
                    } )*
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
            type Target = ();
            type Context<'a> = ReadTicket<'a>;
            type CommitCheck = ut::AnyTransactionCheck;
            type Output = transaction::NoOutput;
            type Mismatch = AnyTransactionMismatch;

            fn check(
                &self,
                _target: &(),
            ) -> Result<Self::CommitCheck, Self::Mismatch> {
                Ok::<Self::CommitCheck, Self::Mismatch>(match self {
                    Self::Noop => Box::new(()),
                    $(
                        Self::$member_type(t) => Box::new(t.check(&())
                            .map_err(AnyTransactionMismatch::$member_type)?),
                    )*
                })
            }

            fn commit(
                &self,
                _target: &mut (),
                read_ticket: Self::Context<'_>,
                check: Self::CommitCheck,
                outputs: &mut dyn FnMut(Self::Output),
            ) -> Result<(), transaction::CommitError> {
                match self {
                    Self::Noop => Ok(()),
                    $( Self::$member_type(t) => ut::anytxn_commit_helper(t, read_ticket, check, outputs), )*
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
    }
}

// This macro does not handle everything.
// To add another type, it is also necessary to update:
//    Universe::delete
//    Universe::gc
//    Universe::fix_deserialized_handles
//    Universe::step (if the members do anything on step)
//    struct PartialUniverse
//    impl Serialize for PartialUniverse
//    save::schema::MemberSer
//    transaction::universe_txn::*
member_enums_and_impls!(
    (BlockDef, blocks),
    (Character, characters),
    (SoundDef, sounds),
    (Space, spaces),
    (TagDef, tags),
);

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

impl ErasedHandle for AnyHandle {
    fn name(&self) -> Name {
        let r: &dyn ErasedHandle = &**self;
        r.name()
    }

    fn universe_id(&self) -> Option<super::UniverseId> {
        let r: &dyn ErasedHandle = &**self;
        r.universe_id()
    }

    fn to_any_handle(&self) -> AnyHandle {
        self.clone()
    }

    #[cfg(feature = "save")]
    fn fix_deserialized(
        &self,
        universe_id: super::UniverseId,
        privacy_token: super::ErasedHandleInternalToken,
    ) -> Result<(), super::HandleError> {
        let r: &dyn ErasedHandle = &**self;
        r.fix_deserialized(universe_id, privacy_token)
    }
}

/// Helper for `UniverseTable::fmt_members`
fn fmt_members_of_type<T>(table: &Storage<T>, ds: &mut fmt::DebugStruct<'_, '_>)
where
    Universe: UniverseTable<T, Table = Storage<T>>,
{
    for name in table.keys() {
        // match root.strong_ref.try_borrow() {
        //     Ok(entry) => ds.field(&name.to_string(), &entry.data),
        //     Err(_) => ds.field(&name.to_string(), &"<in use>"),
        // };
        ds.field(
            &name.to_string(),
            &core::marker::PhantomData::<T>.refmt(&crate::util::TypeName),
        );
    }
}

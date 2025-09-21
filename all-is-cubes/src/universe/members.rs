//! Module defining various traits and impls relating to [`Universe`] containing different
//! types of members.
//!
//! This module is not public and that is part of the protection of several items
//! inside it (the public-in-private trick).

use alloc::boxed::Box;
use core::any::Any;
use core::fmt;

use bevy_ecs::prelude as ecs;

use crate::block::BlockDef;
use crate::character::Character;
use crate::sound::SoundDef;
use crate::space::Space;
use crate::tag::TagDef;
use crate::transaction;
use crate::universe::{
    ErasedHandle, Handle, InsertError, Name, Universe, VisitableComponents, universe_txn as ut,
};

/// Trait for every type which can be a named member of a universe.
///
/// This trait is also public-in-private and thus prevents anything bounded by it from
/// being called or implemented by an unsupported type (“sealing”). However, because of
/// that, it also cannot mention anything we don't also want to make public or
/// public-in-private.
#[doc(hidden)]
#[expect(unnameable_types)]
// TODO(ecs): make Component not publicly a supertrait, once we figure out what components typed members actually have
pub trait UniverseMember:
    Sized + 'static + fmt::Debug + ecs::Component<Mutability = bevy_ecs::component::Mutable>
{
    /// Generic constructor for [`AnyHandle`].
    fn into_any_handle(r: Handle<Self>) -> AnyHandle;
}

/// Generates impls for a specific Universe member type.
macro_rules! impl_universe_for_member {
    ($member_type:ident, $table:ident) => {
        impl UniverseMember for $member_type {
            fn into_any_handle(handle: Handle<$member_type>) -> AnyHandle {
                AnyHandle::$member_type(handle)
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
                    // This also registers the components themselves as a side benefit.
                    VisitableComponents::register::<$member_type>(world);
                )*
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
                read_ticket_for_self: $crate::universe::ReadTicket<'_>,
                universe_id: $crate::universe::UniverseId,
            ) -> Result<(), $crate::universe::InsertError> {
                match self {
                    $(
                        Self::$member_type(handle) => {
                            handle.check_upgrade_pending(read_ticket_for_self, universe_id)
                        }
                    )*
                }
            }

            /// Used by [`MemberTxn::commit()`] to perform inserts.
            pub(crate) fn insert_and_upgrade_pending(
                &self,
                universe: &mut Universe,
            ) -> Result<(), InsertError> {
                match self {
                    $( AnyHandle::$member_type(pending_handle) => {
                        pending_handle.upgrade_pending(universe)
                    } )*
                }
            }

            /// For debugging — not guaranteed to be stable.
            #[doc(hidden)] // TODO: not great API, but used by all-is-cubes-port
            pub fn member_type_name(&self) -> &'static str {
                 match self {
                    $( AnyHandle::$member_type(_) => {
                        core::any::type_name::<$member_type>()
                    } )*
                }
            }

            #[cfg(feature = "save")]
            pub(crate) fn not_still_deserializing(
                &self,
                read_ticket: $crate::universe::ReadTicket<'_>,
            ) -> bool {
                match self {
                    $( AnyHandle::$member_type(h) => {
                        !matches!(h.read(read_ticket), Err($crate::universe::HandleError::NotReady(_)))
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
                universe: &Universe,
            ) -> Result<Self::CommitCheck, Self::Mismatch> {
                Ok::<Self::CommitCheck, Self::Mismatch>(match self {
                    Self::Noop => Box::new(()),
                    $(
                        Self::$member_type(t) => Box::new(t.check(universe)
                            .map_err(AnyTransactionMismatch::$member_type)?),
                    )*
                })
            }

            fn commit(
                self,
                universe: &mut Universe,
                (): Self::Context<'_>,
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

impl AnyHandle {
    /// Downcast to a specific `Handle<T>` type.
    pub fn downcast_ref<T: 'static>(&self) -> Option<&Handle<T>> {
        let handle: &dyn ErasedHandle = self.as_ref();
        let handle: &dyn Any = handle;
        handle.downcast_ref()
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
    ) -> Result<ecs::Entity, crate::universe::handle::HandleError> {
        let r: &dyn ErasedHandle = &**self;
        r.as_entity(expected_universe)
    }

    fn to_any_handle(&self) -> AnyHandle {
        self.clone()
    }
}

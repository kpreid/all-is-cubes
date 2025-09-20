#![expect(clippy::elidable_lifetime_names, reason = "names for clarity")]

use core::any::Any;
use core::any::type_name;
use core::fmt;
use core::mem;
use core::panic::Location;

use bevy_ecs::prelude as ecs;
use bevy_ecs::world::unsafe_world_cell::UnsafeWorldCell;

use crate::universe::{self, GoneReason, HandleError, Universe, UniverseId};

#[cfg(doc)]
use crate::{block::Block, universe::Handle};

// -------------------------------------------------------------------------------------------------

/// Permission to call [`Handle::read()`] to access the handle’s referent.
///
/// Functions which call [`Handle::read()`] need to run only when there is no possibility of a
/// conflicting write operation; thus, they should take [`ReadTicket`] to notify their callers
/// that they are going to perform reads.
///
/// To obtain a [`ReadTicket`], do one of the following:
///
/// * Call [`Universe::read_ticket()`].
/// * If the operation will not actually read using any handles (e.g. calling [`Block::evaluate()`]
///   on a [`Block`] that contains no handles), use [`ReadTicket::stub()`].
#[derive(Clone, Copy, Debug)]
pub struct ReadTicket<'universe> {
    access: TicketAccess<'universe>,

    /// If present, allows access to pending handles from the given transaction.
    transaction_access: Option<&'universe super::UniverseTransaction>,

    universe_id: Option<UniverseId>,

    /// Where this ticket was created.
    pub(in crate::universe) origin: &'static Location<'static>,

    /// If true, don't log failures.
    pub(in crate::universe) expect_may_fail: bool,
}

#[derive(Clone, Copy)]
enum TicketAccess<'u> {
    /// Full access to a universe.
    World(&'u ecs::World),

    /// Access only to things required for [`Block::evaluate()`].
    BlockDataSources(QueryBlockDataSources<'u, 'u>),

    /// Access to all but one entity.
    EverythingBut(UnsafeWorldCell<'u>, ecs::Entity),

    /// No access.
    Stub,
}

#[derive(Clone, Copy, Debug, bevy_ecs::system::SystemParam)]
pub(crate) struct QueryBlockDataSources<'w, 's> {
    universe_id: UniverseId,
    query: ecs::Query<
        'w,
        's,
        ecs::AnyOf<(
            &'static crate::block::BlockDef,
            &'static crate::space::Space,
        )>,
    >,
}

impl<'universe> ReadTicket<'universe> {
    #[track_caller]
    pub(crate) fn from_universe(universe: &'universe Universe) -> Self {
        Self {
            access: TicketAccess::World(&universe.world),
            transaction_access: None,
            universe_id: Some(universe.universe_id()),
            origin: Location::caller(),
            expect_may_fail: false,
        }
    }

    /// Adds to this ticket the ability to read from a not-yet-committed transaction.
    // TODO: decide what repeated calls should do
    #[track_caller]
    #[must_use]
    pub fn with_transaction(mut self, transaction: &'universe super::UniverseTransaction) -> Self {
        self.transaction_access = Some(transaction);
        self
    }

    /// Create a [`ReadTicket`] which does not guarantee access to anything.
    ///
    /// This may be used for evaluating universe-independent [`Block`]s.
    #[track_caller]
    pub const fn stub() -> Self {
        Self {
            access: TicketAccess::Stub,
            transaction_access: None,
            universe_id: None,
            origin: Location::caller(),
            expect_may_fail: false,
        }
    }

    #[track_caller]
    pub(crate) fn from_block_data_sources(
        data_sources: QueryBlockDataSources<'universe, 'universe>,
    ) -> Self {
        ReadTicket {
            access: TicketAccess::BlockDataSources(data_sources),
            transaction_access: None,
            universe_id: Some(data_sources.universe_id),
            origin: Location::caller(),
            expect_may_fail: false,
        }
    }

    /// Create a [`ReadTicket`] allowing access to every entity in the world except the specified one.
    ///
    /// # Safety
    ///
    /// There must be no concurrent access to any entity other than `entity`,
    /// until the lifetime `'u` expires.
    #[track_caller]
    pub(in crate::universe) unsafe fn everything_but<'u>(
        universe_id: UniverseId,
        world: UnsafeWorldCell<'u>,
        entity: ecs::Entity,
    ) -> ReadTicket<'u> {
        ReadTicket {
            access: TicketAccess::EverythingBut(world, entity),
            transaction_access: None,
            universe_id: Some(universe_id),
            origin: Location::caller(),
            expect_may_fail: false,
        }
    }

    /// Get a component.
    /// Returns None if any of:
    ///
    /// * entity does not exist
    /// * entity does not have a `T` component
    /// * this ticket does not allow access to that component
    pub(crate) fn get<T: ecs::Component>(
        &self,
        entity: ecs::Entity,
    ) -> Result<&'universe T, ReadTicketError> {
        // inner function returns TicketErrorKind
        let inner = || match self.access {
            TicketAccess::World(world) => world.get(entity).ok_or_else(|| {
                if world.get_entity(entity).is_ok() {
                    TicketErrorKind::MissingComponent {
                        type_name: type_name::<T>(),
                    }
                } else {
                    TicketErrorKind::MissingEntity
                }
            }),
            TicketAccess::BlockDataSources(query) => {
                let (block_component, space_component) =
                    query.query.get_inner(entity).map_err(|error| {
                        use bevy_ecs::query::QueryEntityError as E;
                        match error {
                            E::QueryDoesNotMatch(..) => TicketErrorKind::ComponentNotAllowed {
                                type_name: type_name::<T>(),
                            },
                            E::EntityDoesNotExist(_) => TicketErrorKind::MissingEntity,
                            E::AliasedMutability(_) => {
                                unreachable!("not a query for mutable access")
                            }
                        }
                    })?;
                block_component
                    .and_then(|r| <dyn Any>::downcast_ref(r))
                    .or_else(|| space_component.and_then(|r| <dyn Any>::downcast_ref(r)))
                    .ok_or_else(|| TicketErrorKind::MissingComponent {
                        type_name: type_name::<T>(),
                    })
            }
            TicketAccess::EverythingBut(world, prohibited_entity) => {
                if entity == prohibited_entity {
                    Err(TicketErrorKind::BeingMutated)
                } else {
                    let entity_ref = world
                        .get_entity(entity)
                        .map_err(|_| TicketErrorKind::MissingEntity)?;
                    // SAFETY:
                    // If [`ReadTicket::everything_but()`]'s safety conditions were met,
                    // this access will not alias.
                    unsafe { entity_ref.get::<T>() }.ok_or_else(|| {
                        TicketErrorKind::MissingComponent {
                            type_name: type_name::<T>(),
                        }
                    })
                }
            }
            TicketAccess::Stub => Err(TicketErrorKind::Stub),
        };

        inner().map_err(|kind: TicketErrorKind| ReadTicketError {
            kind,
            ticket_origin: self.origin,
        })
    }

    /// Returns the ID of the universe this ticket allows access to, if there is exactly one
    /// such universe.
    pub fn universe_id(&self) -> Option<UniverseId> {
        self.universe_id
    }

    /// Indicate that sometimes using this ticket with the wrong universe is expected and should
    /// not be treated as a sign of a bug.
    //---
    // TODO(read_ticket) TODO(inventory): Stop needing this. Its uses come up in relation to
    // tool icons being given by either the game universe or UI universe, which we want to replace
    // anyway.
    #[doc(hidden)]
    #[must_use]
    pub fn expect_may_fail(mut self) -> Self {
        self.expect_may_fail = true;
        self
    }

    /// Succeeds only if this ticket allows access to a transaction.
    ///
    /// TODO: This is a placeholder for the access actually being made through the transaction
    /// reference.
    pub(crate) fn ensure_has_transaction_access(&self) -> Result<(), ReadTicketError> {
        match self.transaction_access {
            Some(_) => Ok(()),
            None => Err(ReadTicketError {
                kind: TicketErrorKind::Transaction,
                ticket_origin: self.origin,
            }),
        }
    }
}

impl<'a, 'b> PartialEq<ReadTicket<'b>> for ReadTicket<'a> {
    /// This implementation is for pragmatic purposes like assertions on data structures that
    /// contain tickets. Precise outcomes are not guaranteed, only:
    ///
    /// * Tickets for different universes will always be unequal.
    /// * Tickets created by the same function call site for the same universe will always be equal.
    fn eq(&self, other: &ReadTicket<'b>) -> bool {
        self.origin == other.origin
            && self.universe_id == other.universe_id
            && mem::discriminant(&self.access) == mem::discriminant(&other.access)
    }
}
impl Eq for ReadTicket<'_> {}

impl<'u> fmt::Debug for TicketAccess<'u> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::World(_) => f.debug_tuple("World").finish_non_exhaustive(),
            Self::BlockDataSources(_) => f.debug_tuple("BlockDataSources").finish_non_exhaustive(),
            Self::EverythingBut(_, entity) => f.debug_tuple("EverythingBut").field(entity).finish(),
            Self::Stub => write!(f, "Stub"),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Error when a [`ReadTicket`] does not have sufficient access.
///
/// It is also possible for this error to occur when the [`Universe`] has entered an invalid state,
/// in which case it indicates a bug in All is Cubes.
//---
// Design note: This type exists solely to hide the variants of `TicketErrorKind` so that they are
// not stable public API. We could also consider adding the entity ID, though.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ReadTicketError {
    kind: TicketErrorKind,
    /// Code location which constructed the unsuitable ticket.
    ticket_origin: &'static Location<'static>,
}

/// Low-level errors that can result from attempting to use a [`ReadTicket`] to fetch a component.
///
/// This enum is sort of an internal version of [`HandleError`]; it differs in that it does not
/// contain the [`Name`] of the handle (nor the entity ID), since that is easier to obtain at higher
/// levels.
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[allow(dead_code, reason = "used for Debug printing")]
pub(crate) enum TicketErrorKind {
    /// The entity requested does not exist in the world.
    #[displaydoc("handle’s entity is missing")]
    MissingEntity,
    /// The component requested does not exist in the world.
    #[displaydoc("component {type_name} is missing")]
    MissingComponent { type_name: &'static str },
    /// The component may or may not exist, but this ticket does not allow access to it.
    #[displaydoc("component {type_name} is not accessible using this ticket")]
    ComponentNotAllowed { type_name: &'static str },
    /// The ticket does not allow access to the entity because the entity is being mutated.
    #[displaydoc("this entity is being mutated")]
    BeingMutated,
    /// The ticket does not allow access to this universe or any other.
    #[displaydoc("ticket is for a transaction, not a universe")]
    Transaction,
    /// The ticket does not allow access to this universe or any other.
    #[displaydoc("ticket is a stub")]
    Stub,
}

impl ReadTicketError {
    /// Convert to the corresponding high-level error,
    /// or panic if the error “can’t happen”.
    ///
    /// Depending on the specific case, the resulting [`HandleError`]
    /// may be a [`HandleError::InvalidTicket`] or something more specific.
    pub(crate) fn into_handle_error(self, handle: &dyn universe::ErasedHandle) -> HandleError {
        match self.kind {
            TicketErrorKind::MissingEntity => HandleError::Gone {
                name: handle.name(),
                // TODO(ecs): we don't know that this is the true reason.
                // Should this be panicking instead?
                reason: GoneReason::Deleted {},
            },
            TicketErrorKind::MissingComponent { type_name: _ } => panic!("{self:?}"), // TODO: improve
            TicketErrorKind::ComponentNotAllowed { type_name: _ } => HandleError::InvalidTicket {
                name: handle.name(),
                error: self,
            },
            TicketErrorKind::BeingMutated => HandleError::InUse(handle.name()),
            TicketErrorKind::Transaction => HandleError::WrongUniverse {
                ticket_universe_id: None,
                handle_universe_id: handle.universe_id(),
                name: handle.name(),
                ticket_origin: self.ticket_origin,
            },
            TicketErrorKind::Stub => unreachable!("universe ID should already have been checked"),
        }
    }
}

impl core::error::Error for ReadTicketError {}
impl fmt::Display for ReadTicketError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equality() {
        let universes: [Universe; 2] = std::array::from_fn(|_| Universe::new());

        assert_ne!(
            ReadTicket::stub(),
            ReadTicket::stub(),
            "different by location"
        );

        let stubs: [ReadTicket<'_>; 2] = std::array::from_fn(|_| ReadTicket::stub());
        assert_eq!(stubs[0], stubs[1], "identical stub()");

        let different_universe_tickets: [ReadTicket<'_>; 2] =
            std::array::from_fn(|i| universes[i].read_ticket());
        assert_ne!(
            different_universe_tickets[0], different_universe_tickets[1],
            "different universes"
        );

        let same_universe_tickets: [ReadTicket<'_>; 2] =
            std::array::from_fn(|_| universes[0].read_ticket());
        assert_eq!(
            same_universe_tickets[0], same_universe_tickets[1],
            "same universes"
        );
    }

    #[test]
    fn debug() {
        let universe = Universe::new();
        let ticket = universe.read_ticket();

        // Fetch data that makes variable parts of the string to keep the test robust
        let id = universe.id;
        let origin = ticket.origin;

        assert_eq!(
            format!("{ticket:?}"),
            format!(
                "ReadTicket {{ \
                    access: World(..), \
                    transaction_access: None, \
                    universe_id: Some({id:?}), \
                    origin: {origin:?}, \
                    expect_may_fail: false \
                }}"
            ),
        );
    }

    #[test]
    fn debug_stub() {
        let ticket = ReadTicket::stub();
        let origin = ticket.origin;
        assert_eq!(
            format!("{ticket:?}"),
            format!(
                "ReadTicket {{ \
                    access: Stub, \
                    transaction_access: None, \
                    universe_id: None, \
                    origin: {origin:?}, \
                    expect_may_fail: false \
                }}"
            ),
        );
    }
}

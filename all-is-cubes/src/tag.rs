//! [`Tag`] and related types, defining categories of entities for game rules.

use core::convert::Infallible;

use crate::transaction;
use crate::universe::{self, Handle};

#[cfg(doc)]
use crate::universe::Universe;

//--------------------------------------------------------------------------------------------------

/// Identifies game entities of a certain game-mechanical kind.
///
/// For example, blocks that can be destroyed with a particular tool.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Tag {
    /// A tag defined by its identity as a [`Universe`] member.
    Handle(Handle<TagDef>),
    // System(SysTag),
}

// pub enum SysTag {
//     ///
//     Replaceable,
// }

/// A [`Tag`] in the role of “this entity is tagged with this tag”.
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Be(pub Tag);

/// A [`Tag`] in the role of “check whether other entities are tagged with this tag”.
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Is(pub Tag);

//--------------------------------------------------------------------------------------------------

impl universe::VisitHandles for Tag {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        match self {
            Tag::Handle(handle) => handle.visit_handles(visitor),
        }
    }
}
impl universe::VisitHandles for Be {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        match self {
            Be(tag) => tag.visit_handles(visitor),
        }
    }
}
impl universe::VisitHandles for Is {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        match self {
            Is(tag) => tag.visit_handles(visitor),
        }
    }
}

//--------------------------------------------------------------------------------------------------

/// A tag defined by its identity as a [`Universe`] member.
///
#[doc = include_str!("save/serde-warning.md")]
#[expect(clippy::exhaustive_structs)]
#[expect(clippy::module_name_repetitions, reason = "TODO: reconsider")]
#[derive(Debug, bevy_ecs::component::Component)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TagDef;

impl universe::VisitHandles for TagDef {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        let &TagDef = self;
        // No handles
    }
}

impl transaction::Transactional for TagDef {
    type Transaction = DefTransaction;
}

/// Transaction type for [`TagDef`].
///
/// Currently, there is nothing for it to do.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DefTransaction {}

impl transaction::Merge for DefTransaction {
    type MergeCheck = ();
    type Conflict = Infallible;

    fn check_merge(&self, _: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        match *self {}
    }

    fn commit_merge(&mut self, _: Self, (): Self::MergeCheck) {
        match *self {}
    }
}

impl transaction::Transaction for DefTransaction {
    type Target = TagDef;
    // This ReadTicket is not currently used, but at least for now, *all* universe member transactions are to have ReadTicket as their context type.
    type Context<'a> = universe::ReadTicket<'a>;
    type CommitCheck = ();
    type Output = transaction::NoOutput;
    type Mismatch = Infallible;

    fn check(&self, _: &Self::Target) -> Result<Self::CommitCheck, Self::Mismatch> {
        match *self {}
    }

    fn commit(
        &self,
        _: &mut Self::Target,
        _: Self::Context<'_>,
        (): Self::CommitCheck,
        _: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        match *self {}
    }
}

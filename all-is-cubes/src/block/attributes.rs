//! [`BlockAttributes`] and closely related types.

use core::{fmt, ops};

use arcstr::ArcStr;

use crate::inv::InvInBlock;
use crate::math::{Face6, GridRotation};
use crate::op::Operation;

use crate::block::Modifier;
use crate::time;
#[cfg(doc)]
use crate::{
    block::{Block, BlockDef, Primitive},
    space::Space,
    time::TickSchedule,
};

/// Miscellaneous properties of blocks that are not the block’s voxels.
///
/// `BlockAttributes::default()` will produce a reasonable set of defaults for “ordinary”
/// blocks.
#[derive(Clone, Eq, Hash, PartialEq)]
#[expect(clippy::exhaustive_structs)] // TODO: Make this non_exhaustive but give users a way to construct it easily, possibly via block::Builder.
pub struct BlockAttributes {
    /// The name that should be displayed to players.
    ///
    /// The default value is the empty string. The empty string should be considered a
    /// reasonable choice for solid-color blocks with no special features.
    //---
    // Design note: The use of `ArcStr` allows cloning a `BlockAttributes` to be O(1)
    // allocate no additional memory.
    pub display_name: ArcStr,

    /// Whether players' [cursors](crate::character::Cursor) target it or pass through it.
    ///
    /// The default value is `true`.
    pub selectable: bool,

    /// Definition of, if this block has an attached [`Modifier::Inventory`],
    /// what size and rendering it has.
    pub inventory: InvInBlock,

    /// Rule about how this block should be rotated, or not, when placed in a [`Space`] by
    /// some agent not otherwise specifying rotation.
    ///
    /// The default value is [`RotationPlacementRule::Never`].
    //---
    // TODO: Replace this with `placement_action` features?
    pub rotation_rule: RotationPlacementRule,

    /// Something to do instead of placing this block in a [`Space`].
    //---
    // TODO: Should this not be optional, instead having a value that expresses the default
    // block placement behavior?
    pub placement_action: Option<PlacementAction>,

    /// Something this block does when time passes.
    pub tick_action: Option<TickAction>,

    /// Something this block does when activated with [`Activate`](crate::inv::Tool::Activate).
    pub activation_action: Option<Operation>,

    /// Advice to the renderer about how to expect this block to change, and hence
    /// what rendering strategy to use.
    ///
    /// Note: This is automatically augmented for [`Primitive::Recur`] blocks if they
    /// contain voxels with animation hints themselves.
    pub animation_hint: AnimationHint,
    //
    // Reminder: When adding new fields, add them to block::Builder too.
}

impl fmt::Debug for BlockAttributes {
    /// Only attributes which differ from the default are shown.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self == Self::DEFAULT_REF {
            // Avoid the braceless formatting that `debug_struct` uses if no fields are given.
            write!(f, "BlockAttributes {{}}")
        } else {
            let Self {
                display_name,
                selectable,
                inventory,
                rotation_rule,
                placement_action,
                tick_action,
                activation_action,
                animation_hint,
            } = self;

            let mut s = f.debug_struct("BlockAttributes");
            if *display_name != Self::DEFAULT_REF.display_name {
                // Unwrap the `Cow` for tidier formatting.
                s.field("display_name", &&**display_name);
            }
            if *selectable != Self::DEFAULT_REF.selectable {
                s.field("selectable", selectable);
            }
            if *inventory != Self::DEFAULT_REF.inventory {
                s.field("inventory", inventory);
            }
            if *rotation_rule != Self::DEFAULT_REF.rotation_rule {
                s.field("rotation_rule", rotation_rule);
            }
            if *placement_action != Self::DEFAULT_REF.placement_action {
                s.field("placement_action", placement_action);
            }
            if *tick_action != Self::DEFAULT_REF.tick_action {
                s.field("tick_action", tick_action);
            }
            if *activation_action != Self::DEFAULT_REF.activation_action {
                s.field("activation_action", activation_action);
            }
            if *animation_hint != Self::DEFAULT_REF.animation_hint {
                s.field("animation_hint", animation_hint);
            }
            s.finish()
        }
    }
}

impl BlockAttributes {
    const DEFAULT: Self = BlockAttributes {
        display_name: arcstr::literal!(""),
        selectable: true,
        inventory: InvInBlock::EMPTY,
        rotation_rule: RotationPlacementRule::Never,
        placement_action: None,
        tick_action: None,
        activation_action: None,
        animation_hint: AnimationHint::UNCHANGING,
    };
    /// Declaring a &'static reference allows accessing the default's fields in constant
    /// without constructing and dropping an instance which would trigger a
    /// `feature(const_precise_live_drops)` requirement.
    pub(crate) const DEFAULT_REF: &'static Self = &Self::DEFAULT;

    /// Block attributes suitable as default values for in-game use.
    ///
    /// This function differs from the [`Default::default`] trait implementation only
    /// in that it is a `const fn`.
    pub const fn default() -> BlockAttributes {
        Self::DEFAULT
    }

    #[mutants::skip] // currently used only as an optimization, and hard to test usefully
    pub(crate) fn rotationally_symmetric(&self) -> bool {
        let Self {
            display_name: _,
            selectable: _,
            inventory,
            rotation_rule,
            placement_action,
            tick_action,
            activation_action,
            animation_hint: _,
        } = self;

        inventory.rotationally_symmetric()
            && rotation_rule.rotationally_symmetric()
            && placement_action
                .as_ref()
                .is_none_or(|a| a.rotationally_symmetric())
            && tick_action
                .as_ref()
                .is_none_or(|a| a.rotationally_symmetric())
            && activation_action
                .as_ref()
                .is_none_or(|a| a.rotationally_symmetric())
    }

    pub(crate) fn rotate(self, rotation: GridRotation) -> BlockAttributes {
        let Self {
            display_name,
            selectable,
            inventory,
            rotation_rule,
            placement_action,
            tick_action,
            activation_action,
            animation_hint,
        } = self;

        Self {
            display_name,
            selectable,
            inventory: inventory.rotate(rotation),
            rotation_rule: rotation_rule.rotate(rotation),
            placement_action: placement_action.map(|a| a.rotate(rotation)),
            tick_action: tick_action.map(|a| a.rotate(rotation)),
            activation_action: activation_action.map(|a| a.rotate(rotation)),
            animation_hint,
        }
    }
}

impl Default for BlockAttributes {
    /// Block attributes suitable as default values for in-game use.
    fn default() -> BlockAttributes {
        // Delegate to the inherent impl `const fn`.
        BlockAttributes::default()
    }
}

// `ArcStr` and `GridPoint` don't implement `Arbitrary`
#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for BlockAttributes {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(BlockAttributes {
            display_name: u.arbitrary::<alloc::string::String>()?.into(),
            selectable: u.arbitrary()?,
            inventory: u.arbitrary()?,
            rotation_rule: u.arbitrary()?,
            placement_action: u.arbitrary()?,
            tick_action: u.arbitrary()?,
            activation_action: u.arbitrary()?,
            animation_hint: u.arbitrary()?,
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        Self::try_size_hint(depth).unwrap_or_default()
    }
    fn try_size_hint(
        depth: usize,
    ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        arbitrary::size_hint::try_recursion_guard(depth, |depth| {
            Ok(arbitrary::size_hint::and_all(&[
                alloc::string::String::try_size_hint(depth)?,
                bool::try_size_hint(depth)?,
                InvInBlock::try_size_hint(depth)?,
                RotationPlacementRule::try_size_hint(depth)?,
                Option::<PlacementAction>::try_size_hint(depth)?,
                Option::<TickAction>::try_size_hint(depth)?,
                AnimationHint::try_size_hint(depth)?,
            ]))
        })
    }
}

impl crate::universe::VisitHandles for BlockAttributes {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        let Self {
            display_name: _,
            selectable: _,
            inventory,
            rotation_rule: _,
            placement_action,
            tick_action,
            activation_action,
            animation_hint: _,
        } = self;
        inventory.visit_handles(visitor);
        placement_action.visit_handles(visitor);
        tick_action.visit_handles(visitor);
        activation_action.visit_handles(visitor);
    }
}

impl From<BlockAttributes> for Modifier {
    /// Converts [`BlockAttributes`] to a modifier that applies them to a block.
    fn from(value: BlockAttributes) -> Self {
        Modifier::Attributes(alloc::sync::Arc::new(value))
    }
}

/// Specifies the effect on a [`Body`](crate::physics::Body) of colliding with a
/// [`Primitive::Atom`] block or voxel having this property.
//
// TODO: This is no longer a part of attributes. Move? Rename?
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum BlockCollision {
    /// The atom can be passed through; it is not an obstacle (though intersecting it
    /// might cause other effects not directly part of collision response).
    None,
    /// The atom is a perfectly solid obstacle occupying its entire bounding cube.
    ///
    /// This is the default value used for most blocks.
    Hard,
    // Future values might include bouncy solid, water-like resistance, force fields, etc.
}

impl BlockCollision {
    /// Value used when we are constructing a block from a color with default other
    /// characteristics. This is not just a [`Default`] impl so that if we later decide
    /// that e.g. transparent atoms automatically become non-colliding, we can replace
    /// uses of this constant with that.
    pub(crate) const DEFAULT_FOR_FROM_COLOR: Self = Self::Hard;
}

/// Rule about how this block should be rotated, or not, when placed in a [`Space`] by
/// some agent not otherwise specifying rotation.
///
/// TODO: We may want to replace this with a struct that also carries declared symmetries
/// ("this is a vertical pillar so never make it upside down") and/or prohibited rotations
/// rather than requiring each individual rule variant to be sufficiently expressive.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum RotationPlacementRule {
    /// Never rotate the block.
    Never,
    /// Rotate the block so that the specified face meets the face it was placed against.
    Attach {
        /// This face of the placed block will meet the face it was placed against.
        ///
        /// If the block was somehow placed without such an adjacent block, it will not be
        /// rotated.
        by: Face6,
        // TODO: control rotation about additional axis
    },
}

impl RotationPlacementRule {
    pub(crate) fn rotationally_symmetric(self) -> bool {
        match self {
            RotationPlacementRule::Never => true,
            RotationPlacementRule::Attach { by: _ } => false,
        }
    }

    fn rotate(mut self, rotation: GridRotation) -> RotationPlacementRule {
        match &mut self {
            RotationPlacementRule::Attach { by } => *by = rotation.transform(*by),
            RotationPlacementRule::Never => {}
        }
        self
    }
}

/// Specifies how a [`Block`] might change in the very near future, for the benefit
/// of rendering algorithms. Does not currently describe non-visual aspects of a block.
///
/// This should be configured for blocks which either are continuously animated in some
/// fashion, or for which it is especially important that the specified changes are handled
/// efficiently, at the possible cost of spending more resources on those blocks. Blocks
/// which merely might change in response to user action should not set this hint.
///
/// The `|` operator may be used to combine multiple hints into “both of these will happen”.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct AnimationHint {
    /// Ways in which the block's definition might change (via modification to a
    /// [`BlockDef`] or recursive [`Space`]) such that many instances of this block
    /// will become another simultaneously.
    pub redefinition: AnimationChange,

    /// If this block is likely to be replaced in a [`Space`] by another, this field
    /// specifies the replacement's relation to this.
    pub replacement: AnimationChange,
}

impl AnimationHint {
    // TODO: get rid of these constants or replace them with a clearer new system

    /// There are no expectations that the block is soon going to change.
    ///
    /// This is the default value of this type and within [`BlockAttributes`].
    pub const UNCHANGING: Self = Self {
        redefinition: AnimationChange::None,
        replacement: AnimationChange::None,
    };

    /// Creates a hint that the block definition might be redefined,
    /// in the ways specified by `change`.
    pub const fn redefinition(change: AnimationChange) -> Self {
        Self {
            redefinition: change,
            replacement: AnimationChange::None,
        }
    }

    /// Creates a hint that the block will be replaced with another block, which differs in the
    /// ways specified by `change`.
    pub const fn replacement(change: AnimationChange) -> Self {
        Self {
            redefinition: AnimationChange::None,
            replacement: change,
        }
    }

    /// Returns whether this block's value for [`EvaluatedBlock::visible`] is likely to
    /// change from `false` to `true` for animation reasons.
    pub(crate) fn might_become_visible(self) -> bool {
        self.redefinition.might_become_visible() || self.replacement.might_become_visible()
    }
}

impl Default for AnimationHint {
    fn default() -> Self {
        Self::UNCHANGING
    }
}

impl ops::BitOr for AnimationHint {
    type Output = Self;
    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            redefinition: self.redefinition | rhs.redefinition,
            replacement: self.replacement | rhs.replacement,
        }
    }
}
impl ops::BitOrAssign for AnimationHint {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

/// Component of [`AnimationHint`], describing the type of change predicted.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum AnimationChange {
    /// Expect no changes.
    None,
    /// Expect that the block’s voxels’ colors will change, while remaining within the
    /// same [`OpacityCategory`](crate::math::OpacityCategory); that is, the alpha will
    /// remain 0, remain 1, or remain neither 0 nor 1.
    ///
    /// Suggestion to renderers: prepare to update texturing without recomputing an
    /// otherwise identical mesh.
    ColorSameCategory,
    /// Expect that the block’s colors and shape will change; that is, at least some
    /// voxels’ alpha will move from one [`OpacityCategory`](crate::math::OpacityCategory)
    /// to another.
    ///
    /// Suggestion to renderers: use a rendering strategy which is shape-independent, or
    /// prepare to efficiently recompute the mesh (don't merge with neighbors).
    Shape,
}

impl AnimationChange {
    /// Helper for [`AnimationHint::might_become_visible`].
    fn might_become_visible(self) -> bool {
        match self {
            AnimationChange::None => false,
            // same category implies not becoming visible if invisible
            AnimationChange::ColorSameCategory => false,
            AnimationChange::Shape => true,
        }
    }
}

impl ops::BitOr for AnimationChange {
    type Output = Self;
    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        use AnimationChange::*;
        match (self, rhs) {
            (Shape, _) | (_, Shape) => Shape,
            (ColorSameCategory, _) | (_, ColorSameCategory) => ColorSameCategory,
            (None, None) => None,
        }
    }
}

/// Something a block does when time passes.
///
/// Stored in [`BlockAttributes`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[expect(clippy::exhaustive_structs, reason = "will deliberately break")]
pub struct TickAction {
    /// Operation to perform on the schedule.
    pub operation: Operation,
    /// Period, relative to the universe's [`TickSchedule`]'s tick length, with which
    /// to perform this action.
    ///
    /// For example, if this is `1`, then it will be executed on every tick.
    //---
    // TODO: This should probably be its own data type
    pub schedule: time::Schedule,
}

impl TickAction {
    fn rotationally_symmetric(&self) -> bool {
        let Self {
            operation,
            schedule: _,
        } = self;
        operation.rotationally_symmetric()
    }

    fn rotate(self, rotation: GridRotation) -> TickAction {
        let Self {
            operation,
            schedule,
        } = self;
        let operation = operation.rotate(rotation);
        Self {
            operation,
            schedule,
        }
    }
}

impl From<Operation> for TickAction {
    /// TODO: remove uses of this
    fn from(operation: Operation) -> Self {
        Self {
            operation,
            schedule: time::Schedule::EVERY_TICK,
        }
    }
}

impl crate::universe::VisitHandles for TickAction {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        let Self {
            operation,
            schedule: _,
        } = self;
        operation.visit_handles(visitor);
    }
}

/// [Attribute](BlockAttributes) of a [`Block`] that specifies what happens instead of the default
/// when a player attempts to place it in some [`Space`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[expect(clippy::exhaustive_structs, reason = "no better plan yet")]
pub struct PlacementAction {
    /// The operation to perform instead of placing this block.
    pub operation: Operation,

    /// Whether to affect the cube in front of (nearer to the character than) the targeted cube,
    /// instead of the targeted block itself.
    /// `true` is the normal behavior for placed blocks;
    /// `false` is for modifying a block already present in some way.
    pub in_front: bool,
}

impl PlacementAction {
    fn rotationally_symmetric(&self) -> bool {
        let Self {
            operation,
            in_front: _,
        } = self;
        operation.rotationally_symmetric()
    }

    fn rotate(self, rotation: GridRotation) -> PlacementAction {
        Self {
            operation: self.operation.rotate(rotation),
            in_front: self.in_front,
        }
    }
}

impl crate::universe::VisitHandles for PlacementAction {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        let Self {
            operation,
            in_front: _,
        } = self;
        operation.visit_handles(visitor);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::String;

    /// [`BlockAttributes`] has an inherent `default()` function, which should be
    /// equivalent to the [`Default`] trait function.
    #[test]
    fn default_equivalent() {
        assert_eq!(
            BlockAttributes::default(),
            <BlockAttributes as Default>::default()
        );
    }

    #[test]
    fn debug() {
        let default = BlockAttributes::default;
        #[expect(clippy::needless_pass_by_value)]
        fn debug(a: BlockAttributes) -> String {
            format!("{a:?}")
        }
        assert_eq!(&*debug(BlockAttributes::default()), "BlockAttributes {}",);
        assert_eq!(
            &*debug(BlockAttributes {
                display_name: "x".into(),
                ..default()
            }),
            "BlockAttributes { display_name: \"x\" }",
        );
        assert_eq!(
            &*debug(BlockAttributes {
                selectable: false,
                ..default()
            }),
            "BlockAttributes { selectable: false }",
        );
        assert_eq!(
            &*debug(BlockAttributes {
                animation_hint: AnimationHint::replacement(AnimationChange::Shape),
                ..default()
            }),
            "BlockAttributes { animation_hint: \
            AnimationHint { redefinition: None, replacement: Shape } }",
        );

        // Test a case of multiple attributes
        assert_eq!(
            &*debug(BlockAttributes {
                display_name: "y".into(),
                selectable: false,
                ..default()
            }),
            "BlockAttributes { display_name: \"y\", selectable: false }",
        );
    }
}

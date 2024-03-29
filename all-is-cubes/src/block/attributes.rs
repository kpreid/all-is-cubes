//! [`BlockAttributes`] and closely related types.

use core::num::NonZeroU16;
use core::{fmt, ops};

use arcstr::ArcStr;

use crate::math::{Face6, GridRotation};
use crate::op::Operation;

#[cfg(doc)]
use crate::{
    block::{Block, BlockDef, Primitive},
    space::Space,
    time::TickSchedule,
};

/// Collection of miscellaneous attribute data for blocks that doesn't come in variants.
///
/// `BlockAttributes::default()` will produce a reasonable set of defaults for “ordinary”
/// blocks.
#[derive(Clone, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)] // TODO: Make this non_exhaustive but give users a way to construct it easily, possibly via BlockBuilder.
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

    /// Rule about how this block should be rotated, or not, when placed in a [`Space`] by
    /// some agent not otherwise specifying rotation.
    ///
    /// The default value is [`RotationPlacementRule::Never`].
    pub rotation_rule: RotationPlacementRule,

    /// Something this block does when time passes.
    pub tick_action: Option<TickAction>,

    /// Advice to the renderer about how to expect this block to change, and hence
    /// what rendering strategy to use.
    ///
    /// Note: This is automatically augmented for [`Primitive::Recur`] blocks if they
    /// contain voxels with animation hints themselves.
    pub animation_hint: AnimationHint,
    //
    // Reminder: When adding new fields, add them to BlockBuilder too.
    //
    // TODO: add 'behavior' functionality, if we don't come up with something else
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
                rotation_rule,
                tick_action,
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
            if *rotation_rule != Self::DEFAULT_REF.rotation_rule {
                s.field("rotation_rule", rotation_rule);
            }
            if *tick_action != Self::DEFAULT_REF.tick_action {
                s.field("tick_action", tick_action);
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
        rotation_rule: RotationPlacementRule::Never,
        tick_action: None,
        animation_hint: AnimationHint::UNCHANGING,
    };
    const DEFAULT_REF: &'static Self = &Self::DEFAULT;

    /// Block attributes suitable as default values for in-game use.
    ///
    /// This function differs from the [`Default::default`] trait implementation only
    /// in that it is a `const fn`.
    pub const fn default() -> BlockAttributes {
        Self::DEFAULT
    }

    pub(crate) fn rotationally_symmetric(&self) -> bool {
        let Self {
            display_name: _,
            selectable: _,
            rotation_rule,
            tick_action,
            animation_hint: _,
        } = self;

        rotation_rule.rotationally_symmetric()
            && !tick_action
                .as_ref()
                .is_some_and(|a| !a.rotationally_symmetric())
    }

    pub(crate) fn rotate(self, rotation: GridRotation) -> BlockAttributes {
        let Self {
            display_name,
            selectable,
            rotation_rule,
            mut tick_action,
            animation_hint,
        } = self;

        if let Some(a) = tick_action {
            tick_action = Some(a.rotate(rotation));
        }

        Self {
            display_name,
            selectable,
            rotation_rule: rotation_rule.rotate(rotation),
            tick_action,
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

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for BlockAttributes {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(BlockAttributes {
            display_name: u.arbitrary::<alloc::string::String>()?.into(),
            selectable: u.arbitrary()?,
            rotation_rule: u.arbitrary()?,
            tick_action: u.arbitrary()?,
            animation_hint: u.arbitrary()?,
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        arbitrary::size_hint::and_all(&[
            alloc::string::String::size_hint(depth),
            bool::size_hint(depth),
            RotationPlacementRule::size_hint(depth),
            TickAction::size_hint(depth),
            AnimationHint::size_hint(depth),
        ])
    }
}

impl crate::universe::VisitHandles for BlockAttributes {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        let Self {
            display_name: _,
            selectable: _,
            rotation_rule: _,
            tick_action,
            animation_hint: _,
        } = self;
        tick_action.visit_handles(visitor);
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
#[allow(clippy::exhaustive_structs)] // will deliberately break
pub struct TickAction {
    /// Operation to perform on the schedule.
    pub operation: Operation,
    /// Period, relative to the universe's [`TickSchedule`]'s tick length, with which
    /// to perform this action.
    ///
    /// For example, if this is `1`, then it will be executed on every tick.
    //---
    // TODO: This should probably be its own data type
    pub period: NonZeroU16,
}

impl TickAction {
    fn rotationally_symmetric(&self) -> bool {
        let Self {
            operation,
            period: _,
        } = self;
        operation.rotationally_symmetric()
    }

    fn rotate(self, rotation: GridRotation) -> TickAction {
        let Self { operation, period } = self;
        let operation = operation.rotate(rotation);
        Self { operation, period }
    }
}

impl From<Operation> for TickAction {
    /// TODO: remove uses of this
    fn from(operation: Operation) -> Self {
        Self {
            operation,
            period: NonZeroU16::MIN,
        }
    }
}

impl crate::universe::VisitHandles for TickAction {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        let Self {
            operation,
            period: _,
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
        #[allow(clippy::needless_pass_by_value)]
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
                animation_hint: AnimationHint::replacement(AnimationChange::Shape,),
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

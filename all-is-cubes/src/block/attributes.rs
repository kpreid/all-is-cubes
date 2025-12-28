//! [`BlockAttributes`] and closely related types.

use core::{fmt, ops};

use arcstr::ArcStr;

use crate::block::Modifier;
use crate::inv::InvInBlock;
use crate::math::{Face6, GridRotation};
use crate::op::Operation;
use crate::sound;
use crate::time;
use crate::universe::{HandleVisitor, VisitHandles};

#[cfg(doc)]
use crate::{
    block::{Block, BlockDef, Primitive},
    space::Space,
    time::TickSchedule,
};

/// This single-use macro takes the [`BlockAttributes`] struct declaration and generates things that
/// need to handle every attribute:
///
/// * Implementations on [`BlockAttributes`]
/// * The [`SetAttribute`] enum which has variants where [`BlockAttributes`] has fields
/// * Attribute methods on [`crate::block::Builder`]
macro_rules! derive_attribute_helpers {
    ($(#[$_:meta])* pub struct BlockAttributes {
        $(
            $(#[doc = $field_doc:literal])*
            #[custom(
                arbitrary_type = $arbitrary_type:ty,
                builder_param_style = $builder_param_style:tt,
            )]
            pub $field_name:ident: $field_type:ty,
        )*
    }) => {
        paste::paste! {
            /// Part of [`Modifier::SetAttribute`] specifying an attribute to set.
            #[derive(Clone, Eq, Hash, PartialEq)]
            #[non_exhaustive]
            pub enum SetAttribute {
                $(
                    #[doc = concat!(
                        "Replaces the value of [`BlockAttributes::", stringify!($field_name), "`]."
                    )]
                    [< $field_name :camel >]($field_type),
                )*
            }
        }

        impl BlockAttributes {
            /// For each field with a non-default value in `self`, produce a corresponding
            /// [`SetAttribute`].
            pub(crate) fn into_set_attributes(self) -> impl Iterator<Item = SetAttribute> {
                paste::paste! {
                    let Self { $( $field_name, )* } = self;
                    itertools::chain!(
                        $(
                            if $field_name != Self::DEFAULT_REF.$field_name {
                                Some(SetAttribute::[< $field_name :camel >]($field_name))
                            } else {
                                None
                            }
                        ),*
                    )
                }
            }
        }

        impl SetAttribute {
            pub(crate) fn apply(&self, attributes: &mut BlockAttributes) {
                paste::paste! {
                    match self {
                        $(
                            Self::[< $field_name :camel >](value) =>
                                attributes.$field_name = <$field_type as Clone>::clone(value),
                        )*
                    }
                }
            }
        }

        impl fmt::Debug for BlockAttributes {
            /// Only attributes which differ from the default are shown.
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if self == Self::DEFAULT_REF {
                    // Avoid the braceless formatting that `debug_struct` uses
                    // if no fields are given.
                    write!(f, "BlockAttributes {{}}")
                } else {
                    let Self {
                        $($field_name,)*
                    } = self;

                    let mut debug_struct = f.debug_struct("BlockAttributes");
                    $(
                        if *$field_name != Self::DEFAULT_REF.$field_name {
                            debug_struct.field(stringify!($field_name), $field_name);
                        }
                    )*
                    debug_struct.finish()
                }
            }
        }

        impl fmt::Debug for SetAttribute {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                paste::paste! {
                    match self {
                        $(
                            Self::[< $field_name :camel >](value) => {
                                // Differs from derive(Debug) in that:
                                // * One line unless the value is multiline
                                // * Includes the enum name
                                write!(f, "SetAttribute::{}(",
                                    stringify!([< $field_name :camel >]),
                                )?;
                                value.fmt(f)?;
                                write!(f, ")")
                            }
                        )*
                    }
                }
            }
        }

        impl VisitHandles for BlockAttributes {
            fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
                let Self { $( $field_name, )* } = self;
                $(
                    <$field_type as VisitHandles>::visit_handles($field_name, visitor);
                )*
            }
        }

        paste::paste! {
            impl VisitHandles for SetAttribute {
                fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
                    match self {
                        $(
                            Self::[< $field_name :camel >]($field_name) =>
                                VisitHandles::visit_handles($field_name, visitor),
                        )*
                    }
                }
            }
        }

        impl BlRotate for BlockAttributes {
            fn rotationally_symmetric(&self) -> bool {
                $(
                    <$field_type as BlRotate>::rotationally_symmetric(&self.$field_name) &&
                )*
                true // finish the && sequence
            }

            fn rotate(self, rotation: GridRotation) -> BlockAttributes {
                Self {
                    $(
                        $field_name: <$field_type as BlRotate>::rotate(self.$field_name, rotation),
                    )*
                }
            }
        }

        paste::paste! {
            impl BlRotate for SetAttribute {
                fn rotationally_symmetric(&self) -> bool {
                    match self {
                        $(
                            Self::[< $field_name :camel >]($field_name) =>
                                <$field_type as BlRotate>::rotationally_symmetric($field_name),
                        )*
                    }
                }

                fn rotate(self, rotation: GridRotation) -> Self {
                    match self {
                        $(
                            Self::[< $field_name :camel >]($field_name) =>
                                Self::[< $field_name :camel >](
                                    <$field_type as BlRotate>::rotate($field_name, rotation)
                                ),
                        )*
                    }
                }
            }
        }

        #[cfg(feature = "arbitrary")]
        #[mutants::skip]
        impl<'a> arbitrary::Arbitrary<'a> for BlockAttributes {
            fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
                Ok(BlockAttributes {
                    $(
                        $field_name:
                            <$arbitrary_type as arbitrary::Arbitrary>::arbitrary(u)?.into(),
                    )*
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
                        $(
                            <$arbitrary_type as arbitrary::Arbitrary>::try_size_hint(depth)?,
                        )*
                    ]))
                })
            }
        }

        #[cfg(feature = "arbitrary")]
        #[mutants::skip]
        impl<'a> arbitrary::Arbitrary<'a> for SetAttribute {
            fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
                _ = u;
                todo!()
            }

            fn size_hint(depth: usize) -> (usize, Option<usize>) {
                Self::try_size_hint(depth).unwrap_or_default()
            }
            fn try_size_hint(
                depth: usize,
            ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
                arbitrary::size_hint::try_recursion_guard(depth, |depth| {
                    Ok(arbitrary::size_hint::or_all(&[
                        $(
                            <$arbitrary_type as arbitrary::Arbitrary>::try_size_hint(depth)?,
                        )*
                    ]))
                })
            }
        }

        /// Methods for setting [attributes](BlockAttributes) of the block.
        impl<P, Txn> crate::block::Builder<'_, P, Txn> {
            $(
                attribute_builder_method!($(#[doc = $field_doc] )* $builder_param_style $field_name: $field_type);
            )*
        }
    };
}

#[rustfmt::skip] // avoid bug <https://github.com/rust-lang/rustfmt/issues/5489>
macro_rules! attribute_builder_method {
    // Custom name for the `inventory` field.
    ($(#[doc = $field_doc:literal] )* exact inventory: $field_type:ty) => {
        #[doc = concat!(
            "Sets the value for [`BlockAttributes::inventory`], which is:",
        )]
        #[doc = ""]
        $(#[doc = concat!("> ", $field_doc)] )*
        pub fn inventory_config(mut self, value: crate::inv::InvInBlock) -> Self {
            self.attributes.inventory = value.into();
            self
        }
    };

    // These two rules are identical except for the type of the method’s value parameter.
    ($(#[doc = $field_doc:literal] )* exact $field_name:ident: $field_type:ty) => {
        #[doc = concat!(
            "Sets the value for [`BlockAttributes::",
            stringify!($field_name),
            "`], which is:",
        )]
        #[doc = ""]
        $(#[doc = concat!("> ", $field_doc)] )*
        pub fn $field_name(mut self, value: $field_type) -> Self {
            self.attributes.$field_name = value;
            self
        }
    };
    ($(#[doc = $field_doc:literal] )* into $field_name:ident: $field_type:ty) => {
        #[doc = concat!(
            "Sets the value for [`BlockAttributes::",
            stringify!($field_name),
            "`], which is:",
        )]
        #[doc = ""]
        $(#[doc = concat!("> ", $field_doc)] )*
        pub fn $field_name(mut self, value: impl Into<$field_type>) -> Self {
            self.attributes.$field_name = value.into();
            self
        }
    };
}

/// Properties of blocks that are not the block’s voxels.
///
/// Attributes are set on blocks using [`Modifier::SetAttribute`].
/// Some other modifiers also implicitly change attributes, such as the
/// [animation hint][BlockAttributes::animation_hint].
///
/// `BlockAttributes::default()` returns the attributes which a block with no modifiers has.
#[derive(Clone, Eq, Hash, PartialEq)]
#[non_exhaustive]
#[macro_rules_attribute::derive(derive_attribute_helpers!)]
pub struct BlockAttributes {
    // CAUTION: The ordering of fields in this struct has effects beyond only documentation:
    //
    // * It is the ordering in which `block::Builder` creates `SetAttribute` modifiers.
    //---
    //
    /// The name that should be displayed to players.
    ///
    /// The default value is the empty string. The empty string should be considered a
    /// reasonable choice for solid-color blocks with no special features.
    //---
    // Design note: The use of `ArcStr` allows cloning a `BlockAttributes` to be O(1)
    // allocate no additional memory.
    #[custom(
        arbitrary_type = alloc::string::String,
        builder_param_style = into,
    )]
    pub display_name: ArcStr,

    /// Whether players' [cursors](crate::character::Cursor) target it or pass through it.
    ///
    /// The default value is `true`.
    #[custom(
        arbitrary_type = bool,
        builder_param_style = exact,
    )]
    pub selectable: bool,

    /// Definition of, if this block has an attached [`Modifier::Inventory`],
    /// what size and rendering it has.
    #[custom(
        arbitrary_type = InvInBlock,
        builder_param_style = exact,
    )]
    pub inventory: InvInBlock,

    /// Continuous sound emission and absorption by this block.
    #[custom(
        arbitrary_type = sound::Ambient,
        builder_param_style = exact,
    )]
    pub ambient_sound: sound::Ambient,

    /// Rule about how this block should be rotated, or not, when placed in a [`Space`] by
    /// some agent not otherwise specifying rotation.
    ///
    /// The default value is [`RotationPlacementRule::Never`].
    //---
    // TODO: Replace this with `placement_action` features?
    #[custom(
        arbitrary_type = RotationPlacementRule,
        builder_param_style = exact,
    )]
    pub rotation_rule: RotationPlacementRule,

    /// Something to do instead of placing this block in a [`Space`].
    //---
    // TODO: Should this not be optional, instead having a value that expresses the default
    // block placement behavior?
    #[custom(
        arbitrary_type = Option<PlacementAction>,
        builder_param_style = into,
    )]
    pub placement_action: Option<PlacementAction>,

    /// Something this block does when time passes.
    #[custom(
        arbitrary_type =  Option<TickAction>,
        builder_param_style = into,
    )]
    pub tick_action: Option<TickAction>,

    /// Something this block does when activated with [`Activate`](crate::inv::Tool::Activate).
    #[custom(
        arbitrary_type = Option<Operation>,
        builder_param_style = into,
    )]
    pub activation_action: Option<Operation>,

    /// Advice to the renderer about how to expect this block to change, and hence
    /// what rendering strategy to use.
    ///
    /// Note: This is automatically augmented for [`Primitive::Recur`] blocks if they
    /// contain voxels with animation hints themselves.
    #[custom(
        arbitrary_type = AnimationHint,
        builder_param_style = exact,
    )]
    pub animation_hint: AnimationHint,
}

impl BlockAttributes {
    const DEFAULT: Self = BlockAttributes {
        display_name: arcstr::literal!(""),
        selectable: true,
        inventory: InvInBlock::EMPTY,
        ambient_sound: sound::Ambient::SILENT,
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
}

impl Default for BlockAttributes {
    /// Block attributes suitable as default values for in-game use.
    ///
    /// These attributes are equal to the attributes of a block with no modifiers.
    fn default() -> BlockAttributes {
        // Delegate to the inherent impl `const fn`.
        BlockAttributes::default()
    }
}

impl From<SetAttribute> for Modifier {
    /// Wraps [`SetAttribute`] in [`Modifier::SetAttribute`].
    fn from(value: SetAttribute) -> Self {
        Modifier::SetAttribute(value)
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

impl BlRotate for RotationPlacementRule {
    fn rotationally_symmetric(&self) -> bool {
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

impl VisitHandles for RotationPlacementRule {
    fn visit_handles(&self, _: &mut dyn HandleVisitor) {}
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

impl VisitHandles for AnimationHint {
    fn visit_handles(&self, _: &mut dyn HandleVisitor) {}
}

impl BlRotate for AnimationHint {
    fn rotate(self, _: GridRotation) -> Self {
        self
    }
    fn rotationally_symmetric(&self) -> bool {
        true
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

impl BlRotate for TickAction {
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

impl VisitHandles for TickAction {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
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

impl BlRotate for PlacementAction {
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

impl VisitHandles for PlacementAction {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self {
            operation,
            in_front: _,
        } = self;
        operation.visit_handles(visitor);
    }
}

// -------------------------------------------------------------------------------------------------

/// Values which can be rotated, and have or not have symmetry with respect to that rotation
/// operation, in the way that blocks do.
///
/// This trait is used to assist with rotation of [`BlockAttributes`].
/// It is not public because there are edge cases in what it means to rotate a thing and
/// it is implemented for only the types that it is needed for; and in general, I have not thought
/// about whether it makes sense in general, only used it to solve a code organization problem.
pub(crate) trait BlRotate {
    // TODO: Add in-place rotation operation as the basic operation, to help with minimizing
    // reallocation?

    /// Rotate `self` by `rotation`.
    #[must_use]
    fn rotate(self, rotation: GridRotation) -> Self;

    /// Returns whether [`rotate()`][Self::rotate] would have no effect.
    #[must_use]
    fn rotationally_symmetric(&self) -> bool;
}

mod impl_rotate {
    use super::*;

    macro_rules! impl_rotate_always_symmetric {
        ($type:ty) => {
            impl BlRotate for $type {
                #[inline]
                fn rotate(self, _: GridRotation) -> Self {
                    self
                }

                #[inline]
                fn rotationally_symmetric(&self) -> bool {
                    true
                }
            }
        };
    }

    impl_rotate_always_symmetric!(());
    impl_rotate_always_symmetric!(bool);
    impl_rotate_always_symmetric!(ArcStr);
    impl_rotate_always_symmetric!(sound::Ambient);

    impl<T: BlRotate> BlRotate for Option<T> {
        fn rotate(self, rotation: GridRotation) -> Self {
            self.map(|value| value.rotate(rotation))
        }

        fn rotationally_symmetric(&self) -> bool {
            self.as_ref().is_none_or(<T as BlRotate>::rotationally_symmetric)
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::String;

    #[test]
    fn size() {
        let size = size_of::<BlockAttributes>();
        assert!(
            size <= 360,
            "size_of::<BlockAttributes>() = {size} unexpectedly large"
        );
    }

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

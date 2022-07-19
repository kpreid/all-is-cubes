// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Lesser-used helpers for [`BlockBuilder`].

use cgmath::EuclideanSpace as _;
use std::borrow::Cow;
use std::sync::Arc;

use crate::block::{
    AnimationHint, Block, BlockAttributes, BlockCollision, BlockDef, BlockParts, BlockPtr,
    Modifier, Primitive, Resolution, RotationPlacementRule,
};
use crate::drawing::VoxelBrush;
use crate::math::{GridPoint, Rgb, Rgba};
use crate::space::{SetCubeError, Space};
use crate::universe::{Name, URef, Universe, UniverseIndex};

/// Tool for constructing [`Block`] values conveniently.
///
/// To create one, call [`Block::builder()`].
/// ([`BlockBuilder::default()`] is also available.)
///
/// ```
/// use all_is_cubes::block::Block;
/// use all_is_cubes::math::Rgba;
///
/// let block = Block::builder()
///    .display_name("BROWN")
///    .color(Rgba::new(0.5, 0.5, 0., 1.))
///    .build();
///
/// assert_eq!(block.evaluate().unwrap().color, Rgba::new(0.5, 0.5, 0., 1.));
/// assert_eq!(
///     block.evaluate().unwrap().attributes.display_name.as_ref(),
///     "BROWN",
/// );
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
#[must_use]
pub struct BlockBuilder<P> {
    attributes: BlockAttributes,
    primitive_builder: P,
    modifiers: Vec<Modifier>,
}

impl Default for BlockBuilder<NeedsPrimitive> {
    fn default() -> Self {
        Self::new()
    }
}

impl BlockBuilder<NeedsPrimitive> {
    /// Common implementation of [`Block::builder`] and [`Default::default`]; use one of those to call this.
    pub(super) const fn new() -> BlockBuilder<NeedsPrimitive> {
        BlockBuilder {
            attributes: BlockAttributes::default(),
            primitive_builder: NeedsPrimitive,
            modifiers: Vec::new(),
        }
    }
}

impl<C> BlockBuilder<C> {
    // TODO: When #![feature(const_precise_live_drops)] becomes stable, we can make
    // this builder mostly usable in const contexts.
    // https://github.com/rust-lang/rust/issues/73255
    // Doing that will also require creating non-trait-using alternate methods,
    // until const traits https://github.com/rust-lang/rust/issues/67792 is also available.

    /// Sets the [`BlockAttributes`] the block will have.
    /// This replaces individual attribute values set using other builder methods.
    pub fn attributes(mut self, value: BlockAttributes) -> Self {
        self.attributes = value;
        self
    }

    /// Sets the value for [`BlockAttributes::display_name`].
    pub fn display_name(mut self, value: impl Into<Cow<'static, str>>) -> Self {
        self.attributes.display_name = value.into();
        self
    }

    /// Sets the value for [`BlockAttributes::selectable`].
    pub const fn selectable(mut self, value: bool) -> Self {
        self.attributes.selectable = value;
        self
    }

    /// Sets the value for [`BlockAttributes::collision`].
    pub const fn collision(mut self, value: BlockCollision) -> Self {
        self.attributes.collision = value;
        self
    }

    /// Sets the value for [`BlockAttributes::rotation_rule`].
    pub const fn rotation_rule(mut self, value: RotationPlacementRule) -> Self {
        self.attributes.rotation_rule = value;
        self
    }

    /// Sets the value for [`BlockAttributes::light_emission`].
    pub fn light_emission(mut self, value: impl Into<Rgb>) -> Self {
        self.attributes.light_emission = value.into();
        self
    }

    /// Sets the value for [`BlockAttributes::tick_action`].
    pub fn tick_action(mut self, value: Option<VoxelBrush<'static>>) -> Self {
        self.attributes.tick_action = value;
        self
    }

    /// Sets the value for [`BlockAttributes::animation_hint`].
    pub fn animation_hint(mut self, value: AnimationHint) -> Self {
        self.attributes.animation_hint = value;
        self
    }

    pub fn modifier(mut self, modifier: Modifier) -> Self {
        // TODO: implement a modifier canonicalization procedure here
        self.modifiers.push(modifier);
        self
    }

    /// Sets the color value for building a [`Primitive::Atom`].
    ///
    /// This will replace any previous color **or voxels.**
    pub fn color(self, color: impl Into<Rgba>) -> BlockBuilder<Rgba> {
        BlockBuilder {
            attributes: self.attributes,
            primitive_builder: color.into(),
            modifiers: Vec::new(),
        }
    }

    /// Sets the space for building a [`Primitive::Recur`].
    ///
    /// This will replace any previous voxels **or color.**
    pub fn voxels_ref(
        self,
        resolution: Resolution,
        space: URef<Space>,
    ) -> BlockBuilder<BlockBuilderVoxels> {
        BlockBuilder {
            attributes: self.attributes,
            primitive_builder: BlockBuilderVoxels {
                space,
                resolution,
                offset: GridPoint::origin(),
            },
            modifiers: Vec::new(),
        }
    }

    /// Constructs a `Space` for building a [`Primitive::Recur`], and calls
    /// the given function to fill it with blocks, in the manner of [`Space::fill`].
    ///
    /// Note that the resulting builder is cloned, all clones will share the same
    /// space.
    // TODO: (doc) test for this
    pub fn voxels_fn<F, B>(
        self,
        // TODO: awkward requirement for universe; defer computation instead, or
        // add a `.universe()` method to provide it once.
        universe: &mut Universe,
        // TODO: Maybe resolution should be a separate method? Check usage patterns later.
        resolution: Resolution,
        mut function: F,
    ) -> Result<BlockBuilder<BlockBuilderVoxels>, SetCubeError>
    where
        F: FnMut(GridPoint) -> B,
        B: std::borrow::Borrow<Block>,
    {
        let mut space = Space::for_block(resolution).build_empty();
        // TODO: Teach the SpaceBuilder to accept a function in the same way
        space.fill(space.bounds(), |point| Some(function(point)))?;
        Ok(self.voxels_ref(resolution, universe.insert_anonymous(space)))
    }

    /// Converts this builder into a block value.
    pub fn build(self) -> Block
    where
        C: BuildPrimitiveIndependent,
    {
        Block(BlockPtr::Owned(Arc::new(BlockParts {
            primitive: self.primitive_builder.build_i(self.attributes),
            modifiers: self.modifiers,
        })))
    }

    /// Converts this builder into a block value and stores it as a [`BlockDef`] in
    /// the given [`Universe`] with the given name, then returns a [`Primitive::Indirect`]
    /// block referring to it.
    // TODO: This is not being used, because most named block definitions use BlockProvider,
    // and complicates the builder unnecessarily. Remove it or figure out how it aligns with
    // BlockProvider.
    pub fn into_named_definition(
        self,
        universe: &mut Universe,
        name: impl Into<Name>,
    ) -> Result<Block, crate::universe::InsertError>
    where
        C: BuildPrimitiveInUniverse,
    {
        let block = Block(BlockPtr::Owned(Arc::new(BlockParts {
            primitive: self.primitive_builder.build_u(self.attributes, universe),
            modifiers: self.modifiers,
        })));
        let def_ref = universe.insert(name.into(), BlockDef::new(block))?;
        Ok(Block::from_primitive(Primitive::Indirect(def_ref)))
    }
}

/// Voxel-specific builder methods.
impl BlockBuilder<BlockBuilderVoxels> {
    /// Sets the coordinate offset for building a [`Primitive::Recur`]:
    /// the lower-bound corner of the region of the [`Space`]
    /// which will be used for block voxels. The default is zero.
    pub fn offset(mut self, offset: GridPoint) -> Self {
        self.primitive_builder.offset = offset;
        self
    }

    // TODO: It might be useful to have "offset equal to resolution"
    // and "add offset", but don't add those until use cases are seen.
}

/// Allows implicitly converting `BlockBuilder` to the block it would build.
impl<C: BuildPrimitiveIndependent> From<BlockBuilder<C>> for Block {
    fn from(builder: BlockBuilder<C>) -> Self {
        builder.build()
    }
}
/// Equivalent to `Block::builder().color(color)`.
impl From<Rgba> for BlockBuilder<Rgba> {
    fn from(color: Rgba) -> Self {
        Block::builder().color(color)
    }
}
/// Equivalent to `Block::builder().color(color.with_alpha_one())`.
impl From<Rgb> for BlockBuilder<Rgba> {
    fn from(color: Rgb) -> Self {
        Block::builder().color(color.with_alpha_one())
    }
}

/// Placeholder type for an incomplete [`BlockBuilder`]'s content. The builder
/// cannot create an actual block until this is replaced.
#[allow(clippy::exhaustive_structs)]
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct NeedsPrimitive;

/// Primitive-builder of a [`BlockBuilder`] that can build a block without a [`Universe`].
pub trait BuildPrimitiveIndependent {
    fn build_i(self, attributes: BlockAttributes) -> Primitive;
}
/// Primitive-builder of a [`BlockBuilder`] that can only build a block with a [`Universe`].
pub trait BuildPrimitiveInUniverse {
    fn build_u(self, attributes: BlockAttributes, universe: &mut Universe) -> Primitive;
}
/// Every [`BuildPrimitiveIndependent`] can act as [`BuildPrimitiveInUniverse`].
impl<T: BuildPrimitiveIndependent> BuildPrimitiveInUniverse for T {
    fn build_u(self, attributes: BlockAttributes, _: &mut Universe) -> Primitive {
        self.build_i(attributes)
    }
}

/// Used by [`BlockBuilder::color`].
impl BuildPrimitiveIndependent for Rgba {
    fn build_i(self, attributes: BlockAttributes) -> Primitive {
        Primitive::Atom(attributes, self)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct BlockBuilderVoxels {
    space: URef<Space>,
    resolution: Resolution,
    offset: GridPoint,
}
impl BuildPrimitiveIndependent for BlockBuilderVoxels {
    fn build_i(self, attributes: BlockAttributes) -> Primitive {
        Primitive::Recur {
            attributes,
            offset: self.offset,
            resolution: self.resolution,
            space: self.space,
        }
    }
}

//! Lesser-used helpers for [`BlockBuilder`].

use alloc::borrow::Cow;
use alloc::sync::Arc;
use alloc::vec::Vec;

use arcstr::ArcStr;

use crate::block::{
    AnimationHint, Atom, Block, BlockAttributes, BlockCollision, BlockParts, BlockPtr, Modifier,
    Primitive, Resolution, RotationPlacementRule, AIR,
};
use crate::math::{Cube, GridAab, GridPoint, Rgb, Rgba};
use crate::space::{SetCubeError, Space};
use crate::transaction::{self, Merge, Transaction};
use crate::universe::{Handle, Name, Universe, UniverseTransaction};

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
///     block.evaluate().unwrap().attributes.display_name.as_str(),
///     "BROWN",
/// );
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
#[must_use]
pub struct BlockBuilder<P, Txn> {
    attributes: BlockAttributes,
    primitive_builder: P,
    modifiers: Vec<Modifier>,

    /// If this is a [`UniverseTransaction`], then it must be produced for the caller to execute.
    /// If this is `()`, then it may be disregarded.
    transaction: Txn,
}

impl Default for BlockBuilder<NeedsPrimitive, ()> {
    fn default() -> Self {
        Self::new()
    }
}

impl BlockBuilder<NeedsPrimitive, ()> {
    /// Common implementation of [`Block::builder`] and [`Default::default`]; use one of those to call this.
    pub(super) const fn new() -> Self {
        BlockBuilder {
            attributes: BlockAttributes::default(),
            primitive_builder: NeedsPrimitive,
            modifiers: Vec::new(),
            transaction: (),
        }
    }
}

impl<P, Txn> BlockBuilder<P, Txn> {
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
    pub fn display_name(mut self, value: impl Into<ArcStr>) -> Self {
        self.attributes.display_name = value.into();
        self
    }

    /// Sets the value for [`BlockAttributes::selectable`].
    pub const fn selectable(mut self, value: bool) -> Self {
        self.attributes.selectable = value;
        self
    }

    /// Sets the value for [`BlockAttributes::rotation_rule`].
    pub const fn rotation_rule(mut self, value: RotationPlacementRule) -> Self {
        self.attributes.rotation_rule = value;
        self
    }

    /// Sets the value for [`BlockAttributes::tick_action`].
    pub fn tick_action(mut self, value: impl Into<Option<super::TickAction>>) -> Self {
        self.attributes.tick_action = value.into();
        self
    }

    /// Sets the value for [`BlockAttributes::animation_hint`].
    pub fn animation_hint(mut self, value: AnimationHint) -> Self {
        self.attributes.animation_hint = value;
        self
    }

    /// Adds a modifier to the end of the list of modifiers for the block.
    /// It will be applied after all previously specified modifiers.
    pub fn modifier(mut self, modifier: Modifier) -> Self {
        // TODO: implement a modifier canonicalization procedure here
        self.modifiers.push(modifier);
        self
    }

    /// Sets the color value for building a [`Primitive::Atom`].
    ///
    /// This will replace any previous color **or voxels.**
    pub fn color(self, color: impl Into<Rgba>) -> BlockBuilder<BlockBuilderAtom, ()> {
        BlockBuilder {
            attributes: self.attributes,
            primitive_builder: BlockBuilderAtom {
                color: color.into(),
                emission: Rgb::ZERO,
                collision: BlockCollision::Hard,
            },
            modifiers: Vec::new(),
            // TODO: This might not be the right thing in more general transaction usage.
            // For now, it's OK that we discard the transaction because it can only ever be
            // inserting a `Space`.
            transaction: (),
        }
    }

    /// Sets the space for building a [`Primitive::Recur`].
    ///
    /// This will replace any previous voxels **or color.**
    pub fn voxels_handle(
        self,
        resolution: Resolution,
        space: Handle<Space>,
    ) -> BlockBuilder<BlockBuilderVoxels, ()> {
        BlockBuilder {
            attributes: self.attributes,
            primitive_builder: BlockBuilderVoxels {
                space,
                resolution,
                offset: GridPoint::origin(),
            },
            modifiers: Vec::new(),
            transaction: (),
        }
    }

    /// Constructs a `Space` for building a [`Primitive::Recur`], and calls
    /// the given function to fill it with blocks, in the manner of [`Space::fill`].
    ///
    /// If the voxels do not fill the entire volume of the block being built — that is, there is
    /// some smaller region outside of which they are all [`AIR`] — then the [`Space`] will be
    /// shrunk to tightly enclose that region, to improve performance. However, this still requires
    /// the function to be called on all positions within the full block bounds, so for very high
    /// ratios of resolution to actual content, it may be wise to use
    /// [`voxels_handle()`](Self::voxels_handle) instead.
    ///
    /// Note that if the resulting builder is cloned, all clones will share the same
    /// space.
    // TODO: (doc) test for this
    pub fn voxels_fn<'a, F, B>(
        self,
        // TODO: Maybe resolution should be a separate method? Check usage patterns later.
        resolution: Resolution,
        mut function: F,
    ) -> Result<BlockBuilder<BlockBuilderVoxels, UniverseTransaction>, SetCubeError>
    where
        F: FnMut(Cube) -> B,
        B: Into<Cow<'a, Block>>,
    {
        // This is a worldgen convenience, not the most efficient possible path (which would be
        // `SpaceBuilder::palette_and_contents()`), so save quite a lot of code generation
        // by keeping it monomorphic and not inlined.
        #[inline(never)]
        fn voxels_fn_impl<'a>(
            attributes: BlockAttributes,
            modifiers: Vec<Modifier>,
            resolution: Resolution,
            function: &mut dyn FnMut(Cube) -> Cow<'a, Block>,
        ) -> Result<BlockBuilder<BlockBuilderVoxels, UniverseTransaction>, SetCubeError> {
            let mut not_air_bounds: Option<GridAab> = None;

            let mut space = Space::for_block(resolution).build();
            // TODO: Teach the SpaceBuilder to accept a function in the same way?
            space.fill(space.bounds(), |cube| {
                let block = function(cube);

                // Track which of the blocks are not equal to AIR, for later use.
                if block.as_ref() != &AIR {
                    let cube_bb = cube.grid_aab();
                    not_air_bounds = Some(if let Some(bounds) = not_air_bounds {
                        bounds.union(cube_bb).unwrap()
                    } else {
                        cube_bb
                    });
                }

                Some(block)
            })?;

            // If the block bounding box is not full of non-AIR blocks, then construct a replacement
            // Space that is smaller. This is equivalent, but improves the performance of all future
            // uses of this block.
            let not_air_bounds = not_air_bounds.unwrap_or(GridAab::ORIGIN_EMPTY);
            if space.bounds() != not_air_bounds {
                // TODO: Eventually we should be able to ask the Space to resize itself,
                // but that is not yet an available operation.
                let mut shrunk = Space::builder(not_air_bounds)
                    .physics(space.physics().clone())
                    .build();
                shrunk.fill(not_air_bounds, |cube| Some(&space[cube]))?;
                space = shrunk;
            }

            let space_handle = Handle::new_pending(Name::Pending, space);

            Ok(BlockBuilder {
                attributes,
                primitive_builder: BlockBuilderVoxels {
                    space: space_handle.clone(),
                    resolution,
                    offset: GridPoint::origin(),
                },
                modifiers,
                transaction: UniverseTransaction::insert(space_handle),
            })
        }

        voxels_fn_impl(self.attributes, self.modifiers, resolution, &mut |cube| {
            function(cube).into()
        })
    }

    fn build_block_and_txn_internal(self) -> (Block, Txn)
    where
        P: BuildPrimitive,
    {
        let primitive = self.primitive_builder.build_primitive(self.attributes);
        let block = if matches!(primitive, Primitive::Air) && self.modifiers.is_empty() {
            // Avoid allocating an Arc.
            AIR
        } else {
            Block(BlockPtr::Owned(Arc::new(BlockParts {
                primitive,
                modifiers: self.modifiers,
            })))
        };
        (block, self.transaction)
    }
}

impl<P: BuildPrimitive> BlockBuilder<P, ()> {
    /// Converts this builder into a block value.
    ///
    /// This method may only be used when the builder has *not* been used with `voxels_fn()`,
    /// since in that case a universe transaction must be executed.
    pub fn build(self) -> Block {
        let (block, ()) = self.build_block_and_txn_internal();
        block
    }
}

impl<P: BuildPrimitive> BlockBuilder<P, UniverseTransaction> {
    // TODO: Also allow extracting the transaction for later use

    /// Converts this builder into a block value, and inserts its associated [`Space`] into the
    /// given universe.
    pub fn build_into(self, universe: &mut Universe) -> Block {
        let (block, transaction) = self.build_block_and_txn_internal();

        // The transaction is always an insert_anonymous, which cannot fail.
        transaction
            .execute(universe, &mut transaction::no_outputs)
            .unwrap();

        block
    }

    /// Converts this builder into a [`Block`] value, and modifies the given transaction to include
    /// inserting the associated space into the universe the block is to be used in.
    pub fn build_txn(self, transaction: &mut UniverseTransaction) -> Block {
        let (block, txn) = self.build_block_and_txn_internal();
        transaction.merge_from(txn).unwrap();
        block
    }
}

/// Atom-specific builder methods.
impl<Txn> BlockBuilder<BlockBuilderAtom, Txn> {
    /// Sets the collision behavior of a [`Primitive::Atom`] block.
    pub const fn collision(mut self, collision: BlockCollision) -> Self {
        self.primitive_builder.collision = collision;
        self
    }

    /// Sets the light emission of a [`Primitive::Atom`] block.
    ///
    /// This quantity is the [_luminance_](https://en.wikipedia.org/wiki/Luminance) of
    /// the block surface, in unspecified units where 1.0 is the display white level
    /// (except for the effects of tone mapping).
    /// In the future this may be redefined in terms of a physical unit, but with the same
    /// dimensions.
    ///
    /// TODO: Define the interpretation for non-opaque blocks.
    pub fn light_emission(mut self, value: impl Into<Rgb>) -> Self {
        self.primitive_builder.emission = value.into();
        self
    }
}

/// Voxel-specific builder methods.
impl<Txn> BlockBuilder<BlockBuilderVoxels, Txn> {
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
impl<C: BuildPrimitive> From<BlockBuilder<C, ()>> for Block {
    fn from(builder: BlockBuilder<C, ()>) -> Self {
        builder.build()
    }
}
/// Equivalent to `Block::builder().color(color)`.
impl From<Rgba> for BlockBuilder<BlockBuilderAtom, ()> {
    fn from(color: Rgba) -> Self {
        Block::builder().color(color)
    }
}
/// Equivalent to `Block::builder().color(color.with_alpha_one())`.
impl From<Rgb> for BlockBuilder<BlockBuilderAtom, ()> {
    fn from(color: Rgb) -> Self {
        Block::builder().color(color.with_alpha_one())
    }
}

/// Placeholder type for an incomplete [`BlockBuilder`]'s content. The builder
/// cannot create an actual block until this is replaced.
#[allow(clippy::exhaustive_structs)]
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct NeedsPrimitive;

/// Something that a parameterized [`BlockBuilder`] can use to construct a block's primitive.
#[doc(hidden)]
pub trait BuildPrimitive {
    fn build_primitive(self, attributes: BlockAttributes) -> Primitive;
}

/// Parameter type for [`BlockBuilder::color`], building [`Primitive::Atom`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct BlockBuilderAtom {
    color: Rgba,
    emission: Rgb,
    collision: BlockCollision,
}
impl BuildPrimitive for BlockBuilderAtom {
    fn build_primitive(self, attributes: BlockAttributes) -> Primitive {
        Primitive::Atom(Atom {
            attributes,
            color: self.color,
            emission: self.emission,
            collision: self.collision,
        })
    }
}

/// Parameter type for a [`BlockBuilder`] that is building a block with voxels
/// ([`Primitive::Recur`]).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct BlockBuilderVoxels {
    space: Handle<Space>,
    resolution: Resolution,
    offset: GridPoint,
}
impl BuildPrimitive for BlockBuilderVoxels {
    fn build_primitive(self, attributes: BlockAttributes) -> Primitive {
        Primitive::Recur {
            attributes,
            offset: self.offset,
            resolution: self.resolution,
            space: self.space,
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::boxed::Box;

    use crate::block::{self, Resolution::*, TickAction};
    use crate::content::palette;
    use crate::math::{Face6, Vol};
    use crate::op::Operation;
    use crate::space::SpacePhysics;
    use crate::transaction::Transactional as _;

    use super::*;

    #[test]
    fn defaults() {
        let color = Rgba::new(0.1, 0.2, 0.3, 0.4);
        assert_eq!(
            Block::builder().color(color).build(),
            Block::from(Atom {
                attributes: BlockAttributes::default(),
                color,
                emission: Rgb::ZERO,
                collision: BlockCollision::Hard,
            }),
        );
    }

    #[test]
    fn default_equivalent() {
        assert_eq!(
            BlockBuilder::new(),
            <BlockBuilder<NeedsPrimitive, ()> as Default>::default()
        );
    }

    #[test]
    fn every_field_nondefault() {
        let color = Rgba::new(0.1, 0.2, 0.3, 0.4);
        let emission = Rgb::new(0.1, 3.0, 0.1);
        let rotation_rule = RotationPlacementRule::Attach { by: Face6::NZ };
        let tick_action = Some(TickAction::from(Operation::Become(AIR)));
        assert_eq!(
            Block::builder()
                .color(color)
                .display_name("hello world")
                .collision(BlockCollision::None)
                .rotation_rule(rotation_rule)
                .selectable(false)
                .light_emission(emission)
                .tick_action(tick_action.clone())
                .animation_hint(AnimationHint::replacement(block::AnimationChange::Shape))
                .build(),
            Block::from(Atom {
                attributes: BlockAttributes {
                    display_name: "hello world".into(),
                    rotation_rule,
                    selectable: false,
                    tick_action,
                    animation_hint: AnimationHint::replacement(block::AnimationChange::Shape),
                },
                color,
                emission,
                collision: BlockCollision::None,
            }),
        );
    }

    #[test]
    fn voxels_from_space() {
        let mut universe = Universe::new();
        let space_handle = universe.insert_anonymous(Space::empty_positive(1, 1, 1));

        assert_eq!(
            Block::builder()
                .display_name("hello world")
                .voxels_handle(R2, space_handle.clone())
                .build(),
            Block::from_primitive(Primitive::Recur {
                attributes: BlockAttributes {
                    display_name: "hello world".into(),
                    ..BlockAttributes::default()
                },
                offset: GridPoint::origin(),
                resolution: R2, // not same as space size
                space: space_handle
            }),
        );
    }

    #[test]
    fn voxels_from_fn_basic() {
        let mut universe = Universe::new();

        let resolution = R4;
        let expected_bounds = GridAab::for_block(resolution);
        let atom = color_block!(palette::DIRT);
        let block = Block::builder()
            .display_name("hello world")
            .voxels_fn(resolution, |_cube| &atom)
            .unwrap()
            .build_into(&mut universe);

        // Extract the implicitly constructed space handle
        let space_handle = if let Primitive::Recur { space, .. } = block.primitive() {
            space.clone()
        } else {
            panic!("expected Recur, found {block:?}");
        };

        assert_eq!(
            block,
            Block::from_primitive(Primitive::Recur {
                attributes: BlockAttributes {
                    display_name: "hello world".into(),
                    ..BlockAttributes::default()
                },
                offset: GridPoint::origin(),
                resolution,
                space: space_handle.clone()
            }),
        );

        // Check the space's characteristics
        let space = space_handle.read().unwrap();
        assert_eq!(space.bounds(), expected_bounds);
        assert_eq!(space.physics(), &SpacePhysics::DEFAULT_FOR_BLOCK);
        assert_eq!(
            space.extract(expected_bounds, |e| e.block_data().block()),
            Vol::<Box<[&Block]>>::from_fn(expected_bounds, |_| &atom)
        );
    }

    /// `voxels_fn()` automatically shrinks the space bounds to fit only the nonair blocks.
    #[test]
    fn voxels_from_fn_shrinkwrap() {
        let mut universe = Universe::new();

        let resolution = R4;
        let expected_bounds = GridAab::from_lower_upper([0, 0, 0], [2, 4, 4]);
        let atom = color_block!(palette::DIRT);
        let block = Block::builder()
            .display_name("hello world")
            .voxels_fn(resolution, |cube| {
                if expected_bounds.contains_cube(cube) {
                    &atom
                } else {
                    &AIR
                }
            })
            .unwrap()
            .build_into(&mut universe);

        // Extract the implicitly constructed space handle
        let space_handle = if let Primitive::Recur { space, .. } = block.primitive() {
            space.clone()
        } else {
            panic!("expected Recur, found {block:?}");
        };

        // Check the space's characteristics; not just that it has the smaller bounds, but that
        // it has the expected physics and contents.
        let space = space_handle.read().unwrap();
        assert_eq!(space.bounds(), expected_bounds);
        assert_eq!(space.physics(), &SpacePhysics::DEFAULT_FOR_BLOCK);
        assert_eq!(
            space.extract(expected_bounds, |e| e.block_data().block()),
            Vol::<Box<[&Block]>>::from_fn(expected_bounds, |_| &atom)
        );
    }

    #[test]
    fn explicit_txn() {
        let resolution = R8;
        let mut universe = Universe::new();
        let _block = universe
            .transact(|txn, _| {
                Ok(Block::builder()
                    .display_name("hello world")
                    .voxels_fn(resolution, |_cube| &AIR)
                    .unwrap()
                    .build_txn(txn))
            })
            .unwrap();

        assert_eq!(universe.iter_by_type::<Space>().count(), 1);
    }
}

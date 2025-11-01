//! Support for [`Builder`].

use alloc::borrow::Cow;
use alloc::sync::Arc;
use alloc::vec::Vec;

use crate::block::{
    self, AIR, Block, BlockAttributes, BlockCollision, BlockParts, BlockPtr, Modifier, Primitive,
    Resolution,
};
use crate::math::{Cube, GridAab, GridPoint, Rgb, Rgb01, Rgba};
use crate::space::{SetCubeError, Space};
use crate::transaction::{self, Merge, Transaction};
use crate::universe::{Handle, Name, ReadTicket, Universe, UniverseTransaction};

#[cfg(doc)]
use crate::space;

/// Tool for constructing [`Block`] values conveniently.
///
/// It can also be used to construct [`BlockAttributes`] values.
///
/// To create one, call [`Block::builder()`].
/// ([`Builder::default()`] is also available.)
///
/// # Example
///
/// ```
/// use all_is_cubes::block::{Block, EvaluatedBlock};
/// use all_is_cubes::math::Rgba;
/// use all_is_cubes::universe::ReadTicket;
///
/// let block = Block::builder()
///    .display_name("BROWN")
///    .color(Rgba::new(0.5, 0.5, 0., 1.))
///    .build();
///
/// let evaluated: EvaluatedBlock = block.evaluate(ReadTicket::stub()).unwrap();
/// assert_eq!(evaluated.color(), Rgba::new(0.5, 0.5, 0., 1.));
/// assert_eq!(evaluated.attributes().display_name.as_str(), "BROWN");
/// ```
///
/// # Type parameters
///
/// * `P` is a type corresponding to the type of [`Primitive`] that is being built.
/// * `Txn` is [`UniverseTransaction`] if the block builder is also building a transaction
///   that must be executed, and [`()`][primitive@unit] otherwise.
#[derive(Clone, Debug, Eq, PartialEq)]
#[must_use]
pub struct Builder<'u, P, Txn> {
    read_ticket: ReadTicket<'u>,

    /// public so that `BlockAttributes`'s macros can define methods for us
    pub(in crate::block) attributes: BlockAttributes,

    primitive_builder: P,

    modifiers: Vec<Modifier>,

    /// If this is a [`UniverseTransaction`], then it must be produced for the caller to execute.
    /// If this is `()`, then it may be disregarded.
    transaction: Txn,
}

impl Default for Builder<'_, NeedsPrimitive, ()> {
    fn default() -> Self {
        Self::new()
    }
}

impl Builder<'_, NeedsPrimitive, ()> {
    /// Common implementation of [`Block::builder`] and [`Default::default`]; use one of those to call this.
    pub(super) const fn new() -> Self {
        Builder {
            read_ticket: ReadTicket::stub(),
            attributes: BlockAttributes::default(),
            primitive_builder: NeedsPrimitive,
            modifiers: Vec::new(),
            transaction: (),
        }
    }

    /// Returns a [`BlockAttributes`] instead of building a block with those attributes.
    ///
    /// Panics if any modifiers were added to the builder.
    #[track_caller]
    pub fn build_attributes(self) -> BlockAttributes {
        let Self {
            read_ticket: _,
            attributes,
            primitive_builder: NeedsPrimitive,
            modifiers,
            transaction: (),
        } = self;
        assert_eq!(modifiers, []);
        attributes
    }
}

impl<'u, P, Txn> Builder<'u, P, Txn> {
    /// Sets the [`BlockAttributes`] the block will have.
    /// This replaces individual attribute values set using other builder methods.
    pub fn attributes(mut self, value: BlockAttributes) -> Self {
        self.attributes = value;
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
    pub fn color(self, color: impl Into<Rgba>) -> Builder<'u, Atom, ()> {
        let Self {
            read_ticket,
            attributes,
            primitive_builder: _,
            modifiers,
            transaction: _,
        } = self;
        Builder {
            read_ticket,
            attributes,
            primitive_builder: Atom {
                color: color.into(),
                emission: Rgb::ZERO,
                collision: BlockCollision::Hard,
            },
            modifiers,
            // TODO: This might not be the right thing in more general transaction usage.
            // For now, it's OK that we discard the transaction because it can only ever be
            // inserting a `Space` we are not going to use any more.
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
    ) -> Builder<'u, Voxels, ()> {
        let Self {
            read_ticket,
            attributes,
            primitive_builder: _,
            modifiers,
            transaction: _,
        } = self;
        Builder {
            read_ticket,
            attributes,
            primitive_builder: Voxels {
                space,
                resolution,
                offset: GridPoint::origin(),
            },
            modifiers,
            // TODO: This might not be the right thing in more general transaction usage.
            // For now, it's OK that we discard the transaction because it can only ever be
            // inserting a `Space` we are not going to use any more.
            transaction: (),
        }
    }

    /// As [`Self::voxels_handle()`], but for inserting the [`Space`] too.
    ///
    /// TODO: good public API?
    pub(crate) fn voxels_space(
        self,
        resolution: Resolution,
        space: Space,
    ) -> Builder<'u, Voxels, UniverseTransaction> {
        let (space_handle, transaction) = UniverseTransaction::insert(Name::Pending, space);

        let Self {
            read_ticket,
            attributes,
            primitive_builder: _,
            modifiers,
            transaction: _,
        } = self;
        Builder {
            read_ticket,
            attributes,
            primitive_builder: Voxels {
                space: space_handle,
                resolution,
                offset: GridPoint::origin(),
            },
            modifiers,
            // TODO: This might not be the right thing in more general transaction usage.
            // For now, it's OK that we discard the transaction because it can only ever be
            // inserting a `Space` we are not going to use any more.
            transaction,
        }
    }

    /// Constructs a `Space` for building a [`Primitive::Recur`], and calls
    /// the given function to fill it with blocks, in the manner of [`space::Mutation::fill()`].
    ///
    /// You must provide a [`ReadTicket`] using [`Builder::read_ticket()`]
    /// if any of the provided voxel blocks contain relevant handles.
    /// (Blocks constructed purely from colors do not.)
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
    ) -> Result<Builder<'u, Voxels, UniverseTransaction>, SetCubeError>
    where
        F: FnMut(Cube) -> B,
        B: Into<Cow<'a, Block>>,
    {
        // This is a worldgen convenience, not the most efficient possible path (which would be
        // `Builder::palette_and_contents()`), so save quite a lot of code generation
        // by keeping it monomorphic and not inlined.
        #[inline(never)]
        fn voxels_fn_impl<'a, 'u>(
            read_ticket: ReadTicket<'u>,
            attributes: BlockAttributes,
            modifiers: Vec<Modifier>,
            resolution: Resolution,
            function: &mut dyn FnMut(Cube) -> Cow<'a, Block>,
        ) -> Result<Builder<'u, Voxels, UniverseTransaction>, SetCubeError> {
            let mut not_air_bounds: Option<GridAab> = None;

            let mut space = Space::for_block(resolution).build();
            // TODO: Teach the Space Builder to accept a function in the same way?
            space.mutate(read_ticket, |m| {
                m.fill_all(|cube| {
                    let block = function(cube);

                    // Track which of the blocks are not equal to AIR, for later use.
                    if block.as_ref() != &AIR {
                        let cube_bb = cube.grid_aab();
                        not_air_bounds = Some(if let Some(bounds) = not_air_bounds {
                            bounds.union_box(cube_bb)
                        } else {
                            cube_bb
                        });
                    }

                    Some(block)
                })
            })?;

            // If the block bounding box is not full of non-AIR blocks, then construct a replacement
            // Space that is smaller. This is equivalent, but improves the performance of all future
            // uses of this block.
            let not_air_bounds = not_air_bounds.unwrap_or(GridAab::ORIGIN_EMPTY);
            if space.bounds() != not_air_bounds {
                // TODO: Eventually we should be able to ask the Space to resize itself,
                // but that is not yet an available operation.
                let mut shrunk =
                    Space::builder(not_air_bounds).physics(space.physics().clone()).build();
                shrunk.mutate(read_ticket, |m| {
                    m.fill(not_air_bounds, |cube| Some(&space[cube]))
                })?;
                space = shrunk;
            }

            let (space_handle, transaction) = UniverseTransaction::insert(Name::Pending, space);

            Ok(Builder {
                read_ticket,
                attributes,
                primitive_builder: Voxels {
                    space: space_handle,
                    resolution,
                    offset: GridPoint::origin(),
                },
                modifiers,
                transaction,
            })
        }

        let Self {
            read_ticket,
            attributes,
            primitive_builder: _,
            modifiers,
            transaction: _,
        } = self;
        voxels_fn_impl(
            read_ticket,
            attributes,
            modifiers,
            resolution,
            &mut |cube| function(cube).into(),
        )
    }

    /// Set the [`ReadTicket`] used by this builder.
    ///
    /// This is currently only necessary when using [`Builder::voxels_fn()`] with blocks that
    /// contain handles. It is not necessary when simple solid-color blocks are used.
    ///
    /// The default is [`ReadTicket::stub()`].
    //---
    #[expect(clippy::elidable_lifetime_names, reason = "names for clarity")]
    pub fn read_ticket<'u2>(self, read_ticket: ReadTicket<'u2>) -> Builder<'u2, P, Txn> {
        Builder {
            read_ticket,
            attributes: self.attributes,
            primitive_builder: self.primitive_builder,
            modifiers: self.modifiers,
            transaction: self.transaction,
        }
    }

    fn build_block_and_txn_internal(self) -> (Block, Txn)
    where
        P: BuildPrimitive,
    {
        let Self {
            read_ticket: _,
            attributes,
            primitive_builder,
            mut modifiers,
            transaction,
        } = self;
        let primitive = primitive_builder.build_primitive();

        if attributes != BlockAttributes::default() {
            modifiers.insert(0, Modifier::Attributes(Arc::new(attributes)));
        }

        let block = if matches!(primitive, Primitive::Air) && modifiers.is_empty() {
            // Avoid allocating an Arc.
            AIR
        } else {
            Block(BlockPtr::Owned(Arc::new(BlockParts {
                primitive,
                modifiers,
            })))
        };

        (block, transaction)
    }
}

impl<P: BuildPrimitive> Builder<'_, P, ()> {
    /// Converts this builder into a block value.
    ///
    /// This method may only be used when the builder has *not* been used with `voxels_fn()`,
    /// since in that case a universe transaction must be executed.
    pub fn build(self) -> Block {
        let (block, ()) = self.build_block_and_txn_internal();
        block
    }
}

impl<P: BuildPrimitive> Builder<'_, P, UniverseTransaction> {
    // TODO: Also allow extracting the transaction for later use

    /// Converts this builder into a block value, and inserts its associated [`Space`] into the
    /// given universe.
    pub fn build_into(self, universe: &mut Universe) -> Block {
        let (block, transaction) = self.build_block_and_txn_internal();

        // The transaction is always an insert_anonymous, which cannot fail.
        transaction.execute(universe, (), &mut transaction::no_outputs).unwrap();

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
impl<Txn> Builder<'_, Atom, Txn> {
    /// Sets the collision behavior of a [`Primitive::Atom`] block.
    pub const fn collision(mut self, collision: BlockCollision) -> Self {
        self.primitive_builder.collision = collision;
        self
    }

    /// Sets the light emission of a [`Primitive::Atom`] block.
    ///
    /// See [`Atom::emission`](block::Atom::emission) for details on the meaning of this value.
    pub fn light_emission(mut self, value: impl Into<Rgb>) -> Self {
        self.primitive_builder.emission = value.into();
        self
    }
}

/// Voxel-specific builder methods.
impl<Txn> Builder<'_, Voxels, Txn> {
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

/// Allows implicitly converting [`Builder`] to the block it would build.
impl<C: BuildPrimitive> From<Builder<'_, C, ()>> for Block {
    fn from(builder: Builder<'_, C, ()>) -> Self {
        builder.build()
    }
}
/// Equivalent to `Block::builder().color(color)`.
impl From<Rgba> for Builder<'_, Atom, ()> {
    fn from(color: Rgba) -> Self {
        Block::builder().color(color)
    }
}
/// Equivalent to `Block::builder().color(color.with_alpha_one())`.
impl From<Rgb01> for Builder<'_, Atom, ()> {
    fn from(color: Rgb01) -> Self {
        Block::builder().color(color.with_alpha_one())
    }
}

/// Placeholder type for an incomplete [`Builder`]'s content. The builder
/// cannot create an actual block until this is replaced.
#[expect(clippy::exhaustive_structs)]
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct NeedsPrimitive;

/// Something that a parameterized [`Builder`] can use to construct a block's primitive.
///
/// TODO: This is not currently necessary; we can replace the BuildPrimitive types with the
/// primitive itself. (But will that remain true?)
#[doc(hidden)]
pub trait BuildPrimitive {
    fn build_primitive(self) -> Primitive;
}

/// Parameter type for a [`Builder`] that is building a block with a [`Primitive::Atom`].
///
/// This is not the same as the [`block::Atom`] type.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Atom {
    color: Rgba,
    emission: Rgb,
    collision: BlockCollision,
}
impl BuildPrimitive for Atom {
    fn build_primitive(self) -> Primitive {
        Primitive::Atom(block::Atom {
            color: self.color,
            emission: self.emission,
            collision: self.collision,
        })
    }
}

/// Parameter type for a [`Builder`] that is building a block with voxels ([`Primitive::Recur`]).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Voxels {
    space: Handle<Space>,
    resolution: Resolution,
    offset: GridPoint,
}
impl BuildPrimitive for Voxels {
    fn build_primitive(self) -> Primitive {
        Primitive::Recur {
            offset: self.offset,
            resolution: self.resolution,
            space: self.space,
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::boxed::Box;
    use euclid::{point3, vec3};

    use crate::block::{self, Resolution::*, TickAction};
    use crate::content::palette;
    use crate::inv;
    use crate::math::{Face6, GridRotation, Vol};
    use crate::op::Operation;
    use crate::space::SpacePhysics;
    use crate::transaction::Transactional as _;

    use super::*;

    #[test]
    fn defaults() {
        let color = Rgba::new(0.1, 0.2, 0.3, 0.4);
        assert_eq!(
            Block::builder().color(color).build(),
            Block::from(block::Atom {
                color,
                emission: Rgb::ZERO,
                collision: BlockCollision::Hard,
            }),
        );
    }

    #[test]
    fn default_equivalent() {
        assert_eq!(
            Builder::new(),
            <Builder<'_, NeedsPrimitive, ()> as Default>::default()
        );
    }

    #[test]
    fn every_field_nondefault() {
        let color = Rgba::new(0.1, 0.2, 0.3, 0.4);
        let emission = Rgb::new(0.1, 3.0, 0.1);
        let inventory = inv::InvInBlock::new(
            9,
            R4,
            R16,
            vec![
                inv::IconRow::new(0..3, point3(1, 1, 1), vec3(5, 0, 0)),
                inv::IconRow::new(3..6, point3(1, 1, 6), vec3(5, 0, 0)),
                inv::IconRow::new(6..9, point3(1, 1, 11), vec3(5, 0, 0)),
            ],
        );
        let rotation_rule = block::RotationPlacementRule::Attach { by: Face6::NZ };
        let placement_action = Some(block::PlacementAction {
            operation: Operation::Become(block::from_color!(1.0, 0.0, 1.0)),
            in_front: false,
        });
        let tick_action = Some(TickAction::from(Operation::Become(AIR)));
        let activation_action = Some(Operation::Become(block::from_color!(1.0, 1.0, 1.0)));
        assert_eq!(
            Block::builder()
                .color(color)
                .display_name("hello world")
                .inventory_config(inventory.clone())
                .collision(BlockCollision::None)
                .rotation_rule(rotation_rule)
                .selectable(false)
                .light_emission(emission)
                .placement_action(placement_action.clone())
                .tick_action(tick_action.clone())
                .activation_action(activation_action.clone())
                .animation_hint(block::AnimationHint::replacement(
                    block::AnimationChange::Shape
                ))
                .modifier(Modifier::Rotate(Face6::PY.clockwise()))
                .build(),
            Block::from(block::Atom {
                color,
                emission,
                collision: BlockCollision::None,
            })
            .with_modifier(BlockAttributes {
                display_name: "hello world".into(),
                selectable: false,
                placement_action,
                inventory,
                rotation_rule,
                tick_action,
                activation_action,
                animation_hint: block::AnimationHint::replacement(block::AnimationChange::Shape),
            })
            .with_modifier(Modifier::Rotate(Face6::PY.clockwise()))
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
                offset: GridPoint::origin(),
                resolution: R2, // not same as space size
                space: space_handle
            })
            .with_modifier(Modifier::Attributes(Arc::new(BlockAttributes {
                display_name: "hello world".into(),
                ..BlockAttributes::default()
            }))),
        );
    }

    #[test]
    fn voxels_from_fn_basic() {
        let mut universe = Universe::new();

        let resolution = R4;
        let expected_bounds = GridAab::for_block(resolution);
        let atom = block::from_color!(palette::DIRT);
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
                offset: GridPoint::origin(),
                resolution,
                space: space_handle.clone()
            })
            .with_modifier(BlockAttributes {
                display_name: "hello world".into(),
                ..BlockAttributes::default()
            }),
        );

        // Check the space's characteristics
        let space = space_handle.read(universe.read_ticket()).unwrap();
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
        let atom = block::from_color!(palette::DIRT);
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
        let space = space_handle.read(universe.read_ticket()).unwrap();
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

    #[test]
    fn modifier_before_color_is_equivalent() {
        assert_eq!(
            Block::builder()
                .modifier(Modifier::Rotate(GridRotation::RXYz))
                .color(Rgba::WHITE),
            Block::builder()
                .color(Rgba::WHITE)
                .modifier(Modifier::Rotate(GridRotation::RXYz))
        );
    }

    #[test]
    fn modifier_before_voxels_is_equivalent() {
        let h = Handle::new_gone(Name::Pending);
        assert_eq!(
            Block::builder()
                .modifier(Modifier::Rotate(GridRotation::RXYz))
                .voxels_handle(R8, h.clone()),
            Block::builder()
                .voxels_handle(R8, h)
                .modifier(Modifier::Rotate(GridRotation::RXYz))
        );
    }
}

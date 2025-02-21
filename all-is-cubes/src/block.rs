//! Definition of blocks, which are the game objects which occupy the grid of a
//! [`Space`]. See [`Block`] for details.
//!
//! The types of most interest in this module are [`Block`], [`Primitive`],
//! [`BlockAttributes`], and [`Modifier`].

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::collections::VecDeque;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;

use crate::listen::{self, Listen as _, Listener};
use crate::math::{GridAab, GridCoordinate, GridPoint, GridRotation, GridVector, Rgb, Rgba, Vol};
use crate::space::{SetCubeError, Space, SpaceChange};
use crate::universe::{Handle, HandleVisitor, VisitHandles};

/// Construct a [`Block`] with the given reflectance color.
///
/// This is equivalent to calling `Block::from()`, except that:
///
/// * the arguments must be constant expressions,
/// * no allocations are performed, and
/// * the value may be used in `const` evaluation.
///
/// The color may be specified as an expression which returns [`Rgb`] or [`Rgba`], or as three
/// or four [`f32`] literal color components.
///
/// ```
/// use all_is_cubes::{block::Block, color_block, math::{Rgb, rgb_const}};
///
/// assert_eq!(
///     color_block!(rgb_const!(1.0, 0.5, 0.0)),
///     Block::from(Rgb::new(1.0, 0.5, 0.0)),
/// );
///
/// assert_eq!(
///     color_block!(rgb_const!(1.0, 0.5, 0.0)),
///     color_block!(1.0, 0.5, 0.0),
/// );
///
/// assert_eq!(
///     color_block!(1.0, 0.5, 0.0),
///     color_block!(1.0, 0.5, 0.0, 1.0),
/// );
/// ```
// ---
// Must declare this macro before child modules, so they can use it.
#[macro_export]
#[expect(
    clippy::module_name_repetitions,
    reason = "macro export is actually at crate root"
)]
macro_rules! color_block {
    ($color:expr) => {
        $crate::block::Block::from_static_primitive(const {
            &$crate::block::Primitive::from_color($color.with_alpha_one_if_has_no_alpha())
        })
    };

    ($r:literal, $g:literal, $b:literal $(,)?) => {
        $crate::color_block!($crate::math::rgb_const!($r, $g, $b))
    };

    ($r:literal, $g:literal, $b:literal, $a:literal $(,)?) => {
        $crate::color_block!($crate::math::rgba_const!($r, $g, $b, $a))
    };
}

mod attributes;
pub use attributes::*;

mod block_def;
pub use block_def::*;

pub mod builder;
#[doc(inline)]
pub use builder::Builder;

mod eval;
pub use eval::*;

mod modifier;
pub use modifier::*;

/// Scale factor between a [`Block`] and its component voxels.
///
/// This resolution cubed is the number of voxels making up a block.
///
/// Resolutions are always powers of 2. This ensures that the arithmetic is well-behaved
/// (no division by zero, exact floating-point representation, and the potential of
/// fixed-point representation),
/// and that it is always possible to subdivide a block further (up to the limit) without
/// shifting the existing voxel boundaries.
///
/// Note that while quite high resolutions are permitted, this does not mean that it is
/// practical to routinely use full blocks at that resolution. For example, 64 × 64 × 64
/// = 262,144 voxels, occupying several megabytes just for color data.
/// High resolutions are permitted for special purposes that do not necessarily use the
/// full cube volume:
///
/// * *Thin* blocks (e.g. 128 × 128 × 1) can display high resolution text and other 2D
///   images.
/// * Multi-block structures can be defined using [`Modifier::Zoom`]; their total size
///   is limited by the resolution limit.
pub use all_is_cubes_base::resolution::Resolution;
pub use all_is_cubes_base::resolution::*;

pub mod text;

#[cfg(test)]
mod tests;

// --- Block type declarations ---
// File organization: This is a series of closely related type definitions together before
// any of their `impl`s, so the types can be understood in context.

/// A [`Block`] is something that can exist in the grid of a [`Space`]; it occupies one
/// unit cube of simulated physical space, and has a specified appearance and behavior.
///
/// A [`Block`] is made up of a [`Primitive`] and zero or more [`Modifier`]s.
///
/// In general, when a block appears multiple times from an in-game perspective, that may
/// or may not be the the same copy; `Block`s are "by value" and any block [`Eq`] to
/// another will behave identically and should be treated identically. However, some
/// blocks are defined by reference to shared mutable data, and [`Block`] containers such
/// as [`Space`] must follow those changes.
///
/// To determine the concrete appearance and behavior of a block, use [`Block::evaluate()`]
/// or [`Block::evaluate_and_listen()`], which will return an [`EvaluatedBlock`] value.
/// Additional operations for manipulating the block are available on [`EvaluatedBlock`].
///
#[doc = include_str!("save/serde-warning.md")]
#[derive(Clone)]
pub struct Block(BlockPtr);

/// Pointer to data of a [`Block`] value.
///
/// This is a separate type so that the enum variants are not exposed.
/// It does not implement Eq and Hash, but Block does through it.
#[derive(Clone, Debug)]
enum BlockPtr {
    Static(&'static Primitive),
    Owned(Arc<BlockParts>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct BlockParts {
    primitive: Primitive,
    /// Modifiers are stored in innermost-first order.
    modifiers: Vec<Modifier>,
}

/// The possible fundamental representations of a [`Block`]'s shape.
///
#[doc = include_str!("save/serde-warning.md")]
#[derive(Clone, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Primitive {
    /// A block whose definition is stored elsewhere in a
    /// [`Universe`](crate::universe::Universe).
    ///
    /// Note that this is a handle to a [`Block`], not a [`Primitive`]; the referenced
    /// [`BlockDef`] may have its own [`Modifier`]s, and thus the result of
    /// [evaluating](Block::evaluate) a primitive with no modifiers is not necessarily
    /// free of the effects of modifiers.
    Indirect(Handle<BlockDef>),

    /// A block of totally uniform properties.
    Atom(Atom),

    /// A block that is composed of smaller blocks, defined by the referenced [`Space`].
    Recur {
        /// The space from which voxels are taken.
        space: Handle<Space>,

        /// Which portion of the space will be used, specified by the most negative
        /// corner.
        offset: GridPoint,

        /// The side length of the cubical volume of sub-blocks (voxels) used for this
        /// block.
        resolution: Resolution,
    },

    /// An invisible, unselectable, inert block used as “no block”; the primitive of [`AIR`].
    ///
    /// This is essentially a specific [`Primitive::Atom`]. There are a number of
    /// algorithms which treat this block specially or which return it (e.g. outside the
    /// bounds of a `Space`), so it exists here to make it an explicit element of the
    /// data model — so that if it is, say, serialized and loaded in a future version,
    /// it is still recognized as [`AIR`]. Additionally, it's cheaper to compare this way.
    Air,

    /// A piece of text rendered as voxels.
    ///
    /// To combine the text with other shapes, use [`Modifier::Composite`].
    Text {
        /// The text to draw, and the font and text-layout-dependent positioning.
        text: text::Text,

        /// Translation, in whole cubes, of the region of the text to draw.
        ///
        /// For text within a single block, this should be zero.
        /// For multi-block text, this should be equal to the difference between
        /// the adjacent blocks' positions.
        offset: GridVector,
    },
}

/// Data of [`Primitive::Atom`]. The definition of a single [block](Block) that has uniform
/// material properties rather than spatially varying ones; a single voxel.
///
/// All properties of an atom are [intensive properties].
///
/// [intensive properties]: https://en.wikipedia.org/wiki/Intensive_and_extensive_properties
#[derive(Clone, Eq, Hash, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct Atom {
    /// The color exhibited by diffuse reflection from this block.
    ///
    /// The RGB components of this color are the *[reflectance]:* the fraction of incoming light
    /// that is reflected rather than absorbed.
    ///
    /// The alpha (<var>α</var>) component of this color specifies the opacity of this block,
    /// that is, the fraction of light that is reflected or absorbed rather than transmitted.
    ///
    /// Whenever <var>α</var> is neither 1 nor 0 (and, trivially, in those cases too),
    /// the reflectance and opacity
    /// should be interpreted as being of **a unit thickness of** this material. Thus, they may be
    /// modified by the length of material through which a light ray passes, either due to
    /// viewing angle or due to using this block as a voxel in a [`Primitive::Recur`].
    ///
    /// This transformation is best understood in terms of _transmittance_ <var>T</var>,
    /// defined as 1 &minus; <var>α</var>.
    /// The transmittance of a given thickness of material, <var>T</var><sub><var>d</var></sub>,
    /// is defined in terms of the transmittance of a unit thickness,
    /// <var>T</var><sub><var>1</var></sub>, as:
    ///
    /// <p style="text-align: center">
    /// <var>T</var><sub><var>d</var></sub> = (<var>T</var><sub>1</sub>)<sup><var>d</var></sup>.
    /// </p>
    ///
    /// Therefore,
    ///
    /// <p style="text-align: center">
    /// <var>α</var><sub>d</sub> =
    /// 1 &minus; (1 &minus; <var>α</var><sub>1</sub>)<sup><var>d</var></sup>.
    /// </p>
    ///
    /// [reflectance]: https://en.wikipedia.org/wiki/Reflectance
    pub color: Rgba,

    /// Light emitted (not reflected) by the block.
    ///
    /// This quantity is the emitted portion of the *[luminance]* of this material, in unspecified
    /// units where 1.0 is the display white level (except for the effects of tone mapping).
    /// In the future this may be redefined in terms of a physical unit, but with the same
    /// dimensions.
    ///
    /// Because we are describing a volume, not a surface, the physical
    /// interpretation of this value depends on the opacity of the material.
    /// If `self.color.alpha()` is 1.0, then this light escaping a surface must have been emitted at
    /// the surface; if the alpha is 0.0, then it must have been emitted throughout the volume; and
    /// in intermediate cases, then the light emitted within the volume must be greater per unit
    /// volume to compensate for internal absorption. Still, these are not distinct cases but form
    /// a continuum.
    ///
    /// The emission <var>E</var><sub><var>d</var></sub> of a particular thickness <var>d</var>
    /// of this material is
    ///
    /// <p style="text-align: center">
    /// <var>E</var><sub><var>d</var></sub> = <var>E</var><sub>1</sub> ·
    ///     ∫<sub>0</sub><sup><var>d</var></sup>
    ///         (<var>T</var><sub>1</sub>)<sup><var>x</var></sup>
    ///     <var>dx</var>
    /// </p>
    ///
    /// where <var>E</var><sub>1</sub> = `self.emission` and
    /// <var>T</var><sub>1</sub> = `1.0 - self.color.alpha()`.
    /// When integrated, this becomes
    ///
    /// <p style="text-align: center">
    /// <var>E</var><sub><var>d</var></sub> = <var>E</var><sub>1</sub> · <var>d</var>
    /// </p>
    ///
    /// when <var>α</var> = 0 (<var>T</var> = 1) and
    ///
    /// <p style="text-align: center">
    /// <var>E</var><sub><var>d</var></sub> = <var>E</var><sub>1</sub> ·
    /// (<var>T</var><sub><var>d</var></sub> - 1) / (<var>T</var><sub>1</sub> - 1)
    /// </p>
    ///
    /// otherwise.
    ///
    ///
    /// [luminance]: https://en.wikipedia.org/wiki/Luminance
    pub emission: Rgb,

    /// The effect on a [`Body`](crate::physics::Body) of colliding with this block.
    ///
    /// The default value is [`BlockCollision::Hard`].
    pub collision: BlockCollision,
}

// --- End of type declarations, beginning of impls ---

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("Block");
        s.field("primitive", self.primitive());
        let modifiers = self.modifiers();
        if !modifiers.is_empty() {
            s.field("modifiers", &self.modifiers());
        }
        s.finish()
    }
}

impl Block {
    /// Returns a new [`Builder`] which may be used to construct a [`Block`] value
    /// from various inputs with convenient syntax.
    pub const fn builder() -> Builder<builder::NeedsPrimitive, ()> {
        Builder::new()
    }

    /// Construct a [`Block`] from a [`Primitive`] value.
    // TODO: Decide whether this should go away as temporary from refactoring.
    pub fn from_primitive(p: Primitive) -> Self {
        if let Primitive::Air = p {
            // Avoid allocating an Arc.
            AIR
        } else {
            Block(BlockPtr::Owned(Arc::new(BlockParts {
                primitive: p,
                modifiers: vec![],
            })))
        }
    }

    /// Constructs a [`Block`] which references the given static [`Primitive`].
    ///
    /// This performs no allocation.
    /// It is also available as a [`From`] implementation.
    #[doc(hidden)] // used by `color_block!()`, but I'm not sure whether to make it really public
    pub const fn from_static_primitive(r: &'static Primitive) -> Self {
        Block(BlockPtr::Static(r))
    }

    /// Returns the [`Primitive`] which defines this block before any
    /// [`Modifier`]s are applied.
    pub fn primitive(&self) -> &Primitive {
        match self.0 {
            BlockPtr::Static(primitive) => primitive,
            BlockPtr::Owned(ref arc) => &arc.primitive,
        }
    }

    /// Returns a mutable reference to the [`Primitive`] which defines this block before
    /// any [`Modifier`]s are applied.
    ///
    /// This may cause part or all of the block's data to stop sharing storage with other
    /// blocks.
    pub fn primitive_mut(&mut self) -> &mut Primitive {
        &mut self.make_parts_mut().primitive
    }

    /// Returns all the modifiers of this block.
    ///
    /// Modifiers are arranged in order of their application to the primitive,
    /// or “innermost” to “outermost”.
    ///
    /// Note that this does not necessarily return all modifiers involved in its
    /// definition; modifiers on the far end of a [`Primitive::Indirect`] are
    /// not reported here, even though they take effect when evaluated.
    pub fn modifiers(&self) -> &[Modifier] {
        match self.0 {
            BlockPtr::Static(_) => &[],
            BlockPtr::Owned(ref arc_parts) => &arc_parts.modifiers,
        }
    }

    /// Returns a mutable reference to the vector of [`Modifier`]s on this block.
    ///
    /// This may cause part or all of the block's data to stop sharing storage with other
    /// blocks.
    // TODO: This nails down our representation a bit much
    pub fn modifiers_mut(&mut self) -> &mut Vec<Modifier> {
        &mut self.make_parts_mut().modifiers
    }

    fn make_parts_mut(&mut self) -> &mut BlockParts {
        match self.0 {
            BlockPtr::Static(static_primitive) => {
                *self = Block(BlockPtr::Owned(Arc::new(BlockParts {
                    primitive: static_primitive.clone(),
                    modifiers: vec![],
                })));
                match self.0 {
                    BlockPtr::Owned(ref mut arc_repr) => Arc::make_mut(arc_repr),
                    BlockPtr::Static(_) => unreachable!(),
                }
            }
            BlockPtr::Owned(ref mut arc_repr) => Arc::make_mut(arc_repr),
        }
    }

    /// Add the given modifier to this block.
    ///
    /// This is a convenience operation which is exactly equivalent to
    /// doing `block.modifiers_mut().push(modifier.into())`. It does not do any of the
    /// special case logic that, for example, [`Block::rotate()`] does.
    #[must_use]
    pub fn with_modifier(mut self, modifier: impl Into<Modifier>) -> Self {
        self.modifiers_mut().push(modifier.into());
        self
    }

    /// Returns whether this block’s evaluation would be affected at all by adding
    /// or removing a [`Modifier::Rotate`].
    ///
    /// Note that this does not account for symmetry of the block’s evaluation; it only checks
    /// the primitive’s own data, and so answers whether this primitive is *always* symmetric
    /// under all possible conditions of the rest of the universe.
    #[doc(hidden)] // TODO: unclear if good public API, but public for fuzz testing
    pub fn rotationally_symmetric(&self) -> bool {
        // TODO: Just checking the definitions does not reveal sufficient information.
        // In particular, `Primitive::Indirect` is opaque. Therefore, for some applications,
        // we want a version that operates on `EvaluatedBlock` which can consult whether the
        // block is symmetric accounting for all parts of its definition. On the other hand,
        // other applications might care not whether it is *currently* symmetric but whether
        // it can ever change to be asymmetric, for which this is the actual right answer.
        self.primitive().rotationally_symmetric()
            && self
                .modifiers()
                .iter()
                .all(|m| m.does_not_introduce_asymmetry())
    }

    /// Add a [`Modifier::Attributes`] if there isn't one already.
    /// Evaluates the block if needed to get existing attributes.
    ///
    /// TODO: bad API probably, because it overwrites/freezes attributes; this was added in a hurry
    /// to tidy up the attributes-is-a-modifer refactor. The proper API is more like with_modifier()
    /// for a single attribute, but we don't have single attribute override modifiers yet.
    #[doc(hidden)]
    pub fn freezing_get_attributes_mut(&mut self) -> &mut BlockAttributes {
        if !matches!(self.modifiers().last(), Some(Modifier::Attributes(_))) {
            let attr_modifier = self.evaluate().unwrap().attributes.into();
            self.modifiers_mut().push(attr_modifier);
        }
        let Some(Modifier::Attributes(a)) = self.modifiers_mut().last_mut() else {
            unreachable!();
        };
        Arc::make_mut(a)
    }

    /// Rotates this block by the specified rotation.
    ///
    /// Compared to direct use of [`Modifier::Rotate`], this will:
    ///
    /// * Avoid constructing chains of redundant modifiers.
    /// * Not rotate blocks that should never appear rotated (including atom blocks).
    ///
    /// (TODO: This should be replaced with `with_modifier()` or similar having a general
    /// rule set for combining modifiers.)
    ///
    /// ```
    /// use all_is_cubes::block::{AIR, Block, Modifier};
    /// use all_is_cubes::color_block;
    /// use all_is_cubes::content::make_some_voxel_blocks;
    /// use all_is_cubes::math::{GridRotation, Rgba};
    /// use all_is_cubes::universe::Universe;
    ///
    /// let mut universe = Universe::new();
    /// let [mut block] = make_some_voxel_blocks(&mut universe);
    /// block.modifiers_mut().clear();
    /// let clockwise = GridRotation::CLOCKWISE;
    ///
    /// // Basic rotation
    /// let rotated = block.clone().rotate(clockwise);
    /// assert_eq!(rotated.modifiers(), &[Modifier::Rotate(clockwise)]);
    ///
    /// // Multiple rotations are combined
    /// let double = rotated.clone().rotate(clockwise);
    /// assert_eq!(double.modifiers(), &[Modifier::Rotate(clockwise * clockwise)]);
    ///
    /// // Atoms and AIR are never rotated
    /// let atom = color_block!(Rgba::WHITE);
    /// assert_eq!(atom.clone().rotate(clockwise), atom);
    /// assert_eq!(AIR.rotate(clockwise), AIR);
    /// ```
    #[must_use]
    pub fn rotate(mut self, rotation: GridRotation) -> Self {
        if rotation == GridRotation::IDENTITY {
            // TODO: Should we *remove* any identity rotation already present,
            // to make a fully canonical result?
            return self;
        }

        if self.rotationally_symmetric() {
            return self;
        }

        let parts = self.make_parts_mut();
        match parts.modifiers.last_mut() {
            // TODO: If the combined rotation is the identity, discard the modifier
            Some(Modifier::Rotate(existing_rotation)) => {
                *existing_rotation = rotation * *existing_rotation;
            }
            None | Some(_) => parts.modifiers.push(Modifier::Rotate(rotation)),
        }
        self
    }

    /// Standardizes any characteristics of this block which may be presumed to be
    /// specific to its usage in its current location, so that it can be used elsewhere
    /// or compared with others. Specifically, it has the following effects:
    ///
    /// * Removes [`Modifier::Rotate`].
    /// * Splits some [`Modifier::Composite`] into their parts.
    ///
    /// In future versions there may be additional changes or ones customizable per block.
    ///
    /// # Examples
    ///
    /// Removing rotation:
    /// ```
    /// use all_is_cubes::block::Block;
    /// # use all_is_cubes::content::make_some_voxel_blocks;
    /// use all_is_cubes::math::GridRotation;
    /// use all_is_cubes::universe::Universe;
    ///
    /// let mut universe = Universe::new();
    /// let [block] = make_some_voxel_blocks(&mut universe);
    /// let rotated = block.clone().rotate(GridRotation::CLOCKWISE);
    ///
    /// assert_ne!(&block, &rotated);
    /// assert_eq!(vec![block], rotated.clone().unspecialize());
    /// ```
    #[must_use]
    pub fn unspecialize(&self) -> Vec<Block> {
        let mut queue = VecDeque::from([self.clone()]);
        let mut output = Vec::new();

        'queue: while let Some(mut block) = queue.pop_front() {
            if block.modifiers().is_empty() {
                // No need to reify the modifier list if it doesn't exist already.
                output.push(block);
                continue;
            }

            while let Some(modifier) = block.modifiers().last() {
                match modifier.unspecialize(&block) {
                    ModifierUnspecialize::Keep => {
                        output.push(block);
                        continue 'queue;
                    }
                    ModifierUnspecialize::Pop => {
                        block.modifiers_mut().pop();
                        // and continue to possibly pop more...
                    }
                    ModifierUnspecialize::Replace(replacements) => {
                        let replacements = replacements.into_iter().inspect(|r| {
                            assert_ne!(
                                r, &block,
                                "infinite loop detected: \
                            modifier returned original block from unspecialize()"
                            );
                        });
                        queue.extend(replacements);
                        continue 'queue;
                    }
                }
            }
            // If and only if we got here rather than doing something else, the block
            // now has all its unwanted modifiers popped or replaced.
            output.push(block);
        }

        output
    }

    /// Converts this `Block` into a “flattened” and snapshotted form which contains all
    /// information needed for rendering and physics, and does not require [`Handle`] access
    /// to other objects.
    pub fn evaluate(&self) -> Result<EvaluatedBlock, EvalBlockError> {
        self.evaluate2(&EvalFilter {
            skip_eval: false,
            listener: None,
            budget: Default::default(),
        })
    }

    /// As [`Block::evaluate()`], but also installs a listener which will be notified of
    /// changes in all data sources that might affect the evaluation result.
    ///
    /// Note that this does not listen for mutations of the [`Block`] value itself, in the
    /// sense that none of the methods on [`Block`] will cause this listener to fire.
    /// Rather, it listens for changes in by-reference-to-interior-mutable-data sources
    /// such as the [`Space`] referred to by a [`Primitive::Recur`] or the [`BlockDef`]
    /// referred to by a [`Primitive::Indirect`].
    ///
    /// # Errors
    ///
    /// If an evaluation error is reported, the [`Listener`] may have been installed
    /// incompletely or not at all. It should not be relied on.
    pub fn evaluate_and_listen(
        &self,
        listener: impl listen::IntoDynListener<BlockChange, listen::DynListener<BlockChange>>,
    ) -> Result<EvaluatedBlock, EvalBlockError> {
        self.evaluate2(&EvalFilter {
            skip_eval: false,
            listener: Some(listener.into_dyn_listener()),
            budget: Default::default(),
        })
    }

    /// Internal general entry point for block evaluation.
    ///    
    /// TODO: Placeholder name. At some point we may expose `EvalFilter` directly and make
    /// this be just `evaluate()`.
    pub(crate) fn evaluate2(&self, filter: &EvalFilter) -> Result<EvaluatedBlock, EvalBlockError> {
        finish_evaluation(
            self.clone(),
            filter.budget.get(),
            self.evaluate_impl(filter),
            filter,
        )
    }

    /// Equivalent to `Evoxel::from_block(block.evaluate2(filter))` except for the error type.
    /// For use when blocks contain other blocks as voxels.
    fn evaluate_to_evoxel_internal(&self, filter: &EvalFilter) -> Result<Evoxel, InEvalError> {
        // TODO: Make this more efficient by not building the full `EvaluatedBlock`
        self.evaluate_impl(filter)
            .map(|minev| Evoxel::from_block(&minev.finish(self.clone(), Cost::ZERO /* ignored */)))
    }

    #[inline]
    fn evaluate_impl(&self, filter: &EvalFilter) -> Result<MinEval, InEvalError> {
        // The block's primitive counts as 1 component.
        Budget::decrement_components(&filter.budget)?;

        let mut value: MinEval = match *self.primitive() {
            Primitive::Indirect(ref def_handle) => def_handle.read()?.evaluate_impl(filter)?,

            Primitive::Atom(Atom {
                color,
                emission,
                collision,
            }) => MinEval::new(
                BlockAttributes::default(),
                Evoxels::from_one(Evoxel {
                    color,
                    emission,
                    selectable: true,
                    collision,
                }),
            ),

            Primitive::Air => AIR_EVALUATED_MIN,

            Primitive::Recur {
                offset,
                resolution,
                space: ref space_handle,
            } => {
                let block_space = space_handle.read()?;

                // The region of `space` that the parameters say to look at.
                let full_resolution_bounds =
                    GridAab::for_block(resolution).translate(offset.to_vector());

                if let Some(listener) = &filter.listener {
                    block_space.listen(listener.clone().filter(
                        move |msg: &SpaceChange| -> Option<BlockChange> {
                            match *msg {
                                SpaceChange::CubeBlock { cube, .. }
                                    if full_resolution_bounds.contains_cube(cube) =>
                                {
                                    Some(BlockChange::new())
                                }
                                SpaceChange::CubeBlock { .. } => None,
                                SpaceChange::EveryBlock => Some(BlockChange::new()),

                                // TODO: It would be nice if the space gave more precise updates
                                // such that we could conclude e.g. "this is a new/removed block
                                // in an unaffected area" without needing to store any data.
                                SpaceChange::BlockEvaluation(_) => Some(BlockChange::new()),

                                // Index changes by themselves cannot affect the result.
                                SpaceChange::BlockIndex(_) => None,

                                // Things that do not matter.
                                SpaceChange::CubeLight { .. } => None,
                                SpaceChange::Physics => None,
                            }
                        },
                    ));
                }

                // Intersect that region with the actual bounds of `space`.
                let mut voxels_animation_hint = AnimationHint::UNCHANGING;
                let voxels: Vol<Arc<[Evoxel]>> = match full_resolution_bounds
                    .intersection_cubes(block_space.bounds())
                    .filter(|_| !filter.skip_eval)
                {
                    Some(occupied_bounds) => {
                        Budget::decrement_voxels(
                            &filter.budget,
                            occupied_bounds.volume().unwrap(),
                        )?;

                        block_space
                            .extract(
                                occupied_bounds,
                                #[inline(always)]
                                |extract| {
                                    let ev = extract.block_data().evaluated();
                                    voxels_animation_hint |= ev.attributes().animation_hint;
                                    Evoxel::from_block(ev)
                                },
                            )
                            .translate(-offset.to_vector())
                    }
                    None => {
                        // If there is no intersection, then return an empty voxel array,
                        // with an arbitrary position.
                        // Also applies when skip_eval is true
                        Vol::from_elements(GridAab::ORIGIN_EMPTY, Box::<[Evoxel]>::default())
                            .unwrap()
                    }
                };

                MinEval::new(
                    BlockAttributes {
                        // Translate the voxels' animation hints into their effect on
                        // the outer block.
                        animation_hint: AnimationHint {
                            redefinition: voxels_animation_hint.redefinition
                                | voxels_animation_hint.replacement,
                            replacement: AnimationChange::None,
                        },
                        ..BlockAttributes::default()
                    },
                    Evoxels::from_many(resolution, voxels),
                )
            }

            Primitive::Text { ref text, offset } => text.evaluate(offset, filter)?,
        };

        #[cfg(debug_assertions)]
        value.consistency_check();

        for (index, modifier) in self.modifiers().iter().enumerate() {
            value = modifier.evaluate(self, index, value, filter)?;

            #[cfg(debug_assertions)]
            value.consistency_check();
        }

        Ok(value)
    }

    /// Returns the single [`Rgba`] color of this block's [`Primitive::Atom`] or
    /// [`Primitive::Air`], or panics if it has a different kind of primitive.
    /// **Intended for use in tests only.**
    pub fn color(&self) -> Rgba {
        match *self.primitive() {
            Primitive::Atom(Atom { color, .. }) => color,
            Primitive::Air => AIR_EVALUATED.color(),
            Primitive::Indirect(_) | Primitive::Recur { .. } | Primitive::Text { .. } => {
                panic!("Block::color not defined for non-atom blocks")
            }
        }
    }
}

// Manual implementations of Eq and Hash ensure that the [`BlockPtr`] storage
// choices do not affect equality.
impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        self.primitive() == other.primitive() && self.modifiers() == other.modifiers()
    }
}
impl Eq for Block {}
impl core::hash::Hash for Block {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.primitive().hash(state);
        self.modifiers().hash(state);
    }
}

impl VisitHandles for Block {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        self.primitive().visit_handles(visitor);
        for modifier in self.modifiers() {
            modifier.visit_handles(visitor)
        }
    }
}

impl From<&'static Primitive> for Block {
    /// Constructs a [`Block`] which references the given static [`Primitive`].
    ///
    /// This performs no allocation.
    fn from(r: &'static Primitive) -> Self {
        Block(BlockPtr::Static(r))
    }
}

impl From<Primitive> for Block {
    /// Constructs a [`Block`] that owns the given [`Primitive`].
    ///
    /// This operation creates a heap allocation for the [`Primitive`].
    fn from(primitive: Primitive) -> Self {
        Block::from_primitive(primitive)
    }
}

// Implementing conversions to `Cow` allow various functions to accept either an owned
// or borrowed `Block`. The motivation for this is to avoid unnecessary cloning
// (in case an individual block has large data).
// TODO: Eliminate these given the new Block-is-a-pointer world.
impl From<Block> for Cow<'_, Block> {
    fn from(block: Block) -> Self {
        Cow::Owned(block)
    }
}
impl<'a> From<&'a Block> for Cow<'a, Block> {
    fn from(block: &'a Block) -> Self {
        Cow::Borrowed(block)
    }
}

// Converting colors to blocks.
impl From<Rgb> for Block {
    /// Constructs a [`Block`] with the given reflectance color, and default attributes.
    ///
    /// This operation allocates a new [`Primitive`] value on the heap.
    /// If the color is a constant, you may use [`color_block!`] instead to avoid allocation.
    fn from(color: Rgb) -> Self {
        Block::from(color.with_alpha_one())
    }
}
impl From<Rgba> for Block {
    /// Construct a [`Block`] with the given reflectance color, and default attributes.
    ///
    /// This operation allocates a new [`Primitive`] value on the heap.
    /// If the color is a constant, you may use [`color_block!`] instead to avoid allocation.
    fn from(color: Rgba) -> Self {
        Block::from_primitive(Primitive::Atom(Atom::from(color)))
    }
}

#[cfg(feature = "arbitrary")]
mod arbitrary_block {
    use super::*;
    use arbitrary::{Arbitrary, Unstructured};

    // Manual impl to skip past BlockPtr etc.
    // This means we're not exercising the `&'static` case, but that's not possible
    // unless we decide to leak memory.
    impl<'a> Arbitrary<'a> for Block {
        fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
            let mut block = Block::from_primitive(Primitive::arbitrary(u)?);
            *block.modifiers_mut() = Vec::arbitrary(u)?;
            Ok(block)
        }

        fn size_hint(_depth: usize) -> (usize, Option<usize>) {
            // Both `Primitive` and `Modifier` are arbitrarily recursive because they can
            // contain `Block`s. Therefore, the size hint calculation will always hit the depth
            // limit, and we should skip it for efficiency.
            // The lower bound is 2 because `Primitive` and `Modifiers` will each require
            // at least one byte to make a choice.
            (2, None)
        }
    }

    // Manual impl because `GridPoint` doesn't impl Arbitrary.
    impl<'a> Arbitrary<'a> for Primitive {
        fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
            Ok(match u.int_in_range(0..=4)? {
                0 => Primitive::Air,
                1 => Primitive::Atom(Atom {
                    color: Rgba::arbitrary(u)?,
                    emission: Rgb::arbitrary(u)?,
                    collision: BlockCollision::arbitrary(u)?,
                }),
                2 => Primitive::Indirect(Handle::arbitrary(u)?),
                3 => Primitive::Recur {
                    offset: GridPoint::from(<[i32; 3]>::arbitrary(u)?),
                    resolution: Resolution::arbitrary(u)?,
                    space: Handle::arbitrary(u)?,
                },
                4 => Primitive::Text {
                    text: text::Text::arbitrary(u)?,
                    // TODO: fix unhandled overflows so this can be full i32 range
                    offset: GridVector::from(<[i16; 3]>::arbitrary(u)?.map(i32::from)),
                },
                _ => unreachable!(),
            })
        }

        fn size_hint(_depth: usize) -> (usize, Option<usize>) {
            // `Primitive` is arbitrarily recursive because it can contain `Block`s
            // (via `Indirect` and `Text`). Therefore, the size hint calculation will always hit
            // the depth limit, and we should skip it for efficiency.
            // The lower bound is 1 because we need at least one byte to make a choice of primitive,
            // but if that primitive is `AIR` then we need no more bytes.
            (1, None)
        }
    }
}

/// An invisible, unselectable, inert block used as “no block”.
///
/// It is used by [`Space`] to respond to out-of-bounds requests,
/// as well as other algorithms treating it as replaceable or discardable.
///
/// When evaluated, will always produce [`AIR_EVALUATED`].
pub const AIR: Block = Block(BlockPtr::Static(&Primitive::Air));

// TODO: uncomfortable with where this impl block is located
impl Primitive {
    /// Construct a [`Primitive`] from a reflectance color.
    ///
    /// This function is equivalent to `Block::from(color)` but it can be used in const contexts.
    pub const fn from_color(color: Rgba) -> Primitive {
        Primitive::Atom(Atom::from_color(color))
    }

    /// Returns whether this primitive would be changed at all by a [`Modifier::Rotate`].
    ///
    /// Note that this does not account for symmetry of the block’s evaluation; it only checks
    /// the primitive’s own data, and so answers whether this primitive is *always* symmetric
    /// under all possible conditions of the rest of the universe.
    pub(in crate::block) fn rotationally_symmetric(&self) -> bool {
        match self {
            Primitive::Indirect(_) => false, // could point to anything
            Primitive::Atom(atom) => atom.rotationally_symmetric(),
            Primitive::Recur { .. } => false, // could point to anything
            Primitive::Air => true,
            Primitive::Text { .. } => false, // always asymmetric unless it's trivial
        }
    }
}

impl Atom {
    fn rotationally_symmetric(&self) -> bool {
        let Self {
            color: _,
            emission: _,
            collision: _,
        } = self;
        // I'm planning to eventually have non-uniform collision behaviors
        // or visual effects such as normal mapping,
        // at which point this will be sometimes false.
        true
    }
}

impl fmt::Debug for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Indirect(def) => f.debug_tuple("Indirect").field(def).finish(),
            Self::Atom(atom) => atom.fmt(f),
            Self::Recur {
                space,
                offset,
                resolution,
            } => f
                .debug_struct("Recur")
                .field("space", space)
                .field("offset", offset)
                .field("resolution", resolution)
                .finish(),
            Self::Air => write!(f, "Air"),
            Self::Text { text, offset } => f
                .debug_struct("Text")
                .field("offset", offset)
                .field("text", text)
                .finish(),
        }
    }
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self {
            color,
            emission,
            collision,
        } = self;
        let mut s = f.debug_struct("Atom");
        s.field("color", &color);
        if emission != Rgb::ZERO {
            s.field("emission", &emission);
        }
        s.field("collision", &collision);
        s.finish()
    }
}

impl VisitHandles for Primitive {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        match self {
            Primitive::Indirect(block_handle) => visitor.visit(block_handle),
            Primitive::Atom(atom) => atom.visit_handles(visitor),
            Primitive::Air => {}
            Primitive::Recur {
                space,
                offset: _,
                resolution: _,
            } => {
                visitor.visit(space);
            }
            Primitive::Text { text, offset: _ } => text.visit_handles(visitor),
        }
    }
}

impl VisitHandles for Atom {
    fn visit_handles(&self, _: &mut dyn HandleVisitor) {
        let Self {
            color: _,
            emission: _,
            collision: _,
        } = self;
    }
}

mod conversions_for_atom {
    use super::*;

    impl Atom {
        /// Construct an [`Atom`] with the given reflectance color.
        ///
        /// This is identical to `From<Rgba>::from()` except that it is a `const fn`.
        // TODO: public API?
        pub(crate) const fn from_color(color: Rgba) -> Self {
            Atom {
                color,
                emission: Rgb::ZERO,
                collision: BlockCollision::DEFAULT_FOR_FROM_COLOR,
            }
        }
    }

    impl From<Rgb> for Atom {
        /// Construct an [`Atom`] with the given reflectance color, and default attributes.
        fn from(color: Rgb) -> Self {
            Self::from_color(color.with_alpha_one())
        }
    }
    impl From<Rgba> for Atom {
        /// Construct an [`Atom`] with the given reflectance color, and default attributes.
        fn from(color: Rgba) -> Self {
            Self::from_color(color)
        }
    }

    impl From<Atom> for Primitive {
        fn from(value: Atom) -> Self {
            Primitive::Atom(value)
        }
    }

    impl From<Atom> for Block {
        fn from(value: Atom) -> Self {
            Block::from_primitive(Primitive::Atom(value))
        }
    }
}

mod conversions_for_indirect {
    use super::*;

    impl From<Handle<BlockDef>> for Primitive {
        /// Convert a `Handle<BlockDef>` into a [`Primitive::Indirect`] that refers to it.
        fn from(block_def_handle: Handle<BlockDef>) -> Self {
            Primitive::Indirect(block_def_handle)
        }
    }

    impl From<Handle<BlockDef>> for Block {
        /// Convert a `Handle<BlockDef>` into a block with [`Primitive::Indirect`] that refers to it.
        ///
        /// The returned block will evaluate to the same [`EvaluatedBlock`] as the block contained
        /// within the given [`BlockDef`] (except in case of errors).
        fn from(block_def_handle: Handle<BlockDef>) -> Self {
            Block::from_primitive(Primitive::Indirect(block_def_handle))
        }
    }
}

/// Notification when an [`EvaluatedBlock`] result changes.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
#[expect(clippy::module_name_repetitions)] // TODO: rename?
pub struct BlockChange {
    /// I expect there _might_ be future uses for a set of flags of what changed;
    /// this helps preserve the option of adding them.
    _not_public: (),
}

impl BlockChange {
    #[expect(clippy::new_without_default)]
    #[allow(missing_docs)] // TODO: why is this public, anyway?
    pub fn new() -> BlockChange {
        BlockChange { _not_public: () }
    }
}

/// Construct a set of [`Primitive::Recur`] blocks that form a miniature of the given `space`.
///
/// The returned [`Space`] contains each of the blocks; its coordinates will correspond to
/// those of the input, scaled down by `resolution`.
///
/// Panics if the `Space` cannot be accessed, and returns
/// [`SetCubeError::TooManyBlocks`] if the space volume is too large.
//---
// TODO: This is only used once ... is it really a good public API?
pub fn space_to_blocks(
    resolution: Resolution,
    space_handle: Handle<Space>,
    block_transform: &mut dyn FnMut(Block) -> Block,
) -> Result<Space, SetCubeError> {
    let resolution_g: GridCoordinate = resolution.into();
    let source_bounds = space_handle
        .read()
        .expect("space_to_blocks() could not read() provided space")
        .bounds();
    let destination_bounds = source_bounds.divide(resolution_g);

    let mut destination_space = Space::empty(destination_bounds);
    destination_space.fill(destination_bounds, move |cube| {
        Some(block_transform(Block::from_primitive(Primitive::Recur {
            offset: (cube.lower_bounds().to_vector() * resolution_g).to_point(),
            resolution,
            space: space_handle.clone(),
        })))
    })?;
    Ok(destination_space)
}

// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! [`BlockAttributes`] and closely related types.

use std::borrow::Cow;
use std::fmt;

use crate::math::Rgb;

#[cfg(doc)]
use crate::{block::Block, space::Space};

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
    pub display_name: Cow<'static, str>,

    /// Whether players' cursors target it or pass through it.
    ///
    /// The default value is `true`.
    pub selectable: bool,

    /// The effect on a [`Body`](crate::physics::Body) of colliding with this block.
    ///
    /// The default value is [`BlockCollision::Hard`].
    pub collision: BlockCollision,

    /// Light emitted by the block.
    ///
    /// The default value is [`Rgb::ZERO`].
    pub light_emission: Rgb,

    /// Advice to the renderer about how to expect this block to change, and hence
    /// what rendering strategy to use.
    pub animation_hint: AnimationHint,
    //
    // Reminder: When adding new fields, add them to the Debug implementation
    // and BlockBuilder.
    //
    // TODO: add 'behavior' functionality, if we don't come up with something else
}

impl fmt::Debug for BlockAttributes {
    /// Only attributes which differ from the default are shown.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self == &Self::default() {
            // Avoid the braceless formatting used for structs with literally no fields.
            write!(f, "BlockAttributes {{}}")
        } else {
            let mut s = f.debug_struct("BlockAttributes");
            if self.display_name != Self::default().display_name {
                // Unwrap the `Cow` for tidier formatting.
                s.field("display_name", &&*self.display_name);
            }
            if self.selectable != Self::default().selectable {
                s.field("selectable", &self.selectable);
            }
            if self.collision != Self::default().collision {
                s.field("collision", &self.collision);
            }
            if self.light_emission != Self::default().light_emission {
                s.field("light_emission", &self.light_emission);
            }
            if self.animation_hint != Self::default().animation_hint {
                s.field("animation_hint", &self.animation_hint);
            }
            s.finish()
        }
    }
}

impl BlockAttributes {
    /// Block attributes suitable as default values for in-game use.
    ///
    /// This function differs from the [`Default::default`] trait implementation only
    /// in that it is a `const fn`.
    pub const fn default() -> BlockAttributes {
        BlockAttributes {
            display_name: Cow::Borrowed(""),
            selectable: true,
            collision: BlockCollision::Hard,
            light_emission: Rgb::ZERO,
            animation_hint: AnimationHint::UNCHANGING,
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
impl<'a> arbitrary::Arbitrary<'a> for BlockAttributes {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(BlockAttributes {
            display_name: Cow::Owned(u.arbitrary()?),
            selectable: u.arbitrary()?,
            collision: u.arbitrary()?,
            light_emission: u.arbitrary()?,
            animation_hint: u.arbitrary()?,
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        arbitrary::size_hint::and_all(&[
            String::size_hint(depth),
            bool::size_hint(depth),
            BlockCollision::size_hint(depth),
            Rgb::size_hint(depth),
            AnimationHint::size_hint(depth),
        ])
    }
}

/// Specifies the effect on a [`Body`](crate::physics::Body) of colliding with the
/// [`Block`] this applies to.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum BlockCollision {
    /// The block can be passed through; it is not an obstacle (though intersecting it
    /// might cause other effects not directly part of collision response).
    None,
    /// The block is a perfectly solid obstacle occupying its entire bounding cube.
    ///
    /// This is the default value used for most blocks. (Caveat: The default might be
    /// changed to `Recur` if that proves more ergonomic.)
    Hard,
    /// Collide with the block's component voxels individually.
    ///
    /// If the block does not have voxels then this is equivalent to [`Hard`](Self::Hard).
    Recur,
    // Future values might include bouncy solid, water-like resistance, force fields, etc.
}

/// Specifies how the appearance of a [`Block`] might change, for the benefit of rendering
/// algorithms. This hint applies both to a block's definition changing and to it being
/// replaced with some successor block.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct AnimationHint {
    /// Expect that the block might soon be replaced with an unrelated block.
    /// Suggestion: avoid combining it with other block meshes.
    pub(crate) expect_replace: bool,

    /// Expect that the block's shape will change; some of its voxels will not be the same
    /// [`OpacityCategory`](crate::math::OpacityCategory).
    ///
    /// Suggestion: use a rendering strategy which is shape-independent.
    pub(crate) expect_shape_update: bool,

    /// Expect that the block's voxels' colors (and alpha other than the special 0 and 1
    /// cases) will change.
    ///
    /// Suggestion: prepare to update texturing without unnecesarily regenerating the mesh.
    pub(crate) expect_color_update: bool,
}

impl AnimationHint {
    /// There are no expectations that the block is soon going to change.
    ///
    /// This is the default value of this type and within [`BlockAttributes`].
    pub const UNCHANGING: Self = Self {
        expect_replace: false,
        expect_shape_update: false,
        expect_color_update: false,
    };

    /// The block is not going to exist in its current form for long.
    ///
    /// This suggests using a rendering technique which is comparatively expensive
    /// per-block but allows it (and any successors that are also `TEMPORARY`) to be added
    /// and removed cheaply.
    pub const TEMPORARY: Self = Self {
        expect_replace: true,
        ..Self::UNCHANGING
    };

    /// The block's appearance is expected to change very frequently, but not by replacing
    /// the block in its [`Space`].
    ///
    /// This suggests using a rendering technique which optimizes for not needing to e.g.
    /// rebuild chunk meshes.
    pub const CONTINUOUS: Self = Self {
        expect_color_update: true,
        expect_shape_update: true,
        ..Self::UNCHANGING
    };

    /// Returns whether this block's value for [`EvaluatedBlock::visible`] is likely to
    /// change from `false` to `true`.
    pub(crate) fn might_become_visible(&self) -> bool {
        self.expect_shape_update
    }
}

impl Default for AnimationHint {
    fn default() -> Self {
        Self::UNCHANGING
    }
}

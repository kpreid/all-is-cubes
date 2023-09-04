use cgmath::{EuclideanSpace, InnerSpace, Point3};

use crate::behavior::BehaviorSet;
use crate::block::{Block, AIR};
use crate::character::Spawn;
use crate::math::{FreeCoordinate, GridArray, Rgb};
use crate::space::{
    BlockIndex, GridAab, LightPhysics, PackedLight, Palette, PaletteError, Space, SpacePhysics,
};

/// Tool for constructing new [`Space`]s.
///
/// To create one, call [`Space::builder()`](Space::builder).
///
/// TODO: Allow specifying behaviors.
///
/// # Type parameters
///
/// * `B` is either `()` or `GridAab` according to whether the bounds have been specified.
#[derive(Clone, Debug)]
#[must_use]
pub struct SpaceBuilder<B> {
    pub(super) bounds: B,
    pub(super) spawn: Option<Spawn>,
    pub(super) physics: SpacePhysics,
    pub(super) behaviors: BehaviorSet<Space>,
    pub(super) contents: Fill,
}

#[derive(Clone, Debug)]
pub(super) enum Fill {
    Block(Block),
    Data {
        /// Note: this palette has its block counts already set to match contents
        palette: Palette,
        contents: GridArray<BlockIndex>,
        light: Option<GridArray<PackedLight>>,
    },
}

impl<B> SpaceBuilder<B> {
    /// Sets the [`Block`] that the space's volume will be filled with.
    ///
    /// Calling this method will replace any previous specification of the contents,
    /// such as [`palette_and_contents()`](Self::palette_and_contents()).
    pub fn filled_with(mut self, block: Block) -> Self {
        self.contents = Fill::Block(block);
        self
    }

    /// Sets the value for [`Space::physics`], which determines global characteristics
    /// of gravity and light in the space.
    pub fn physics(mut self, physics: SpacePhysics) -> Self {
        self.physics = physics;
        self
    }

    /// Sets the value of [`SpacePhysics::sky_color`] for the space.
    pub fn sky_color(mut self, color: Rgb) -> Self {
        self.physics.sky_color = color;
        self
    }

    /// Sets the value of [`SpacePhysics::light`] for the space, which determines the
    /// behavior of light within the space.
    pub fn light_physics(mut self, light_physics: LightPhysics) -> Self {
        self.physics.light = light_physics;
        self
    }

    /// Sets the value for [`Space::spawn`], which determines the default circumstances of
    /// new characters.
    ///
    /// If not set, the default spawn position will be [0, 0, 0].
    /// (TODO: Improve this and document it centrally.)
    pub fn spawn(mut self, spawn: Spawn) -> Self {
        self.spawn = Some(spawn);
        self
    }

    /// TODO: not sure if this is good public API
    #[allow(unused)] // currently only used on feature=save
    pub(crate) fn behaviors(mut self, behaviors: BehaviorSet<Space>) -> Self {
        self.behaviors = behaviors;
        self
    }
}

impl<B: SpaceBuilderBounds> SpaceBuilder<B> {
    /// Set the bounds unless they have already been set.
    pub fn bounds_if_not_set(self, bounds_fn: impl FnOnce() -> GridAab) -> SpaceBuilder<GridAab> {
        // Delegate to the trait. (This method exists so the trait need not be imported.)
        SpaceBuilderBounds::bounds_if_not_set(self, bounds_fn)
    }
}

impl SpaceBuilder<()> {
    /// Use [`SpaceBuilder::default()`] as the public way to call this.
    pub(super) const fn new() -> Self {
        Self {
            bounds: (),
            spawn: None,
            physics: SpacePhysics::DEFAULT,
            behaviors: BehaviorSet::new(),
            contents: Fill::Block(AIR),
        }
    }

    /// Set the bounds of the space, outside which no blocks may be placed.
    pub fn bounds(self, bounds: GridAab) -> SpaceBuilder<GridAab> {
        SpaceBuilder {
            bounds,
            spawn: self.spawn,
            physics: self.physics,
            behaviors: self.behaviors,
            contents: self.contents,
        }
    }
}

impl SpaceBuilder<GridAab> {
    /// Sets the default spawn location of new characters.
    ///
    /// Panics if any of the given coordinates is infinite or NaN.
    #[track_caller]
    pub fn spawn_position(mut self, position: Point3<FreeCoordinate>) -> Self {
        assert!(
            position.to_vec().magnitude2().is_finite(),
            "spawn_position must be finite"
        );

        let bounds = self.bounds;
        let mut spawn = self
            .spawn
            .unwrap_or_else(|| Spawn::default_for_new_space(bounds));
        spawn.set_eye_position(position);
        self.spawn = Some(spawn);
        self
    }

    /// Sets the initial contents of the space using a palette (numbered list of blocks)
    /// and indices into that palette for every in-bounds cube.
    ///
    /// The input data must meet all of these requirements, or a [`PaletteError`] will be
    /// returned:
    ///
    /// * `palette` must have no more than `BlockIndex::MAX + 1` elements.
    /// * `contents` must have the same bounds as were set for this space.
    /// * `contents` must contain no elements that are out of bounds of the `palette`.
    /// * `light`, if specified, must have the same bounds as were set for this space.
    ///
    /// The `palette` is allowed to contain duplicate elements, but they will be combined.
    /// In general, the produced [`Space`] will not necessarily have the same indices
    /// as were provided.
    ///
    /// Calling this method will replace any previous specification of the contents,
    /// such as [`filled_with()`](Self::filled_with()).
    pub fn palette_and_contents<P>(
        self,
        palette: P,
        contents: GridArray<BlockIndex>,
        light: Option<GridArray<PackedLight>>,
    ) -> Result<Self, PaletteError>
    where
        P: IntoIterator,
        P::IntoIter: ExactSizeIterator<Item = Block>,
    {
        self.palette_and_contents_impl(&mut palette.into_iter(), contents, light)
    }

    fn palette_and_contents_impl(
        mut self,
        palette: &mut dyn ExactSizeIterator<Item = Block>,
        mut contents: GridArray<BlockIndex>,
        light: Option<GridArray<PackedLight>>,
    ) -> Result<Self, PaletteError> {
        // Validate palette.
        let (mut palette, remapping) = Palette::from_blocks(palette)?;

        // Validate bounds.
        if contents.bounds() != self.bounds {
            return Err(PaletteError::WrongDataBounds {
                expected: self.bounds,
                actual: contents.bounds(),
            });
        }
        if let Some(light) = light.as_ref() {
            if light.bounds() != self.bounds {
                return Err(PaletteError::WrongDataBounds {
                    expected: self.bounds,
                    actual: light.bounds(),
                });
            }
        }

        // Validate data and update palette contents
        let palette_len = palette.entries().len();
        for (cube, contents_block_index) in contents.iter_mut() {
            if let Some(&new_block_index) = remapping.get(contents_block_index) {
                // Remap indices in the case where the palette contained duplicates
                *contents_block_index = new_block_index;
            } else if usize::from(*contents_block_index) >= palette_len {
                // If the index was not remapped and is out of range then it's invalid.
                return Err(PaletteError::Index {
                    index: *contents_block_index,
                    cube,
                    palette_len,
                });
            }

            palette.increment(*contents_block_index);
        }

        palette.free_all_zero_counts();

        // Store data
        self.contents = Fill::Data {
            palette,
            contents,
            light,
        };

        Ok(self)
    }

    /// Construct a [`Space`] with the contents and settings from this builder.
    ///
    /// The builder must have had bounds specified.
    pub fn build(self) -> Space {
        Space::new_from_builder(self)
    }
}

impl Default for SpaceBuilder<()> {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper for [`SpaceBuilder::bounds_if_not_set()`]. Do not call or implement this trait.
pub trait SpaceBuilderBounds: sbb::SbbSealed + Sized {
    /// Set the bounds unless they have already been set.
    ///
    /// This function is an implementation detail; call
    /// [`SpaceBuilder::bounds_if_not_set()`] instead.
    #[doc(hidden)]
    fn bounds_if_not_set(
        builder: SpaceBuilder<Self>,
        bounds_fn: impl FnOnce() -> GridAab,
    ) -> SpaceBuilder<GridAab>;
}

impl SpaceBuilderBounds for () {
    fn bounds_if_not_set(
        builder: SpaceBuilder<Self>,
        bounds_fn: impl FnOnce() -> GridAab,
    ) -> SpaceBuilder<GridAab> {
        builder.bounds(bounds_fn())
    }
}

impl SpaceBuilderBounds for GridAab {
    fn bounds_if_not_set(
        builder: SpaceBuilder<Self>,
        _bounds_fn: impl FnOnce() -> GridAab,
    ) -> SpaceBuilder<GridAab> {
        builder
    }
}

/// Module for sealed trait
mod sbb {
    use super::*;
    #[doc(hidden)]
    pub trait SbbSealed {}
    impl SbbSealed for () {}
    impl SbbSealed for GridAab {}
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for Space {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        use crate::content::make_some_blocks;

        // TODO: Should be reusing GridArray as Arbitrary for this.

        let bounds = GridAab::arbitrary_with_max_volume(u, 2048)?;
        let mut space = Space::builder(bounds)
            .physics(u.arbitrary()?)
            .spawn(u.arbitrary()?)
            .build();

        // Generate some blocks to put in the space
        let mut blocks = Vec::from(make_some_blocks::<2>()); // TODO: generate arbitrary blocks with attributes
        #[allow(clippy::same_item_push)]
        for _ in 0..6 {
            // Make it probable that blocks are AIR
            blocks.push(AIR);
        }

        // Fill space with blocks
        // TODO: use palette mechanism instead now that we have it
        let mut failure = None;
        space
            .fill(space.bounds(), |_| {
                match u.choose(&blocks) {
                    Ok(block) => Some(block),
                    Err(e) => {
                        // We can't abort a space.fill() early unless we resort to catch_unwind.
                        failure = Some(e);
                        None
                    }
                }
            })
            .unwrap();
        if let Some(e) = failure {
            return Err(e);
        }

        Ok(space)
    }
}

#[cfg(test)]
mod tests {
    use crate::content::make_some_blocks;
    use crate::math::{Cube, Rgba};

    use super::*;

    #[test]
    fn defaults() {
        let bounds = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
        let space = Space::builder(bounds).build();
        space.consistency_check();
        assert_eq!(space.bounds(), bounds);
        assert_eq!(space[bounds.lower_bounds()], AIR);
        assert_eq!(space.physics(), &SpacePhysics::default());
        assert_eq!(space.spawn(), &Spawn::default_for_new_space(bounds));
    }

    #[test]
    fn filled_with() {
        let bounds = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
        let block = Block::from(Rgba::WHITE);
        let space = Space::builder(bounds).filled_with(block.clone()).build();
        space.consistency_check();
        assert_eq!(space[bounds.lower_bounds()], block);
    }

    #[test]
    fn bounds_if_not_set_when_not_set() {
        let bounds = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
        assert_eq!(
            SpaceBuilder::new()
                .bounds_if_not_set(|| bounds)
                .build()
                .bounds(),
            bounds
        );
    }

    #[test]
    fn bounds_if_not_set_when_already_set() {
        let first_bounds = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
        let ignored_bounds = GridAab::from_lower_size([100, 2, 3], [1, 1, 1]);
        assert_eq!(
            Space::builder(first_bounds)
                .bounds_if_not_set(|| ignored_bounds)
                .build()
                .bounds(),
            first_bounds
        );
    }

    #[test]
    fn palette_err_too_long() {
        let bounds = GridAab::ORIGIN_CUBE;
        assert_eq!(
            Space::builder(bounds)
                .palette_and_contents(vec![AIR; 65537], GridArray::from_element(2), None,)
                .unwrap_err(),
            PaletteError::PaletteTooLarge { len: 65537 }
        );
    }

    #[test]
    fn palette_err_too_short_for_contents() {
        let bounds = GridAab::ORIGIN_CUBE;
        assert_eq!(
            Space::builder(bounds)
                .palette_and_contents(&mut [AIR].into_iter(), GridArray::from_element(2), None,)
                .unwrap_err(),
            PaletteError::Index {
                index: 2,
                cube: Cube::new(0, 0, 0),
                palette_len: 1
            }
        );
    }

    #[test]
    fn palette_err_contents_wrong_bounds() {
        assert_eq!(
            Space::builder(GridAab::single_cube(Cube::new(1, 0, 0)))
                .palette_and_contents([AIR], GridArray::from_element(0), None)
                .unwrap_err(),
            PaletteError::WrongDataBounds {
                expected: GridAab::single_cube(Cube::new(1, 0, 0)),
                actual: GridAab::ORIGIN_CUBE,
            }
        );
    }

    /// Duplicate blocks are permitted in the input palette even though `Space` doesn't
    /// allow duplicates in its own palette. This is because deserialized/imported input
    /// might have duplicates it did not intend, once the foreign or old blocks are
    /// converted into specific [`Block`] instances.
    #[test]
    fn palette_with_duplicate_entries() {
        let bounds = GridAab::from_lower_size([0, 0, 0], [3, 1, 1]);
        let [block0, block1] = make_some_blocks();
        let space = Space::builder(bounds)
            .palette_and_contents(
                [block0.clone(), block1.clone(), block0.clone()],
                GridArray::from_elements(bounds, [0, 1, 2]).unwrap(),
                None,
            )
            .unwrap()
            .build();

        space.consistency_check();

        // We do not require the new space to have exactly the same indices as the input,
        // but the blocks should match.
        assert_eq!(space[[0, 0, 0]], block0);
        assert_eq!(space[[1, 0, 0]], block1);
        assert_eq!(space[[2, 0, 0]], block0);
    }

    /// Unused entries in a palette should be converted to canonical tombstone entries.
    #[test]
    fn palette_with_unused_entries() {
        let bounds = GridAab::from_lower_size([0, 0, 0], [2, 1, 1]);
        let blocks = make_some_blocks::<3>();
        let space = Space::builder(bounds)
            .palette_and_contents(
                blocks.clone(),
                GridArray::from_elements(bounds, [0, 2]).unwrap(),
                None,
            )
            .unwrap()
            .build();

        space.consistency_check();

        // blocks[1] was not used so it should not be in the palette.
        let found = space
            .block_data()
            .iter()
            .find(|entry| entry.block == blocks[1]);
        assert!(found.is_none(), "{found:?}");
    }

    // TODO: test and implement initial fill that has a tick_action that needs to be
    // activated properly

    // TODO: test all builder features
}

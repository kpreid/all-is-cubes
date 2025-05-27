//! Parts of advancing time in a universe.
//!
//! TODO(ecs): Figure out if this module makes sense after we finish the ECS migration.

use bevy_ecs::prelude as ecs;
use hashbrown::HashMap;

use crate::block;
use crate::space::{BlockIndex, Space};
use crate::universe::ReadTicket;

// -------------------------------------------------------------------------------------------------

/// When updating spaces' palettes, this temporarily stores the new block evaluations.
/// into the `BlockDef` component.
#[derive(bevy_ecs::component::Component, Default)]
pub(crate) struct SpacePaletteNextValue(pub(crate) HashMap<BlockIndex, block::EvaluatedBlock>);

// -------------------------------------------------------------------------------------------------

/// ECS system that computes but does not apply updates to [`Space`]'s `Palette`.
///
/// TODO: this is basically a copy of similar code for `BlockDef`
pub(crate) fn update_palette_phase_1(
    mut spaces: ecs::Query<'_, '_, (&Space, &mut SpacePaletteNextValue)>,
    data_sources: crate::universe::QueryBlockDataSources<'_, '_>,
) {
    let read_ticket = ReadTicket::from_block_data_sources(data_sources);

    // TODO: parallel iter, + pipe out update info
    for (space, mut next_palette) in spaces.iter_mut() {
        debug_assert!(
            next_palette.0.is_empty(),
            "SpacePaletteNextValue should have been cleared"
        );

        // TODO: Only run this update on dirty flag.

        space.palette.prepare_update(read_ticket, &mut next_palette);
    }
}

/// System that moves new block evaluations from `SpacePaletteNextValue` to [`Space`]'s
/// [`Palette`].
///
/// This system being separate resolves the borrow conflict between writing to a [`Space`]
/// and block evaluation (which may read from any [`Space`]).
pub(crate) fn update_palette_phase_2(
    mut spaces: ecs::Query<'_, '_, (&mut Space, &mut SpacePaletteNextValue)>,
) {
    // TODO(ecs): run this only on entities that need it, somehow
    spaces
        .par_iter_mut()
        .for_each(|(mut space, mut next_palette)| {
            let space: &mut Space = &mut space; // deref for borrow splitting
            space
                .palette
                .apply_update(&mut next_palette, &mut space.change_notifier.buffer());
        });
}

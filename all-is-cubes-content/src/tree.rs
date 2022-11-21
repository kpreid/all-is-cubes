use std::fmt;

use all_is_cubes::block::{self, Block, AIR};
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{Face6, FaceMap, GridCoordinate, GridPoint, GridRotation, GridVector};
use all_is_cubes::space::SpaceTransaction;

use crate::LandscapeBlocks::Leaves;

/// Tree segment sizes or growth stages.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, exhaust::Exhaust)]
#[allow(clippy::exhaustive_enums)]
pub enum TreeGrowth {
    Sapling = 1, // radius = 1
    G2,          // radius = 2
    G3,
    G4,
    G5,
    G6,
    G7,
    Block,
}

impl fmt::Display for TreeGrowth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self as GridCoordinate)
    }
}

impl TreeGrowth {
    pub fn from_radius(index: GridCoordinate) -> Option<Self> {
        Some(match index {
            1 => Self::Sapling,
            2 => Self::G2,
            3 => Self::G3,
            4 => Self::G4,
            5 => Self::G5,
            6 => Self::G6,
            7 => Self::G7,
            8 => Self::Block,
            _ => return None,
        })
    }

    pub fn radius(self) -> GridCoordinate {
        self as i32
    }
}

/// Construct a tree log/branch block whose faces each have the specified cross-section size,
/// or [`None`] for no branch in that direction.
pub(crate) fn make_log(
    blocks: &BlockProvider<crate::LandscapeBlocks>,
    directions: FaceMap<Option<TreeGrowth>>,
) -> Block {
    // TODO: this needs to canonicalize rotations so that we don't end up with
    // identical-looking but differently defined blocks.

    let mut parts: Vec<Block> = directions
        .iter()
        .map(|(face, &growth)| {
            let Ok(face) = Face6::try_from(face) else { return AIR; };
            let Some(growth) = growth else { return AIR; };
            blocks[crate::LandscapeBlocks::Log(growth)].clone().rotate(
                GridRotation::from_to(Face6::NY, face, Face6::PX)
                    .or_else(|| GridRotation::from_to(Face6::NY, face, Face6::PZ))
                    .unwrap(),
            )
        })
        .collect();

    let Some(mut block) = parts.pop() else { return AIR; };
    for next in parts {
        // compose_or_replace will take care of simplifying away `AIR`s.
        block =
            block::Composite::new(next, block::CompositeOperator::Over).compose_or_replace(block);
    }
    block
}

/// Construct a tree whose lowest trunk piece is at `origin` and whose maximum height is `height`.
pub(crate) fn make_tree(
    blocks: &BlockProvider<crate::LandscapeBlocks>,
    mut rng: impl rand::Rng,
    mut origin: GridPoint,
    mut height: GridCoordinate,
) -> Result<SpaceTransaction, InGenError> {
    let mut txn = SpaceTransaction::default();

    while height > 1 {
        let log = make_log(
            blocks,
            FaceMap {
                ny: Some(TreeGrowth::from_radius(height).unwrap_or(TreeGrowth::Block)),
                py: Some(TreeGrowth::from_radius(height - 1).unwrap_or(TreeGrowth::Block)),
                nx: if rng.gen_bool(0.2) {
                    Some(TreeGrowth::G2)
                } else {
                    None
                },
                ..FaceMap::repeat(None)
            },
        );
        txn.set_overwrite(origin, log);
        origin += GridVector::unit_y();
        height -= 1;
    }
    if height > 0 {
        txn.set_overwrite(origin, blocks[Leaves(TreeGrowth::G7)].clone());
    }

    Ok(txn)
}

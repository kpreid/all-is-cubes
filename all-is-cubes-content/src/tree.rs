use std::fmt;

use all_is_cubes::block::{self, Block, AIR};
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{Face6, FaceMap, GridCoordinate, GridPoint, GridRotation, GridVector};
use all_is_cubes::space::SpaceTransaction;
use all_is_cubes::transaction::Merge;

use crate::LandscapeBlocks::{self, Leaves, Log};

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
    /// Convert a log radius (meaningful range 1 to 8) to the enum.
    /// Out-of-range values will be clamped/saturated.
    pub fn from_radius(index: GridCoordinate) -> Self {
        match index {
            i if i <= 1 => Self::Sapling,
            2 => Self::G2,
            3 => Self::G3,
            4 => Self::G4,
            5 => Self::G5,
            6 => Self::G6,
            7 => Self::G7,
            _ => Self::Block,
        }
    }

    pub fn radius(self) -> GridCoordinate {
        self as i32
    }
}

/// Construct a tree log/branch block whose faces each have the specified cross-section size,
/// or [`None`] for no branch in that direction.
pub(crate) fn make_log(
    blocks: &BlockProvider<LandscapeBlocks>,
    directions: FaceMap<Option<TreeGrowth>>,
    leaves: Option<TreeGrowth>,
) -> Block {
    // TODO: this needs to canonicalize rotations so that we don't end up with
    // identical-looking but differently defined blocks.

    let mut parts: Vec<Block> = directions
        .iter()
        .map(|(face, &growth)| {
            let Some(growth) = growth else { return AIR; };
            blocks[Log(growth)].clone().rotate(
                GridRotation::from_to(Face6::NY, face, Face6::PX)
                    .or_else(|| GridRotation::from_to(Face6::NY, face, Face6::PZ))
                    .unwrap(),
            )
        })
        .collect();

    if let Some(leaves_growth) = leaves {
        parts.push(blocks[Leaves(leaves_growth)].clone());
    }

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
    blocks: &BlockProvider<LandscapeBlocks>,
    rng: &mut impl rand::Rng,
    mut origin: GridPoint,
    mut height: GridCoordinate,
) -> Result<SpaceTransaction, InGenError> {
    let mut txn = SpaceTransaction::default();

    while height > 1 {
        let mut branches = FaceMap {
            ny: Some(TreeGrowth::from_radius(height)),
            py: Some(TreeGrowth::from_radius(height - 1)),
            ..FaceMap::repeat(None)
        };

        for side in [Face6::PX, Face6::PZ, Face6::NX, Face6::NZ] {
            if rng.gen_bool(0.2 / f64::from(height)) {
                let branch_length = rng.gen_range(0..height - 1);
                if branch_length > 0 {
                    let branch_base_growth = TreeGrowth::from_radius(branch_length);
                    branches[side] = Some(branch_base_growth);

                    txn.set_overwrite(
                        origin + side.normal_vector(),
                        make_log(
                            blocks,
                            FaceMap::default()
                                .with(Face6::PY, Some(branch_base_growth))
                                .with(side.opposite(), Some(branch_base_growth)),
                            None,
                        ),
                    );

                    let branch_txn = make_tree(
                        blocks,
                        rng,
                        origin + side.normal_vector() + GridVector::unit_y(),
                        branch_length,
                    )?;
                    if let Ok(check) = txn.check_merge(&branch_txn) {
                        txn = txn.commit_merge(branch_txn, check);
                    }
                }
            }
        }

        txn.set_overwrite(origin, make_log(blocks, branches, None));
        origin += GridVector::unit_y();
        height -= 1;
    }
    if height > 0 {
        txn.set_overwrite(
            origin,
            make_log(
                blocks,
                FaceMap::default().with(Face6::NY, Some(TreeGrowth::Sapling)),
                Some(TreeGrowth::G6),
            ),
        );
    }

    Ok(txn)
}

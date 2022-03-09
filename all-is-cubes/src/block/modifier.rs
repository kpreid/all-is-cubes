// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use crate::block::{EvalBlockError, EvaluatedBlock};
use crate::math::GridRotation;
use crate::space::GridArray;

// Things mentioned in doc comments only
#[cfg(doc)]
use super::Block;

/// Modifiers can be applied to a [`Block`] to change the result of
/// [`evaluate()`](Block::evaluate)ing it.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Modifier {
    /// Rotate the block about its cube center by the given rotation.
    Rotate(GridRotation),
}

impl Modifier {
    pub(crate) fn apply(
        &self,
        value: EvaluatedBlock,
        _depth: u8,
    ) -> Result<EvaluatedBlock, EvalBlockError> {
        Ok(match *self {
            Modifier::Rotate(rotation) => {
                if value.voxels.is_none() && value.voxel_opacity_mask.is_none() {
                    // Skip computation of transforms
                    value
                } else {
                    // TODO: Add a shuffle-in-place rotation operation to GridArray and try implementing this using that, which should have less arithmetic involved than these matrix ops
                    let resolution = value.resolution;
                    let inner_to_outer = rotation.to_positive_octant_matrix(resolution.into());
                    let outer_to_inner = rotation
                        .inverse()
                        .to_positive_octant_matrix(resolution.into());

                    EvaluatedBlock {
                        voxels: value.voxels.map(|voxels| {
                            GridArray::from_fn(
                                voxels.grid().transform(inner_to_outer).unwrap(),
                                |cube| voxels[outer_to_inner.transform_cube(cube)],
                            )
                        }),
                        voxel_opacity_mask: value.voxel_opacity_mask.map(|mask| {
                            GridArray::from_fn(
                                mask.grid().transform(inner_to_outer).unwrap(),
                                |cube| mask[outer_to_inner.transform_cube(cube)],
                            )
                        }),

                        // Unaffected
                        attributes: value.attributes,
                        color: value.color,
                        resolution,
                        opaque: value.opaque,
                        visible: value.visible,
                    }
                }
            }
        })
    }
}

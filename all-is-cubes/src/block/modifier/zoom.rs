use cgmath::{EuclideanSpace as _, Point3};

use crate::block::{
    self, Evoxel, Evoxels, MinEval, Modifier,
    Resolution::{self, R1},
};
use crate::math::{Cube, GridAab, GridArray, GridCoordinate, GridPoint};
use crate::universe;

/// Data for [`Modifier::Zoom`], describing a portion of the original block that is scaled
/// up to become the whole block.
///
/// Design note: This is a struct separate from [`Modifier`] so that it can have a
/// constructor accepting only valid bounds.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Zoom {
    /// Scale factor to zoom in by.
    scale: Resolution,

    /// Which portion of the block/space will be used, specified in terms of an offset
    /// in the grid of zoomed blocks (that is, this should have coordinates between `0`
    /// and `scale - 1`).
    offset: Point3<u8>,
    // /// If present, a space to extract voxels from _instead of_ the underlying
    // /// [`Primitive`]. This may be used so that the before-zooming block can be a
    // /// custom preview rather than an exact miniature of the multi-block
    // /// structure.
    // space: Option<URef<Space>>,
}

impl Zoom {
    /// Construct a [`Zoom`] which enlarges the original block's voxels by `scale` and
    /// selects the region of them whose lower corner is `offset * scale`.
    ///
    /// Panics if any of `offset`'s components are out of bounds, i.e. less than 0 or
    /// greater than `scale - 1`.
    #[track_caller]
    pub fn new(scale: Resolution, offset: GridPoint) -> Self {
        if !GridAab::for_block(scale).contains_cube(Cube::from(offset)) {
            panic!("Zoom offset {offset:?} out of bounds for {scale}");
        }

        Self {
            scale,
            offset: offset.map(|c| c as u8),
        }
    }

    /// Decompose into parts, for serialization.
    #[cfg(feature = "save")]
    pub(crate) fn to_serial_schema(&self) -> crate::save::schema::ModifierSer {
        let Zoom { scale, offset } = *self;
        crate::save::schema::ModifierSer::ZoomV1 {
            scale,
            offset: offset.into(),
        }
    }

    pub(super) fn evaluate(&self, input: MinEval) -> Result<MinEval, block::EvalBlockError> {
        let Zoom {
            offset: offset_in_zoomed_blocks,
            scale,
        } = *self;

        // TODO: To efficiently implement this, we should be able to run in a phase
        // *before* the `Primitive` evaluation, which allows us to reduce how many
        // of the primitive voxels are evaluated. (Modifier::Move will also benefit.)

        let original_resolution = input.resolution();
        let MinEval { attributes, voxels } = input;

        // TODO: write test cases for what happens if the division fails
        // (this is probably wrong in that we need to duplicate voxels if it happens)
        let zoom_resolution = (original_resolution / scale).unwrap_or(R1);

        Ok(match voxels {
            Evoxels::One(_) => {
                // Block has resolution 1.
                // Zoom::new() checks that the region is not outside the block's unit cube,
                // so we can just unconditionally return the original color.
                MinEval { attributes, voxels }
            }
            Evoxels::Many(_, voxels) => {
                let voxel_offset = offset_in_zoomed_blocks.map(GridCoordinate::from).to_vec()
                    * GridCoordinate::from(zoom_resolution);
                match GridAab::for_block(zoom_resolution)
                    .intersection(voxels.bounds().translate(-voxel_offset))
                {
                    // This case occurs when the voxels' actual bounds (which may be smaller
                    // than the block bounding box) don't intersect the zoom region.
                    None => MinEval {
                        attributes,
                        voxels: Evoxels::One(Evoxel::AIR),
                    },
                    Some(intersected_bounds) => MinEval {
                        attributes,
                        voxels: Evoxels::Many(
                            zoom_resolution,
                            GridArray::from_fn(intersected_bounds, |p| voxels[p + voxel_offset]),
                        ),
                    },
                }
            }
        })
    }
}

impl From<Zoom> for block::Modifier {
    fn from(value: Zoom) -> Self {
        Modifier::Zoom(value)
    }
}

impl universe::VisitRefs for Zoom {
    fn visit_refs(&self, _visitor: &mut dyn universe::RefVisitor) {
        let Zoom {
            scale: _,
            offset: _,
        } = self;
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Zoom {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let scale = u.arbitrary()?;
        let max_offset = GridCoordinate::from(scale) - 1;
        Ok(Self::new(
            scale,
            GridPoint::new(
                u.int_in_range(0..=max_offset)?,
                u.int_in_range(0..=max_offset)?,
                u.int_in_range(0..=max_offset)?,
            ),
        ))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        use arbitrary::{size_hint::and_all, Arbitrary};
        and_all(&[
            <Resolution as Arbitrary>::size_hint(depth),
            <[GridCoordinate; 3] as Arbitrary>::size_hint(depth),
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{EvaluatedBlock, Resolution::R2};
    use crate::content::{make_some_blocks, make_some_voxel_blocks};
    use crate::math::{GridAab, GridVector, Rgba};
    use crate::universe::Universe;
    use pretty_assertions::assert_eq;

    #[test]
    #[should_panic(expected = "Zoom offset Point3 [2, 1, 1] out of bounds for 2")]
    fn construction_out_of_range_high() {
        Zoom::new(R2, Point3::new(2, 1, 1));
    }

    #[test]
    #[should_panic(expected = "Zoom offset Point3 [-1, 1, 1] out of bounds for 2")]
    fn construction_out_of_range_low() {
        Zoom::new(R2, Point3::new(-1, 1, 1));
    }

    #[test]
    fn evaluation() {
        let mut universe = Universe::new();
        let [original_block] = make_some_voxel_blocks(&mut universe);

        let ev_original = original_block.evaluate().unwrap();
        let zoom_resolution = ev_original.resolution().halve().unwrap();
        let original_voxels = &ev_original.voxels;

        // Try zoom at multiple offset steps.
        for x in 0i32..2 {
            dbg!(x);
            let zoomed = original_block.clone().with_modifier(Zoom::new(
                R2, // scale up by two = divide resolution by two
                Point3::new(x, 0, 0),
            ));
            let ev_zoomed = zoomed.evaluate().unwrap();
            assert_eq!(
                ev_zoomed,
                if x >= 2 {
                    // out of range
                    EvaluatedBlock::from_voxels(
                        ev_original.attributes.clone(),
                        Evoxels::One(Evoxel::from_color(Rgba::TRANSPARENT)),
                    )
                } else {
                    EvaluatedBlock::from_voxels(
                        ev_original.attributes.clone(),
                        Evoxels::Many(
                            zoom_resolution,
                            GridArray::from_fn(GridAab::for_block(zoom_resolution), |p| {
                                original_voxels[p + GridVector::new(
                                    GridCoordinate::from(zoom_resolution) * x,
                                    0,
                                    0,
                                )]
                            }),
                        ),
                    )
                }
            );
        }
    }

    #[test]
    fn atom_in_bounds() {
        let [original] = make_some_blocks();
        let mut zoomed = original.clone();
        zoomed.modifiers_mut().push(Modifier::Zoom(Zoom {
            scale: R2,
            offset: Point3::new(1, 0, 0),
        }));
        assert_eq!(zoomed.evaluate().unwrap().color, original.color());
    }
}

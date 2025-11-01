use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;

use all_is_cubes::block::{self, Block, Primitive, Resolution};
use all_is_cubes::euclid::Size3D;
use all_is_cubes::math::{GridAab, GridMatrix, GridPoint, GridSizeCoord, GridVector};
use all_is_cubes::space::{Space, SpaceTransaction};
use all_is_cubes::universe::StrongHandle;

use crate::vui::{self, Layoutable as _};

/// Widget that takes a [`Space`] and displays it shrunken, such as for a text label,
/// with no interactive behavior.
///
/// TODO: add a click response feature
#[derive(Clone, Debug)]
pub struct Voxels {
    // TODO(ecs): temporary kludge to fix bugs. This should not be a StrongHandle; instead,
    // widgets should be subject to VisitHandles, but there’s an upcoming big VUI refactor first.
    space: StrongHandle<Space>,
    region: GridAab,
    scale: Resolution,
    modifiers: Vec<block::Modifier>,
}

impl Voxels {
    /// Create [`Voxels`] that displays the specified `region` of the `space`.
    ///
    /// When the `region`'s dimensions are not multiples of `scale`, their alignment within the
    /// block grid will be determined by the layout gravity. Note that areas of `space` outside of
    /// `region` may be displayed in that case.
    pub fn new(
        region: GridAab,
        space: StrongHandle<Space>,
        scale: Resolution,
        modifiers: impl IntoIterator<Item = block::Modifier>,
    ) -> Self {
        // Design note: We could take `region` from `space` but that'd require locking it,
        // and the caller is very likely to already have that information.
        Self {
            space,
            region,
            scale,
            modifiers: modifiers.into_iter().collect(),
        }
    }
}

impl vui::Layoutable for Voxels {
    fn requirements(&self) -> vui::LayoutRequest {
        let scale = GridSizeCoord::from(self.scale);
        let size = self.region.size();

        vui::LayoutRequest {
            // Divide rounding up.
            // Note: Not using GridAab::divide because that would take into account the
            // absolute position, which we *don't* want.
            minimum: (size + Size3D::splat(scale - 1)) / scale,
        }
    }
}

impl vui::Widget for Voxels {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        // This is similar but not identical to block::space_to_blocks().

        let position = position.shrink_to(self.requirements().minimum, true);

        // Calculate where the voxels should land in the blocks, respecting layout gravity,
        // by using the shrink_to algorithm again.
        let grant_in_voxels = GridAab::from_lower_size(
            GridPoint::origin(),
            position.bounds.size() * GridSizeCoord::from(self.scale),
        );
        let gravity_offset_in_voxels: GridVector = vui::LayoutGrant {
            bounds: grant_in_voxels,
            gravity: position.gravity,
        }
        .shrink_to(self.region.size(), false)
        .bounds
        .lower_bounds()
        .to_vector();
        // gravity_offset_in_voxels is now the offset within the low-corner block at
        // which the low-corner voxels should start.

        let block_to_voxels_transform = {
            // Apply gravity and the translation that was requested
            GridMatrix::from_translation(
                self.region.lower_bounds().to_vector() - gravity_offset_in_voxels)
            // Scale up from blocks to voxels
            * GridMatrix::from_scale(self.scale.into())
            // Subtract the absolute position to get relative position
            * GridMatrix::from_translation(-position.bounds.lower_bounds().to_vector())
        };

        let mut txn = SpaceTransaction::default();
        for cube in position.bounds.interior_iter() {
            let mut block = Block::from_primitive(Primitive::Recur {
                offset: block_to_voxels_transform.transform_cube(cube).lower_bounds(),
                resolution: self.scale,
                space: self.space.clone().into(),
            });
            block.modifiers_mut().clone_from(&self.modifiers);
            txn.at(cube).overwrite(block);
        }
        super::OneshotController::new(txn)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vui::{Align, instantiate_widget};
    use all_is_cubes::block::Resolution::*;
    use all_is_cubes::euclid::Vector3D;
    use all_is_cubes::math::Cube;
    use all_is_cubes::universe::{ReadTicket, Universe};

    fn test_voxels_widget(
        voxel_space_bounds: GridAab,
        grant: vui::LayoutGrant,
    ) -> (Option<GridAab>, Space) {
        let mut universe = Universe::new();
        instantiate_widget(
            ReadTicket::stub(),
            grant,
            Voxels::new(
                voxel_space_bounds,
                StrongHandle::from(
                    universe.insert_anonymous(Space::builder(voxel_space_bounds).build()),
                ),
                R8,
                [],
            ),
        )
    }

    // TODO: not sure if this is actually something the widget system wants to support or not
    #[test]
    fn voxels_in_too_small_grant_succeeds() {
        let v_space_bounds = GridAab::from_lower_upper([0, 0, 0], [7, 8, 9]);
        let output_origin = Cube::new(100, 100, 100);
        let _output = test_voxels_widget(
            v_space_bounds,
            vui::LayoutGrant::new(GridAab::single_cube(output_origin)),
        );
        // TODO assertions about resulting blocks
    }

    #[test]
    fn voxels_alignment() {
        // Space bounds such that X is smaller, Y is exact, and Z is bigger than one block
        // TODO: test voxel space lower bounds being nonzero
        let v_space_bounds = GridAab::from_lower_upper([0, 0, 0], [7, 8, 9]);
        // (TODO: put something in v_space so we can visualize)
        // Arbitrary nonzero origin point
        let output_origin = GridPoint::new(100, 100, 100);
        let grant = vui::LayoutGrant {
            bounds: GridAab::from_lower_size(output_origin, [3, 3, 3]),
            gravity: Vector3D::new(Align::Low, Align::Center, Align::High),
        };
        let (output_bounds, output) = test_voxels_widget(v_space_bounds, grant);
        assert_eq!(
            output_bounds.unwrap(),
            GridAab::from_lower_size([100, 101, 101], [1, 1, 2])
        );
        // Expect two adjacent recursive blocks
        match *output[[100, 101, 101]].primitive() {
            Primitive::Recur {
                offset,
                resolution,
                space: _,
            } => {
                assert_eq!(resolution, R8);
                assert_eq!(offset, GridPoint::new(0, 0, -7));
            }
            ref p => panic!("unexpected primitive {p:?}"),
        }
        match *output[[100, 101, 102]].primitive() {
            Primitive::Recur {
                offset,
                resolution,
                space: _,
            } => {
                assert_eq!(resolution, R8);
                assert_eq!(offset, GridPoint::new(0, 0, 1));
            }
            ref p => panic!("unexpected primitive {p:?}"),
        }
    }
}

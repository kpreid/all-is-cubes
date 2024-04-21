//! Rerun-based visualization of the block mesh generation algorithm.
//! Intended to be used to validate that the intermediate steps are proceeding correctly,
//! and just to be cool to look at.

#![allow(clippy::unused_self)]

use all_is_cubes::block::Evoxels;
use all_is_cubes::math::{Face6, FreePoint, GridCoordinate, GridPoint};

#[cfg(feature = "rerun")]
use {
    all_is_cubes::block::Resolution,
    all_is_cubes::math::{Aab, GridAab},
    all_is_cubes::rerun_glue as rg,
    alloc::vec::Vec,
};

/// Optional handle to Rerun recording used by the block mesh generator
/// to visualize its algorithm steps.
///
/// Encapsulates all the stuff we want to *not* do if recording is disabled, and in particular,
/// compiles to nothing if the "rerun" feature is not enabled.
#[doc(hidden)]
#[allow(missing_debug_implementations)]
#[non_exhaustive]
pub enum Viz {
    Disabled,
    // If this variant is absent, then `Viz` is a zero-sized type.
    #[cfg(feature = "rerun")]
    Enabled(Inner),
}

/// Use [`Viz::new()`] instead of constructing this directly.
#[cfg(feature = "rerun")]
#[doc(hidden)]
#[allow(missing_debug_implementations)]
pub struct Inner {
    // Info captured from the `Evoxels` to give context to later data`
    resolution: Option<Resolution>,
    data_bounds: Option<GridAab>,

    destination: rg::Destination,
    window_path: rg::EntityPath,
    occupied_path: rg::EntityPath,
    layer_path: rg::EntityPath,
    mesh_edges_path: rg::EntityPath,

    // These two together make up the mesh display entity's data
    mesh_edge_positions: Vec<rg::components::LineStrip3D>,
    mesh_edge_classes: Vec<rg::components::ClassId>,
}

cfg_if::cfg_if! {
    if #[cfg(feature = "rerun")] {
        const BOUNDS_LINE_RADIUS: f32 = 0.01;
        const VOXEL_RADIUS: f32 = 0.1;
        const OCCUPIED_RADIUS: f32 = 0.01;
        const EDGE_LINE_RADIUS: f32 = 0.04;
    }
}

impl Viz {
    #[cfg(feature = "rerun")]
    pub fn new(destination: rg::Destination) -> Self {
        if !destination.is_enabled() {
            Self::disabled()
        } else {
            Self::Enabled(Inner {
                destination,
                window_path: rg::entity_path!["analysis", "window"],
                occupied_path: rg::entity_path!["analysis", "occupied"],
                layer_path: rg::entity_path!["compute", "layer"],
                mesh_edges_path: rg::entity_path!["compute", "mesh_edges"],
                resolution: None,
                data_bounds: None,
                mesh_edge_positions: Vec::new(),
                mesh_edge_classes: Vec::new(),
            })
        }
    }

    pub fn disabled() -> Self {
        Self::Disabled
    }

    pub(crate) fn voxels(&mut self, #[allow(unused)] voxels: &Evoxels) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.resolution = Some(voxels.resolution());
            state.data_bounds = Some(voxels.bounds());

            state.destination.log(
                &rg::entity_path!["block_bounds"],
                &rg::convert_grid_aabs([GridAab::for_block(voxels.resolution())])
                    .with_class_ids([rg::ClassId::MeshVizBlockBounds])
                    .with_radii([BOUNDS_LINE_RADIUS]),
            );
            state.destination.log(
                &rg::entity_path!["data_bounds"],
                &rg::convert_grid_aabs([voxels.bounds()])
                    .with_class_ids([rg::ClassId::MeshVizVoxelBounds])
                    .with_radii([BOUNDS_LINE_RADIUS]),
            );

            let voxel_vol = voxels.as_vol_ref();
            let voxel_iter = voxel_vol
                .iter()
                .filter(|&(_, voxel)| !voxel.color.fully_transparent());

            state.destination.log(
                &rg::entity_path!["voxels"],
                &rg::archetypes::Points3D::new(
                    voxel_iter
                        .clone()
                        .map(|(cube, _)| rg::convert_point(cube.midpoint())),
                )
                .with_colors(voxel_iter.map(|(_, voxel)| rg::components::Color(voxel.color.into())))
                .with_radii([VOXEL_RADIUS]),
            );
        }
    }

    pub(crate) fn window(&self, #[allow(unused)] center: GridPoint) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.destination.log(
                &state.window_path,
                // box big enough to enclose the relevant voxel points, but not so big it looks distractingly chunky
                &rg::convert_aabs(
                    [Aab::from_lower_upper(
                        FreePoint::splat(-0.75),
                        FreePoint::splat(0.75),
                    )],
                    center.to_f64().to_vector(),
                )
                .with_class_ids([rg::ClassId::MeshVizWipWindow]),
            );
        }
    }

    pub(crate) fn clear_window(&self) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.destination.clear_recursive(&state.window_path);
        }
    }

    pub(crate) fn clear_analysis(&self) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.destination.clear_recursive(&state.occupied_path);
        }
    }

    pub(crate) fn analysis_in_progress(
        &self,
        #[allow(unused)] analysis: &super::analyze::Analysis,
    ) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            let iter = Face6::ALL.into_iter().flat_map(|face| {
                analysis
                    .occupied_planes(face)
                    .map(move |layer| state.layer_box(face, layer))
            });
            state.destination.log(
                &state.occupied_path,
                &rg::convert_grid_aabs(iter)
                    .with_class_ids([rg::ClassId::MeshVizOccupiedPlane])
                    .with_radii([OCCUPIED_RADIUS]),
            );
        }
    }

    pub(crate) fn clear_layer_in_progress(&self) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.destination.clear_recursive(&state.layer_path);
        }
    }

    pub(crate) fn set_layer_in_progress(
        &self,
        #[allow(unused)] face: Face6,
        #[allow(unused)] layer: GridCoordinate,
    ) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.destination.log(
                &state.layer_path,
                &rg::convert_grid_aabs([state.layer_box(face, layer)])
                    .with_class_ids([rg::ClassId::MeshVizWipPlane])
                    .with_radii([OCCUPIED_RADIUS * 2.0]),
            )
        }
    }

    pub(crate) fn extend_vertices<I: Iterator<Item = FreePoint>>(
        &mut self,
        #[allow(unused)] mut vertex_position_iter: I,
        #[allow(unused)] normal: Face6,
    ) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            let [a, b, c, d] = std::array::from_fn(|_| {
                rg::convert_vec(vertex_position_iter.next().unwrap().to_vector())
            });

            // Fake the triangles by drawing 5 lines as 1 line strip.
            // We'll have to change that when we for-real use arbitrary triangles.
            state
                .mesh_edge_positions
                .push(rg::components::LineStrip3D(vec![a, b, d, c, a, d]));
            state.mesh_edge_classes.push(
                match normal {
                    Face6::NX => rg::ClassId::MeshVizEdgeNx,
                    Face6::NY => rg::ClassId::MeshVizEdgeNy,
                    Face6::NZ => rg::ClassId::MeshVizEdgeNz,
                    Face6::PX => rg::ClassId::MeshVizEdgePx,
                    Face6::PY => rg::ClassId::MeshVizEdgePy,
                    Face6::PZ => rg::ClassId::MeshVizEdgePz,
                }
                .into(),
            );

            // Log the *entire* mesh so far.
            // (Rerun doesn't have a way to say "add new instances".)
            state.destination.log(
                &state.mesh_edges_path,
                &rg::archetypes::LineStrips3D::new(state.mesh_edge_positions.iter().cloned())
                    .with_class_ids(state.mesh_edge_classes.iter().copied())
                    .with_radii([EDGE_LINE_RADIUS]),
            )
        }
    }
}

#[cfg(feature = "rerun")]
impl Inner {
    fn layer_box(&self, face: Face6, layer: GridCoordinate) -> GridAab {
        let layer_box = GridAab::for_block(self.resolution.unwrap())
            .abut(face, -layer)
            .unwrap()
            .abut(face.opposite(), 0)
            .unwrap();

        // trim to data bounds for a tidier visualization
        layer_box
            .intersection_box(self.data_bounds.unwrap())
            .unwrap_or(layer_box)
    }
}

//! Rerun-based visualization of the block mesh generation algorithm.
//! Intended to be used to validate that the intermediate steps are proceeding correctly,
//! and just to be cool to look at.

#![cfg_attr(not(feature = "rerun"), expect(clippy::unused_self))]

use alloc::collections::VecDeque;

use all_is_cubes::block::{Evoxel, Evoxels};
use all_is_cubes::math::{Face6, GridCoordinate, GridPoint, Rgba, Vol};

use crate::Position;
use crate::block_mesh::analyze::Analysis;
use crate::block_mesh::planar_new::FrontierVertex;

#[cfg(feature = "rerun")]
use {
    crate::block_mesh::analyze::AnalysisVertex,
    all_is_cubes::block::Resolution,
    all_is_cubes::euclid::Vector3D,
    all_is_cubes::math::{Cube, GridAab, GridVector, Octant},
    all_is_cubes::rerun_glue as rg,
    alloc::{vec, vec::Vec},
    core::iter,
    core::mem,
    itertools::Itertools as _,
};

/// Optional handle to Rerun recording used by the block mesh generator
/// to visualize its algorithm steps.
///
/// Encapsulates all the stuff we want to *not* do if recording is disabled, and in particular,
/// compiles to nothing if the "rerun" feature is not enabled.
#[allow(clippy::large_enum_variant)]
#[non_exhaustive]
#[cfg_attr(
    feature = "_special_testing",
    visibility::make(pub),
    expect(missing_docs, missing_debug_implementations)
)]
pub(crate) enum Viz {
    Disabled,
    // If this variant is absent, then `Viz` is a zero-sized type.
    #[cfg(feature = "rerun")]
    #[cfg_attr(not(feature = "_special_testing"), allow(dead_code))]
    Enabled(Inner),
}

/// Use [`Viz::new()`] instead of constructing this directly.
#[cfg(feature = "rerun")]
#[doc(hidden)]
#[allow(missing_debug_implementations, unnameable_types)]
pub struct Inner {
    /// Rerun log time counter
    timestamp: i64,

    // Info captured from the `Evoxels` to give context to later data
    resolution: Option<Resolution>,
    data_bounds: Option<GridAab>,

    analysis: Analysis,

    // Constants for logging to Rerun.
    destination: rg::Destination,
    timeline_name: rg::TimelineName,
    window_voxels_path: rg::EntityPath,
    layer_path: rg::EntityPath,
    occupied_path: rg::EntityPath,
    mesh_surface_path: rg::EntityPath,
    mesh_edges_path: rg::EntityPath,
    analysis_vertices_path: rg::EntityPath,
    analysis_transparent_box_path: rg::EntityPath,
    triangulator_old_frontier_points_path: rg::EntityPath,
    triangulator_new_frontier_points_path: rg::EntityPath,
    triangulator_current_vertex_path: rg::EntityPath,

    // These two together make up the mesh edge display entity's data
    mesh_edge_positions: Vec<rg::components::LineStrip3D>,
    mesh_edge_classes: Vec<rg::components::ClassId>,

    // Ingredients for a Mesh3D
    mesh_vertex_positions: Vec<rg::components::Position3D>,
    mesh_vertex_colors: Vec<rg::components::Color>,
    mesh_vertex_normals: Vec<rg::components::Vector3D>,
    mesh_triangle_indices: Vec<rg::components::TriangleIndices>,
}

cfg_if::cfg_if! {
    if #[cfg(feature = "rerun")] {
        const BOUNDS_LINE_RADIUS: f32 = 0.01;
        const VOXEL_RADIUS: f32 = 0.1;
        const OCCUPIED_RADIUS: f32 = 0.01;
        const EDGE_LINE_RADIUS: f32 = 0.04;
        // should be larger than EDGE_LINE_RADIUS and OCCUPIED_RADIUS
        const IN_PROGRESS_LINE_RADIUS: f32 = 0.07;
    }
}

impl Viz {
    /// Creates an [`Viz`] that writes what it is given to the given Rerun stream.
    #[cfg(feature = "rerun")]
    #[cfg_attr(not(feature = "_special_testing"), expect(dead_code))]
    pub fn new(destination: rg::Destination) -> Self {
        if !destination.is_enabled() {
            Self::disabled()
        } else {
            let mut new_self = Self::Enabled(Inner {
                timestamp: -1,
                destination,
                timeline_name: rg::TimelineName::new("block_mesh_step"),
                window_voxels_path: rg::entity_path!["progress", "analysis_window"],
                layer_path: rg::entity_path!["progress", "mesh_plane"],
                occupied_path: rg::entity_path!["occupied_planes"],
                mesh_surface_path: rg::entity_path!["mesh", "surface"],
                mesh_edges_path: rg::entity_path!["mesh", "edges"],
                analysis_vertices_path: rg::entity_path!["analysis", "vertices"],
                analysis_transparent_box_path: rg::entity_path!["analysis", "transparent_box"],
                triangulator_old_frontier_points_path: rg::entity_path![
                    "triangulate",
                    "old_frontier"
                ],
                triangulator_new_frontier_points_path: rg::entity_path![
                    "triangulate",
                    "new_frontier"
                ],
                triangulator_current_vertex_path: rg::entity_path!["progress", "vertex"],
                resolution: None,
                data_bounds: None,
                analysis: Analysis::EMPTY,
                mesh_edge_positions: Vec::new(),
                mesh_edge_classes: Vec::new(),
                mesh_vertex_positions: Vec::new(),
                mesh_vertex_colors: Vec::new(),
                mesh_vertex_normals: Vec::new(),
                mesh_triangle_indices: Vec::new(),
            });
            new_self.completed_step(); // perform initial set_time_sequence()
            new_self
        }
    }

    /// Creates an [`Viz`] that discards all its input.
    pub fn disabled() -> Self {
        Self::Disabled
    }

    /// Declare that all visualization data for a particular step of the algorithm has been
    /// completed.
    ///
    /// This advances the custom Rerun timeline.
    pub(crate) fn completed_step(&mut self) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.timestamp += 1;
            state.destination.stream.set_time_sequence(state.timeline_name, state.timestamp);
        }
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
                .map(|(cube, &voxel)| (cube, voxel))
                .filter(|&(_, voxel)| !voxel.color.fully_transparent());

            state.log_voxels(&rg::entity_path!("voxels"), voxel_iter, VOXEL_RADIUS);
        }
    }

    pub(crate) fn window(
        &self,
        #[allow(unused)] center: GridPoint,
        #[allow(unused)] voxels: Vol<&[Evoxel]>,
    ) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            let window_grid_aab = GridAab::from_lower_upper(
                center - GridVector::splat(1),
                center + GridVector::splat(1),
            );

            // Log the voxels the window is looking at, with bigger radius to highlight them.
            let voxel_iter = window_grid_aab
                .interior_iter()
                .map(|cube| (cube, voxels.get(cube).copied().unwrap_or(Evoxel::AIR)));
            state.log_voxels(&state.window_voxels_path, voxel_iter, 0.5);
        }
    }

    pub(crate) fn clear_window(&self) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.destination.clear_recursive(&state.window_voxels_path);
        }
    }

    pub(crate) fn analysis_in_progress(&mut self, #[allow(unused)] analysis: &Analysis) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            // TODO: Compare what parts of the analysis have changed to deduplicate updates
            state.update_and_log_analysis(analysis);
        }
    }

    pub(crate) fn clear_in_progress_markers(&self) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            let d = &state.destination;
            d.clear_recursive(&state.layer_path);
            d.clear_recursive(&state.triangulator_current_vertex_path);
            d.clear_recursive(&state.triangulator_old_frontier_points_path);
            d.clear_recursive(&state.triangulator_new_frontier_points_path);
        }
    }

    pub(crate) fn set_layer_in_progress(
        &mut self,
        #[allow(unused)] face: Face6,
        #[allow(unused)] layer: GridCoordinate,
    ) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.destination.log(
                &state.layer_path,
                &rg::convert_grid_aabs([state.layer_box(face, layer)])
                    .with_class_ids([rg::ClassId::MeshVizWipPlane])
                    .with_radii([IN_PROGRESS_LINE_RADIUS]),
            );

            // Delete the corresponding analysis rect, to indicate that we're (about to be)
            // entirely done with it.
            state.analysis.delete_occupied_plane(face, layer);
            state.log_occupied_planes();
        }
    }

    pub(crate) fn set_frontier(
        &mut self,
        #[allow(unused)] old_frontier: &VecDeque<FrontierVertex>,
        #[allow(unused)] new_frontier: &VecDeque<FrontierVertex>,
    ) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.destination.log(
                &state.triangulator_old_frontier_points_path,
                &convert_frontier_vertices(
                    &mut old_frontier.iter(),
                    rg::components::Color::from_rgb(100, 0, 0),
                ),
            );
            state.destination.log(
                &state.triangulator_new_frontier_points_path,
                &convert_frontier_vertices(
                    &mut new_frontier.iter(),
                    rg::components::Color::from_rgb(255, 100, 100),
                ),
            );
        }
    }

    pub(crate) fn set_current_triangulation_vertex(
        &mut self,
        #[allow(unused)] vertex: &FrontierVertex,
    ) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            state.destination.log(
                &state.triangulator_current_vertex_path,
                &convert_frontier_vertices(
                    &mut [vertex].into_iter(),
                    rg::components::Color::from_rgb(255, 255, 100),
                ),
            );
        }
    }

    /// Add more triangles to the final output mesh
    pub(crate) fn extend_vertices(
        &mut self,
        #[allow(unused)] mut vertex_position_iter: impl Iterator<Item = Position> + Clone,
        #[allow(unused)] mut relative_indices_iter: impl Iterator<Item = u32> + Clone,
        #[allow(unused)] color_fn: impl FnOnce() -> Rgba,
        #[allow(unused)] normal: Face6,
    ) {
        #[cfg(feature = "rerun")]
        if let Self::Enabled(state) = self {
            let vertex_positions = vertex_position_iter.map(rg::convert_point).collect_vec();

            // Append vertices for real mesh
            let index_base = state.mesh_vertex_positions.len() as u32;
            state.mesh_vertex_positions.extend(vertex_positions.iter().copied());
            state.mesh_vertex_colors.extend(iter::repeat_n(
                rg::components::Color(color_fn().into()),
                vertex_positions.len(),
            ));
            state.mesh_vertex_normals.extend(iter::repeat_n(
                rg::components::Vector3D(rg::convert_vec(normal.normal_vector::<f32, ()>())),
                vertex_positions.len(),
            ));
            state.mesh_triangle_indices.extend(
                relative_indices_iter
                    .clone()
                    .map(|rel_index| rel_index + index_base)
                    .tuples()
                    .map(|(i1, i2, i3)| {
                        rg::components::TriangleIndices(rg::datatypes::UVec3D::new(i1, i2, i3))
                    }),
            );

            // Draw edges of each triangle — by interpreting the indices
            for (i1, i2, i3) in relative_indices_iter.tuples() {
                let p1 = vertex_positions[i1 as usize];
                let p2 = vertex_positions[i2 as usize];
                let p3 = vertex_positions[i3 as usize];
                state
                    .mesh_edge_positions
                    .push(rg::components::LineStrip3D(vec![p1.0, p2.0, p3.0, p1.0]));
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
            }

            // Rerun doesn't have a way to say "add new instances", so we have to re-log the
            // entire mesh so far.

            // Log edges.
            state.destination.log(
                &state.mesh_edges_path,
                &rg::archetypes::LineStrips3D::new(state.mesh_edge_positions.iter().cloned())
                    .with_class_ids(state.mesh_edge_classes.iter().copied())
                    .with_radii([EDGE_LINE_RADIUS]),
            );

            // Log mesh triangles. (Do this second so that they appear inside the edges.)
            state.destination.log(
                &state.mesh_surface_path,
                &rg::archetypes::Mesh3D::new(state.mesh_vertex_positions.iter().copied())
                    .with_vertex_colors(state.mesh_vertex_colors.iter().copied())
                    .with_vertex_normals(state.mesh_vertex_normals.iter().copied())
                    .with_triangle_indices(state.mesh_triangle_indices.iter().copied()),
            );
        }
    }
}

#[cfg(feature = "rerun")]
impl Inner {
    fn layer_box(&self, face: Face6, layer: GridCoordinate) -> GridAab {
        self.analysis.occupied_plane_box(face, layer).unwrap()
    }

    /// Logs the current contents of the [`Analysis`], replacing any prior data.
    fn update_and_log_analysis(&mut self, new_analysis: &Analysis) {
        // We compare current to previous data and log it only if it has changed.
        // This is mainly so that the resulting Rerun dataset is not huge and the
        // timeline is more useful.

        if *new_analysis == self.analysis {
            return;
        }
        let old_analysis = mem::replace(&mut self.analysis, new_analysis.clone());

        if self.analysis.occupied_planes != old_analysis.occupied_planes {
            self.log_occupied_planes();
        }

        if let Some(bbox) = self.analysis.transparent_bounding_box
            && Some(bbox) != old_analysis.transparent_bounding_box
        {
            self.destination.log(
                &self.analysis_transparent_box_path,
                &rg::convert_aabs([bbox.to_free()], Vector3D::zero())
                    .with_class_ids([rg::ClassId::MeshVizTransparentBounds]),
            );
        }

        if self.analysis.vertices != old_analysis.vertices {
            let entity_path = &self.analysis_vertices_path;
            self.destination.log(
                entity_path,
                &convert_vertices(&self.analysis.vertices, 0.18),
            )
        }
    }

    /// Logs the current `self.analysis.occupied_planes`.
    /// Used from two different phases.
    fn log_occupied_planes(&self) {
        let iter = Face6::ALL.into_iter().flat_map(|face| {
            self.analysis
                .occupied_planes(face)
                .map(move |(layer, _)| self.layer_box(face, layer))
        });
        self.destination.log(
            &self.occupied_path,
            &rg::convert_grid_aabs(iter)
                .with_class_ids([rg::ClassId::MeshVizOccupiedPlane])
                .with_radii([OCCUPIED_RADIUS]),
        );
    }

    fn log_voxels(
        &self,
        path: &rg::EntityPath,
        voxel_iter: impl Iterator<Item = (Cube, Evoxel)> + Clone,
        radius: f32,
    ) {
        self.destination.log(
            path,
            &rg::archetypes::Points3D::new(
                voxel_iter.clone().map(|(cube, _)| rg::convert_point(cube.center())),
            )
            .with_colors(voxel_iter.map(|(_, voxel)| rg::components::Color(voxel.color.into())))
            .with_radii([radius]),
        );
    }
}

#[cfg(feature = "rerun")]
fn convert_vertices(vertices: &[AnalysisVertex], box_half_size: f32) -> rg::archetypes::Boxes3D {
    // We use `Boxes3D` that are offset mostly into each octant, so that we can
    // view which interior volumes the vertex is indicating as filled.

    let opaque_color = rg::components::Color::from_rgb(80, 80, 255);
    let transparent_color = rg::components::Color::from_rgb(255, 255, 80);
    let offset = Vector3D::splat(box_half_size - 0.01);

    // Iterator over all the boxes that should exist, which we clone and reuse to produce the
    // parallel vectors for Rerun.
    let which_boxes = vertices.iter().cartesian_product(Octant::ALL).filter_map(
        move |(vertex, octant): (&AnalysisVertex, Octant)| {
            vertex.renderable.get(octant).then_some((vertex, octant))
        },
    );

    rg::archetypes::Boxes3D::from_centers_and_half_sizes(
        which_boxes.clone().map(|(vertex, octant)| {
            rg::components::PoseTranslation3D(rg::convert_vec(
                vertex.position.to_vector().to_f32() + octant.reflect(offset),
            ))
        }),
        [rg::components::HalfSize3D::splat(box_half_size)],
    )
    .with_colors(which_boxes.clone().map(|(vertex, octant)| {
        if vertex.opaque.get(octant) {
            opaque_color
        } else {
            transparent_color
        }
    }))
    .with_fill_mode(rg::components::FillMode::Solid)
}

#[cfg(feature = "rerun")]
fn convert_frontier_vertices(
    vertices: &mut dyn ExactSizeIterator<Item = &FrontierVertex>,
    color: rg::components::Color,
) -> rg::archetypes::Ellipsoids3D {
    #[allow(
        clippy::len_zero,
        reason = "false positive https://github.com/rust-lang/rust-clippy/issues/15890"
    )]
    let empty = vertices.len() == 0;
    // Frontier vertices are always copies (ish) of the original analysis vertices, so don't
    // try to visualize their octants; just add markers for them
    let radius = 0.45; // needs to be bigger than the solid octant boxes from convert_vertices()
    rg::archetypes::Ellipsoids3D::from_centers_and_radii(
        vertices.map(|fv| rg::convert_vec(fv.v.position.to_vector())),
        if empty {
            None // avoids a quirk where we get one placed at the origin
        } else {
            Some(radius)
        },
    )
    .with_colors([color])
    .with_fill_mode(rg::components::FillMode::MajorWireframe)
}

use alloc::vec::Vec;
use core::fmt;
use core::iter;

use hashbrown::HashMap;
use itertools::Itertools;

use all_is_cubes::block::{self, Evoxel, Resolution};
use all_is_cubes::euclid::{Box2D, Point2D, Point3D, point3, vec3};
use all_is_cubes::math::{
    Axis, Cube, Face, FaceMap, GridAab, GridCoordinate, GridPoint, Octant, OctantMap, OctantMask,
    OpacityCategory, Vol, ZMaj,
};

use crate::OutOfMemory;
use crate::TransparencyFormat;
use crate::block_mesh::viz::Viz;

#[cfg(doc)]
use crate::BlockMesh;

// -------------------------------------------------------------------------------------------------

/// Coordinate system marker type for [`Analysis::occupied_planes`] rectangles as stored, not as
/// exposed outside.
///
/// In this 2D coordinate system, 1 unit = 1 voxel edge, the origin is the lower corner of the
/// block, and the axes are the 3D axes with the axis perpendicular to the plane deleted.
/// That is, the `xy` of these coordinates are the `xy`, `xz`, or `yz` of the original voxel
/// coordinates.
pub(crate) struct OccupiedInternal;

/// Maximum number of occupied planes/layers in any block.
///
/// The plus one is because we're counting fenceposts (coordinate planes) — wait.
/// TODO: We should never have anything in the deepest plane. Reduce this and test that's ok.
const MAX_PLANES: usize = Resolution::MAX.to_grid() as usize + 1;

type Rect = Box2D<GridCoordinate, Cube>;

/// Box which bounds the voxel surface that needs triangles generated for it,
/// within one particular plane intersecting the block being analyzed.
/// These boxes are found in [`Analysis::occupied_planes`] and are not exposed outside this module
/// except for [`Viz`]’s purposes.
///
/// This is a `struct` rather than a `type` so that it can have a higher alignment, to allow
/// the machine code using it to be more efficient by treating the whole box as an aligned value.
#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(align(4))]
pub(crate) struct PlaneBox(pub Box2D<u8, OccupiedInternal>);

/// A maximally inside-out box, used as the `occupied_planes` value containing no voxels.
/// When it meets any actual voxel it will get replaced implicitly by the min/max operations.
const EMPTY_PLANE_BOX: PlaneBox = PlaneBox(Box2D {
    min: Point2D::new(u8::MAX, u8::MAX),
    max: Point2D::new(u8::MIN, u8::MIN),
});

/// The boundary of a voxel shape.
///
/// Analysis consists of iterating over all of the provided voxel data, and identifying positions
/// that are vertices of the boundary polyhedron(s), as well as some other supporting information.
/// This allows further work, such as mesh building, to be done at a cost which scales with the
/// complexity of the boundary rather than the number of voxels.
///
/// This data type is used by [`BlockMesh`] as part of its process for building meshes.
/// However, that is currently limited to an implementation detail; it is not possible to provide
/// [`Analysis`] to or obtain it from the [`BlockMesh`] computation.
/// That may be rectified in future versions.
//
// TODO: Consider whether this should be renamed to `Shape` or another less abstract name.
#[derive(Clone, Debug, PartialEq)]
pub struct Analysis {
    /// For each face normal, which depths will need any triangles generated,
    /// and for those that do, which bounds need to be scanned.
    ///
    /// Index 0 is depth 0 (the surface of the block volume), index 1 is one voxel
    /// deeper, and so on.
    ///
    /// If a given plane contains no voxels, its value will be [`EMPTY_PLANE_BOX`].
    pub(crate) occupied_planes: FaceMap<[PlaneBox; MAX_PLANES]>,

    /// If there are any transparent voxels, and `transparency_format` is
    /// [`TransparencyFormat::BoundingBox`], this is their bounding box.
    pub(crate) transparent_bounding_box: Option<GridAab>,

    resolution: Resolution,

    /// Whether there are any adjacent visible voxels that have different colors, and therefore
    /// probably need a texture rather than vertex colors.
    ///
    /// Note: Currently, this is strictly an overestimate in that it will be true even for,
    /// say, diagonally adjacent voxels. However, this simplifies the job of the mesh builder,
    /// which can always use one mesh vertex per [`AnalysisVertex`] per face, rather than needing
    /// to split vertices by color.
    pub(crate) needs_texture: bool,

    vertices: Vec<AnalysisVertex>,
}

/// An abstract (not textured, not face-specific) vertex of a voxel shape.
///
/// Obtain this from [`Analysis::vertices()`].
#[derive(Clone, Copy, Eq, PartialEq)]
#[non_exhaustive]
// repr and field order chosen by benchmarking (on Apple M4 Max CPU) to get +2% for less complex
// blocks, compared to not increasing the alignment.
#[repr(C, align(8))]
pub struct AnalysisVertex {
    /// Bitmask of which volumes adjacent to this vertex are occupied by a visible material.
    pub renderable: OctantMask,

    /// Bitmask of which volumes adjacent to this vertex are occupied by a fully opaque material.
    ///
    /// `opaque` is always a subset of `renderable`.
    pub opaque: OctantMask,

    /// Position of this vertex.
    pub position: Point3D<u8, Cube>,
}

// -------------------------------------------------------------------------------------------------

impl Analysis {
    /// [`Analysis`] value which corresponds to a block with no visible voxels.
    //---
    // This is not public because we may want to avoid promising that this is const-constructible.
    // It is available in non-const public form as `impl Default`.
    // If we ever decide to allow reuse of `Analysis` for multiple blocks, we should revisit
    // offering this.
    pub(crate) const EMPTY: Self = {
        Analysis {
            occupied_planes: FaceMap::splat_copy([EMPTY_PLANE_BOX; MAX_PLANES]),
            transparent_bounding_box: None,
            needs_texture: false,
            resolution: Resolution::R1,
            vertices: Vec::new(),
        }
    };

    /// Analyze the given block.
    ///
    /// This function’s [time complexity] is O(<var>M</var> + <var>N</var>),
    /// where <var>M</var> is the number of input voxels and <var>N</var> is the number of vertices
    /// in the output.
    ///
    /// # Errors
    ///
    /// Returns an error if memory allocation fails during the analysis.
    /// (That is, this function will never result in the allocation error handler being called,
    /// and thus will not panic or abort the process for that reason.)
    ///
    /// [time complexity]: https://en.wikipedia.org/wiki/Time_complexity
    //---
    // Note: This function is only for public use; internally we short-circuit some cases to
    // avoid building the analysis at all.
    pub fn analyze(
        block: &block::EvaluatedBlock,
        options: &crate::MeshOptions,
    ) -> Result<Self, OutOfMemory> {
        analyze(
            block.resolution(),
            block.voxels().as_vol_ref(),
            options.transparency_format(),
            &mut Viz::Disabled,
        )
    }

    /// Returns the resolution of the analyzed block.
    ///
    /// This is the inclusive upper bound on the coordinates found in
    /// [`vertices()`][Self::vertices].
    pub fn resolution(&self) -> Resolution {
        self.resolution
    }

    /// Returns the vertices of the shape.
    ///
    /// In the mathematical sense, these are the [vertices] of the shape considered as an
    /// [orthogonal polyhedron], which occur at every point where two or more edges end
    /// *or intersect.*
    /// In the computer graphics sense, these vertices include the positions of every [`Vertex`] of
    /// a mesh built from this analysis. However, if the mesh vertices have normals (as triangle
    /// meshes built by this library do), the mesh will necessarily split these vertices into
    /// vertices with the same position but different normals.
    ///
    /// The analysis contains no information about edges; edges must be derived from
    /// the connectivity information included in these vertices.
    ///
    /// The vertices are ordered such that for any edge, its vertices will be encountered in
    /// increasing order of their coordinates; e.g. `[4, 0, 4]` will always appear before
    /// `[4, 2, 4]`.
    ///
    // Non-public guarantee: More specifically, the vertices are sorted in [`ZMaj`] order.
    ///
    /// [vertices]: https://en.wikipedia.org/wiki/Vertex_(geometry)
    /// [orthogonal polyhedron]: https://en.wikipedia.org/wiki/Orthogonal_polyhedron
    /// [`Vertex`]: crate::Vertex
    #[inline]
    pub fn vertices(&self) -> &[AnalysisVertex] {
        &self.vertices
    }

    /// For each face normal, which depths will need any triangles generated.
    /// Index 0 is depth 0 (the surface of the block volume), index 1 is one voxel
    /// deeper, and so on.
    pub(crate) fn occupied_planes(
        &self,
        face: Face,
    ) -> impl Iterator<Item = (GridCoordinate, Rect)> + '_ {
        iter::zip(
            0..GridCoordinate::from(self.resolution),
            self.occupied_planes[face],
        )
        .filter_map(move |(i, pbox)| {
            if pbox.0.is_empty() {
                None
            } else {
                Some((i, self.pbox_to_rect(face, pbox)))
            }
        })
    }

    /// Compute the geometric edges of this shape.
    /// The returned numbers are indices into [`Analysis::vertices()`].
    ///
    /// Note that these are not the edges of a triangle mesh of the shape, but only the edges of the
    /// shape itself.
    /// In particular, the result contains no diagonal lines.
    ///
    /// This operation’s overall [time complexity] is O(<var>N</var>), where <var>N</var> is the
    /// number of vertices; each step of the iterator is amortized O(1).
    /// It allocates temporary memory which is released when the iterator is dropped.
    ///
    /// [time complexity]: https://en.wikipedia.org/wiki/Time_complexity
    //---
    // TODO: make index type be u32 just like other mesh functions? or don’t bother?
    pub fn compute_edges(&self) -> impl Iterator<Item = [usize; 2]> {
        // Maps a lookup key for the lower vertex of the edge to the index of that vertex.
        //
        // Each lookup key is made by forgetting where along its line the edge started.
        let mut incomplete_edges: HashMap<(Axis, Point2D<u8, Cube>), usize> =
            HashMap::with_capacity(if self.vertices().is_empty() { 0 } else { 3 });

        self.vertices().iter().enumerate().flat_map(move |(vertex_index, vertex)| {
            Axis::ALL
                .map(|axis| {
                    let key = (
                        axis,
                        match axis {
                            Axis::X => vertex.position.yz(),
                            Axis::Y => vertex.position.xz(),
                            Axis::Z => vertex.position.xy(),
                        },
                    );

                    let has_positive_edge =
                        has_edge_in_direction(vertex.renderable, axis.positive_face());
                    let has_negative_edge =
                        has_edge_in_direction(vertex.renderable, axis.negative_face());

                    match (has_negative_edge, has_positive_edge) {
                        // At this point, an edge begins and extends in some positive direction.
                        (false, true) => {
                            incomplete_edges.insert(key, vertex_index);
                            None
                        }

                        // At this point, a previously encountered edge ends.
                        (true, false) => {
                            let Some(starting_vertex_index) = incomplete_edges.remove(&key) else {
                                unreachable!();
                            };
                            Some([starting_vertex_index, vertex_index])
                        }

                        // Either an edge continues or there is no edge; do nothing.
                        (true, true) | (false, false) => None,
                    }
                })
                .into_iter()
                .flatten()
        })
    }

    #[cfg(feature = "rerun")]
    #[doc(hidden)] // not yet clear whether we want to expose the layer coordinate system
    pub fn occupied_plane_box(&self, face: Face, layer: GridCoordinate) -> Option<GridAab> {
        let pbox = self.occupied_planes[face][usize::try_from(layer).unwrap()];
        if pbox.0.is_empty() {
            return None;
        }
        let mc = if face.is_negative() {
            layer
        } else {
            GridCoordinate::from(self.resolution) - layer
        };
        Some(GridAab::from_lower_upper(
            unflatten(face.axis(), mc, pbox.0.min),
            unflatten(face.axis(), mc, pbox.0.max),
        ))
    }

    pub(crate) fn surface_is_occupied(&self, face: Face) -> bool {
        !self.occupied_planes[face][0].0.is_empty()
    }

    fn pbox_to_rect(&self, face: Face, pbox: PlaneBox) -> Rect {
        let t = face.face_transform(self.resolution.into()).inverse();
        Rect::from_points([
            t.transform_point(unflatten(face.axis(), 0, pbox.0.min)).to_2d(),
            t.transform_point(unflatten(face.axis(), 0, pbox.0.max)).to_2d(),
        ])
    }

    /// Expand one of the rectangles in [`Analysis::occupied_planes`] with a new point.
    ///
    /// Performance note: making the input coordinates `u8` was slower. This is odd, but it’s why
    /// we aren't being consistent with `PlaneBox`.
    #[inline(always)] // we want this specialized for each case
    fn expand_rect(&mut self, face: Face, layer: u8, center: Point2D<u8, Cube>) {
        // We didn't check *exactly* which voxels in the window need pixels, but this is
        // exactly compensated for by the fact that we'll always have a window spilling over
        // the edge, so the range of window-center-points is equal to the range we need to
        // consider meshing.
        let existing_box = &mut self.occupied_planes[face][layer as usize].0;

        let center = center.cast_unit::<OccupiedInternal>().cast::<u8>();

        // Can't use union() because that special cases zero-sized boxes in a way we don't want.
        *existing_box = Box2D::new(existing_box.min.min(center), existing_box.max.max(center))
    }

    #[cfg(feature = "rerun")]
    pub(crate) fn delete_occupied_plane(&mut self, face: Face, layer: GridCoordinate) {
        self.occupied_planes[face][layer as usize] = EMPTY_PLANE_BOX;
    }
}

// -------------------------------------------------------------------------------------------------

/// Analyze a block's voxel array and find characteristics its mesh will have.
///
/// This is a preliminary step before creating the actual vertices and texture of a `BlockMesh`.
///
/// TODO: consider taking an `&mut Analysis` value to reuse allocation and reduce stack size.
pub(crate) fn analyze(
    resolution: Resolution,
    voxels: Vol<&[Evoxel]>,
    transparency: TransparencyFormat,
    viz: &mut Viz,
) -> Result<Analysis, OutOfMemory> {
    let mut analysis = Analysis::EMPTY;
    analysis.resolution = resolution;
    viz.analysis_in_progress(&analysis);

    // TODO: Have EvaluatedBlock tell us when a block is fully cubical and opaque,
    // and then avoid scanning the interior volume. EvaluatedBlock.opaque
    // is not quite that because it is defined to allow concavities.

    for_each_window(voxels, |(center, window_voxels)| {
        viz.window(center, voxels);
        analyze_one_window(&mut analysis, center, window_voxels, transparency)?;
        viz.analysis_in_progress(&analysis);
        viz.completed_step();
        Ok(())
    })?;
    viz.clear_window();
    viz.completed_step();

    Ok(analysis)
}

/// Take one of the outputs of [`windows()`] and compute its contribution to [`analysis`].
#[inline]
fn analyze_one_window(
    analysis: &mut Analysis,
    center: Point3D<u8, Cube>,
    window: OctantMap<&Evoxel>,
    transparency: TransparencyFormat,
) -> Result<(), OutOfMemory> {
    use Face::*;
    const ALL: OctantMask = OctantMask::ALL;
    const NONE: OctantMask = OctantMask::NONE;

    let opaque = window.to_mask(|voxel| voxel.opacity_category() == OpacityCategory::Opaque);
    let semitransparent = match transparency {
        TransparencyFormat::Surfaces => {
            window.to_mask(|voxel| voxel.opacity_category() == OpacityCategory::Partial)
        }
        TransparencyFormat::BoundingBox => {
            if window[Octant::Ppp].opacity_category() == OpacityCategory::Partial {
                let cube = Cube::from(center.map(GridCoordinate::from));
                match &mut analysis.transparent_bounding_box {
                    Some(v) => {
                        *v = {
                            // If there is more than one voxel involved, assume we need a texture.
                            // We have to check this separately from the regular needs_texture check
                            // below because in this mode we disable having it look at transparent
                            // voxels.
                            // In principle we should check color uniformity too,
                            // but making use of that will also require extra support.
                            analysis.needs_texture = true;

                            v.union_cube(cube)
                        }
                    }
                    v @ None => *v = Some(cube.grid_aab()),
                }
            }

            // If using bounding box format, disregard transparent voxel surfaces.
            NONE
        }
    };
    let renderable = opaque | semitransparent;

    // First, quickly check if there are any visible surfaces at all here.
    if opaque != NONE && opaque != ALL || semitransparent != NONE && semitransparent != ALL {
        let resolution_coord = u8::from(analysis.resolution);

        // Note: this comparison has false positives, in that it will return true for some
        // cases that *could* be meshed without any textures. However, some of these false
        // positives — in particular, those for voxels that share an edge, i.e. are diagonally
        // adjacent from a 2D perspective — simplify the job of the mesh builder.
        // See comments on the `needs_texture` field.
        analysis.needs_texture = analysis.needs_texture
            || !window
                .values()
                .copied()
                .filter(|voxel| voxel.opacity_category() != OpacityCategory::Invisible)
                .map(|voxel| (voxel.color, voxel.emission))
                .all_equal();

        // Bitmask of which axes are going to have a visible surface
        let mut axes_involved = 0;

        // For each direction, check if any of the voxels in the deeper side are visible
        // and not covered by a voxel of the same or stronger category in the shallower side,
        // and mark that plane as occupied if so.
        //
        // `unflatten()` undoes the axis dropping/swapping this code does.
        if uncovered(opaque, PX) || uncovered(renderable, PX) {
            axes_involved |= 0b1;
            analysis.expand_rect(NX, center.x, center.yz());
        }
        if uncovered(opaque, PY) || uncovered(renderable, PY) {
            axes_involved |= 0b10;
            analysis.expand_rect(NY, center.y, center.xz());
        }
        if uncovered(opaque, PZ) || uncovered(renderable, PZ) {
            axes_involved |= 0b100;
            analysis.expand_rect(NZ, center.z, center.xy());
        }
        if uncovered(opaque, NX) || uncovered(renderable, NX) {
            axes_involved |= 0b1;
            analysis.expand_rect(PX, resolution_coord - center.x, center.yz());
        }
        if uncovered(opaque, NY) || uncovered(renderable, NY) {
            axes_involved |= 0b10;
            analysis.expand_rect(PY, resolution_coord - center.y, center.xz());
        }
        if uncovered(opaque, NZ) || uncovered(renderable, NZ) {
            axes_involved |= 0b100;
            analysis.expand_rect(PZ, resolution_coord - center.z, center.xy());
        }

        if axes_involved == 0b111 {
            // If all three axes have visible surfaces at this point, then this must become a
            // vertex of the mesh. We need to record such vertices as part of the analysis,
            // because not all such vertices are identifiable by being corners in a 2D slice of
            // the volume — there are also vertices where an edge on one plane meets a corner on
            // another plane, and we must consistently treat them as vertices to avoid
            // “T-junctions”: places where an edge of one triangle meets a vertex of other
            // triangles, which are subject to numerical error during rendering that causes visible
            // gaps.
            let vertex = AnalysisVertex {
                position: center,
                renderable,
                opaque,
            };
            analysis.vertices.try_reserve(1)?;
            analysis.vertices.push(vertex);
        }
    }
    Ok(())
}

/// Returns whether any bits in the more `face`ward side of `mask` are set while the
/// corresponding bits on the opposite side are *not* set.
fn uncovered(mask: OctantMask, face: Face) -> bool {
    (mask & OctantMask::from_face(face) & !mask.shift(face)).any()
}

/// Iterates over all 2×2×2 windows that intersect at least one voxel, and calls `f` with
/// the center point of the window and the voxel data.
///
/// This is expressed as internal iteration (a callback rather than `-> impl Iterator`) for ease
/// of implementing caching across steps.
///
/// Note: Changing the iteration order will change the ordering of the [`Analysis::vertices`]
/// vector, which must be taken into account.
fn for_each_window<'a>(
    voxels: Vol<&'a [Evoxel], ZMaj>,
    mut f: impl FnMut((Point3D<u8, Cube>, OctantMap<&'a Evoxel>)) -> Result<(), OutOfMemory>,
) -> Result<(), OutOfMemory> {
    if voxels.bounds().is_empty() {
        // exit early so the rest of the logic can assume our ranges contain at least 1 element
        return Ok(());
    }

    // For 2³ windows that spill 1 voxel outside, expand by 1 to get the lower cubes.
    let window_lb_bounds = voxels.bounds().expand(FaceMap {
        nx: 1,
        ny: 1,
        nz: 1,
        px: 0,
        py: 0,
        pz: 0,
    });

    // TODO: try using explicitly computed linear indices instead of passing a Cube every time
    // (might not be a win given needing to handle the out-of-bounds cases).
    let get_voxel = |cube: Cube| voxels.get_ref(cube).unwrap_or(const { &Evoxel::AIR });

    for x in window_lb_bounds.x_range() {
        for y in window_lb_bounds.y_range() {
            // For each contiguous row (z axis, according to ZMaj ordering),
            // we cache 4 of the 8 voxels that make up the window.
            // We don't need to actually look them up for the first iteration because
            // the first window in every axis is always hanging out-of-bounds except
            // on the side that'll be written to.

            let mut oct_voxels: OctantMap<&'a Evoxel> = OctantMap::repeat(&Evoxel::AIR);

            for z in window_lb_bounds.z_range() {
                // wrapping_add() used because we know the starting values are far too small for
                // adding 1 to overflow, so overflow checks are never useful.

                let new_quad_lb = Cube::new(x, y, z.wrapping_add(1));

                // Shift previously fetched voxels
                oct_voxels[Octant::Nnn] = oct_voxels[Octant::Nnp];
                oct_voxels[Octant::Npn] = oct_voxels[Octant::Npp];
                oct_voxels[Octant::Pnn] = oct_voxels[Octant::Pnp];
                oct_voxels[Octant::Ppn] = oct_voxels[Octant::Ppp];
                // Fetch voxels newly within the window
                let uppermost_cube = new_quad_lb.wrapping_add(vec3(1, 1, 0));
                let center = uppermost_cube.lower_bounds();
                oct_voxels[Octant::Nnp] = get_voxel(new_quad_lb);
                oct_voxels[Octant::Npp] = get_voxel(new_quad_lb.wrapping_add(vec3(0, 1, 0)));
                oct_voxels[Octant::Pnp] = get_voxel(new_quad_lb.wrapping_add(vec3(1, 0, 0)));
                oct_voxels[Octant::Ppp] = get_voxel(uppermost_cube);

                // can't overflow due to the `Resolution` limit on blocks
                f((center.map(|coord| coord as u8), oct_voxels))?;
            }
        }
    }

    Ok(())
}

/// Reverses the 2D-ification transformation done to `occupied_planes`.
fn unflatten(
    axis: Axis,
    missing_coordinate: GridCoordinate,
    point: Point2D<u8, OccupiedInternal>,
) -> GridPoint {
    let point = point.to_i32();
    match axis {
        Axis::X => point3(missing_coordinate, point.x, point.y),
        Axis::Y => point3(point.x, missing_coordinate, point.y),
        Axis::Z => point3(point.x, point.y, missing_coordinate),
    }
}

/// Given the [`mask`] describing the neighborhood around some vertex, returns whether the
/// wireframe of the shape has an edge extending from that vertex in the given `direction`.
///
/// This is a helper for [`Analysis::edges()`].
fn has_edge_in_direction(mask: OctantMask, direction: Face) -> bool {
    let axis = direction.axis();

    // Shift away the part which should not be tested.
    let only_this_side = mask.shift_copy(direction.opposite());

    // There is an edge if and only if there are two faces that meet at a right angle here.
    // There is a face with a normal on a given axis if the mask is non-uniform on that axis.
    // Therefore, check uniformity along the two axes perpendicular to `direction`, and if it is
    // non-uniform on both axes, then there is a right angle, and therefore an edge.
    !only_this_side.is_uniform_on(axis.decrement())
        && !only_this_side.is_uniform_on(axis.increment())
}

// -------------------------------------------------------------------------------------------------
// Trait implementations, not related to the analysis itself

impl Default for Analysis {
    fn default() -> Self {
        Analysis::EMPTY
    }
}

impl fmt::Debug for AnalysisVertex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Always-single-line formatting
        let Self {
            position,
            renderable,
            opaque,
        } = self;
        write!(
            f,
            "{{ position: {position:?}, renderable: {renderable:?}, opaque: {opaque:?} }}"
        )
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::{self, Block};
    use all_is_cubes::content::make_some_blocks;
    use all_is_cubes::euclid::{point2, point3, size2};
    use all_is_cubes::math::{GridAab, Rgb, Rgba, ZMaj, rgba_const};
    use all_is_cubes::universe::ReadTicket;
    use all_is_cubes::universe::Universe;
    use alloc::sync::Arc;
    use alloc::{vec, vec::Vec};
    use pretty_assertions::assert_eq;
    use std::dbg;

    #[test]
    fn windows_test() {
        let tr = Rgba::TRANSPARENT;
        let red = rgba_const!(1.0, 0.0, 0.0, 1.0);
        // This test is ordering-sensitive but doesn't truly care what ordering; just, we'll
        // need to change the coordinates if we change the ordering.
        let vol: Vol<Arc<[Evoxel]>, ZMaj> =
            Vol::from_elements(GridAab::ORIGIN_CUBE, vec![Evoxel::from_color(red)]).unwrap();
        let mut results: Vec<(Point3D<u8, Cube>, [Rgba; 8])> = Vec::new();
        for_each_window(vol.as_ref(), |(point, colors)| {
            results.push((point, colors.into_zmaj_array().map(|voxel| voxel.color)));
            Ok(())
        })
        .unwrap();
        assert_eq!(
            results,
            vec![
                (point3(0, 0, 0), [tr, tr, tr, tr, tr, tr, tr, red]),
                (point3(0, 0, 1), [tr, tr, tr, tr, tr, tr, red, tr]),
                (point3(0, 1, 0), [tr, tr, tr, tr, tr, red, tr, tr]),
                (point3(0, 1, 1), [tr, tr, tr, tr, red, tr, tr, tr]),
                (point3(1, 0, 0), [tr, tr, tr, red, tr, tr, tr, tr]),
                (point3(1, 0, 1), [tr, tr, red, tr, tr, tr, tr, tr]),
                (point3(1, 1, 0), [tr, red, tr, tr, tr, tr, tr, tr]),
                (point3(1, 1, 1), [red, tr, tr, tr, tr, tr, tr, tr]),
            ]
        )
    }

    /// `Analysis` is a huge data type.
    /// Check that it doesn't get any bigger without warning.
    #[test]
    fn analysis_size() {
        let plane_box_size = 4; // 4 × u8
        assert_eq!(size_of::<PlaneBox>(), plane_box_size);
        let planes_per_face = MAX_PLANES;
        let faces = Face::ALL.len();
        // One GridAab + one Vec + 2 byte-sized fields
        let other_fields = (size_of::<GridAab>() + size_of::<Vec<AnalysisVertex>>() + 2)
            .next_multiple_of(align_of::<Vec<AnalysisVertex>>());
        assert_eq!(
            dbg!(size_of::<Analysis>()),
            plane_box_size * planes_per_face * faces + other_fields
        );
    }

    /// Exercise the analysis on the outputs of `make_slab()`.
    #[rstest::rstest]
    fn analyze_slab(#[values(0, 1, 2, 3, 4)] thickness: i32) {
        let mut u = Universe::new();

        let slab = all_is_cubes::content::make_slab(&mut u, thickness, Resolution::R4);
        let ev = slab.evaluate(u.read_ticket()).unwrap();
        let analysis = analyze(
            ev.resolution(),
            ev.voxels().as_vol_ref(),
            TransparencyFormat::Surfaces,
            &mut Viz::disabled(),
        )
        .unwrap();

        let occupied_planes = FaceMap::from_fn(|f| analysis.occupied_planes(f).collect_vec());
        if thickness == 0 {
            assert_eq!(analysis.needs_texture, false, "needs_texture");
            assert_eq!(
                occupied_planes,
                FaceMap::default(), // all empty
            );
        } else {
            assert_eq!(analysis.needs_texture, true, "needs_texture");
            // Note: the orientation of these rects depends on the arbitrary choices of
            // Face::face_transform().
            assert_eq!(
                occupied_planes,
                FaceMap {
                    nx: vec![(
                        0,
                        Rect::from_origin_and_size(point2(0, 0), size2(thickness, 4))
                    )],
                    ny: vec![(0, Rect::from_origin_and_size(point2(0, 0), size2(4, 4)))],
                    nz: vec![(
                        0,
                        Rect::from_origin_and_size(point2(0, 0), size2(4, thickness))
                    )],
                    px: vec![(
                        0,
                        Rect::from_origin_and_size(point2(4 - thickness, 0), size2(thickness, 4))
                    )],
                    py: vec![(
                        4 - thickness,
                        Rect::from_origin_and_size(point2(0, 0), size2(4, 4))
                    )],
                    pz: vec![(
                        0,
                        Rect::from_origin_and_size(point2(0, 4 - thickness), size2(4, thickness))
                    )],
                }
            );
        }
    }

    /// Check that emission-only voxels are counted as visible.
    ///
    /// TODO: We also want to know that `needs_texture` is set when applicable.
    #[test]
    fn analyze_emission_only() {
        let mut voxel = Evoxel::from_color(Rgba::TRANSPARENT);
        voxel.emission = Rgb::ONE;

        let analysis = analyze(
            Resolution::R1,
            Vol::from_elements(GridAab::for_block(Resolution::R1), [voxel].as_slice()).unwrap(),
            TransparencyFormat::Surfaces,
            &mut Viz::disabled(),
        )
        .unwrap();

        assert!(analysis.occupied_planes(Face::NX).next().is_some());
    }

    // TODO: We have no test coverage for uniform-color transparent blocks
    #[test]
    fn transparent_volume_counts_as_needs_texture() {
        let mut u = Universe::new();

        let block = Block::builder()
            .voxels_fn(Resolution::R4, |cube| {
                Block::from(Rgba::new(cube.x as f32 * 0.1, 0.0, 0.0, 0.5))
            })
            .unwrap()
            .build_into(&mut u);
        let ev = block.evaluate(u.read_ticket()).unwrap();
        let analysis = analyze(
            ev.resolution(),
            ev.voxels().as_vol_ref(),
            TransparencyFormat::BoundingBox,
            &mut Viz::disabled(),
        )
        .unwrap();

        // No occupied planes, but...
        assert_eq!(
            FaceMap::from_fn(|f| analysis.occupied_planes(f).collect_vec()),
            FaceMap::splat(vec![]),
        );
        // ...still needs the texture.
        assert!(analysis.needs_texture);
    }

    /// Regression test: don’t consider the insides of partially-transparent volumes occupied.
    #[test]
    fn analyze_transparent_interior() {
        let mut u = Universe::new();

        let block = Block::builder()
            .voxels_fn(Resolution::R4, |_| block::from_color!(0.0, 0.0, 0.0, 0.5))
            .unwrap()
            .build_into(&mut u);
        let ev = block.evaluate(u.read_ticket()).unwrap();
        let analysis = analyze(
            ev.resolution(),
            ev.voxels().as_vol_ref(),
            TransparencyFormat::Surfaces,
            &mut Viz::disabled(),
        )
        .unwrap();

        let occupied_planes = FaceMap::from_fn(|f| analysis.occupied_planes(f).collect_vec());

        // Occupancy is at the surface only, with no interior planes.
        assert_eq!(
            occupied_planes,
            FaceMap::splat(vec![(
                0,
                Rect::from_origin_and_size(point2(0, 0), size2(4, 4))
            )])
        );
    }

    #[test]
    fn edges() {
        let [block] = make_some_blocks();
        let ev = block.evaluate(ReadTicket::stub()).unwrap();
        let analysis = analyze(
            ev.resolution(),
            ev.voxels().as_vol_ref(),
            TransparencyFormat::Surfaces,
            &mut Viz::disabled(),
        )
        .unwrap();

        let edges: Vec<[usize; 2]> = analysis.compute_edges().collect();

        assert_eq!(
            Vec::from_iter(analysis.vertices().iter().map(|v| v.position)),
            vec![
                point3(0, 0, 0),
                point3(0, 0, 1),
                point3(0, 1, 0),
                point3(0, 1, 1),
                point3(1, 0, 0),
                point3(1, 0, 1),
                point3(1, 1, 0),
                point3(1, 1, 1),
            ]
        );

        assert_eq!(
            edges,
            vec![
                // Fun fact: In a cube wireframe/graph with this numbering, every edge is
                // between two vertices which differ in exactly one bit.
                [0b000, 0b001],
                [0b000, 0b010],
                [0b001, 0b011],
                [0b010, 0b011],
                [0b000, 0b100],
                [0b001, 0b101],
                [0b100, 0b101],
                [0b010, 0b110],
                [0b100, 0b110],
                [0b011, 0b111],
                [0b101, 0b111],
                [0b110, 0b111],
            ]
        )
    }
}

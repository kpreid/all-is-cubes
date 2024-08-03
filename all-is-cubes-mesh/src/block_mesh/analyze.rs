use itertools::Itertools;

use all_is_cubes::block::{Evoxel, Resolution};
use all_is_cubes::euclid::{point3, Point2D};
use all_is_cubes::math::{
    Axis, Cube, Face6, FaceMap, GridCoordinate, GridPoint, OctantMap, OctantMask, Rgba, Vol,
};

#[cfg(feature = "rerun")]
use all_is_cubes::math::GridAab;

use crate::block_mesh::viz::Viz;

/// Maximum number of occupied planes/layers in any block.
///
/// The plus one is because we're counting fenceposts (coordinate planes) — wait.
/// TODO: We should never have anything in the deepest plane. Reduce this and test that's ok.
const MAX_PLANES: usize = Resolution::MAX.to_grid() as usize + 1;

type Rect = all_is_cubes::euclid::Box2D<GridCoordinate, Cube>;

/// Flat (one axis zero sized) box whose bounds are a voxel surface that needs triangles
/// generated for it.
///
/// Note: I tried using [`u8`] coordinate storage and it was significantly slower.
type PlaneBox = all_is_cubes::euclid::Box2D<GridCoordinate, Cube>;

/// A maximally inside-out box, used as the `occupied_planes` value containing no voxels.
/// When it meets any actual voxel it will get replaced implicitly by the min/max operations.
const EMPTY_PLANE_BOX: PlaneBox = PlaneBox {
    min: Point2D::new(GridCoordinate::MAX, GridCoordinate::MAX),
    max: Point2D::new(GridCoordinate::MIN, GridCoordinate::MIN),
};

#[derive(Clone, Debug)] // could be Copy, but it's large so let's not
#[repr(C, align(16))] // For perf, ensure the arrays are aligned to whole `PlaneBox`es, 4 × i32
pub(crate) struct Analysis {
    /// For each face normal, which depths will need any triangles generated,
    /// and for those that do, which bounds need to be scanned.
    ///
    /// Index 0 is depth 0 (the surface of the block volume), index 1 is one voxel
    /// deeper, and so on.
    ///
    /// If a given plane contains no voxels, its value will be [`EMPTY_PLANE_BOX`].
    ///
    /// The boxes' thickness happens to be equal to the layer position but this is coincidental.
    occupied_planes: FaceMap<[PlaneBox; MAX_PLANES]>,

    resolution: Resolution,

    /// Whether there are any adjacent visible voxels that have different colors, and therefore
    /// probably need a texture rather than vertex colors.
    pub needs_texture: bool,
}

/// Reverses the 2D-ification transformation done to `occupied_planes`.
fn unflatten(
    axis: Axis,
    missing_coordinate: GridCoordinate,
    point: Point2D<GridCoordinate, Cube>,
) -> GridPoint {
    match axis {
        Axis::X => point3(missing_coordinate, point.x, point.y),
        Axis::Y => point3(point.x, missing_coordinate, point.y),
        Axis::Z => point3(point.x, point.y, missing_coordinate),
    }
}

impl Analysis {
    pub fn empty() -> Self {
        Analysis {
            occupied_planes: FaceMap::repeat([EMPTY_PLANE_BOX; MAX_PLANES]),
            needs_texture: false,
            resolution: Resolution::R1,
        }
    }

    /// For each face normal, which depths will need any triangles generated.
    /// Index 0 is depth 0 (the surface of the block volume), index 1 is one voxel
    /// deeper, and so on.
    pub fn occupied_planes(
        &self,
        face: Face6,
    ) -> impl Iterator<Item = (GridCoordinate, Rect)> + '_ {
        (0..GridCoordinate::from(self.resolution))
            .zip(self.occupied_planes[face])
            .filter_map(move |(i, pbox)| {
                if pbox.is_empty() {
                    None
                } else {
                    Some((i, self.pbox_to_rect(face, pbox)))
                }
            })
    }

    #[cfg(feature = "rerun")]
    pub fn occupied_plane_box(&self, face: Face6, layer: GridCoordinate) -> Option<GridAab> {
        let pbox = self.occupied_planes[face][usize::try_from(layer).unwrap()];
        if pbox.is_empty() {
            return None;
        }
        let mc = if face.is_negative() {
            layer
        } else {
            GridCoordinate::from(self.resolution) - layer
        };
        Some(GridAab::from_lower_upper(
            unflatten(face.axis(), mc, pbox.min),
            unflatten(face.axis(), mc, pbox.max),
        ))
    }

    pub(crate) fn surface_is_occupied(&self, face: Face6) -> bool {
        !self.occupied_planes[face][0].is_empty()
    }

    fn pbox_to_rect(&self, face: Face6, pbox: PlaneBox) -> Rect {
        let t = face.face_transform(self.resolution.into()).inverse();
        Rect::from_points([
            t.transform_point(unflatten(face.axis(), 0, pbox.min))
                .to_2d(),
            t.transform_point(unflatten(face.axis(), 0, pbox.max))
                .to_2d(),
        ])
    }

    #[inline(always)] // we want this specialized for each case
    fn expand_rect(
        &mut self,
        face: Face6,
        layer: GridCoordinate,
        center: Point2D<GridCoordinate, Cube>,
    ) {
        // We didn't check *exactly* which voxels in the window need pixels, but this is
        // exactly compensated for by the fact that we'll always have a window spilling over
        // the edge, so the range of window-center-points is equal to the range we need to
        // consider meshing.
        let existing_box = &mut self.occupied_planes[face][layer as usize];

        // Can't use union() because that special cases zero-sized boxes in a way we don't want.
        *existing_box = PlaneBox::new(existing_box.min.min(center), existing_box.max.max(center))
    }

    #[cfg(feature = "rerun")]
    pub(crate) fn delete_occupied_plane(&mut self, face: Face6, layer: GridCoordinate) {
        self.occupied_planes[face][layer as usize] = EMPTY_PLANE_BOX;
    }
}

/// Analyze a block's voxel array and find characteristics its mesh will have.
///
/// This is a preliminary step before creating the actual vertices and texture of a `BlockMesh`.
pub(crate) fn analyze(resolution: Resolution, voxels: Vol<&[Evoxel]>, viz: &mut Viz) -> Analysis {
    let mut analysis = Analysis::empty();
    analysis.resolution = resolution;
    viz.analysis_in_progress(&analysis);

    // TODO: Have EvaluatedBlock tell us when a block is fully cubical and opaque,
    // and then avoid scanning the interior volume. EvaluatedBlock.opaque
    // is not quite that because it is defined to allow concavities.

    for (center, colors) in windows(voxels) {
        viz.window(center, voxels);
        analyze_one_window(&mut analysis, center, colors);
        viz.analysis_in_progress(&analysis);
    }
    viz.clear_window();

    analysis
}

/// Take one of the outputs of [`windows()`] and compute its contribution to [`analysis`].
#[inline]
fn analyze_one_window(analysis: &mut Analysis, center: GridPoint, colors: OctantMap<Rgba>) {
    use Face6::*;
    const ALL: OctantMask = OctantMask::ALL;
    const NONE: OctantMask = OctantMask::NONE;

    let opaque = bitmask(colors, Rgba::fully_opaque);
    let semitransparent = !(opaque | bitmask(colors, Rgba::fully_transparent));
    let renderable = opaque | semitransparent;

    // First, quickly check if there are any visible surfaces at all here.
    if opaque != NONE && opaque != ALL || semitransparent != NONE && semitransparent != ALL {
        let resolution_coord = GridCoordinate::from(analysis.resolution);

        // TODO: false positives — look only at visible cubes on each axis, instead of all cubes
        analysis.needs_texture = analysis.needs_texture
            || !colors
                .values()
                .filter(|color| !color.fully_transparent())
                .all_equal();

        // For each direction, check if any of the voxels in the deeper side are visible
        // and not covered by opaque blocks in the shallower side,
        // and mark that plane as occupied if so.
        //
        // `unflatten()` undoes the axis dropping/swapping this code does.
        if renderable & ALL.shift(PX) & !opaque.shift(PX) != NONE {
            analysis.expand_rect(NX, center.x, center.yz());
        }
        if renderable & ALL.shift(PY) & !opaque.shift(PY) != NONE {
            analysis.expand_rect(NY, center.y, center.xz());
        }
        if renderable & ALL.shift(PZ) & !opaque.shift(PZ) != NONE {
            analysis.expand_rect(NZ, center.z, center.xy());
        }
        if renderable & ALL.shift(NX) & !opaque.shift(NX) != NONE {
            analysis.expand_rect(PX, resolution_coord - center.x, center.yz());
        }
        if renderable & ALL.shift(NY) & !opaque.shift(NY) != NONE {
            analysis.expand_rect(PY, resolution_coord - center.y, center.xz());
        }
        if renderable & ALL.shift(NZ) & !opaque.shift(NZ) != NONE {
            analysis.expand_rect(PZ, resolution_coord - center.z, center.xy());
        }
    }
}

/// Iterates over all 2×2×2 windows that intersect at least one voxel, and returns their center
/// points and their colors.
///
/// The colors are ordered in Z-major fashion; that is,
///
/// * `colors[0b000] = voxels[[low, low, low]]`
/// * `colors[0b001] = voxels[[low, low, high]]`
/// * `colors[0b010] = voxels[[low, high, low]]`
/// * ...
fn windows(voxels: Vol<&[Evoxel]>) -> impl Iterator<Item = (GridPoint, OctantMap<Rgba>)> + '_ {
    // For 2³ windows that spill 1 voxel outside, expand by 1 to get the lower cubes.
    let window_lb_bounds = voxels.bounds().expand(FaceMap {
        nx: 1,
        ny: 1,
        nz: 1,
        px: 0,
        py: 0,
        pz: 0,
    });

    window_lb_bounds.interior_iter().map(move |window_lb| {
        let colors: OctantMap<Rgba> = OctantMap::from_fn(|octant| {
            voxels
                .get(window_lb + octant.to_positive_cube().lower_bounds().to_vector())
                .unwrap_or(&Evoxel::AIR)
                .color
        });
        (window_lb.upper_bounds(), colors)
    })
}

/// Given a group of colors produced by [`windows()`], map it to a bitmask.
fn bitmask(v: OctantMap<Rgba>, f: impl Fn(Rgba) -> bool) -> OctantMask {
    v.to_mask(|&color| f(color))
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::euclid::{point2, size2};
    use all_is_cubes::math::{rgba_const, GridAab, ZMaj};

    use all_is_cubes::universe::Universe;
    use alloc::sync::Arc;
    use alloc::vec::Vec;
    use pretty_assertions::assert_eq;

    #[test]
    fn windows_test() {
        let tr = Rgba::TRANSPARENT;
        let red = rgba_const!(1.0, 0.0, 0.0, 1.0);
        // This test is ordering-sensitive but doesn't truly care what ordering; just, we'll
        // need to change the coordinates if we change the ordering.
        let vol: Vol<Arc<[Evoxel]>, ZMaj> =
            Vol::from_elements(GridAab::ORIGIN_CUBE, vec![Evoxel::from_color(red)]).unwrap();
        assert_eq!(
            windows(vol.as_ref())
                .map(|(point, colors)| (point, colors.into_zmaj_array()))
                .collect::<Vec<(GridPoint, [Rgba; 8])>>(),
            vec![
                (GridPoint::new(0, 0, 0), [tr, tr, tr, tr, tr, tr, tr, red]),
                (GridPoint::new(0, 0, 1), [tr, tr, tr, tr, tr, tr, red, tr]),
                (GridPoint::new(0, 1, 0), [tr, tr, tr, tr, tr, red, tr, tr]),
                (GridPoint::new(0, 1, 1), [tr, tr, tr, tr, red, tr, tr, tr]),
                (GridPoint::new(1, 0, 0), [tr, tr, tr, red, tr, tr, tr, tr]),
                (GridPoint::new(1, 0, 1), [tr, tr, red, tr, tr, tr, tr, tr]),
                (GridPoint::new(1, 1, 0), [tr, red, tr, tr, tr, tr, tr, tr]),
                (GridPoint::new(1, 1, 1), [red, tr, tr, tr, tr, tr, tr, tr]),
            ]
        )
    }

    /// `Analysis` is a huge data type.
    /// Check that it doesn't get any bigger without warning.
    #[test]
    fn analysis_size() {
        let plane_box_size = 16; // 4 × i32
        let planes_per_face = MAX_PLANES;
        let faces = Face6::ALL.len();
        let other_fields = 16; // 2 byte-sized fields, aligned to 16
        assert_eq!(
            dbg!(size_of::<Analysis>()),
            plane_box_size * planes_per_face * faces + other_fields
        );
    }

    /// Exercise the analysis on the outputs of `make_slab()`.
    #[test]
    fn analyze_slab() {
        let mut u = Universe::new();
        for thickness in [0, 1, 2, 3, 4] {
            eprintln!("thickness {thickness}:");
            let slab = all_is_cubes::content::make_slab(&mut u, thickness, Resolution::R4);
            let ev = slab.evaluate().unwrap();
            let analysis = analyze(
                ev.resolution(),
                ev.voxels().as_vol_ref(),
                &mut Viz::disabled(),
            );

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
                // Face6::face_transform().
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
                            Rect::from_origin_and_size(
                                point2(4 - thickness, 0),
                                size2(thickness, 4)
                            )
                        )],
                        py: vec![(
                            4 - thickness,
                            Rect::from_origin_and_size(point2(0, 0), size2(4, 4))
                        )],
                        pz: vec![(
                            0,
                            Rect::from_origin_and_size(
                                point2(0, 4 - thickness),
                                size2(4, thickness)
                            )
                        )],
                    }
                );
            }
        }
    }
}

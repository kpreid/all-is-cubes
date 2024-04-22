use std::array::from_fn as arr;

use itertools::Itertools;

use all_is_cubes::block::{Evoxel, Resolution};
use all_is_cubes::euclid::vec3;
use all_is_cubes::math::{Cube, Face6, FaceMap, GridCoordinate, GridPoint, Rgba, Vol};

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
/// TODO: Investigate representation improvements for this, and if none succeed, make this an
/// ordinary `GridAab` (after we get rid of the `GridAab` volume restriction).
type PlaneBox = all_is_cubes::euclid::Box3D<GridCoordinate, Cube>;

#[derive(Clone, Debug)] // could be Copy, but it's large so let's not
pub(crate) struct Analysis {
    resolution: Resolution,

    /// For each face normal, which depths will need any triangles generated,
    /// and for those that do, which bounds need to be scanned.
    ///
    /// Index 0 is depth 0 (the surface of the block volume), index 1 is one voxel
    /// deeper, and so on.
    ///
    /// The boxes' thickness happens to be equal to the layer position but this is coincidental.
    occupied_planes: FaceMap<[Option<PlaneBox>; MAX_PLANES]>,

    /// Whether there are any adjacent visible voxels that have different colors, and therefore
    /// probably need a texture rather than vertex colors.
    pub needs_texture: bool,
}

impl Analysis {
    pub fn empty() -> Self {
        Analysis {
            occupied_planes: FaceMap::repeat([None; MAX_PLANES]),
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
        (0i32..)
            .zip(self.occupied_planes[face])
            .filter_map(move |(i, pbox)| pbox.map(move |pbox| (i, self.pbox_to_rect(face, pbox))))
    }

    #[cfg(feature = "rerun")]
    pub fn occupied_plane_box(&self, face: Face6, layer: GridCoordinate) -> Option<GridAab> {
        let pbox = self.occupied_planes[face][usize::try_from(layer).unwrap()]?;
        Some(GridAab::from_lower_upper(pbox.min, pbox.max))
    }

    pub(crate) fn surface_is_occupied(&self, face: Face6) -> bool {
        self.occupied_planes[face][0].is_some()
    }

    fn pbox_to_rect(&self, face: Face6, pbox: PlaneBox) -> Rect {
        let t = face.face_transform(self.resolution.into()).inverse();
        Rect::from_points([
            t.transform_point(pbox.min).to_2d(),
            t.transform_point(pbox.max).to_2d(),
        ])
    }

    #[inline(always)] // we want this specialized for each case
    fn expand_rect(&mut self, face: Face6, layer: GridCoordinate, center: GridPoint) {
        // We didn't check *exactly* which voxels in the window need pixels, but this is
        // exactly compensated for by the fact that we'll always have a window spilling over
        // the edge, so the range of window-center-points is equal to the range we need to
        // consider meshing.
        match &mut self.occupied_planes[face][layer as usize] {
            Some(existing_box) => {
                // Can't use union() because that special cases zero-sized boxes.
                *existing_box =
                    PlaneBox::new(existing_box.min.min(center), existing_box.max.max(center))
            }
            empty @ None => *empty = Some(PlaneBox::new(center, center)),
        }
    }
}

/// Analyze a block's voxel array and find characteristics its mesh will have.
///
/// This is a preliminary step before creating the actual vertices and texture of a `BlockMesh`.
pub(crate) fn analyze(resolution: Resolution, voxels: Vol<&[Evoxel]>, viz: &mut Viz) -> Analysis {
    let resolution_coord = GridCoordinate::from(resolution);

    let mut analysis = Analysis::empty();
    analysis.resolution = resolution;
    viz.analysis_in_progress(&analysis);

    // TODO: Have EvaluatedBlock tell us when a block is fully cubical and opaque,
    // and then avoid scanning the interior volume. EvaluatedBlock.opaque
    // is not quite that because it is defined to allow concavities.

    for (center, colors) in windows(voxels) {
        viz.window(center);

        let opaque = bitmask(colors, Rgba::fully_opaque);
        let semitransparent = !(opaque | bitmask(colors, Rgba::fully_transparent));
        let renderable = opaque | semitransparent;

        // First, quickly check if there are any visible surfaces at all here.
        if opaque != 0x00 && opaque != 0xFF || semitransparent != 0x00 && semitransparent != 0xFF {
            // TODO: false positives — look only at visible cubes on each axis, instead of all cubes
            analysis.needs_texture = analysis.needs_texture
                || !colors
                    .iter()
                    .filter(|color| !color.fully_transparent())
                    .all_equal();

            // For each direction, check if any of the voxels in the deeper side are visible
            // and not covered by opaque blocks in the shallower side,
            // and mark that plane as occupied if so.
            if renderable & shift_px(0xFF) & !shift_px(opaque) != 0 {
                analysis.expand_rect(Face6::NX, center.x, center);
            }
            if renderable & shift_py(0xFF) & !shift_py(opaque) != 0 {
                analysis.expand_rect(Face6::NY, center.y, center);
            }
            if renderable & shift_pz(0xFF) & !shift_pz(opaque) != 0 {
                analysis.expand_rect(Face6::NZ, center.z, center);
            }
            if renderable & shift_nx(0xFF) & !shift_nx(opaque) != 0 {
                analysis.expand_rect(Face6::PX, resolution_coord - center.x, center);
            }
            if renderable & shift_ny(0xFF) & !shift_ny(opaque) != 0 {
                analysis.expand_rect(Face6::PY, resolution_coord - center.y, center);
            }
            if renderable & shift_nz(0xFF) & !shift_nz(opaque) != 0 {
                analysis.expand_rect(Face6::PZ, resolution_coord - center.z, center);
            }
        }

        viz.analysis_in_progress(&analysis);
    }
    viz.clear_window();

    analysis
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
fn windows(voxels: Vol<&[Evoxel]>) -> impl Iterator<Item = (GridPoint, [Rgba; 8])> + '_ {
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
        let colors: [Rgba; 8] = arr(|i| {
            let i = i as GridCoordinate;
            voxels
                .get(window_lb + vec3((i >> 2) & 1, (i >> 1) & 1, i & 1).to_i32())
                .unwrap_or(&Evoxel::AIR)
                .color
        });
        (window_lb.upper_bounds(), colors)
    })
}

/// Given a group of colors produced by [`windows()`], map it to a bitmask.
fn bitmask(v: [Rgba; 8], f: impl Fn(Rgba) -> bool) -> u8 {
    let mut output = 0;
    for (i, &color) in v.iter().enumerate() {
        output |= u8::from(f(color)) << i;
    }
    output
}

// Shift the contents of a [`bitmask()`] in the specified direction, discarding overflows.
fn shift_px(bits: u8) -> u8 {
    bits << 4
}
fn shift_nx(bits: u8) -> u8 {
    bits >> 4
}
fn shift_py(bits: u8) -> u8 {
    (bits & 0b00110011) << 2
}
fn shift_ny(bits: u8) -> u8 {
    (bits & 0b11001100) >> 2
}
fn shift_pz(bits: u8) -> u8 {
    (bits & 0b01010101) << 1
}
fn shift_nz(bits: u8) -> u8 {
    (bits & 0b10101010) >> 1
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::euclid::{point2, size2};
    use all_is_cubes::math::{GridAab, ZMaj};
    use all_is_cubes::rgba_const;
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
            windows(vol.as_ref()).collect::<Vec<_>>(),
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

    #[test]
    fn shifts() {
        assert_eq!(shift_nx(0xFF), 0b00001111);
        assert_eq!(shift_px(0xFF), 0b11110000);
        assert_eq!(shift_ny(0xFF), 0b00110011);
        assert_eq!(shift_py(0xFF), 0b11001100);
        assert_eq!(shift_nz(0xFF), 0b01010101);
        assert_eq!(shift_pz(0xFF), 0b10101010);
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
                ev.voxels.as_vol_ref(),
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

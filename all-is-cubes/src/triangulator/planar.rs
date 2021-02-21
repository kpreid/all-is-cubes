// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Triangulator's 2D plane operations (sliced voxels to texels and meshes).

use cgmath::{
    ElementWise as _, EuclideanSpace as _, Point2, Point3, Transform as _, Vector2, Vector3,
    Zero as _,
};
use std::convert::{TryFrom, TryInto};

use crate::math::{Face, FreeCoordinate, GridCoordinate, Rgba};
use crate::triangulator::{BlockVertex, Coloring, TextureCoordinate, TextureTile};

/// Data structure for the state and components of the "greedy meshing" algorithm.
/// <https://0fps.net/2012/06/30/meshing-in-a-minecraft-game/>
pub(crate) struct GreedyMesher {
    visible_image: Vec<Rgba>,
    resolution_g: GridCoordinate,
    resolution_s: usize,
    /// Contains a color if all voxels examined so far have that color.
    pub(crate) single_color: Option<Rgba>,
    pub(crate) rect_has_alpha: bool,
}
impl GreedyMesher {
    /// Create the initial state.
    pub(crate) fn new(visible_image: Vec<Rgba>, resolution: GridCoordinate) -> Self {
        Self {
            visible_image,
            resolution_g: resolution,
            resolution_s: resolution.try_into().unwrap(),
            single_color: None,
            rect_has_alpha: false,
        }
    }

    /// Actually run the algorithm.
    pub(crate) fn run(
        mut self,
        mut quad_callback: impl FnMut(&Self, Point2<FreeCoordinate>, Point2<FreeCoordinate>),
    ) {
        for tl in 0..self.resolution_g {
            for sl in 0..self.resolution_g {
                if !self.add_seed(sl, tl) {
                    continue;
                }
                // Find the largest width that works.
                let mut sh = sl;
                loop {
                    sh += 1;
                    if sh >= self.resolution_g {
                        break; // Found the far edge
                    }
                    if !self.add_candidate(sh, tl) {
                        break;
                    }
                }
                // Find the largest height that works
                let mut th = tl;
                'expand_t: loop {
                    th += 1;
                    if th >= self.resolution_g {
                        break; // Found the far edge
                    }
                    // Check if all the voxels are wanted
                    for s in sl..sh {
                        if !self.add_candidate(s, th) {
                            break 'expand_t;
                        }
                    }
                }

                // Erase all the voxels that we just built a rectangle on, to remember not
                // to do it again. (We don't need to do this last, because the actual data
                // is either in the texture or in `single_color`.
                for t in tl..th {
                    for s in sl..sh {
                        self.erase(s, t);
                    }
                }
                let map_coord =
                    |c| FreeCoordinate::from(c) / FreeCoordinate::from(self.resolution_g);
                quad_callback(
                    &self,
                    Point2::new(map_coord(sl), map_coord(tl)),
                    Point2::new(map_coord(sh), map_coord(th)),
                );
            }
        }
    }

    #[inline]
    fn index(&self, s: GridCoordinate, t: GridCoordinate) -> usize {
        // Can't fail because a usize ≈ u16 platform is too small anyway.
        let s = usize::try_from(s).unwrap();
        let t = usize::try_from(t).unwrap();
        t * self.resolution_s as usize + s
    }

    /// Checks if a voxel is visible and thus can be the seed of a rectangle,
    /// returns false if not, and updates `single_color`.
    #[inline]
    fn add_seed(&mut self, s: GridCoordinate, t: GridCoordinate) -> bool {
        if s >= self.resolution_g || t >= self.resolution_g {
            panic!("seed loop ran out of bounds");
        }
        let color = self.visible_image[self.index(s, t)];
        if color.fully_transparent() {
            return false;
        }
        self.rect_has_alpha = !color.fully_opaque();
        self.single_color = Some(color);
        true
    }

    /// Checks if a voxel is suitable for adding to the current rectangle, and either
    /// returns false if not, and updates `single_color`.
    #[inline]
    fn add_candidate(&mut self, s: GridCoordinate, t: GridCoordinate) -> bool {
        if s >= self.resolution_g || t >= self.resolution_g {
            return false;
        }
        let color = self.visible_image[self.index(s, t)];
        if color.fully_transparent() {
            return false;
        }
        if Some(color) != self.single_color {
            self.single_color = None; // Not a uniform color
        }
        if !color.fully_opaque() {
            self.rect_has_alpha = true;
        }
        true
    }

    #[inline]
    fn erase(&mut self, s: GridCoordinate, t: GridCoordinate) {
        let index = self.index(s, t);
        self.visible_image[index] = Rgba::TRANSPARENT;
    }
}

/// Helper for [`push_quad`] which offers the alternatives of solid color or texturing.
/// Compared to [`Coloring`], it describes texturing for an entire quad rather than a vertex.
#[derive(Copy, Clone, Debug)]
pub(super) enum QuadColoring<'a, T> {
    Solid(Rgba),
    Texture(&'a T, TextureCoordinate),
}

/// Compute vertices for a quad and push them into the supplied vectors.
///
/// `depth`, `low_corner`, and `high_corner` are in 0-1 coordinates.
#[inline]
#[allow(clippy::too_many_arguments)] // TODO: Figure out how to simplify
pub(super) fn push_quad<V: From<BlockVertex>>(
    vertices: &mut Vec<V>,
    indices: &mut Vec<u32>,
    face: Face,
    depth: FreeCoordinate,
    low_corner: Point2<FreeCoordinate>,
    high_corner: Point2<FreeCoordinate>,
    coloring: QuadColoring<'_, impl TextureTile>,
    resolution: GridCoordinate,
) {
    // TODO: Refactor so we don't have to do 100% of this anew for each individual quad
    // This is tricky, though, since the coloring can vary per quad (though the scale _can_ be constant).
    let transform_f = face.matrix(1).to_free();
    let transform_t = transform_f.cast::<TextureCoordinate>().unwrap();
    let index_origin: u32 = vertices.len().try_into().expect("vertex index overflow");
    let half_texel = 0.5 / (resolution as TextureCoordinate);
    let depth_fudge = Vector3::new(0., 0., half_texel);

    let (clamp_min, clamp_max) = match coloring {
        QuadColoring::Solid(_) => (Vector3::zero(), Vector3::zero()),
        QuadColoring::Texture(tile, scale) => (
            tile.texcoord(
                transform_t
                    .transform_point(Point3 {
                        x: low_corner.x as TextureCoordinate + half_texel,
                        y: low_corner.y as TextureCoordinate + half_texel,
                        z: depth as TextureCoordinate + half_texel,
                    })
                    .to_vec()
                    * scale,
            ),
            tile.texcoord(
                transform_t
                    .transform_point(Point3 {
                        x: high_corner.x as TextureCoordinate - half_texel,
                        y: high_corner.y as TextureCoordinate - half_texel,
                        z: depth as TextureCoordinate + half_texel,
                    })
                    .to_vec()
                    * scale,
            ),
        ),
    };

    for &p in QUAD_VERTICES {
        // Apply bounding rectangle
        let p = low_corner.to_vec() + p.mul_element_wise(high_corner - low_corner);
        // Apply depth
        let p = Point3::from_vec(p.extend(depth));

        vertices.push(V::from(BlockVertex {
            position: transform_f.transform_point(p),
            face,
            coloring: match coloring {
                // Note: if we're ever looking for microöptimizations, we could try
                // converting this to a trait for static dispatch.
                QuadColoring::Solid(color) => Coloring::Solid(color),
                QuadColoring::Texture(tile, scale) => Coloring::Texture {
                    pos: tile.texcoord(
                        transform_t
                            .transform_point(p.map(|s| s as TextureCoordinate) + depth_fudge)
                            .to_vec()
                            * scale,
                    ),
                    clamp_min,
                    clamp_max,
                },
            },
        }));
    }
    for &i in QUAD_INDICES {
        indices.push(index_origin + i);
    }
}

const QUAD_VERTICES: &[Vector2<FreeCoordinate>; 4] = &[
    // Two-triangle quad.
    // Note that looked at from a X-right Y-up view, these triangles are
    // clockwise, but they're properly counterclockwise from the perspective
    // that we're drawing the face _facing towards negative Z_ (into the screen),
    // which is how cube faces as implicitly defined by Face::matrix work.
    Vector2::new(0.0, 0.0),
    Vector2::new(0.0, 1.0),
    Vector2::new(1.0, 0.0),
    Vector2::new(1.0, 1.0),
];

const QUAD_INDICES: &[u32] = &[0, 1, 2, 2, 1, 3];

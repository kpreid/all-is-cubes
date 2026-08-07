//! Test utilities for [`crate::planar`] algorithms.

use alloc::vec::Vec;

use all_is_cubes::euclid::point3;
use all_is_cubes::math::Face;

use crate::planar::{self, Mask};

// -------------------------------------------------------------------------------------------------

/// Shorthand constructor for [`planar::Vertex`].
pub(crate) fn vert(x: i32, y: i32, z: i32, connectivity: Mask, index: u8) -> planar::Vertex {
    planar::Vertex {
        position: point3(x, y, z),
        connectivity,
        index: u32::from(index),
    }
}

pub(crate) fn test_basis() -> planar::Basis {
    let b = planar::Basis::new(Face::PZ, Face::PX, Face::PY);
    assert!(!b.left_handed); // TODO: could use tests that *are* left-handed
    b
}

/// Given a byte array of ASCII art polygons drawn using:
///
/// * letters or '*' for vertices,
/// * `-` and `|` for edges,
/// * and `.` for interior fill,
///
/// produce [`planar::Vertex`]es.
///
/// The coordinate system is compatible with [`test_basis()`], as well as the depiction in the
/// documentation of [`Mask`].
/// The +X direction of coordinates, the sweep direction, is rightward,
/// and the +Y direction of coordinates, the perpendicular direction, is upward
/// (flipped relative to the array indices).
///
/// The vertical axis is flipped, so that the resulting Y coordinates increase up the page;
/// this coordinate layout matches the documentation of [`Mask`]
///
/// The `-` and `|` are not currently used or validated; only `.` vs ` ` near vertices determines
/// connectivity and thus whether an area is interior.
///
/// `*` vertices are automatically assigned indices.
pub(crate) fn vertices_from_ascii_art<const W: usize, const H: usize>(
    ascii_art: [&[u8; W]; H],
) -> Vec<planar::Vertex> {
    let mut output = Vec::new();
    for x in 0..W {
        for y in (0..H).rev() {
            let get = |dx, dy| {
                let neighbor_x = x.wrapping_add_signed(dx);
                let neighbor_y = y.wrapping_add_signed(dy);
                if neighbor_x >= W || neighbor_y >= H {
                    b' '
                } else {
                    ascii_art[neighbor_y][neighbor_x]
                }
            };
            let is_interior = |dx, dy| match get(dx, dy) {
                b' ' => false,
                b'.' => true,
                ch => panic!(
                    "neighbor of {x},{y} should be '.' or ' ', not {:?}",
                    ch as char
                ),
            };

            match get(0, 0) {
                ch @ (b'*' | b'A'..=b'Z' | b'a'..=b'z') => {
                    #[allow(clippy::cast_possible_wrap)]
                    output.push(planar::Vertex {
                        index: u32::from(ch),
                        position: point3(x as i32, (H - y - 1) as i32, 0),
                        connectivity: {
                            let mut mask = Mask::Empty;
                            // note Y flip
                            if is_interior(1, -1) {
                                mask |= Mask::Fsfp;
                            }
                            if is_interior(1, 1) {
                                mask |= Mask::Fsbp;
                            }
                            if is_interior(-1, -1) {
                                mask |= Mask::Bsfp;
                            }
                            if is_interior(-1, 1) {
                                mask |= Mask::Bsbp;
                            }
                            mask
                        },
                    });
                }
                b' ' | b'|' | b'.' | b'-' => {
                    // do nothing on neighbor-describing characters
                }
                ch => panic!("invalid ascii art character {:?}", ch as char),
            }
        }
    }

    // Assign unique indices to all `*` vertices.
    let mut next_index: u32 = 0;
    for vertex in output.iter_mut() {
        if vertex.index == u32::from(b'*') {
            vertex.index = next_index;

            // Don’t assign letters.
            next_index += 1;
            if next_index == u32::from(b'A') {
                next_index = u32::from(b'Z' + 1);
            }
            if next_index == u32::from(b'a') {
                next_index = u32::from(b'z' + 1);
            }
        }
    }

    output
}

mod tests {
    use super::*;

    #[test]
    fn ascii_art() {
        assert_eq!(
            vertices_from_ascii_art([
                b"B--D", //
                b"|..|", //
                b"A--C", //
            ]),
            [
                vert(0, 0, 0, Mask::Fsfp, b'A'),
                vert(0, 2, 0, Mask::Fsbp, b'B'),
                vert(3, 0, 0, Mask::Bsfp, b'C'),
                vert(3, 2, 0, Mask::Bsbp, b'D'),
            ]
        );
    }
}

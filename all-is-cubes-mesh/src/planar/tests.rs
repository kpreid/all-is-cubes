use alloc::vec::Vec;

use all_is_cubes::euclid::point3;
use all_is_cubes::math::{Face6, GridPoint};

use crate::planar::{self, Mask};

// -------------------------------------------------------------------------------------------------

fn test_basis() -> planar::Basis {
    let b = planar::Basis::new(Face6::PZ, Face6::PX, Face6::PY);
    assert!(!b.left_handed); // TODO: could use tests that *are* left-handed
    b
}

/// `Triangulator::triangulate()` parameterized for simplicity
/// (for tests that aren't trying to exercise rotatability).
#[inline(never)]
#[track_caller]
fn run(vertices: &[planar::Vertex]) -> Vec<[u8; 3]> {
    let mut actual_triangles = Vec::new();
    let mut triangulator = planar::Triangulator::new();
    let basis = test_basis();

    eprintln!("Initial state: {triangulator:#?}");

    let maybe_panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        triangulator.triangulate(
            basis,
            vertices.iter().copied().inspect(|planar_vertex| {
                println!("In: {planar_vertex:?}");
            }),
            |triangle_indices: [u32; 3]| {
                let triangle_positions: [GridPoint; 3] = triangle_indices.map(|index| {
                    vertices
                        .iter()
                        .find(|v| v.index == index)
                        .expect("triangulator returned bad index")
                        .position
                });
                // Print as we go, so if there is a panic we can still see some results.
                println!("Out: {triangle_indices:?} = {triangle_positions:?}");
                actual_triangles
                    .push(triangle_indices.map(|i| u8::try_from(i).expect("index out of range")));

                // println!("State: {triangulator:#?}"); // TODO: make it possible to grab the state for debugging
            },
        )
    }));

    eprintln!("Final state: {triangulator:#?}");

    println!(
        "\n{}\n",
        planar::svg::WriteSvg {
            vertices,
            triangles: &actual_triangles
                .iter()
                .map(|byte_arr| byte_arr.map(u32::from))
                .collect::<Vec<[u32; 3]>>(),
            scale: 20.0,
            show_vertices: true,
            standalone_xml: true,
        }
    );

    if let Err(payload) = maybe_panic {
        eprintln!("Unwinding; triangles before panic: {actual_triangles:#?}");

        std::panic::resume_unwind(payload);
    }

    actual_triangles
}

#[inline(never)]
#[track_caller]
fn check(vertices: &[planar::Vertex], expected_triangles: &[&[u8; 3]]) {
    let actual_triangles = run(vertices);

    // convert to &str for helpful printing
    pretty_assertions::assert_eq!(
        actual_triangles
            .iter()
            .map(|byte_arr| str::from_utf8(byte_arr).unwrap())
            .collect::<Vec<&str>>(),
        expected_triangles
            .iter()
            .map(|&byte_arr| str::from_utf8(byte_arr).unwrap())
            .collect::<Vec<&str>>(),
        "actual triangles != expected triangles"
    );
}

fn vert(x: i32, y: i32, z: i32, connectivity: Mask, index: u8) -> planar::Vertex {
    planar::Vertex {
        position: point3(x, y, z),
        connectivity,
        index: u32::from(index),
    }
}

/// Given a byte array of ASCII art polygons drawn using:
///
/// * letters or '*' for vertices,
/// * `-` and `|` for edges,
/// * and `.` for interior fill,
///
/// produce triangulator input vertices compatible with [`test_basis()`].
///
/// The `-` and `|` are not currently used or validated; only `.` vs ` ` near vertices determines
/// connectivity and thus whether an area is interior.
///
/// `*` vertices are automatically assigned indices.
fn vertices_from_ascii_art<const W: usize, const H: usize>(
    ascii_art: [&[u8; W]; H],
) -> Vec<planar::Vertex> {
    let mut output = Vec::new();
    for x in 0..W {
        for y in (0..H).into_iter().rev() {
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
                            let mut mask = Mask::EMPTY;
                            // note Y flip
                            if is_interior(1, -1) {
                                mask |= Mask::FSFP;
                            }
                            if is_interior(1, 1) {
                                mask |= Mask::FSBP;
                            }
                            if is_interior(-1, -1) {
                                mask |= Mask::BSFP;
                            }
                            if is_interior(-1, 1) {
                                mask |= Mask::BSBP;
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

// -------------------------------------------------------------------------------------------------

#[test]
fn empty() {
    check(&[], &[]);
}

#[test]
fn one_quad() {
    let vertices = vertices_from_ascii_art([
        b"B--D", //
        b"|..|", //
        b"A--C", //
    ]);

    // Meta-test of vertices_from_ascii_art()
    assert_eq!(
        vertices,
        [
            vert(0, 0, 0, Mask::FSFP, b'A'),
            vert(0, 2, 0, Mask::FSBP, b'B'),
            vert(3, 0, 0, Mask::BSFP, b'C'),
            vert(3, 2, 0, Mask::BSBP, b'D'),
        ]
    );

    check(&vertices, &[b"ACB", b"DBC"]);
}

#[test]
fn two_consecutive_quads() {
    check(
        &vertices_from_ascii_art([
            b"B-D b-d", //
            b"|.| |.|", //
            b"A-C a-c", //
        ]),
        &[
            b"ACB", b"DBC", // first quad
            b"acb", b"dbc", // second quad
        ],
    );
}

#[test]
fn quad_with_extra_vertex_back() {
    check(
        &vertices_from_ascii_art([
            b"B-D", //
            b"|.|", //
            b"X.|", //
            b"|.|", //
            b"A-C", //
        ]),
        &[
            b"ACX", // bottom left triangle
            b"XCB", // middle triangle
            b"DBC", // top right triangle
        ],
    );
}

#[test]
fn quad_with_extra_vertex_front() {
    check(
        &vertices_from_ascii_art([
            b"B-D", //
            b"|.|", //
            b"|.X", //
            b"|.|", //
            b"A-C", //
        ]),
        &[
            b"ACB", // bottom left triangle
            b"CXB", // middle triangle
            b"DBX", // top right triangle
        ],
    );
}

#[test]
fn quad_with_extra_vertex_perp_front() {
    check(
        &vertices_from_ascii_art([
            b"B-X-D", //
            b"|...|", //
            b"A---C", //
        ]),
        &[
            b"XBA", // top left triangle
            b"ACX", // middle triangle for bottom edge
            b"DXC", // top right triangle
        ],
    );
}

#[test]
fn quad_with_extra_vertex_perp_back() {
    check(
        &vertices_from_ascii_art([
            b"B---D", //
            b"|...|", //
            b"A-X-C", //
        ]),
        &[
            b"AXB", // bottom left triangle
            b"XCB", // middle triangle with right half of bottom edge
            b"DBC", // bottom right triangle
        ],
    );
}

#[test]
fn hole() {
    check(
        &vertices_from_ascii_art([
            b"B-----D", //
            b"|.....|", //
            b"|.b-d.|", //
            b"|.| |.|", //
            b"|.a-c.|", //
            b"|.....|", //
            b"A-----C", //
        ]),
        &[
            b"AaB", // outer  left  edge & a
            b"abB", // inner  left  edge & B
            b"caA", // inner bottom edge & A
            b"bdB", // inner  top   edge & B
            b"ACc", // outer  top   edge & c
            b"cCd", // inner right  edge & C
            b"DBd", // outer  top   edge & d
            b"DdC", // outer right  edge & d
        ],
    );
}

/// With extra vertices X and Y, correctly processing holes starts to require careful avoidance of
/// reversed triangles spanning gaps, which then need to invoke the ear clipping sub-algorithm
/// to fill in the avoided areas.
#[test]
fn hole_requiring_ear_clipping() {
    check(
        &vertices_from_ascii_art([
            b"B-----D", //
            b"|.....|", //
            b"Y.....|", //
            b"|.....|", //
            b"|.b-d.|", //
            b"|.| |.|", //
            b"|.a-c.|", //
            b"|.....|", //
            b"X.....|", //
            b"|.....|", //
            b"A-----C", //
        ]),
        &[
            b"XaY", // outer left edge middle segment
            b"abY", // inner left edge
            b"caX", // inner bottom edge
            b"bdY", // inner top edge
            b"ACX", // inner bottom edge
            b"XCc", // bottom interior area
            b"cCd", // inner right edge
            b"DBY", // outer top edge & outer left edge, top segment
            b"DYd", // top interior interior
            b"DdC", // outer right edge
        ],
    );
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn duplicate_vertices_0() {
    run(&[
        vert(0, 0, 0, Mask::FSFP, b'A'),
        vert(0, 0, 0, Mask::FSFP, b'X'), // spurious
        vert(0, 2, 0, Mask::FSBP, b'B'),
        vert(3, 0, 0, Mask::BSFP, b'C'),
        vert(3, 2, 0, Mask::BSBP, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn duplicate_vertices_1() {
    run(&[
        vert(0, 0, 0, Mask::FSFP, b'A'),
        vert(0, 2, 0, Mask::FSBP, b'B'),
        vert(0, 2, 0, Mask::FSFP, b'X'), // spurious
        vert(3, 0, 0, Mask::BSFP, b'C'),
        vert(3, 2, 0, Mask::BSBP, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn duplicate_vertices_2() {
    run(&[
        vert(0, 0, 0, Mask::FSFP, b'A'),
        vert(0, 2, 0, Mask::FSBP, b'B'),
        vert(3, 0, 0, Mask::BSFP, b'C'),
        vert(3, 0, 0, Mask::FSFP, b'X'), // spurious
        vert(3, 2, 0, Mask::BSBP, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn duplicate_vertices_3() {
    run(&[
        vert(0, 0, 0, Mask::FSFP, b'A'),
        vert(0, 2, 0, Mask::FSBP, b'B'),
        vert(3, 0, 0, Mask::BSFP, b'C'),
        vert(3, 2, 0, Mask::BSBP, b'D'),
        vert(3, 2, 0, Mask::FSFP, b'X'), // spurious
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn missing_vertices_0() {
    run(&[
        vert(0, 2, 0, Mask::FSBP, b'B'),
        vert(3, 0, 0, Mask::BSFP, b'C'),
        vert(3, 2, 0, Mask::BSBP, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn missing_vertices_1() {
    run(&[
        vert(0, 0, 0, Mask::FSFP, b'A'),
        vert(3, 0, 0, Mask::BSFP, b'C'),
        vert(3, 2, 0, Mask::BSBP, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn missing_vertices_2() {
    run(&[
        vert(0, 0, 0, Mask::FSFP, b'A'),
        vert(0, 2, 0, Mask::FSBP, b'B'),
        vert(3, 2, 0, Mask::BSBP, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn missing_vertices_3() {
    run(&[
        vert(0, 0, 0, Mask::FSFP, b'A'),
        vert(0, 2, 0, Mask::FSBP, b'B'),
        vert(3, 0, 0, Mask::BSFP, b'C'),
    ]);
}

// TODO(planar_new): add tests of further complex cases, such as the ones that require
// the ear-clipping step

// -------------------------------------------------------------------------------------------------

/// Regenerates the image embedded in the [`crate::planar`] documentation.
#[test]
fn doc_example_svg_test() {
    let vertices = &vertices_from_ascii_art([
        b"  *-*   *-* *---*  ", //
        b"  |.|   |.| |...|  ", //
        b"*-*-*-* |.| |.*-*-*", //
        b"|.| |.| |.| |.| |.|", //
        b"|.*-*.| |.| *.| *-*", //
        b"|.....| |.| |.|    ", //
        b"|.*-*.| |.| |.| *-*", //
        b"|.| |.| |.| |.| |.|", //
        b"|.| |.| |.| |.*-*-*", //
        b"|.| |.| |.| |...|  ", //
        b"*-* *-* *-* *---*  ", //
    ]);

    let mut triangles = Vec::new();
    planar::Triangulator::new().triangulate(test_basis(), vertices.iter().copied(), |triangle| {
        triangles.push(triangle)
    });

    let svg = format!(
        "{}",
        planar::svg::WriteSvg {
            vertices,
            triangles: &triangles,
            scale: 30.0,
            show_vertices: true,
            standalone_xml: false,
        }
    );

    // Clean, unquoted copy to paste into the file when it needs updating.
    println!("{svg}");

    pretty_assertions::assert_eq!(svg, include_str!("example.svg"));
}

// -------------------------------------------------------------------------------------------------
// Tests of internals that are harder to test as part of the larger algorithm.

#[test]
fn winding() {
    // Set up right-handed and left-handed in the sense that matters here,
    // which is *not* which way the plane normal is pointing but
    let xy_rh_basis = planar::Basis::new(Face6::PZ, Face6::PX, Face6::PY);
    let xy_lh_basis = planar::Basis::new(Face6::NZ, Face6::PX, Face6::PY);
    let yx_rh_basis = planar::Basis::new(Face6::NZ, Face6::PY, Face6::PX);
    let yx_lh_basis = planar::Basis::new(Face6::PZ, Face6::PY, Face6::PX);
    assert_eq!(xy_rh_basis.left_handed, false, "xy_rh_basis");
    assert_eq!(xy_lh_basis.left_handed, true, "xy_lh_basis");
    assert_eq!(yx_rh_basis.left_handed, false, "yx_rh_basis");
    assert_eq!(yx_lh_basis.left_handed, true, "yx_lh_basis");

    let try_all = |triangle: [&planar::Vertex; 3]| -> [bool; 4] {
        [
            xy_rh_basis.is_correct_winding(triangle),
            xy_lh_basis.is_correct_winding(triangle),
            yx_rh_basis.is_correct_winding(triangle),
            yx_lh_basis.is_correct_winding(triangle),
        ]
    };

    // Some vertices to make both degenerate and non-degenerate triangles
    let vertices = vertices_from_ascii_art([
        b"b  ", //
        b"   ", //
        b" c ", //
        b"   ", //
        b"a d", //
    ]);
    dbg!(&vertices);
    let [a, b, c, d] = &*vertices else {
        unreachable!()
    };

    assert_eq!(
        try_all([a, b, c]),
        [false, false, true, true],
        "clockwise in +x+y"
    );
    assert_eq!(
        try_all([a, c, b]),
        [true, true, false, false],
        "counterclockwise in +x+y"
    );
    assert_eq!(
        try_all([b, c, d]),
        [false, false, false, false],
        "degenerate"
    );
}

use alloc::format;
use alloc::vec::Vec;
use std::println;

use all_is_cubes::math::GridPoint;

use crate::planar::testing::{test_basis, vert, vertices_from_ascii_art};
use crate::planar::{self, Mask};

// -------------------------------------------------------------------------------------------------

/// `Triangulator::triangulate()` parameterized for simplicity
/// (for tests that aren't trying to exercise rotatability).
#[inline(never)]
#[track_caller]
fn run(vertices: &[planar::Vertex]) -> Vec<[u8; 3]> {
    let mut actual_triangles = Vec::new();
    let mut triangulator = planar::Triangulator::new();
    let basis = test_basis();

    println!("Initial state: {triangulator:#?}");

    let maybe_panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        triangulator.triangulate(
            basis,
            vertices.iter().copied().inspect(|planar_vertex| {
                println!("In: {planar_vertex:?}");
            }),
            |triangle_indices: [u32; 3]| -> Result<(), crate::OutOfMemory> {
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

                Ok(())
            },
        )
    }));

    println!("Final state: {triangulator:#?}");

    println!(
        "\n{}\n",
        planar::svg::WriteSvg {
            vertices,
            loops: &actual_triangles
                .iter()
                .map(|byte_arr| byte_arr.map(u32::from))
                .collect::<Vec<[u32; 3]>>(),
            scale: 20.0,
            show_vertices: true,
            standalone_xml: true,
        }
    );

    if let Err(payload) = maybe_panic {
        println!("Unwinding; triangles before panic: {actual_triangles:#?}");

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

// -------------------------------------------------------------------------------------------------

#[test]
fn empty() {
    check(&[], &[]);
}

#[test]
fn one_quad() {
    check(
        &vertices_from_ascii_art([
            b"B--D", //
            b"|..|", //
            b"A--C", //
        ]),
        &[b"ACB", b"DBC"],
    );
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
        vert(0, 0, 0, Mask::Fsfp, b'A'),
        vert(0, 0, 0, Mask::Fsfp, b'X'), // spurious
        vert(0, 2, 0, Mask::Fsbp, b'B'),
        vert(3, 0, 0, Mask::Bsfp, b'C'),
        vert(3, 2, 0, Mask::Bsbp, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn duplicate_vertices_1() {
    run(&[
        vert(0, 0, 0, Mask::Fsfp, b'A'),
        vert(0, 2, 0, Mask::Fsbp, b'B'),
        vert(0, 2, 0, Mask::Fsfp, b'X'), // spurious
        vert(3, 0, 0, Mask::Bsfp, b'C'),
        vert(3, 2, 0, Mask::Bsbp, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn duplicate_vertices_2() {
    run(&[
        vert(0, 0, 0, Mask::Fsfp, b'A'),
        vert(0, 2, 0, Mask::Fsbp, b'B'),
        vert(3, 0, 0, Mask::Bsfp, b'C'),
        vert(3, 0, 0, Mask::Fsfp, b'X'), // spurious
        vert(3, 2, 0, Mask::Bsbp, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn duplicate_vertices_3() {
    run(&[
        vert(0, 0, 0, Mask::Fsfp, b'A'),
        vert(0, 2, 0, Mask::Fsbp, b'B'),
        vert(3, 0, 0, Mask::Bsfp, b'C'),
        vert(3, 2, 0, Mask::Bsbp, b'D'),
        vert(3, 2, 0, Mask::Fsfp, b'X'), // spurious
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn missing_vertices_0() {
    run(&[
        vert(0, 2, 0, Mask::Fsbp, b'B'),
        vert(3, 0, 0, Mask::Bsfp, b'C'),
        vert(3, 2, 0, Mask::Bsbp, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn missing_vertices_1() {
    run(&[
        vert(0, 0, 0, Mask::Fsfp, b'A'),
        vert(3, 0, 0, Mask::Bsfp, b'C'),
        vert(3, 2, 0, Mask::Bsbp, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn missing_vertices_2() {
    run(&[
        vert(0, 0, 0, Mask::Fsfp, b'A'),
        vert(0, 2, 0, Mask::Fsbp, b'B'),
        vert(3, 2, 0, Mask::Bsbp, b'D'),
    ]);
}

#[test]
#[should_panic = "input vertices erroneous or triangulator has a bug;"]
fn missing_vertices_3() {
    run(&[
        vert(0, 0, 0, Mask::Fsfp, b'A'),
        vert(0, 2, 0, Mask::Fsbp, b'B'),
        vert(3, 0, 0, Mask::Bsfp, b'C'),
    ]);
}

// TODO(planar_new): add tests of further complex cases, such as the ones that require
// the ear-clipping step

// -------------------------------------------------------------------------------------------------

/// Regenerates the image embedded in the [`planar::Triangulator`] documentation.
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
    planar::Triangulator::new()
        .triangulate(
            test_basis(),
            vertices.iter().copied(),
            |triangle| -> Result<(), crate::OutOfMemory> {
                triangles.push(triangle);
                Ok(())
            },
        )
        .unwrap();

    let svg = format!(
        "{}",
        planar::svg::WriteSvg {
            vertices,
            loops: &triangles,
            scale: 30.0,
            show_vertices: true,
            standalone_xml: false,
        }
    );

    // Clean, unquoted copy to paste into the file when it needs updating.
    println!("{svg}");

    pretty_assertions::assert_eq!(svg, include_str!("triangulator_example.svg"));
}

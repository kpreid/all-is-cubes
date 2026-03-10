use super::*;
use all_is_cubes::euclid::point3;
use all_is_cubes::math::GridPoint;
use alloc::vec::Vec;

fn test_basis() -> Basis {
    let b = Basis::new(Face6::PZ, Face6::PX, Face6::PY);
    assert!(!b.left_handed); // TODO: could use tests that *are* left-handed
    b
}

/// `Triangulator::triangulate()` parameterized for simplicity
/// (for tests that aren't trying to exercise rotatability)
#[inline(never)]
#[track_caller]
fn check(vertices: &[(GridPoint, Mask)], expected_triangles: &[[GridPoint; 3]]) {
    let mut viz = Viz::Disabled;
    let mut actual_triangles = Vec::new();
    let mut triangulator = Triangulator::new();
    let basis = test_basis();

    eprintln!("Initial state: {triangulator:#?}");

    triangulator.triangulate(
        &mut viz,
        basis,
        vertices.iter().zip(0u32..).map(|(&(position, connectivity), index)| {
            let planar_vertex = Vertex {
                position,
                connectivity,
                index,
            };
            println!("In: {planar_vertex:?}");
            planar_vertex
        }),
        |triangle_indices: [u32; 3]| {
            let triangle_positions: [GridPoint; 3] =
                triangle_indices.map(|index| vertices[index as usize].0);
            // Print as we go, so if there is a panic we can still see some results.
            println!("Out: {triangle_indices:?} = {triangle_positions:?}");
            actual_triangles.push(triangle_positions);

            // println!("State: {triangulator:#?}"); // TODO: make it possible to grab the state for debugging
        },
    );

    eprintln!("Final state: {triangulator:#?}");
    pretty_assertions::assert_eq!(
        expected_triangles,
        actual_triangles.as_slice(),
        "triangles not as expected"
    );
}

fn vert(x: i32, y: i32, z: i32, mask: Mask) -> (GridPoint, Mask) {
    (point3(x, y, z), mask)
}

#[test]
fn empty() {
    check(&[], &[]);
}

#[test]
fn one_quad() {
    check(
        &[
            vert(0, 0, 0, Mask::FSFP),
            vert(0, 3, 0, Mask::FSBP),
            vert(2, 0, 0, Mask::BSFP),
            vert(2, 3, 0, Mask::BSBP),
        ],
        &[
            [point3(0, 0, 0), point3(2, 0, 0), point3(0, 3, 0)],
            [point3(2, 3, 0), point3(0, 3, 0), point3(2, 0, 0)],
        ],
    );
}

#[test]
fn two_consecutive_quads() {
    check(
        &[
            // first quad
            vert(0, 0, 0, Mask::FSFP),
            vert(0, 1, 0, Mask::FSBP),
            vert(1, 0, 0, Mask::BSFP),
            vert(1, 1, 0, Mask::BSBP),
            // second quad
            vert(2, 0, 0, Mask::FSFP),
            vert(2, 1, 0, Mask::FSBP),
            vert(3, 0, 0, Mask::BSFP),
            vert(3, 1, 0, Mask::BSBP),
        ],
        &[
            [point3(0, 0, 0), point3(1, 0, 0), point3(0, 1, 0)],
            [point3(1, 1, 0), point3(0, 1, 0), point3(1, 0, 0)],
            [point3(2, 0, 0), point3(3, 0, 0), point3(2, 1, 0)],
            [point3(3, 1, 0), point3(2, 1, 0), point3(3, 0, 0)],
        ],
    );
}

#[test]
fn quad_with_extra_vertex_back() {
    check(
        &[
            vert(0, 0, 0, Mask::FSFP),
            vert(0, 1, 0, Mask::FSFP | Mask::FSBP), // extra vertex
            vert(0, 2, 0, Mask::FSBP),
            vert(1, 0, 0, Mask::BSFP),
            vert(1, 2, 0, Mask::BSBP),
        ],
        &[
            [point3(0, 0, 0), point3(1, 0, 0), point3(0, 1, 0)], // bottom left triangle
            [point3(0, 1, 0), point3(1, 0, 0), point3(0, 2, 0)], // middle triangle
            [point3(1, 2, 0), point3(0, 2, 0), point3(1, 0, 0)], // top right triangle
        ],
    );
}
// TODO(planar_new): add 3 more tests for extra vertices on all 4 edges

// TODO(planar_new): add tests of polygon with holes and other complex cases
// TODO(planar_new): add tests of obscured faces not being generated

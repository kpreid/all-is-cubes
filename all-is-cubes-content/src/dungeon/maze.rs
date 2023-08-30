use maze_generator::prelude::{Direction, Field, Maze};

use all_is_cubes::math::{Cube, Face6, GridAab, GridArray};

pub fn maze_to_array(maze: &Maze) -> GridArray<Field> {
    GridArray::from_fn(
        GridAab::from_lower_size([0, 0, 0], [maze.size.0, 1, maze.size.1]),
        |p| maze.get_field(&c2m(p)).unwrap(),
    )
}

pub fn c2m(cube: Cube) -> maze_generator::prelude::Coordinates {
    maze_generator::prelude::Coordinates {
        x: cube.x,
        y: cube.z,
    }
}

pub fn f2d(face: Face6) -> Option<Direction> {
    Some(match face {
        Face6::NZ => Direction::North,
        Face6::PX => Direction::East,
        Face6::PZ => Direction::South,
        Face6::NX => Direction::West,
        Face6::NY | Face6::PY => return None,
    })
}

use maze_generator::prelude::{Direction, Field, Maze};

use all_is_cubes::math::{Face6, GridAab, GridArray, GridPoint};

pub fn maze_to_array(maze: &Maze) -> GridArray<Field> {
    GridArray::from_fn(
        GridAab::from_lower_size([0, 0, 0], [maze.size.0, 1, maze.size.1]),
        |p| maze.get_field(&gp2m(p)).unwrap(),
    )
}

pub fn m2gp(p: maze_generator::prelude::Coordinates) -> GridPoint {
    GridPoint::new(p.x, 0, p.y)
}

pub fn gp2m(p: GridPoint) -> maze_generator::prelude::Coordinates {
    maze_generator::prelude::Coordinates { x: p.x, y: p.z }
}

pub fn d2f(direction: Direction) -> Face6 {
    match direction {
        Direction::North => Face6::NZ,
        Direction::East => Face6::PX,
        Direction::South => Face6::PZ,
        Direction::West => Face6::NX,
    }
}

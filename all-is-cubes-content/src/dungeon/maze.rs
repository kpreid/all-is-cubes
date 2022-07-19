// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use maze_generator::prelude::{Direction, Field, Maze};

use all_is_cubes::math::{Face7, Grid, GridArray, GridPoint};

pub fn maze_to_array(maze: &Maze) -> GridArray<Field> {
    GridArray::from_fn(Grid::new([0, 0, 0], [maze.size.0, 1, maze.size.1]), |p| {
        maze.get_field(&gp2m(p)).unwrap()
    })
}

pub fn m2gp(p: maze_generator::prelude::Coordinates) -> GridPoint {
    GridPoint::new(p.x, 0, p.y)
}

pub fn gp2m(p: GridPoint) -> maze_generator::prelude::Coordinates {
    maze_generator::prelude::Coordinates { x: p.x, y: p.z }
}

pub fn d2f(direction: Direction) -> Face7 {
    match direction {
        Direction::North => Face7::NZ,
        Direction::East => Face7::PX,
        Direction::South => Face7::PZ,
        Direction::West => Face7::NX,
    }
}

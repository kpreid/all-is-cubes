// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use maze_generator::prelude::{Direction, Field, Maze};

use all_is_cubes::math::{Face, GridPoint};
use all_is_cubes::space::{Grid, GridArray};

pub fn maze_to_array(maze: &Maze) -> GridArray<Field> {
    GridArray::from_fn(Grid::new([0, 0, 0], [maze.size.0, 1, maze.size.1]), |p| {
        maze.get_field(&gp2m(p)).unwrap()
    })
}

#[allow(dead_code)]
fn m2gp(p: maze_generator::prelude::Coordinates) -> GridPoint {
    GridPoint::new(p.x, 0, p.y)
}

fn gp2m(p: GridPoint) -> maze_generator::prelude::Coordinates {
    maze_generator::prelude::Coordinates { x: p.x, y: p.z }
}

pub fn d2f(direction: Direction) -> Face {
    match direction {
        Direction::North => Face::NZ,
        Direction::East => Face::PX,
        Direction::South => Face::PZ,
        Direction::West => Face::NX,
    }
}

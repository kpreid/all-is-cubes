// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Simple exercise of all_is_cubes::console.

use cgmath::Vector2;
use std::io;
use std::thread::sleep;
use std::time::Duration;
use termion;

use all_is_cubes::block::make_some_blocks;
use all_is_cubes::space::{GridPoint, Space};
use all_is_cubes::console::{View, draw_space};

fn main() -> io::Result<()> {
    let blocks = make_some_blocks(10);

    let mut space = Space::empty_positive(10, 10, 10);
    for x in 0..10 {
        for z in 0..10 {
            space.set(GridPoint::new(x, 0, z), &blocks[x as usize]);
            space.set(GridPoint::new(x, x, z), &blocks[x as usize]);
        }
    }

    let mut view = View::for_grid(Vector2::new(80, 24), space.grid());

    print!("{}", termion::clear::All);
    loop {
        sleep(Duration::from_millis(50));
        view.yaw += 5.0;
        draw_space(&space, &view, &mut io::stdout())?;
        //return Ok(());
    }
}
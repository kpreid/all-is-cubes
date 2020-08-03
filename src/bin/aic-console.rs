// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Simple exercise of all_is_cubes::console.

use cgmath::Vector2;
use std::io;
use std::thread;
use std::sync::mpsc;
use std::time::Duration;
use termion;
use termion::event::{Event, Key};
use termion::raw::IntoRawMode;
use termion::input::TermRead;

use all_is_cubes::block::make_some_blocks;
use all_is_cubes::space::{GridPoint, Space};
use all_is_cubes::console::{View, draw_space, viewport_from_terminal_size};

/// TODO: break this up into testable library code insofar as feasible.

fn main() -> io::Result<()> {
    let blocks = make_some_blocks(10);

    let mut space = Space::empty_positive(10, 10, 10);
    for x in 0..10 {
        for z in 0..10 {
            space.set(GridPoint::new(x, 0, z), &blocks[x as usize]);
            space.set(GridPoint::new(x, x, z), &blocks[x as usize]);
        }
    }

    let mut view = View::for_grid(
        viewport_from_terminal_size()?,
        space.grid());
    let mut auto_rotate = true;

    let mut out = io::stdout().into_raw_mode()?;

    let (event_tx, event_rx) = mpsc::channel();
    thread::spawn(move || {
        for event_result in io::stdin().lock().events() {
            match event_result {
                Ok(event) => event_tx.send(event).unwrap(),
                Err(err) => {
                    eprintln!("stdin read error: {}", err);
                    break;
                },
            }
        }
        eprintln!("read thread exiting");
    });

    print!("{}", termion::clear::All);
    loop {
        thread::sleep(Duration::from_millis(50));

        'input: loop {
            match event_rx.try_recv() {
                Ok(event) => {
                    auto_rotate = false;
                    if let Some(Event::Key(key)) = view.controller(event) {
                        match key {
                            Key::Esc | Key::Ctrl('c') | Key::Ctrl('d') => {
                                return Ok(());
                            },
                            _ => {}
                        }
                    }
                },
                Err(mpsc::TryRecvError::Disconnected) => {
                    eprintln!("input disconnected");
                    return Ok(());
                }
                Err(mpsc::TryRecvError::Empty) => { break 'input; },
            }
        }

        if auto_rotate {
            view.yaw += 5.0;
        }

        draw_space(&space, &view, &mut out)?;
    }
}

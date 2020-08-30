// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Simple exercise of all_is_cubes::console.

use std::io;
use std::thread;
use std::sync::mpsc;
use std::time::{Instant};
use termion::event::{Event, Key};
use termion::raw::IntoRawMode;
use termion::input::TermRead;

use all_is_cubes::camera::{ProjectionHelper};
use all_is_cubes::demo_content::new_universe_with_stuff;
use all_is_cubes::universe::FrameClock;

use all_is_cubes_server::console::{controller, draw_space, viewport_from_terminal_size};

/// TODO: break this up into testable library code insofar as feasible.

fn main() -> io::Result<()> {
    let mut universe = new_universe_with_stuff();
    let camera_ref = universe.get_default_camera();
    let space_ref = universe.get_default_space();
    let mut proj: ProjectionHelper = ProjectionHelper::new(0.5, viewport_from_terminal_size()?);
    let mut out = io::stdout().into_raw_mode()?;

    (*camera_ref.borrow_mut()).auto_rotate = true;

    // Park stdin blocking reads on another thread.
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

    let mut frame_clock = FrameClock::new();
    loop {
        'input: loop {
            match event_rx.try_recv() {
                Ok(event) => {
                    if let Some(Event::Key(key)) = controller(&mut *camera_ref.borrow_mut(), event) {
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

        frame_clock.advance_to(Instant::now());
        if frame_clock.should_step() {
            universe.step(frame_clock.step_length());
            frame_clock.did_step();
        }
        if frame_clock.should_draw() {
            draw_space(&*space_ref.borrow(), &mut proj, &camera_ref.borrow(), &mut out)?;
            frame_clock.did_draw();
        }
    }
}

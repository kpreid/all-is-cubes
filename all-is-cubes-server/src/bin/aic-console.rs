// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Simple exercise of all_is_cubes::console.

use std::io;
use std::thread;
use std::sync::mpsc;
use std::time::Duration;
use termion::event::{Event, Key};
use termion::raw::IntoRawMode;
use termion::input::TermRead;

use all_is_cubes::camera::{ProjectionHelper};
use all_is_cubes::universe::Universe;

use all_is_cubes_server::console::{controller, draw_space, viewport_from_terminal_size};

/// TODO: break this up into testable library code insofar as feasible.

fn main() -> io::Result<()> {
    let mut universe = Universe::new_test_universe();
    let mut proj: ProjectionHelper = ProjectionHelper::new(0.5, viewport_from_terminal_size()?);
    let mut out = io::stdout().into_raw_mode()?;

    universe.camera_mut().auto_rotate = true;

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
    loop {
        // TODO: manage time steps correctly
        thread::sleep(Duration::from_millis(1000/30));
        let timestep = Duration::from_secs_f64(1.0/20.0);

        'input: loop {
            match event_rx.try_recv() {
                Ok(event) => {
                    if let Some(Event::Key(key)) = controller(&mut *universe.camera_mut(), event) {
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

        universe.step(timestep);
        draw_space(&*universe.space(), &mut proj, &universe.camera(), &mut out)?;
    }
}

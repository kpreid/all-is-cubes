// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Simple exercise of all_is_cubes::console.

use std::io;
use std::sync::mpsc;
use std::thread;
use std::time::Instant;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::camera::ProjectionHelper;

use all_is_cubes_server::console::{draw_space, map_termion_event, viewport_from_terminal_size};

/// TODO: break this up into testable library code insofar as feasible.

fn main() -> io::Result<()> {
    let mut app = AllIsCubesAppState::new();
    app.camera().borrow_mut().auto_rotate = true;

    let mut proj: ProjectionHelper = ProjectionHelper::new(0.5, viewport_from_terminal_size()?);
    let mut out = io::stdout().into_raw_mode()?;

    // Park stdin blocking reads on another thread.
    let (event_tx, event_rx) = mpsc::sync_channel(0);
    thread::spawn(move || {
        for event_result in io::stdin().lock().events() {
            match event_result {
                Ok(event) => event_tx.send(event).unwrap(),
                Err(err) => {
                    eprintln!("stdin read error: {}", err);
                    break;
                }
            }
        }
        eprintln!("read thread exiting");
    });

    print!("{}", termion::clear::All);

    loop {
        'input: loop {
            match event_rx.try_recv() {
                Ok(event) => {
                    if let Some(aic_event) = map_termion_event(&event) {
                        if app.input_processor.key_momentary(aic_event) {
                            // Handled by input_processor
                            continue 'input;
                        }
                    }
                    if let Event::Key(key) = event {
                        match key {
                            Key::Esc | Key::Ctrl('c') | Key::Ctrl('d') => {
                                return Ok(());
                            }
                            _ => {}
                        }
                    }
                }
                Err(mpsc::TryRecvError::Disconnected) => {
                    eprintln!("input disconnected");
                    return Ok(());
                }
                Err(mpsc::TryRecvError::Empty) => {
                    break 'input;
                }
            }
        }

        app.frame_clock.advance_to(Instant::now());
        app.maybe_step_universe();
        if app.frame_clock.should_draw() {
            draw_space(&mut proj, &*app.camera().borrow(), &mut out)?;
            app.frame_clock.did_draw();
        }
    }
}

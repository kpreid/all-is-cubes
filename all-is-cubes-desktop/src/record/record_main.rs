// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::io::Write;
use std::time::Duration;

use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};

use all_is_cubes::apps::{Session, StandardCameras};
use all_is_cubes::behavior::AutoRotate;
use all_is_cubes::listen::ListenableCell;
use all_is_cubes::math::NotNan;

use crate::record::{RecordOptions, Recorder};
use crate::session::{ClockSource, DesktopSession};

pub(crate) fn record_main(session: Session, options: RecordOptions) -> Result<(), anyhow::Error> {
    let progress_style = ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6}")
        .on_finish(ProgressFinish::AtCurrentPos);

    let mut stderr = std::io::stderr();

    // Modify graphics options to suit recording
    // TODO: Find a better place to put this policy, and in particular allow the user to
    // override it if they do want to record the UI.
    session
        .graphics_options_mut()
        .update_mut(|mut graphics_options| {
            graphics_options.show_ui = false;
            graphics_options.debug_info_text = false;
        });

    let viewport = options.viewport();

    if let Some(anim) = &options.animation {
        if let Some(character_ref) = session.character().snapshot() {
            // TODO: replace this with a general camera scripting mechanism
            character_ref.try_modify(|c| {
                c.add_behavior(AutoRotate {
                    rate: NotNan::new(360.0 / anim.total_duration().as_secs_f64()).unwrap(),
                })
            })?;
        }
    }

    let viewport_cell = ListenableCell::new(viewport);
    let (recorder, status_receiver) = Recorder::new(
        options.clone(),
        StandardCameras::from_session(&session, viewport_cell.as_source()).unwrap(),
    )?;
    let mut dsession = DesktopSession {
        session,
        renderer: (),
        window: (),
        viewport_cell,
        clock_source: ClockSource::Fixed(match &options.animation {
            Some(anim) => anim.frame_period,
            None => Duration::ZERO,
        }),
        recorder: Some(recorder),
    };

    // Use main thread for universe stepping, raytracer snapshotting, and progress updating.
    // (We could move the universe stepping to another thread to get more precise progress updates,
    // but that doesn't seem necessary.)
    {
        let drawing_progress_bar = ProgressBar::new(options.frame_range().size_hint().0 as u64)
            .with_style(progress_style)
            .with_prefix("Drawing");
        drawing_progress_bar.enable_steady_tick(1000);

        for _ in options.frame_range() {
            // Advance time for next frame.
            dsession.advance_time_and_maybe_step();

            // Update progress bar.
            if let Ok(frame_number) = status_receiver.try_recv() {
                drawing_progress_bar.set_position((frame_number + 1) as u64);
            }
        }
        dsession.recorder.as_mut().unwrap().no_more_frames();

        // We've completed sending frames; now block on their completion.
        while let Ok(frame_number) = status_receiver.recv() {
            drawing_progress_bar.set_position((frame_number + 1) as u64);
        }
        assert_eq!(
            drawing_progress_bar.position() as usize,
            options.frame_range().end() - options.frame_range().start() + 1,
            "Didn't draw the correct number of frames"
        );
    }

    // Report completion
    let _ = writeln!(stderr, "\nWrote {}", options.output_path.to_string_lossy());

    Ok(())
}

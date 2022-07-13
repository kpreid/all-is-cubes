// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::sync::mpsc;
use std::time::Duration;

use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};

use all_is_cubes::apps::{Session, StandardCameras};
use all_is_cubes::behavior::AutoRotate;
use all_is_cubes::camera::Viewport;
use all_is_cubes::listen::ListenableCell;
use all_is_cubes::math::NotNan;

use crate::record::{RecordOptions, Recorder};
use crate::session::{ClockSource, DesktopSession};

// TODO: the status_receiver passing is awkward. Maybe Recorder should just provide it as a broadcast output?

pub(crate) fn create_recording_session(
    session: Session,
    options: &RecordOptions,
    viewport_cell: ListenableCell<Viewport>,
) -> Result<(DesktopSession<(), ()>, mpsc::Receiver<usize>), anyhow::Error> {
    viewport_cell.set(options.viewport());
    let (recorder, status_receiver) = Recorder::new(
        options.clone(),
        StandardCameras::from_session(&session, viewport_cell.as_source()).unwrap(),
    )?;
    let dsession = DesktopSession {
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

    Ok((dsession, status_receiver))
}

pub(crate) fn record_main(
    mut dsession: DesktopSession<(), ()>,
    options: RecordOptions,
    status_receiver: mpsc::Receiver<usize>,
) -> Result<(), anyhow::Error> {
    let progress_style = ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6}")
        .on_finish(ProgressFinish::AtCurrentPos);

    // Modify graphics options to suit recording
    // TODO: Find a better place to put this policy, and in particular allow the user to
    // override it if they do want to record the UI.
    dsession
        .session
        .graphics_options_mut()
        .update_mut(|mut graphics_options| {
            graphics_options.show_ui = false;
            graphics_options.debug_info_text = false;
        });

    if let Some(anim) = &options.animation {
        if let Some(character_ref) = dsession.session.character().snapshot() {
            // TODO: replace this with a general camera scripting mechanism
            character_ref.try_modify(|c| {
                c.add_behavior(AutoRotate {
                    rate: NotNan::new(360.0 / anim.total_duration().as_secs_f64()).unwrap(),
                })
            })?;
        }
    }

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
    eprintln!("\nWrote {}", options.output_path.to_string_lossy());

    Ok(())
}

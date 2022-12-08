use std::sync::mpsc;
use std::time::Duration;

use indicatif::{ProgressBar, ProgressStyle};

use all_is_cubes::apps::{Session, StandardCameras};
use all_is_cubes::behavior::AutoRotate;
use all_is_cubes::camera::{Flaws, Viewport};
use all_is_cubes::listen::ListenableCell;
use all_is_cubes::math::NotNan;

use crate::record::{RecordOptions, Recorder, Status};
use crate::session::{ClockSource, DesktopSession};

// TODO: the status_receiver passing is awkward. Maybe Recorder should just provide it as a broadcast output?

pub(crate) fn create_recording_session(
    session: Session,
    options: &RecordOptions,
    viewport_cell: ListenableCell<Viewport>,
) -> Result<(DesktopSession<(), ()>, mpsc::Receiver<Status>), anyhow::Error> {
    viewport_cell.set(options.viewport());
    let (recorder, status_receiver) = Recorder::new(
        options.clone(),
        StandardCameras::from_session(&session, viewport_cell.as_source()).unwrap(),
    )?;

    let mut dsession = DesktopSession::new((), (), session, viewport_cell);
    dsession.clock_source = ClockSource::Fixed(match &options.animation {
        Some(anim) => anim.frame_period,
        None => Duration::ZERO,
    });
    dsession.recorder = Some(recorder);

    Ok((dsession, status_receiver))
}

pub(crate) fn record_main(
    mut dsession: DesktopSession<(), ()>,
    options: RecordOptions,
    status_receiver: mpsc::Receiver<Status>,
) -> Result<(), anyhow::Error> {
    let progress_style = ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6}")
        .unwrap();

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
    let mut flaws_total = Flaws::empty();
    {
        let drawing_progress_bar = ProgressBar::new(options.frame_range().size_hint().0 as u64)
            .with_style(progress_style)
            .with_prefix("Drawing");
        drawing_progress_bar.enable_steady_tick(Duration::from_secs(1));

        for _ in options.frame_range() {
            // Advance time for next frame.
            dsession.advance_time_and_maybe_step();

            // Update progress bar.
            if let Ok(Status {
                frame_number,
                flaws,
            }) = status_receiver.try_recv()
            {
                drawing_progress_bar.set_position((frame_number + 1) as u64);
                flaws_total |= flaws;
            }
        }
        dsession.recorder.as_mut().unwrap().no_more_frames();

        // We've completed sending frames; now block on their completion.
        // TODO: deduplicate receiving logic
        while let Ok(Status {
            frame_number,
            flaws,
        }) = status_receiver.recv()
        {
            drawing_progress_bar.set_position((frame_number + 1) as u64);
            flaws_total |= flaws;
        }
        assert_eq!(
            drawing_progress_bar.position() as usize,
            options.frame_range().end() - options.frame_range().start() + 1,
            "Didn't draw the correct number of frames"
        );
        drawing_progress_bar.finish();
    }

    // Report completion
    eprintln!("\nWrote {}", options.output_path.to_string_lossy());
    if flaws_total != Flaws::empty() {
        // TODO: write user-facing formatting for Flaws
        eprintln!("Flaws in recording: {:?}", flaws_total);
    }

    Ok(())
}

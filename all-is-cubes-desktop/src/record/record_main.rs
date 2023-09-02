use std::time::Duration;

use all_is_cubes::character::{self, Character};
use all_is_cubes::physics::BodyTransaction;
use all_is_cubes::time::Tick;
use all_is_cubes::{behavior, listen, universe};
use anyhow::Context;
use indicatif::{ProgressBar, ProgressStyle};

use all_is_cubes::camera::Flaws;
use all_is_cubes::listen::Listen as _;
use all_is_cubes::math::NotNan;

use crate::record::{RecordOptions, Status};
use crate::session::{ClockSource, DesktopSession};

/// Opinionated version of [`DesktopSession::start_recording`] that applies [`RecordOptions`]
/// to the session.
pub(crate) fn configure_session_for_recording<Ren, Win>(
    dsession: &mut DesktopSession<Ren, Win>,
    options: &RecordOptions,
    runtime_handle: &tokio::runtime::Handle,
) -> Result<(), anyhow::Error>
where
    Win: crate::glue::Window,
{
    dsession.viewport_cell.set(options.viewport());

    // Use fixed clock source.
    dsession.clock_source = ClockSource::Fixed(match &options.animation {
        Some(anim) => anim.frame_period,
        None => Duration::ZERO,
    });

    // Modify graphics options to suit recording
    // TODO: Find a better place to put this policy, and in particular allow the user to
    // override it if they do want to record the UI.
    dsession
        .session
        .graphics_options_mut()
        .update_mut(|graphics_options| {
            graphics_options.show_ui = false;
            graphics_options.debug_info_text = false;
        });

    // Add some motion to animation recordings.
    // TODO: replace this with a general camera scripting mechanism
    if let Some(anim) = &options.animation {
        if let Some(character_ref) = dsession.session.character().snapshot() {
            character_ref.try_modify(|c| {
                c.add_behavior(AutoRotate {
                    rate: NotNan::new(360.0 / anim.total_duration().as_secs_f64()).unwrap(),
                })
            })?;
        }
    }

    dsession.start_recording(runtime_handle, options)?;

    Ok(())
}

pub(crate) fn record_main(
    mut dsession: DesktopSession<(), ()>,
    options: RecordOptions,
    runtime_handle: &tokio::runtime::Handle,
) -> Result<(), anyhow::Error> {
    let progress_style = ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6}")
        .unwrap();

    // TODO: We should start recording independent of the main loop type being used
    configure_session_for_recording(&mut dsession, &options, runtime_handle)
        .context("failed to configure session for recording")?;

    let (status_tx, mut status_receiver) = tokio::sync::mpsc::unbounded_channel::<Status>();
    dsession
        .recorder
        .as_ref()
        .expect("record_main() requires a recorder present")
        .listen(ChannelListener::new(status_tx));

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
        while let Some(Status {
            frame_number,
            flaws,
        }) = status_receiver.blocking_recv()
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
        eprintln!("Flaws in recording: {flaws_total}");
    }

    Ok(())
}

/// A simple behavior which causes a `Character`'s viewpoint to rotate without user input,
/// currently used so that recording animations does more than nothing.
///
/// TODO: Replace this with a more general camera movement scripting mechanism.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct AutoRotate {
    pub rate: NotNan<f64>,
}
impl behavior::Behavior<character::Character> for AutoRotate {
    fn step(
        &self,
        c: &behavior::BehaviorContext<'_, Character>,
        tick: Tick,
    ) -> universe::UniverseTransaction {
        let mut body_txn = BodyTransaction::default();
        body_txn.delta_yaw = self.rate.into_inner() * tick.delta_t().as_secs_f64();
        c.bind_host(character::CharacterTransaction::body(body_txn))
    }

    fn alive(&self, _context: &behavior::BehaviorContext<'_, Character>) -> bool {
        true
    }

    fn persistence(&self) -> Option<behavior::BehaviorPersistence> {
        None
    }
}

impl universe::VisitRefs for AutoRotate {
    // No references
    fn visit_refs(&self, _visitor: &mut dyn universe::RefVisitor) {}
}

/// Adapt [`tokio::sync::mpsc::UnboundedSender`] to `Listener`.
///
/// Caution: If you care about when the channel is closed, check how long this listener
/// is going to live.
struct ChannelListener<M> {
    sender: tokio::sync::mpsc::UnboundedSender<M>,
}
impl<M: Send> ChannelListener<M> {
    fn new(sender: tokio::sync::mpsc::UnboundedSender<M>) -> Self {
        Self { sender }
    }
}
impl<M: Send> listen::Listener<M> for ChannelListener<M> {
    fn receive(&self, message: M) {
        _ = self.sender.send(message);
    }

    fn alive(&self) -> bool {
        !self.sender.is_closed()
    }
}

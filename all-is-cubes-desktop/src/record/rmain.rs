use std::fmt;
use std::time::Duration;

use all_is_cubes::character::{self, Character};
use all_is_cubes::math::NotNan;
use all_is_cubes::physics::BodyTransaction;
use all_is_cubes::universe::Handle;
use all_is_cubes::{behavior, listen, universe};

use crate::record::RecordOptions;
use crate::session::{ClockSource, DesktopSession};

/// Use [`RecordOptions`] to configure `dsession`'s clock and graphics options.
///
/// TODO: Change things around so that this can be done by the main task as needed.
#[allow(clippy::unnecessary_wraps)]
pub(crate) fn configure_session_for_recording<Ren, Win>(
    dsession: &mut DesktopSession<Ren, Win>,
    options: &RecordOptions,
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

    Ok(())
}

pub(crate) fn configure_universe_for_recording(
    character_handle: Option<&Handle<Character>>,
    options: &RecordOptions,
) {
    // Add some motion to animation recordings.
    // TODO: replace this with a general camera scripting mechanism
    if let Some(character_handle) = character_handle {
        if let Some(anim) = &options.animation {
            character_handle
                .try_modify(|c| {
                    c.add_behavior(AutoRotate {
                        rate: NotNan::new(360.0 / anim.total_duration().as_secs_f64()).unwrap(),
                    })
                })
                .unwrap();
        }
    } else {
        log::warn!("Recording universe contains no character.");
    }
}

/// A simple behavior which causes a `Character`'s viewpoint to rotate without user input,
/// currently used so that recording animations does more than nothing.
///
/// TODO: Replace this with a more general camera movement scripting mechanism.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct AutoRotate {
    pub rate: NotNan<f64>,
}
impl behavior::Behavior<Character> for AutoRotate {
    fn step(
        &self,
        context: &behavior::BehaviorContext<'_, Character>,
    ) -> (universe::UniverseTransaction, behavior::Then) {
        let mut body_txn = BodyTransaction::default();
        body_txn.delta_yaw = self.rate.into_inner() * context.tick.delta_t().as_secs_f64();
        (
            context.bind_host(character::CharacterTransaction::body(body_txn)),
            behavior::Then::Step,
        )
    }

    fn persistence(&self) -> Option<behavior::BehaviorPersistence> {
        None
    }
}

impl universe::VisitHandles for AutoRotate {
    // No handles
    fn visit_handles(&self, _visitor: &mut dyn universe::HandleVisitor) {}
}

/// Adapt [`tokio::sync::mpsc::UnboundedSender`] to `Listener`.
///
/// Caution: If you care about when the channel is closed, check how long this listener
/// is going to live.
pub(super) struct ChannelListener<M> {
    sender: tokio::sync::mpsc::UnboundedSender<M>,
}
impl<M: Send + Clone> ChannelListener<M> {
    pub fn new(sender: tokio::sync::mpsc::UnboundedSender<M>) -> Self {
        Self { sender }
    }
}

impl<M> fmt::Debug for ChannelListener<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ChannelListener")
            .field("is_closed", &self.sender.is_closed())
            .finish_non_exhaustive()
    }
}

impl<M: Send + Clone> listen::Listener<M> for ChannelListener<M> {
    fn receive(&self, messages: &[M]) -> bool {
        if messages.is_empty() {
            return !self.sender.is_closed();
        }
        for message in messages {
            match self.sender.send(message.clone()) {
                Ok(()) => {}
                Err(tokio::sync::mpsc::error::SendError(_)) => {
                    return false;
                }
            }
        }
        true
    }
}

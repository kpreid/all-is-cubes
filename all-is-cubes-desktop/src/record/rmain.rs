use std::fmt;
use std::time::Duration;

use all_is_cubes::character::Character;
use all_is_cubes::listen;
use all_is_cubes::universe::{Handle, UniverseTransaction};
use all_is_cubes_ui::settings;

use crate::record::RecordOptions;
use crate::session::{ClockSource, DesktopSession};

/// Use [`RecordOptions`] to configure `dsession`'s clock and graphics options.
///
/// TODO: Change things around so that this can be done by the main task as needed.
#[expect(clippy::unnecessary_wraps)]
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
    let settings = dsession.session.settings();
    // TODO: instead of blanket disinheriting, there should be a way to set explicitly ephemeral
    // settings.
    settings.disinherit();
    settings.set(settings::SHOW_UI, false);
    settings.set(settings::DEBUG_INFO_TEXT, false);

    Ok(())
}

pub(crate) fn configure_universe_for_recording(
    character_handle: Option<&Handle<Character>>,
    _options: &RecordOptions,
) -> UniverseTransaction {
    // TODO(ecs): this is where we used to add camera movement animation and it should be put back
    if character_handle.is_none() {
        log::warn!("Recording universe contains no character.");
    }
    UniverseTransaction::default()
}

/// Adapt [`async_channel::Sender`] to [`Listener`][listen::Listener].
///
/// The channel *must* be unbounded.
///
/// Caution: If you care about when the channel is closed, check how long this listener
/// is going to live.
pub(super) struct ChannelListener<M> {
    sender: async_channel::Sender<M>,
}
impl<M: Send + Clone> ChannelListener<M> {
    /// Panics if the channel is not unbounded.
    pub fn new(sender: async_channel::Sender<M>) -> Self {
        assert_eq!(sender.capacity(), None);
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
            match self.sender.try_send(message.clone()) {
                Ok(()) => {}
                Err(async_channel::TrySendError::Closed(_)) => {
                    return false;
                }
                Err(async_channel::TrySendError::Full(_)) => {
                    unreachable!();
                }
            }
        }
        true
    }
}

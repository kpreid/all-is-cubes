//! Types used to create notifications displayed to the user.

use alloc::sync::{Arc, Weak};
use alloc::vec::Vec;
use std::sync::Mutex;

use all_is_cubes::listen;

#[cfg(doc)]
use crate::apps::{MainTaskContext, Session};
use crate::vui::widgets::ProgressBarState;

// ---- Types --------------------------------------------------------------------------------------

/// User-visible contents of a [`Notification`].
///
/// This value type is cheap to clone and comparing it compares the entire content.
// TODO: don't expose this enum directly, for future-proofing
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
#[expect(clippy::module_name_repetitions)] // TODO: rename?
pub enum NotificationContent {
    // TODO: Not implemented:
    // /// The message may be multi-line.
    // Message(ArcStr),
    Progress(ProgressBarState),
}

// impl From<ArcStr> for NotificationContent {
//     fn from(message: ArcStr) -> Self {
//         Self::Message(message)
//     }
// }

/// A piece of information for the user's attention.
///
/// The carried message is displayed until the user dismisses it,
/// or this [`Notification`] value is dropped.
///
/// To create and display a notification, call [`Session::show_notification()`] or
/// [`MainTaskContext::show_notification()`].
#[derive(Debug)]
pub struct Notification {
    shared: Arc<Shared>,
}

/// Reasons a notification could not be created.
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum Error {
    /// no UI is available to display a notification
    NoUi,
    /// too many notifications
    Overflow,
}

/// Data shared between [`Notification`] and [`Receiver`].
#[derive(Debug)]
struct Shared {
    content: Mutex<NotificationContent>,
    notifier: listen::Notifier<()>,
}

/// Receiving end of a [`Notification`] channel, owned by [`Hub`].
#[derive(Debug)]
pub(crate) struct Receiver {
    shared: Weak<Shared>,
}

/// Collects input from [`Notification`]s to determine what should be displayed to the user.
#[derive(Debug)]
pub(crate) struct Hub {
    notifications: Vec<Receiver>,

    /// TODO: kludge to get progress UI up and going; eventually everything should be more dynamic
    /// and be able to display however many progress bars and messages.
    primary_progress: listen::ListenableCell<ProgressBarState>,

    has_interrupt: bool,
}

// --- Implementations -----------------------------------------------------------------------------

impl Notification {
    pub(crate) fn new(content: NotificationContent) -> (Self, Receiver) {
        let shared = Arc::new(Shared {
            content: Mutex::new(content),
            notifier: listen::Notifier::new(),
        });

        let receiver = Receiver {
            shared: Arc::downgrade(&shared),
        };
        let notif = Notification { shared };
        (notif, receiver)
    }

    /// Replace the existing content of the notification.
    pub fn set_content(&self, content: NotificationContent) {
        *self.shared.content.lock().unwrap() = content;
        self.shared.notifier.notify(&());
    }
}

impl Receiver {
    /// Returns the current content of this notification.
    ///
    /// Returns `None` if the notification was dropped or its state became poisoned.
    /// In that case, this [`Receiver`] should be discarded.
    pub(crate) fn read_content(&self) -> Option<NotificationContent> {
        let shared = self.shared.upgrade()?;
        let content = shared.content.lock().ok()?.clone();
        Some(content)
    }
}

impl Hub {
    pub fn new() -> Self {
        Self {
            notifications: Vec::new(),
            primary_progress: listen::ListenableCell::new(ProgressBarState::new(0.0)),
            has_interrupt: false,
        }
    }

    pub(crate) fn update(&mut self) {
        let mut progress = None;
        self.notifications.retain(|n| {
            if let Some(content) = n.read_content() {
                match content {
                    // NotificationContent::Message(_) => {}
                    NotificationContent::Progress(p) => {
                        if progress.is_none() {
                            progress = Some(p);
                        }
                    }
                }
                true
            } else {
                false
            }
        });
        self.has_interrupt = progress.is_some();
        self.primary_progress
            .set(progress.unwrap_or(ProgressBarState::new(0.0)));
    }

    pub(crate) fn insert(&mut self, content: NotificationContent) -> Notification {
        let (notification, nrec) = Notification::new(content);
        // TODO: limit maximum number of notifications
        self.notifications.push(nrec);
        notification
    }

    // TODO: should be optional but ProgressBar isn't friendly to that. We really need a ListenableSource::map()
    pub(crate) fn primary_progress(&self) -> listen::ListenableSource<ProgressBarState> {
        self.primary_progress.as_source()
    }

    pub(crate) fn has_interrupt(&self) -> bool {
        self.has_interrupt
    }
}

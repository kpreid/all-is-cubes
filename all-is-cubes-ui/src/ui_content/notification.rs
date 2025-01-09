//! Types used to create notifications displayed to the user.

use alloc::sync::{Arc, Weak};
use alloc::vec::Vec;
use std::sync::Mutex;

use all_is_cubes::arcstr::ArcStr;
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
    Progress {
        /// The activity this is the progress of.
        title: ArcStr,

        /// The amount of progress.
        progress: ProgressBarState,

        /// The particular piece of the overall work that is currently being done
        /// (or was just finished, if that is all that is available).
        part: ArcStr,
    },
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
    /// and be able to display however many notifications.
    primary_content: listen::Cell<Option<NotificationContent>>,

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
            primary_content: listen::Cell::new(None),
            has_interrupt: false,
        }
    }

    pub(crate) fn update(&mut self) {
        let mut primary = None;
        self.notifications.retain(|n| {
            if let Some(content) = n.read_content() {
                primary = Some(content);
                true
            } else {
                false
            }
        });
        self.has_interrupt = primary.is_some();
        self.primary_content.set_if_unequal(primary);
    }

    pub(crate) fn insert(&mut self, content: NotificationContent) -> Notification {
        let (notification, nrec) = Notification::new(content);
        // TODO: limit maximum number of notifications
        self.notifications.push(nrec);
        notification
    }

    pub(crate) fn primary_content(&self) -> listen::DynSource<Option<NotificationContent>> {
        self.primary_content.as_source()
    }

    pub(crate) fn has_interrupt(&self) -> bool {
        self.has_interrupt
    }
}

// fn dummy_content() -> NotificationContent {
//     // TODO: should be not Progress but that is all we have right now
//     NotificationContent::Progress {
//         title: ArcStr::new(),
//         progress: ProgressBarState::new(0.0),
//         part: ArcStr::new(),
//     }
// }

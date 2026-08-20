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
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
#[expect(clippy::module_name_repetitions)] // TODO: rename?
pub enum NotificationContent {
    // TODO: Not implemented:
    // /// The message may be multi-line.
    // Message(ArcStr),
    /// A progress bar.
    Progress {
        /// The overall activity this is the progress of.
        /// Should be a short string which does not change with progress.
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

/// A container of information to be brought to the user's attention.
///
/// The carried [`NotificationContent`] is displayed until the user dismisses it,
/// or this [`Notification`] value is dropped.
/// The message may be updated at any time ([`Notification`] is interior-mutable).
///
/// To display a notification, call [`Notification::new()`], then call
/// [`Session::show_notification()`] or [`MainTaskContext::show_notification()`] to add it to
/// a user session.
///
/// Cloning a [`Notification`] produces another handle to the same notification.
#[derive(Clone, Debug)]
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
    /// Create a new notification.
    ///
    /// This notification does not yet display its contents anywhere.
    /// Call [`Session::show_notification()`] or [`MainTaskContext::show_notification()`] to add it
    /// to a user session.
    pub fn new(content: NotificationContent) -> Self {
        let shared = Arc::new(Shared {
            content: Mutex::new(content),
            notifier: listen::Notifier::new(),
        });

        Notification { shared }
    }

    /// Replace the existing content of the notification.
    pub fn set_content(&self, content: NotificationContent) {
        *self.shared.content.lock().unwrap_or_else(|poison| poison.into_inner()) = content;
        self.shared.notifier.notify(&());
    }

    pub(crate) fn attach(&self) -> Receiver {
        Receiver {
            shared: Arc::downgrade(&self.shared),
        }
    }
}

impl NotificationContent {
    /// Create a new [`Notification`] with `self` as its initial content.
    ///
    /// This is equivalent to [`Notification::new()`].
    pub fn into_notification(self) -> Notification {
        Notification::new(self)
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

    pub(crate) fn insert(&mut self, notification: &Notification) -> Result<(), Error> {
        // TODO: limit maximum number of notifications that can be present at once.
        #![expect(clippy::unnecessary_wraps)]

        self.notifications.push(notification.attach());
        Ok(())
    }

    pub(crate) fn primary_content(&self) -> listen::DynSource<Option<NotificationContent>> {
        self.primary_content.as_source()
    }

    pub(crate) fn has_interrupt(&self) -> bool {
        self.has_interrupt
    }
}

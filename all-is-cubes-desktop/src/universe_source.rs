use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use anyhow::Context as _;
use indicatif::ProgressBar;
use rand::Rng as _;
use tokio::sync::oneshot;

use all_is_cubes::arcstr::literal;
use all_is_cubes::space::{LightUpdatesInfo, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes_content::{TemplateParameters, UniverseTemplate};
use all_is_cubes_ui::notification::{self, Notification};
use all_is_cubes_ui::vui::widgets::ProgressBarState;

use crate::{glue, logging};

/// Source of the universe to create/load.
///
/// This enum lives here in `all_is_cubes_desktop` because up until now, in the dependency graph,
/// templates (from [`all_is_cubes_content`]) and files (from [`all_is_cubes_port`]) haven't met.
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum UniverseSource {
    /// Create a universe from a built-in template.
    Template(UniverseTemplate, TemplateParameters),
    /// Read the given file.
    File(PathBuf),
}

impl UniverseSource {
    /// Perform and log the creation of the universe.
    pub async fn create_universe(
        self,
        precompute_light: bool,
        notif_rx: oneshot::Receiver<Notification>,
        replace_universe_callback: Arc<dyn Fn(&UniverseTemplate) + Send + Sync>,
    ) -> Result<Box<Universe>, anyhow::Error> {
        let start_time = Instant::now();

        // TODO: figure out a cleaner way to wrangle this rx hookup
        let notif_rx = Mutex::new(TryRecvKeep::Rx(notif_rx));
        #[allow(clippy::literal_string_with_formatting_args)]
        let universe_progress_bar = logging::new_progress_bar(100)
            .with_style(
                logging::common_progress_style()
                    .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}% {msg:36}")
                    .unwrap(),
            )
            .with_prefix("Building");
        universe_progress_bar.set_position(0);
        let yield_progress = {
            let universe_progress_bar = universe_progress_bar.clone();
            glue::tokio_yield_progress()
                .progress_using(move |info| {
                    universe_progress_bar.set_position((info.fraction() * 100.0) as u64);
                    universe_progress_bar.set_message(String::from(info.label_str()));

                    if let Some(notification) = notif_rx.lock().unwrap().try_borrow() {
                        notification.set_content(notification::NotificationContent::Progress {
                            title: literal!("Loading..."),
                            progress: ProgressBarState::new(info.fraction().into()),
                            part: info.label_str().into(),
                        });
                    }
                })
                .build()
        };

        let mut universe = match self.clone() {
            // TODO: awkward kludge for hooking up the menu's universe-selection buttons. Instead, `TemplateParameters` should have an extension mechanism by which we can pass the hook function.
            UniverseSource::Template(UniverseTemplate::Menu, _parameters) => {
                let mut universe = Box::new(Universe::new());
                // let mut txn = UniverseTransaction::default();

                let space = all_is_cubes_content::template_menu_space(
                    &mut universe,
                    yield_progress,
                    replace_universe_callback,
                )
                .await?;
                // txn.merge_from(menu_txn).unwrap();

                let space = universe.insert("menu".into(), space)?;
                universe.insert(
                    "character".into(),
                    all_is_cubes::character::Character::spawn_default(
                        universe.read_ticket(),
                        space,
                    ),
                )?;

                universe
            }
            UniverseSource::Template(template, TemplateParameters { seed, size }) => {
                let seed: u64 = seed.unwrap_or_else(|| {
                    let random_seed = rand::rng().random();
                    log::info!("Randomly chosen universe seed: {random_seed}");
                    random_seed
                });
                template
                    .clone()
                    .build(
                        yield_progress,
                        TemplateParameters {
                            seed: Some(seed),
                            size,
                        },
                    )
                    .await
                    .with_context(|| {
                        format!("failed while constructing universe from template {template:?}")
                    })?
            }
            UniverseSource::File(path) => {
                let path = Arc::new(path);
                all_is_cubes_port::load_universe_from_file(yield_progress, path.clone())
                    .await
                    .with_context(|| format!("could not load universe from file {path:?}"))?
            }
        };
        universe_progress_bar.finish();
        let universe_done_time = Instant::now();
        log::debug!(
            "Initialized game state with {:?} ({:.3} s)",
            self,
            universe_done_time.duration_since(start_time).as_secs_f32()
        );

        if precompute_light {
            if let Some(c) = universe.get_default_character() {
                let space_handle = c.read(universe.read_ticket()).unwrap().space.clone();
                universe
                    .try_modify(&space_handle, evaluate_light_with_progress)
                    .unwrap();
            }
        }

        Ok(universe)
    }
}

fn evaluate_light_with_progress(space: &mut Space) {
    let light_progress = logging::new_progress_bar(100).with_prefix("Lighting");
    space.evaluate_light(1, lighting_progress_adapter(&light_progress));
    light_progress.finish();
}

/// Convert `LightUpdatesInfo` data to an approximate completion progress.
/// TODO: Improve this and put it in the lighting module (independent of indicatif).
fn lighting_progress_adapter(progress: &ProgressBar) -> impl FnMut(LightUpdatesInfo) + '_ {
    let mut worst = 1;
    move |info| {
        worst = worst.max(info.queue_count);
        progress.set_length(worst as u64);
        progress.set_position((worst - info.queue_count) as u64);
    }
}

enum TryRecvKeep {
    Rx(oneshot::Receiver<Notification>),
    Have(Notification),
}
impl TryRecvKeep {
    fn try_borrow(&mut self) -> Option<&Notification> {
        if let Self::Rx(rx) = self {
            *self = Self::Have(rx.try_recv().ok()?)
        }
        if let Self::Have(value) = self {
            Some(value)
        } else {
            None
        }
    }
}

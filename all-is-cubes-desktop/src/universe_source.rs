use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use anyhow::Context as _;
use indicatif::ProgressBar;
use rand::Rng as _;

use all_is_cubes::space::{LightUpdatesInfo, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes_content::{TemplateParameters, UniverseTemplate};

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
    pub async fn create_universe(self, precompute_light: bool) -> Result<Universe, anyhow::Error> {
        let start_time = Instant::now();
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
                })
                .build()
        };
        let universe = match self.clone() {
            UniverseSource::Template(template, TemplateParameters { seed, size }) => {
                let seed: u64 = seed.unwrap_or_else(|| {
                    let seed = rand::thread_rng().gen();
                    log::info!("Randomly chosen universe seed: {seed}");
                    seed
                });
                template
                    .clone()
                    .build::<Instant>(
                        yield_progress,
                        TemplateParameters {
                            seed: Some(seed),
                            size,
                        },
                    )
                    .await
                    .with_context(|| {
                        format!("failed while constructing universe from template {template:?}")
                    })
            }
            UniverseSource::File(path) => {
                let path = Arc::new(path);
                all_is_cubes_port::load_universe_from_file(yield_progress, path.clone())
                    .await
                    .with_context(|| format!("could not load universe from file {path:?}"))
            }
        }?;
        universe_progress_bar.finish();
        let universe_done_time = Instant::now();
        log::debug!(
            "Initialized game state with {:?} ({:.3} s)",
            self,
            universe_done_time.duration_since(start_time).as_secs_f32()
        );

        if precompute_light {
            if let Some(c) = universe.get_default_character() {
                c.read()
                    .unwrap()
                    .space
                    .try_modify(evaluate_light_with_progress)
                    .unwrap();
            }
        }

        Ok(universe)
    }
}

fn evaluate_light_with_progress(space: &mut Space) {
    let light_progress = logging::new_progress_bar(100).with_prefix("Lighting");
    space.evaluate_light::<Instant>(1, lighting_progress_adapter(&light_progress));
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

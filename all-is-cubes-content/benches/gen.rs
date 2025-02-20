//! Benchmarks of executing [`UniverseTemplate`]s and other worldgen.
//!
//! These are directly relevant to app startup performance, and also may be useful
//! for testing changes to the underlying data structures being built.

#![allow(missing_docs)]

use criterion::{Criterion, async_executor::FuturesExecutor, criterion_group, criterion_main};

use std::time::Duration;

use strum::IntoEnumIterator as _;

use all_is_cubes::util::yield_progress_for_testing;
use all_is_cubes_content::{TemplateParameters, UniverseTemplate};

fn template_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("template");
    // These benchmarks are slow and noisy. Parameters adjusted so the overall run time
    // is not too long and there won't be too many false reports of change.
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(10));
    group.noise_threshold(0.05);
    group.confidence_level(0.99);

    for template in UniverseTemplate::iter().filter(|t| !matches!(t, UniverseTemplate::Islands)) {
        // TODO: specify a small size for each, where possible
        group.bench_function(format!("{template}"), |b| {
            b.to_async(FuturesExecutor).iter_with_large_drop(|| {
                template.clone().build::<std::time::Instant>(
                    yield_progress_for_testing(),
                    TemplateParameters {
                        seed: Some(0),
                        size: None,
                    },
                )
            })
        });
    }

    group.finish();
}

criterion_group!(benches, template_bench);
criterion_main!(benches);

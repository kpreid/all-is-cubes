#![allow(missing_docs)]

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

use all_is_cubes::camera::{Camera, GraphicsOptions, ViewTransform, Viewport};
use all_is_cubes::chunking::{reset_chunk_chart_cache, ChunkChart, ChunkPos, OctantMask};
use all_is_cubes::math::{Cube, FreeVector, GridAab};

fn chunk_chart_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("ChunkChart");

    for view_distance in [1u8, 20, 100] {
        group.bench_function(BenchmarkId::new("new", view_distance), |b| {
            b.iter_with_large_drop(|| -> ChunkChart<16> {
                reset_chunk_chart_cache();
                ChunkChart::new(f64::from(view_distance))
            });
        });
    }

    group.finish();
}

/// Test the performance of strategies to choose chunks to draw.
fn cull_bench(c: &mut Criterion) {
    let chart = ChunkChart::<16>::new(200.0);
    let bounds = GridAab::from_lower_upper([-100, -30, -100], [100, 30, 100]);
    let chunked_bounds = bounds.divide(16);
    let mut camera = Camera::new(
        GraphicsOptions::default(),
        // only aspect ratio should matter
        Viewport::with_scale(1.0, [800, 600]),
    );
    // Set a small translation to avoid the edge case of being exactly on a chunk boundaries.
    camera.set_view_transform(ViewTransform::from_translation(-FreeVector::new(
        0.01, 0.01, 0.01,
    )));

    c.bench_function("bounds", |b| {
        b.iter(|| {
            let mut matched = 0;
            for p in chart
                .chunks(ChunkPos(Cube::new(0, 0, 0)), OctantMask::ALL)
                .rev()
            {
                if chunked_bounds.contains_cube(p.0) {
                    matched += 1;
                }
            }
            assert_eq!(matched, 784);
            matched
        })
    });

    c.bench_function("frustum-bounds", |b| {
        b.iter(|| {
            let mut matched = 0;
            for p in chart.chunks(ChunkPos(Cube::ORIGIN), OctantMask::ALL).rev() {
                if camera.aab_in_view(p.bounds().into()) && chunked_bounds.contains_cube(p.0) {
                    matched += 1;
                }
            }
            assert_eq!(matched, 280);
            matched
        })
    });

    c.bench_function("bounds-frustum", |b| {
        b.iter(|| {
            let mut matched = 0;
            for p in chart.chunks(ChunkPos(Cube::ORIGIN), OctantMask::ALL).rev() {
                if chunked_bounds.contains_cube(p.0) && camera.aab_in_view(p.bounds().into()) {
                    matched += 1;
                }
            }
            assert_eq!(matched, 280);
            matched
        })
    });

    c.bench_function("mask-frustum-bounds", |b| {
        b.iter(|| {
            let mut matched = 0;
            for p in chart
                .chunks(ChunkPos(Cube::ORIGIN), camera.view_direction_mask())
                .rev()
            {
                if camera.aab_in_view(p.bounds().into()) && chunked_bounds.contains_cube(p.0) {
                    matched += 1;
                }
            }
            assert_eq!(matched, 280);
            matched
        })
    });

    c.bench_function("mask-bounds-frustum", |b| {
        b.iter(|| {
            let mut matched = 0;
            for p in chart
                .chunks(ChunkPos(Cube::ORIGIN), camera.view_direction_mask())
                .rev()
            {
                if chunked_bounds.contains_cube(p.0) && camera.aab_in_view(p.bounds().into()) {
                    matched += 1;
                }
            }
            assert_eq!(matched, 280);
            matched
        })
    });
}

criterion_group!(benches, cull_bench, chunk_chart_bench);
criterion_main!(benches);

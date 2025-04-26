#![allow(missing_docs)]

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};

use all_is_cubes::camera::{Camera, GraphicsOptions, ViewTransform, Viewport};
use all_is_cubes::chunking::{ChunkChart, ChunkPos, reset_chunk_chart_cache};
use all_is_cubes::math::{Cube, FreeVector, GridAab, OctantMask};

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

// Shared configuration for the benches and the test.
fn config() -> (ChunkChart<16>, Camera, GridAab) {
    let chart = ChunkChart::<16>::new(200.0);

    let mut camera = Camera::new(
        GraphicsOptions::default(),
        // only aspect ratio should matter
        Viewport::with_scale(1.0, [800, 600]),
    );
    // Set a small translation to avoid the edge case of being exactly on a chunk boundary.
    camera.set_view_transform(ViewTransform::from_translation(-FreeVector::new(
        0.01, 0.01, 0.01,
    )));

    let bounds = GridAab::from_lower_upper([-100, -30, -100], [100, 30, 100]);
    let chunked_bounds = bounds.divide(16);

    (chart, camera, chunked_bounds)
}

/// Test the performance of strategies to choose chunks to draw.
fn cull_bench(c: &mut Criterion) {
    let (chart, camera, chunked_bounds) = config();

    // This would make more sense as an ignored #[test], but we can't do that
    // while we use harness=false.
    #[cfg(feature = "rerun")]
    if false {
        dump_frustum_culling();
    }

    // Chunk counts for sanity checks that these are all equivalent as expected.
    let expected_chunks_in_space = chunked_bounds.volume().unwrap();
    // Caution: this number is sensitive to the exact projection arithmetic used.
    let expected_chunks_in_frustum = 284;

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
            assert_eq!(matched, expected_chunks_in_space);
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
            assert_eq!(matched, expected_chunks_in_frustum);
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
            assert_eq!(matched, expected_chunks_in_frustum);
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
            assert_eq!(matched, expected_chunks_in_frustum);
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
            assert_eq!(matched, expected_chunks_in_frustum);
            matched
        })
    });
}

/// Connect to Rerun viewer and display the results of exactly the frustum culling this benchmark
/// does.
#[cfg(feature = "rerun")]
fn dump_frustum_culling() {
    use all_is_cubes::math::Wireframe as _;
    use all_is_cubes::rerun_glue as rg;
    use itertools::Itertools;

    let (chart, camera, chunked_bounds) = config();

    let stream = re_sdk::RecordingStreamBuilder::new("all-is-cubes/benches/chunk")
        .default_enabled(true)
        .connect_grpc()
        .unwrap();

    let frustum = camera.view_frustum_geometry();
    let mut frustum_points = Vec::new();
    frustum.wireframe_points(&mut frustum_points);
    let frustum_lines = frustum_points
        .into_iter()
        .map(|vertex| rg::convert_vec(vertex.position.to_vector()))
        .tuples()
        .map(|(a, b)| rg::components::LineStrip3D(vec![a, b]));

    stream
        .log_static(
            rg::entity_path!["frustum"],
            &rg::archetypes::LineStrips3D::new(frustum_lines),
        )
        .unwrap();
    stream
        .log_static(
            rg::entity_path!["aabs"],
            &rg::convert_grid_aabs(
                chart
                    .chunks(ChunkPos(Cube::ORIGIN), camera.view_direction_mask())
                    .filter(|chunk| {
                        camera.aab_in_view(chunk.bounds().into())
                            && chunked_bounds.contains_cube(chunk.0)
                    })
                    .map(|chunk| chunk.bounds()),
            ),
        )
        .unwrap();
}

criterion_group!(benches, cull_bench, chunk_chart_bench);
criterion_main!(benches);

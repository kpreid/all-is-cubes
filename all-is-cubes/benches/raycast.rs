#![allow(missing_docs)]

use criterion::{Criterion, black_box, criterion_group, criterion_main};

use all_is_cubes::math::{Cube, Face7, GridAab};
use all_is_cubes::raycast::{AaRay, Raycaster};

criterion_main!(benches);
criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = raycaster_bench, axis_aligned_raycaster_bench
}

fn raycaster_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("raycaster");

    group.bench_function("single step diagonal", |b| {
        // Test the individual step cost, without setup
        let mut raycaster =
            Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 1.0, 1.0]));
        b.iter(|| raycaster.next().unwrap());
    });

    // Note: When I tried checking the other axes, z was slightly faster. This is
    // probably due to how the axis picking logic is compiled since there shouldn't
    // be any other differences.
    group.bench_function("single step +X", |b| {
        let mut raycaster =
            Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 0.0, 0.0]));
        b.iter(|| raycaster.next().unwrap());
    });

    group.bench_function("initialization", |b| {
        b.iter(|| Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 1.0, 1.0])))
    });

    group.bench_function("initialization with bounds (inside)", |b| {
        b.iter(|| {
            Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 1.0, 1.0]))
                .within(GridAab::from_lower_size([-1, -2, -3], [4, 5, 6]))
        })
    });

    group.bench_function("initialization with bounds (outside)", |b| {
        b.iter(|| {
            Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 1.0, 1.0]))
                .within(GridAab::from_lower_size([101, 102, 103], [4, 5, 6]))
        })
    });

    group.bench_function("many steps inside bounds", |b| {
        b.iter(|| {
            let mut raycaster =
                Raycaster::new(black_box([0.5, 0.5, 0.5]), black_box([1.0, 0.3, 0.7]))
                    .within(GridAab::from_lower_size([0, 0, 0], [100, 1000, 1000]));
            for _ in 1..100 {
                black_box(raycaster.next());
            }
        })
    });

    group.bench_function("many steps from outside bounds", |b| {
        b.iter(|| {
            let mut raycaster =
                Raycaster::new(black_box([-50.0, 0.5, 0.5]), black_box([1.0, 0.3, 0.7]))
                    .within(GridAab::from_lower_size([0, 0, 0], [100, 1000, 1000]));
            for _ in 1..100 {
                black_box(raycaster.next());
            }
        })
    });

    group.finish();
}

/// These are as similar to the normal [`Raycaster`] as possible, but can't be perfect.
///
/// TODO: If we want to rigorously compare things, parameterize this to allow using the regular
/// raycaster on the same inputs.
fn axis_aligned_raycaster_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("aa");

    group.bench_function("single step +X", |b| {
        let mut raycaster = black_box(AaRay::new(Cube::new(0, 0, 0), Face7::PX)).cast();
        b.iter(|| raycaster.next().unwrap());
    });

    group.bench_function("initialization", |b| {
        b.iter(|| black_box(AaRay::new(Cube::new(0, 0, 0), Face7::PX)).cast())
    });

    group.bench_function("initialization with bounds (inside)", |b| {
        b.iter(|| {
            black_box(AaRay::new(Cube::new(0, 0, 0), Face7::PX))
                .cast()
                .within(GridAab::from_lower_size([-1, -2, -3], [4, 5, 6]))
        })
    });

    group.bench_function("initialization with bounds (outside)", |b| {
        b.iter(|| {
            black_box(AaRay::new(Cube::new(0, 0, 0), Face7::PX))
                .cast()
                .within(GridAab::from_lower_size([101, 102, 103], [4, 5, 6]))
        })
    });

    group.bench_function("many steps inside bounds", |b| {
        b.iter(|| {
            let mut raycaster = black_box(AaRay::new(Cube::new(0, 0, 0), Face7::PX))
                .cast()
                .within(GridAab::from_lower_size([0, 0, 0], [100, 1000, 1000]));
            for _ in 1..100 {
                black_box(raycaster.next());
            }
        })
    });

    group.bench_function("many steps from outside bounds", |b| {
        b.iter(|| {
            let mut raycaster = black_box(AaRay::new(Cube::new(-50, 0, 0), Face7::PX))
                .cast()
                .within(GridAab::from_lower_size([0, 0, 0], [100, 1000, 1000]));
            for _ in 1..100 {
                black_box(raycaster.next());
            }
        })
    });

    group.finish();
}

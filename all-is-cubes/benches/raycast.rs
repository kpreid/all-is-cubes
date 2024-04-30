#![allow(missing_docs)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use all_is_cubes::math::GridAab;
use all_is_cubes::raycast::Raycaster;

pub fn raycast_bench(c: &mut Criterion) {
    c.bench_function("raycast: single step diagonal", |b| {
        // Test the individual step cost, without setup
        let mut raycaster =
            Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 1.0, 1.0]));
        b.iter(|| raycaster.next().unwrap());
    });

    // Note: When I tried checking the other axes, z was slightly faster. This is
    // probably due to how the axis picking logic is compiled since there shouldn't
    // be any other differences.
    c.bench_function("raycast: single step +X", |b| {
        let mut raycaster =
            Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 0.0, 0.0]));
        b.iter(|| raycaster.next().unwrap());
    });

    c.bench_function("raycast: initialization", |b| {
        b.iter(|| Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 1.0, 1.0])))
    });

    c.bench_function("raycast: initialization with bounds (inside)", |b| {
        b.iter(|| {
            Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 1.0, 1.0]))
                .within(GridAab::from_lower_size([-1, -2, -3], [4, 5, 6]))
        })
    });

    c.bench_function("raycast: initialization with bounds (outside)", |b| {
        b.iter(|| {
            Raycaster::new(black_box([0.0, -0.25, -0.5]), black_box([1.0, 1.0, 1.0]))
                .within(GridAab::from_lower_size([101, 102, 103], [4, 5, 6]))
        })
    });

    c.bench_function("raycast: many steps inside bounds", |b| {
        b.iter(|| {
            let mut raycaster =
                Raycaster::new(black_box([0.5, 0.5, 0.5]), black_box([1.0, 0.3, 0.7]))
                    .within(GridAab::from_lower_size([0, 0, 0], [100, 1000, 1000]));
            for _ in 1..100 {
                black_box(raycaster.next());
            }
        })
    });

    c.bench_function("raycast: many steps from outside bounds", |b| {
        b.iter(|| {
            let mut raycaster =
                Raycaster::new(black_box([-50.0, 0.5, 0.5]), black_box([1.0, 0.3, 0.7]))
                    .within(GridAab::from_lower_size([0, 0, 0], [100, 1000, 1000]));
            for _ in 1..100 {
                black_box(raycaster.next());
            }
        })
    });
}

criterion_group!(benches, raycast_bench);
criterion_main!(benches);

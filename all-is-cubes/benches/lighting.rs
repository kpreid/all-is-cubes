use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use std::time::Duration;

use all_is_cubes::blockgen::{BlockGen, LandscapeBlocks};
use all_is_cubes::camera::Camera;
use all_is_cubes::space::{Grid, Space, SpaceStepInfo};
use all_is_cubes::universe::{Universe, UniverseIndex as _};
use all_is_cubes::worldgen::{axes, wavy_landscape};

pub fn lighting_bench(c: &mut Criterion) {
    c.bench_function("lighting: new_universe_with_stuff()", |b| {
        b.iter_batched(
            universe_for_lighting_test,
            |universe| {
                let space = &mut *universe.get_default_space().borrow_mut();
                loop {
                    let SpaceStepInfo {
                        light_queue_count, ..
                    } = space.step(Duration::from_millis(10));
                    if light_queue_count == 0 {
                        break;
                    }
                }
            },
            BatchSize::SmallInput,
        )
    });
}

fn universe_for_lighting_test() -> Universe {
    // TODO: For a stable benchmark, replace this with entirely specified data
    // (no "a landscape").

    let mut universe = Universe::new();

    let mut bg = BlockGen {
        universe: &mut universe,
        size: 16,
    };
    let blocks = LandscapeBlocks::new(&mut bg);

    let radius = 20;
    let diameter = radius * 2 + 1;
    let grid = Grid::new((-radius, -radius, -radius), (diameter, diameter, diameter));
    let mut space = Space::empty(grid);
    wavy_landscape(&mut space, &blocks, 1.0);
    axes(&mut space);

    let space_ref = universe.insert("space".into(), space);

    let camera = Camera::for_space(space_ref);
    universe.insert("camera".into(), camera);
    universe
}

criterion_group!(benches, lighting_bench);
criterion_main!(benches);

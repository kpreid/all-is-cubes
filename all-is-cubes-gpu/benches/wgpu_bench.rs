#![allow(missing_docs)]

use std::sync::{Arc, Mutex};
use std::time::Duration;

use all_is_cubes::math::GridAab;
use criterion::Criterion;

use all_is_cubes::block;
use all_is_cubes::camera::{Flaws, GraphicsOptions, HeadlessRenderer, StandardCameras, Viewport};
use all_is_cubes::character::Character;
use all_is_cubes::content::make_some_blocks;
use all_is_cubes::euclid::size3;
use all_is_cubes::space::{Space, SpaceTransaction};
use all_is_cubes::transaction::Transaction;
use all_is_cubes::universe::{self, Universe};

use all_is_cubes_gpu::in_wgpu::{headless, init};
use tokio::runtime::Runtime;

fn main() {
    let runtime = tokio::runtime::Builder::new_multi_thread().build().unwrap();

    let mut criterion: Criterion<_> = Criterion::default().configure_from_args();

    let (_, adapter) = runtime.block_on(init::create_instance_and_adapter_for_test(|msg| {
        eprintln!("{msg}")
    }));
    let adapter = match adapter {
        Some(adapter) => Arc::new(adapter),
        None => {
            // TODO: kludge; would be better if we could get the mode out of Criterion
            if cfg!(test) || std::env::args().any(|s| s == "--test") {
                eprintln!("GPU not available; skipping actually testing bench functions");
                return;
            } else {
                panic!("wgpu_bench requires a GPU, but no adapter was found");
            }
        }
    };

    render_benches(&runtime, &mut criterion, &adapter);
    module_benches(&runtime, &mut criterion, &adapter);

    criterion.final_summary();
}

#[allow(clippy::await_holding_lock)]
fn render_benches(runtime: &Runtime, c: &mut Criterion, adapter: &Arc<wgpu::Adapter>) {
    let mut g = c.benchmark_group("render");

    // Benchmark for running update() only. Insofar as this touches the GPU it will
    // naturally fill up the pipeline as Criterion iterates it.
    g.bench_function("update-only", |b| {
        let (mut universe, space, renderer) =
            runtime.block_on(create_updated_renderer(adapter.clone()));

        let [block] = make_some_blocks();
        let txn1 =
            SpaceTransaction::set_cube([0, 0, 0], None, Some(block::AIR)).bind(space.clone());
        let txn2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block)).bind(space.clone());

        b.to_async(runtime).iter_with_large_drop(move || {
            txn1.execute(&mut universe, &mut drop).unwrap();
            txn2.execute(&mut universe, &mut drop).unwrap();
            let renderer = renderer.clone();
            async move {
                renderer.lock().unwrap().update(None).await.unwrap();
            }
        });
    });

    // Benchmark for running draw() only. Note that this is heavily influenced by
    // latency since it must wait for the image to be fetched. TODO: Figure out how to
    // improve that.
    g.bench_function("draw-only", |b| {
        let (_universe, _space, renderer) =
            runtime.block_on(create_updated_renderer(adapter.clone()));

        b.to_async(runtime).iter_with_large_drop(move || {
            let renderer = renderer.clone();
            async move {
                let image = renderer.lock().unwrap().draw("").await.unwrap();
                assert_eq!(image.flaws, Flaws::empty());
                // if false {
                //     // enable this for a check that we're rendering something sensible
                //     // TODO: will need a type conversion
                //     image.save("./wgpu_bench_output.png").unwrap();
                // }
                image
            }
        });
    });
}

async fn create_updated_renderer(
    adapter: Arc<wgpu::Adapter>,
) -> (
    Universe,
    universe::Handle<Space>,
    Arc<Mutex<headless::Renderer>>,
) {
    let mut universe = Universe::new();
    let space = all_is_cubes::content::testing::lighting_bench_space(
        &mut universe,
        all_is_cubes::util::yield_progress_for_testing(),
        size3(50, 50, 50),
    )
    .await
    .unwrap();
    let space = universe.insert_anonymous(space);
    universe
        .insert("character".into(), Character::spawn_default(space.clone()))
        .unwrap();

    let mut renderer = headless::Builder::from_adapter(adapter)
        .await
        .unwrap()
        .build(StandardCameras::from_constant_for_test(
            GraphicsOptions::default(),
            Viewport::with_scale(1.0, [1024, 1024]),
            &universe,
        ));

    renderer.update(None).await.unwrap();

    // Arc<Mutex< needed to satisfy borrow checking of the benchmark closure
    (universe, space, Arc::new(Mutex::new(renderer)))
}

/// Benchmarks for internal components
fn module_benches(runtime: &Runtime, c: &mut Criterion, adapter: &Arc<wgpu::Adapter>) {
    let mut g = c.benchmark_group("mod");
    g.sample_size(400); // increase sample size from default 100 to reduce noise
    g.measurement_time(Duration::from_secs(10));

    let (device, queue) = runtime
        .block_on(adapter.request_device(&wgpu::DeviceDescriptor::default(), None))
        .unwrap();

    g.bench_function("light-update", |b| {
        let size = 64;
        let bounds = GridAab::from_lower_size([0, 0, 0], [size, size, size]);
        // We're reusing one texture across these tests because it has no observable state that
        // influences the benchmark, and we're not primarily interested in effects like "the light
        // data isn't in cache".
        let mut texture = all_is_cubes_gpu::in_wgpu::LightTexture::new("lt", &device, bounds);
        let space = Space::empty_positive(size, size, size);

        b.iter_with_large_drop(|| {
            texture.update_all(&queue, &space);

            scopeguard::guard((), |()| {
                // flush wgpu's buffering of copy commands (not sure if this is effective).
                queue.submit([]);
            })
        });
    });
}

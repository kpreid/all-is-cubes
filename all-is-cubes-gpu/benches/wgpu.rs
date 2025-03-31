#![allow(missing_docs)]

use std::sync::{Arc, Mutex};
use std::time::Duration;

use criterion::Criterion;
use tokio::runtime::Runtime;

use all_is_cubes::block;
use all_is_cubes::character::Character;
use all_is_cubes::content::make_some_blocks;
use all_is_cubes::euclid::size3;
use all_is_cubes::math::{GridAab, GridPoint, GridSize};
use all_is_cubes::space::{Space, SpaceTransaction};
use all_is_cubes::transaction::Transaction;
use all_is_cubes::universe::{self, Universe};
use all_is_cubes_render::Flaws;
use all_is_cubes_render::HeadlessRenderer;
use all_is_cubes_render::camera::{GraphicsOptions, StandardCameras, Viewport};

use all_is_cubes_gpu::in_wgpu::{LightChunk, LightTexture, headless, init};

fn main() {
    let runtime = tokio::runtime::Builder::new_multi_thread().build().unwrap();

    let mut criterion: Criterion<_> = Criterion::default().configure_from_args();

    let instance = runtime.block_on(init::create_instance_for_test_or_exit());

    render_benches(&runtime, &mut criterion, &instance);
    light_benches(&runtime, &mut criterion, &instance);

    criterion.final_summary();
}

#[expect(clippy::await_holding_lock)]
fn render_benches(runtime: &Runtime, c: &mut Criterion, instance: &wgpu::Instance) {
    let mut g = c.benchmark_group("render");

    // Benchmark for running update() only. Insofar as this touches the GPU it will
    // naturally fill up the pipeline as Criterion iterates it.
    g.bench_function("update-only", |b| {
        let (mut universe, space, renderer) =
            runtime.block_on(create_updated_renderer("update-only", instance));

        let [block] = make_some_blocks();
        let txn1 =
            SpaceTransaction::set_cube([0, 0, 0], None, Some(block::AIR)).bind(space.clone());
        let txn2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block)).bind(space);

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
            runtime.block_on(create_updated_renderer("draw-only", instance));

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
    device_label: &str,
    instance: &wgpu::Instance,
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

    let adapter = init::create_adapter_for_test(instance).await;
    let mut renderer = headless::Builder::from_adapter(device_label, adapter)
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

/// Benchmarks for light storage
fn light_benches(runtime: &Runtime, c: &mut Criterion, instance: &wgpu::Instance) {
    let mut g = c.benchmark_group("light");
    g.sample_size(400); // increase sample size from default 100 to reduce noise
    g.measurement_time(Duration::from_secs(10));

    let (device, queue) = runtime
        .block_on(async {
            let adapter = init::create_adapter_for_test(instance).await;
            adapter
                .request_device(
                    &all_is_cubes_gpu::in_wgpu::device_descriptor(
                        "module_benches",
                        adapter.limits(),
                    ),
                    None,
                )
                .await
        })
        .unwrap();

    let bounds = GridAab::from_lower_size(GridPoint::splat(0), GridSize::splat(32));
    g.throughput(criterion::Throughput::Elements(
        bounds.volume().unwrap().try_into().unwrap(),
    ));

    g.bench_function("bulk", |b| {
        // We're reusing one texture across these tests because it has no observable state that
        // influences the benchmark other than the mapped region state,
        // and we're not primarily interested in effects like "the light data isn't in cache".
        let mut texture =
            LightTexture::new("lt", &device, bounds.size(), wgpu::TextureUsages::empty());
        let space = Space::builder(bounds).build();

        b.iter_with_large_drop(|| {
            texture.forget_mapped();
            texture.ensure_mapped(&queue, &space, bounds);

            scopeguard::guard((), |()| {
                // flush wgpu's buffering of copy commands (not sure if this is effective).
                queue.submit([]);
            })
        });
    });

    g.bench_function("scatter", |b| {
        let mut texture =
            LightTexture::new("lt", &device, bounds.size(), wgpu::TextureUsages::empty());
        let space = Space::builder(bounds).build();

        let updates = LightChunk::all_in_region(bounds);

        // update_scatter() will do nothing if not mapped first
        texture.ensure_mapped(&queue, &space, bounds);

        b.iter_with_large_drop(|| {
            texture.update_scatter(&device, &queue, &space, updates.iter().copied());

            scopeguard::guard((), |()| {
                // flush wgpu's buffering of copy commands (not sure if this is effective).
                queue.submit([]);
            })
        });
    });
}

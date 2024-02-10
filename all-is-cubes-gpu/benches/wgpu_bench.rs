#![allow(missing_docs)]

use std::sync::{Arc, Mutex};

use criterion::Criterion;

use all_is_cubes::block;
use all_is_cubes::camera::{Flaws, GraphicsOptions, HeadlessRenderer, StandardCameras, Viewport};
use all_is_cubes::character::Character;
use all_is_cubes::content::make_some_blocks;
use all_is_cubes::math::GridVector;
use all_is_cubes::space::{Space, SpaceTransaction};
use all_is_cubes::transaction::Transaction;
use all_is_cubes::universe::{self, Universe};

use all_is_cubes_gpu::in_wgpu::{headless, init};
use tokio::runtime::Runtime;

fn main() {
    let runtime = tokio::runtime::Builder::new_multi_thread().build().unwrap();

    let mut criterion: Criterion<_> = criterion::Criterion::default().configure_from_args();
    render_benches(&runtime, &mut criterion);

    criterion.final_summary();
}

pub fn benches() {}

#[allow(clippy::await_holding_lock)]
fn render_benches(runtime: &Runtime, c: &mut Criterion) {
    let mut g = c.benchmark_group("render");

    let (_, adapter) = runtime.block_on(init::create_instance_and_adapter_for_test(|msg| {
        eprintln!("{msg}")
    }));
    let adapter = Arc::new(adapter.expect("render_bench requires GPU"));

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
    universe::URef<Space>,
    Arc<Mutex<headless::Renderer>>,
) {
    let mut universe = Universe::new();
    let space = all_is_cubes::content::testing::lighting_bench_space(
        &mut universe,
        all_is_cubes::util::yield_progress_for_testing(),
        GridVector::new(50, 50, 50),
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

//! Benchmarks of block operations.

#![allow(missing_docs)]

use std::array;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};

use all_is_cubes::block::{
    self, Block, EvaluatedBlock, Modifier,
    Resolution::{R2, R4, R16, R32, R64},
};
use all_is_cubes::content::{make_some_blocks, make_some_voxel_blocks, palette};
use all_is_cubes::euclid::{point3, vec3};
use all_is_cubes::inv;
use all_is_cubes::math::{Face, GridAab, GridVector, Rgb, Rgba};
use all_is_cubes::text;
use all_is_cubes::universe::{ReadTicket, Universe};

// -------------------------------------------------------------------------------------------------

/// For benchmarks which operate on batches of multiple blocks, the number of such blocks.
const BLOCK_COUNT: usize = 10;

criterion_main!(benches);
criterion_group!(benches, evaluate_bench);

fn evaluate_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluate");

    // Evaluate some `Primitive::Atom` blocks.
    group.bench_function("msb", |b| {
        let universe = Universe::new();
        let blocks = make_some_blocks::<BLOCK_COUNT>();

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    // Evaluate `make_some_voxel_blocks()`.
    // This is a composition of `Primitive::Recur` and `Primitive::Text`.
    group.bench_function("msvb", |b| {
        let mut universe = Universe::new();
        let blocks = make_some_voxel_blocks::<BLOCK_COUNT>(&mut universe);

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    group.bench_function("Recur opaque", |b| {
        let mut universe = Universe::new();
        let blocks: [Block; BLOCK_COUNT] = simple_voxel_blocks(&mut universe, 1.0, false);

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    // Evaluate some transparent blocks (has a cost of ray tracing for derived properties)
    group.bench_function("Recur transparent", |b| {
        let mut universe = Universe::new();
        let blocks: [Block; BLOCK_COUNT] = simple_voxel_blocks(&mut universe, 0.25, true);

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    // Evaluate some transparent blocks (has a cost of ray tracing for derived properties)
    group.bench_function("Text", |b| {
        let blocks: [Block; BLOCK_COUNT] = text_blocks();

        iter_block_evaluations(b, ReadTicket::stub(), blocks.each_ref());
    });

    // This serves as a benchmark of evaluating Rotate but also of the rotation math.
    group.bench_function("many Rotate", |b| {
        // use voxel blocks in particular so that the Rotate has to operate on the voxels.
        let mut universe = Universe::new();
        let blocks: [Block; BLOCK_COUNT] =
            make_some_voxel_blocks(&mut universe).map(|mut block| {
                let m = block.modifiers_mut();
                m.extend(vec![Modifier::Rotate(Face::PY.clockwise()); 100]);
                block
            });

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    group.bench_function("Move atom", |b| {
        let blocks: [Block; BLOCK_COUNT] = make_some_blocks()
            .map(|block| block.with_modifier(block::Move::new(Face::PX, R16, 1, 0)));

        iter_block_evaluations(b, ReadTicket::stub(), blocks.each_ref());
    });

    group.bench_function("Move voxels", |b| {
        let mut universe = Universe::new();
        let blocks: [Block; BLOCK_COUNT] = make_some_voxel_blocks(&mut universe)
            .map(|block| block.with_modifier(block::Move::new(Face::PX, R16, 1, 0)));

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    group.bench_function("Quote voxels", |b| {
        let mut universe = Universe::new();
        let blocks: [Block; BLOCK_COUNT] = make_some_voxel_blocks(&mut universe).map(|block| {
            block.with_modifier({
                let mut q = block::Quote::new();
                q.suppress_ambient = true; // this makes it have to process voxels
                q
            })
        });
        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    group.bench_function("Inventory", |b| {
        let mut universe = Universe::new();
        let [item_block] = make_some_blocks();
        let inventory =
            Modifier::Inventory(inv::Inventory::from_slots([
                inv::Tool::Block(item_block).into()
            ]));
        let iib = block::SetAttribute::Inventory(Arc::new(inv::InvInBlock::new(
            1,
            R4,
            R16,
            [inv::IconRow::new(0..1, point3(0, 0, 0), vec3(4, 0, 0))],
        )));

        let blocks: [Block; BLOCK_COUNT] = make_some_voxel_blocks(&mut universe)
            .map(|block| block.with_modifier(iib.clone()).with_modifier(inventory.clone()));

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    group.bench_function("Zoom voxels", |b| {
        let mut universe = Universe::new();
        let blocks: [Block; BLOCK_COUNT] = make_some_voxel_blocks(&mut universe)
            .map(|block| block.with_modifier(block::Zoom::new(R2, point3(0, 0, 0))));
        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    group.finish();
}

// -------------------------------------------------------------------------------------------------

/// Run a benchmark of evaluating `BLOCK_COUNT` blocks.
fn iter_block_evaluations(
    b: &mut criterion::Bencher<'_>,
    read_ticket: ReadTicket<'_>,
    blocks: [&Block; BLOCK_COUNT],
) {
    b.iter_with_large_drop(|| -> [EvaluatedBlock; BLOCK_COUNT] {
        blocks.map(|block| match block.evaluate(read_ticket) {
            Ok(evaluation) => evaluation,
            Err(error) => panic!("{}", all_is_cubes::util::ErrorChain(&error)),
        })
    })
}

// Unlike `make_some_voxel_blocks()`, doesn’t use any modifiers.
fn simple_voxel_blocks(
    universe: &mut Universe,
    alpha: f32,
    emission: bool,
) -> [Block; BLOCK_COUNT] {
    let resolution = R32;
    array::from_fn(|i| {
        Block::builder()
            .voxels_fn(resolution, |cube| {
                Block::builder()
                    .color(Rgba::new(
                        1.0,
                        cube.y as f32 / 32.0,
                        i as f32 / BLOCK_COUNT as f32,
                        alpha,
                    ))
                    .light_emission(if emission { Rgb::ONE } else { Rgb::ZERO })
                    .build()
            })
            .unwrap()
            .build_into(universe)
    })
}

/// Similar to `make_some_voxel_blocks()`'s text, but with no other components.
fn text_blocks() -> [Block; BLOCK_COUNT] {
    let label_voxel = Block::from(palette::ALMOST_BLACK);

    // Flat, so we can afford a large resolution.
    let resolution = R64;

    array::from_fn(|i| {
        Block::from(block::Primitive::Text {
            text: block::Text::builder()
                .string(arcstr::format!("{i}{i}{i}"))
                .font(text::Font::System16)
                .foreground(label_voxel.clone())
                .positioning(text::Positioning {
                    x: text::PositioningX::Center,
                    line_y: text::PositioningY::BodyMiddle,
                    z: text::PositioningZ::Front,
                })
                .layout_bounds(resolution, GridAab::for_block(resolution))
                .build(),
            offset: GridVector::zero(),
        })
    })
}

use super::prelude::*;

#[exhibit(
    name: "ChunkChart",
    subtitle: "Volume of world chunks in view at a distance of 4.99",
    placement: Placement::Surface,
)]
fn CHUNK_CHART(_: Context<'_>) {
    use all_is_cubes::chunking::ChunkChart;

    // TODO: Show more than one size.
    let chart = ChunkChart::<16>::new(16. * 4.99);

    Ok((chart.visualization(), ExhibitTransaction::default()))
}

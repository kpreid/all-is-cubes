use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Modifier::Zoom",
    subtitle: "",
    placement: Placement::Surface,
)]
fn ZOOM(ctx: Context<'_>) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;

    let specimen = &demo_blocks[DemoBlocks::LamppostBase];

    let scale = R8;
    let mut space = Space::builder(GridAab::for_block(scale)).build();

    // TODO: This algorithm should be generically available for creating Zoom instances,
    // rather than only an exhibit.
    for cube in space.bounds().interior_iter() {
        space
            .set(cube, {
                let mut zoom_block = specimen.clone();
                zoom_block
                    .modifiers_mut()
                    .push(Zoom::new(scale, cube.lower_bounds().cast()).into());
                zoom_block
            })
            .unwrap();
        if !space.get_evaluated(cube).visible() {
            // Cancel placing useless invisible zoomed blocks.
            // Note: This is not an equivalent optimization (if the original block has
            // BlockCollision::Hard or animation).
            space.set(cube, AIR).unwrap();
        }
    }

    Ok((space, ExhibitTransaction::default()))
}

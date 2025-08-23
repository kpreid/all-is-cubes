use all_is_cubes::content::load_image::{block_from_image, default_srgb};

use super::prelude::*;

#[exhibit(
    name: "block_from_image()",
    subtitle: "Using rotations XYZ, XyZ, XZY, xYZ",
    placement: Placement::Surface,
)]
fn IMAGES(ctx: Context<'_>) {
    // TODO: it would be nice if this exhibit visualized the generated bounding box somehow

    let mut txn = ExhibitTransaction::default();
    let demo_blocks = BlockProvider::<DemoBlocks>::using(ctx.universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    let mut outer_space = Space::empty(GridAab::from_lower_size([0, 0, 0], [4, 2, 1]));

    let mut place = |position: [i32; 3], rotation: GridRotation| -> Result<(), InGenError> {
        let terrain_map_function = |pixel: [u8; 4]| -> VoxelBrush<'static> {
            let [r, g, b, a] = pixel;
            if (r > b || g > b) && a > 0 {
                let block = Block::from(Rgba::from_srgb8(pixel));
                VoxelBrush::with_thickness(block, 0..2).rotate(rotation)
            } else {
                default_srgb(pixel)
            }
        };

        let image = include_image!("terrain-image.png");
        let block = block_from_image(
            ctx.universe.read_ticket(),
            image,
            rotation,
            &terrain_map_function,
        )?
        .display_name(format!("{rotation:?}"))
        .build_txn(&mut txn);

        outer_space.mutate(ctx.universe.read_ticket(), |m| {
            stack(m, position, [pedestal, &block])
        })?;
        Ok(())
    };
    place([0, 0, 0], GridRotation::RXYZ)?;
    place([1, 0, 0], GridRotation::RXyZ)?;
    place([2, 0, 0], GridRotation::RXZY)?;
    place([3, 0, 0], GridRotation::RxYZ)?;

    Ok((outer_space, txn))
}

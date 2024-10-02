use crate::make_slab_txn;

use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Collision",
    subtitle: "Test cases for character/world collision",
    placement: Placement::Surface,
)]
fn COLLISION(_: Context<'_>) {
    let footprint = GridAab::from_lower_size([0, 0, 0], [5, 2, 4]);
    let mut txn = ExhibitTransaction::default();
    let mut space = Space::empty(footprint);

    let half_block = make_slab_txn(&mut txn, 2, R4);

    for dx in -1..=1 {
        for dz in -1..=1 {
            let offset = GridVector::new(dx, 0, dz);
            space.set(
                GridPoint::new(1, 0, 1) + offset,
                // Rotate block so its +Y is towards the offset vector
                half_block.clone().rotate(match Face6::try_from(offset) {
                    Ok(face) => GridRotation::from_to(
                        Face6::PY,
                        face,
                        face.cross(Face6::PY).try_into().unwrap(),
                    )
                    .unwrap(),
                    Err(GridVector {
                        x: 0,
                        y: 0,
                        z: 0,
                        _unit,
                    }) => GridRotation::RXyZ,
                    Err(_) => GridRotation::IDENTITY,
                }),
            )?;
        }
    }

    let range = footprint.z_range();
    for i in 0..(range.len() as GridCoordinate) {
        space.set(
            [4, 0, range.start + i],
            make_slab_txn(&mut txn, range.end - i, range.len().try_into().unwrap()),
        )?;
    }

    Ok((space, txn))
}

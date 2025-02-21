use super::prelude::*;

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "make_some_blocks()",
    subtitle: "",
    placement: Placement::Surface,
)]
fn MAKE_SOME_BLOCKS(_: Context<'_>) {
    let mut txn = ExhibitTransaction::default();

    const ROWS: GridCoordinate = 5;
    fn make_both_blocks<const N: usize>(txn: &mut ExhibitTransaction) -> (Vec<Block>, Vec<Block>) {
        (
            Vec::from(make_some_blocks::<N>()),
            Vec::from(make_some_voxel_blocks_txn::<N>(txn)),
        )
    }
    let rows: [(Vec<Block>, Vec<Block>); ROWS as usize] = [
        make_both_blocks::<5>(&mut txn),
        make_both_blocks::<4>(&mut txn),
        make_both_blocks::<3>(&mut txn),
        make_both_blocks::<2>(&mut txn),
        make_both_blocks::<1>(&mut txn),
    ];
    let mut space = Space::empty_positive(3, ROWS, ROWS);
    for (y, (blocks_a, blocks_v)) in (0i32..).zip(rows) {
        for (h, (block_a, block_v)) in (0i32..).zip(blocks_a.into_iter().zip(blocks_v)) {
            space.set([0, y, h], block_a)?;
            space.set([2, y, h], block_v)?;
        }
    }
    Ok((space, txn))
}

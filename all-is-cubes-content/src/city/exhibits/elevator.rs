use super::prelude::*;

#[exhibit(
    name: "Elevator",
    subtitle: "OUT OF SERVICE",
    placement: Placement::Underground,
)]
fn ELEVATOR(_: Context<'_>) {
    // The exhibit placement algorithm doesn't care about exhibits punching through the ground.
    // So, by defining a tall, underground exhibit, we can get access from both floors.
    // TODO: Add some way to get up that isn't just flying, and a connection to the surface road.
    // But this is sufficient to be a "hey there is something below" signal.

    let space = Space::empty(GridAab::from_lower_size([0, 0, 0], [3, 16, 3]));

    Ok((space, ExhibitTransaction::default()))
}

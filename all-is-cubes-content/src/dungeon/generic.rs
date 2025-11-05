use alloc::boxed::Box;
use alloc::sync::Arc;

use itertools::Itertools;

use all_is_cubes::arcstr;
use all_is_cubes::character::Spawn;
use all_is_cubes::euclid::Size3D;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{
    Cube, Face6, FaceMap, GridAab, GridCoordinate, GridSize, GridSizeCoord, GridVector,
    VectorOps as _, Vol,
};
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::util::YieldProgress;

use crate::framework;

/// Defines the dimensions that dungeon room construction must live within.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DungeonGrid {
    /// The basic room interior which is going to be duplicated to form the room grid.
    /// This _does not_ include the given wall thickness.
    pub room_box: GridAab,
    /// Thickness of each wall of the room, located outside of `room_box` but still
    /// belonging to that room and not its neighbors.
    ///
    /// TODO: `FaceMap` is a _possible_ representation but not the _most robust_ and we
    /// should instead make this a `GridAab` and add a `GridAab::minkowski_sum` method.
    pub room_wall_thickness: FaceMap<u16>,
    /// Thickness of the space lying between each pair of rooms, belonging to neither.
    pub gap_between_walls: Size3D<u16, Cube>,
}

impl DungeonGrid {
    /// Returns the distance from the end of one room interior to the beginning of
    /// another, along each axis.
    #[rustfmt::skip]
    pub fn gap_between_rooms(&self) -> GridSize {
        GridSize::from(self.gap_between_walls.map(GridSizeCoord::from).to_vector()
            + self.room_wall_thickness.negatives().map(GridSizeCoord::from)
            + self.room_wall_thickness.positives().map(GridSizeCoord::from))
    }

    /// Returns the distances from one room to the same point on the next room, along each axis.
    pub fn room_spacing(&self) -> GridSize {
        self.room_box.size() + self.gap_between_rooms()
    }

    /// Returns the translation which would be applied to move `self.room_box` to the
    /// location of a specific room.
    pub fn room_translation(&self, room_position: Cube) -> GridVector {
        room_position
            .lower_bounds()
            .to_vector()
            .component_mul(self.room_spacing().to_vector().to_i32())
    }

    pub fn room_box_including_walls(&self) -> GridAab {
        self.room_box.expand(self.room_wall_thickness.map(|_, c| c.into()))
    }

    pub fn room_box_at(&self, room_position: Cube) -> GridAab {
        self.room_box.translate(self.room_translation(room_position))
    }

    /// Returns the volume which lies between two rooms and meets their adjoining faces.
    #[expect(dead_code)] // TODO: superseded in use by theme-specific sizes; review if should keep
    pub fn shared_wall_at(&self, room_position: Cube, face: Face6) -> GridAab {
        self.room_box_at(room_position)
            .abut(
                face,
                GridCoordinate::from(self.room_wall_thickness[face])
                    + GridCoordinate::from(self.room_wall_thickness[face.opposite()])
                    + GridCoordinate::from(self.gap_between_walls[face.axis()]),
            )
            .unwrap()
    }

    /// Compute the minimum size of `Space` that will fit all the rooms with coordinates
    /// in `rooms`.
    ///
    /// This includes the `room_wall_thickness` of the outermost rooms, but does not
    /// include a `gap_between_walls` outside of that, since that may not be wanted and
    /// is easy to add using [`GridAab::expand()`].
    ///
    /// TODO: This is off by at least 1 (in the too-big direction); write tests and fix.
    pub fn minimum_space_for_rooms(&self, rooms: GridAab) -> GridAab {
        let spacing = self.room_spacing().to_i32().to_vector();
        let basic_size = GridAab::from_lower_upper(
            rooms.lower_bounds().to_vector().component_mul(spacing).to_point(),
            rooms
                .upper_bounds()
                .to_vector()
                .component_mul(spacing)
                .to_point()
                // Correct "fencepost error": spacing * number of rooms has one extra
                // "post" (gap_between_walls).
                - self.gap_between_walls.map(GridCoordinate::from),
        );
        // basic_size has the correct size, but not the correct position relative to room_box
        // and its walls
        basic_size.translate(self.room_box_including_walls().lower_bounds().to_vector())
    }
}

pub trait Theme<R> {
    fn passes(&self) -> usize;

    // TODO: Replace `&mut Space` with transactions so we can use this post-startup?
    fn place_room(
        &self,
        ctx: &mut space::Mutation<'_, '_>,
        pass_index: usize,
        map: Vol<&[R]>,
        position_in_room_grid: Cube,
        value: &R,
    ) -> Result<Option<Spawn>, InGenError>;
}

pub fn build_dungeon<Room, ThemeT>(
    read_ticket: ReadTicket<'_>,
    space: &mut Space,
    theme: Arc<ThemeT>,
    map: Vol<Arc<[Room]>>,
    progress: YieldProgress,
) -> impl Future<Output = Result<(), InGenError>>
where
    Room: Send + Sync + 'static,
    ThemeT: Theme<Room> + Send + Sync + 'static,
{
    let region = space.bounds(); // TODO: compute individual room regions

    framework::Queue::from_iter(
        Itertools::cartesian_product(0..theme.passes(), map.bounds().interior_iter()).map(
            move |(pass, room_position)| {
                let theme = theme.clone();
                let map = map.clone();
                framework::Task {
                    label: arcstr::format!("pass {pass} @ {room_position:?}", pass = pass + 1),
                    time_estimate: 1.0,
                    region,
                    operation: framework::Operation::Custom(Box::new(move |m| {
                        // TODO: spawn as return value is a quick kludge to organize mutations,
                        // but there is probably some more general thing that is appropriate.
                        let maybe_spawn = theme.place_room(
                            m,
                            pass,
                            map.as_ref(),
                            room_position,
                            map.get(room_position).unwrap(),
                        )?;

                        if let Some(spawn) = maybe_spawn {
                            m.set_spawn(spawn);
                        }
                        Ok(())
                    })),
                }
            },
        ),
    )
    .run(progress, read_ticket, space)
}

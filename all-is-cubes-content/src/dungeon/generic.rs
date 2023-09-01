use all_is_cubes::util::YieldProgress;

use all_is_cubes::cgmath::{ElementWise as _, EuclideanSpace as _, Vector3};
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{
    Cube, Face6, FaceMap, GridAab, GridArray, GridCoordinate, GridPoint, GridVector,
};
use all_is_cubes::space::Space;

/// Defines the dimensions that dungeon room construction must live within.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DungeonGrid {
    /// The basic room interior which is going to be duplicated to form the room grid.
    /// This _does not_ include the given wall thickness.
    pub room_box: GridAab,
    /// Thickness of each wall of the room, located outside of `room_box` but still
    /// belonging to that room and not its neighbors.
    ///
    /// TODO: FaceMap is a _possible_ representation but not the _most robust_ and we
    /// should instead make this a `GridAab` and add a `GridAab::minkowski_sum` method.
    pub room_wall_thickness: FaceMap<u16>,
    /// Thickness of the space lying between each pair of rooms, belonging to neither.
    pub gap_between_walls: Vector3<u16>,
}

impl DungeonGrid {
    /// Returns the distance from the end of one room interior to the beginning of
    /// another, along each axis.
    #[rustfmt::skip]
    pub fn gap_between_rooms(&self) -> Vector3<GridCoordinate> {
        self.gap_between_walls.map(GridCoordinate::from)
            + self.room_wall_thickness.negatives().map(GridCoordinate::from)
            + self.room_wall_thickness.positives().map(GridCoordinate::from)
    }

    /// Returns the distances from one room to the same point on the next room, along each axis.
    pub fn room_spacing(&self) -> Vector3<GridCoordinate> {
        self.room_box.size() + self.gap_between_rooms()
    }

    /// Returns the translation which would be applied to move `self.room_box` to the
    /// location of a specific room.
    pub fn room_translation(&self, room_position: Cube) -> GridVector {
        room_position
            .lower_bounds()
            .to_vec()
            .mul_element_wise(self.room_spacing())
    }

    pub fn room_box_including_walls(&self) -> GridAab {
        self.room_box
            .expand(self.room_wall_thickness.map(|_, c| GridCoordinate::from(c)))
    }

    pub fn room_box_at(&self, room_position: Cube) -> GridAab {
        self.room_box
            .translate(self.room_translation(room_position))
    }

    /// Returns the volume which lies between two rooms and meets their adjoining faces.
    #[allow(dead_code)] // TODO: superseded in use by theme-specific sizes; review if should keep
    pub fn shared_wall_at(&self, room_position: Cube, face: Face6) -> GridAab {
        self.room_box_at(room_position)
            .abut(
                face,
                GridCoordinate::from(self.room_wall_thickness[face])
                    + GridCoordinate::from(self.room_wall_thickness[face.opposite()])
                    + GridCoordinate::from(self.gap_between_walls[face.axis_number()]),
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
        let spacing = GridPoint::from_vec(self.room_spacing());
        let basic_size = GridAab::from_lower_upper(
            rooms.lower_bounds().mul_element_wise(spacing),
            rooms
                .upper_bounds()
                .mul_element_wise(spacing)
                // Correct "fencepost error": spacing * number of rooms has one extra
                // "post" (gap_between_walls).
                .sub_element_wise(GridPoint::from_vec(
                    self.gap_between_walls.map(GridCoordinate::from),
                )),
        );
        // basic_size has the correct size, but not the correct position relative to room_box
        // and its walls
        basic_size.translate(self.room_box_including_walls().lower_bounds().to_vec())
    }
}

pub trait Theme<R> {
    fn grid(&self) -> &DungeonGrid;

    fn passes(&self) -> usize;

    // TODO: Replace `&mut Space` with transactions so we can use this post-startup?
    fn place_room(
        &self,
        space: &mut Space,
        pass_index: usize,
        map: &GridArray<R>,
        position_in_room_grid: Cube,
        value: &R,
    ) -> Result<(), InGenError>;
}

pub async fn build_dungeon<Room, ThemeT: Theme<Room>>(
    space: &mut Space,
    theme: &ThemeT,
    map: &GridArray<Room>,
    progress: YieldProgress,
) -> Result<(), InGenError> {
    let passes = theme.passes();
    for (pass, mut progress) in (0..passes).zip(progress.split_evenly(passes)) {
        progress.set_label(format_args!("pass {pass}/{passes}", pass = pass + 1));
        progress.progress(0.0).await;
        for (room_position, mut progress) in map
            .bounds()
            .interior_iter()
            .zip(progress.split_evenly(map.bounds().volume()))
        {
            progress.set_label(format_args!(
                "pass {pass} @ {room_position:?}",
                pass = pass + 1
            ));
            progress.progress(0.0).await;
            theme.place_room(
                space,
                pass,
                map,
                room_position,
                map.get(room_position).unwrap(),
            )?;
            progress.progress(1.0).await;
        }
    }

    Ok(())
}

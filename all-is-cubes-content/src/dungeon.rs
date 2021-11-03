// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Construction of “dungeons”, meaning building interiors assembled of a variety of rooms
//! on a common grid.
//!
//! TODO: This module is currently private but should be made public if these construction
//! tools turn out reasonably generic.

use maze_generator::prelude::{Direction, FieldType, Generator};

use all_is_cubes::block::{Block, AIR};
use all_is_cubes::cgmath::{ElementWise as _, EuclideanSpace as _, Vector3};
use all_is_cubes::character::Spawn;
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{Face, FaceMap, FreeCoordinate, GridCoordinate, GridPoint, GridRotation};
use all_is_cubes::rgb_const;
use all_is_cubes::space::{Grid, Space};
use all_is_cubes::universe::Universe;

use crate::{four_walls, DemoBlocks, LandscapeBlocks};

/// Defines the dimensions that dungeon room construction must live within.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct DungeonGrid {
    /// The basic room interior which is going to be duplicated to form the room grid.
    /// This _does not_ include the given wall thickness.
    pub room_box: Grid,
    /// Thickness of each wall of the room, located outside of `room_box` but still
    /// belonging to that room and not its neighbors.
    ///
    /// TODO: FaceMap is a _possible_ representation but not the _most robust_ and we
    /// should instead make this a `Grid` and add a `Grid::minkowski_sum` method.
    pub room_wall_thickness: FaceMap<u16>,
    /// Thickness of the space lying between each pair of rooms, belonging to neither.
    pub gap_between_walls: Vector3<u16>,
}

impl DungeonGrid {
    /// Returns the distance from the end of one room interior to the beginning of
    /// another, along each axis.
    #[rustfmt::skip]
    fn gap_between_rooms(&self) -> Vector3<GridCoordinate> {
        self.gap_between_walls.map(GridCoordinate::from)
            + self.room_wall_thickness.negatives().map(GridCoordinate::from)
            + self.room_wall_thickness.positives().map(GridCoordinate::from)
    }

    /// Returns the distances from one room to the same point on the next room, along each axis.
    fn room_spacing(&self) -> Vector3<GridCoordinate> {
        self.room_box.size() + self.gap_between_rooms()
    }

    fn room_box_including_walls(&self) -> Grid {
        self.room_box
            .expand(self.room_wall_thickness.map(|_, c| GridCoordinate::from(c)))
    }

    fn room_box_at(&self, room_position: GridPoint) -> Grid {
        self.room_box
            .translate(room_position.to_vec().mul_element_wise(self.room_spacing()))
    }

    /// Returns the volume which lies between two rooms and meets their adjoining faces.
    fn shared_wall_at(&self, room_position: GridPoint, face: Face) -> Grid {
        self.room_box_at(room_position)
            .abut(
                face,
                GridCoordinate::from(self.room_wall_thickness[face])
                    + GridCoordinate::from(self.room_wall_thickness[face.opposite()])
                    + face
                        .axis_number()
                        .map(|axis| GridCoordinate::from(self.gap_between_walls[axis]))
                        .unwrap_or(0),
            )
            .unwrap()
    }

    /// Compute the minimum size of `Space` that will fit all the rooms with coordinates
    /// in `rooms`.
    ///
    /// This includes the `room_wall_thickness` of the outermost rooms, but does not
    /// include a `gap_between_walls` outside of that, since that may not be wanted and
    /// is easy to add using [`Grid::expand`].
    ///
    /// TODO: This is off by at least 1 (in the too-big direction); write tests and fix.
    fn minimum_space_for_rooms(&self, rooms: Grid) -> Grid {
        let spacing = GridPoint::from_vec(self.room_spacing());
        let basic_size = Grid::from_lower_upper(
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

/// Data to use to construct specific dungeon rooms.
struct DemoTheme {
    dungeon_grid: DungeonGrid,
    wall_block: Block,
    floor_block: Block,
    lamp_block: Block,
}

impl DemoTheme {
    fn plain_room(
        &self,
        wall_block: Option<&Block>,
        space: &mut Space,
        interior: Grid,
    ) -> Result<(), InGenError> {
        let wall_block = wall_block.unwrap_or(&self.wall_block);

        space.fill_uniform(interior.abut(Face::NY, 1).unwrap(), &self.floor_block)?;
        space.fill_uniform(interior.abut(Face::PY, 1).unwrap(), wall_block)?;

        four_walls(
            interior.expand(FaceMap::repeat(1)),
            |_, _, _, wall_excluding_corners| {
                space.fill_uniform(wall_excluding_corners, wall_block)?;
                Ok::<(), InGenError>(())
            },
        )?;

        // A single light for each room for now
        let top_middle = interior
            .abut(Face::PY, -1)
            .unwrap()
            .center()
            .map(|c| c as GridCoordinate);
        space.set(top_middle, &self.lamp_block)?;
        Ok(())
    }

    fn inside_doorway(
        &self,
        space: &mut Space,
        room_position: GridPoint,
        face: Face,
    ) -> Result<(), InGenError> {
        let wall_box = self.dungeon_grid.shared_wall_at(room_position, face);
        let wall_parallel = GridRotation::CLOCKWISE.transform(face);
        let parallel_axis = wall_parallel.axis_number().unwrap();
        assert!(parallel_axis != 1);

        // Compute a doorway 3-4 wide.
        // TODO: Make this math easier -- with mutation methods on Grid?
        let mut lower = wall_box.lower_bounds();
        let mut upper = wall_box.upper_bounds();
        let mid_doubled = lower[parallel_axis] + upper[parallel_axis];
        lower[parallel_axis] = (mid_doubled - 3) / 2;
        upper[parallel_axis] = (mid_doubled + 4) / 2;
        upper.y = lower.y + 3; // set height
        let doorway_box = Grid::from_lower_upper(lower, upper);

        // Cut doorway
        space.fill_uniform(doorway_box, &AIR)?;

        // Add floor and walls
        space.fill_uniform(doorway_box.abut(Face::NY, 1).unwrap(), &self.floor_block)?;
        space.fill_uniform(
            doorway_box.abut(wall_parallel, 1).unwrap(),
            &self.wall_block,
        )?;
        space.fill_uniform(
            doorway_box.abut(wall_parallel.opposite(), 1).unwrap(),
            &self.wall_block,
        )?;
        space.fill_uniform(doorway_box.abut(Face::PY, 1).unwrap(), &self.wall_block)?; // TODO: ceiling block

        Ok(())
    }
}

/// This function is called from `UniverseTemplate`.
pub(crate) async fn demo_dungeon(universe: &mut Universe) -> Result<Space, InGenError> {
    // TODO: reintroduce random elements separate from the maze.
    // let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(38492);

    let dungeon_grid = DungeonGrid {
        room_box: Grid::new([0, 0, 0], [9, 5, 8]),
        room_wall_thickness: FaceMap::repeat(1),
        gap_between_walls: Vector3::new(1, 1, 1),
    };

    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    let theme = DemoTheme {
        dungeon_grid: dungeon_grid.clone(),
        // TODO: use more appropriate blocks
        wall_block: landscape_blocks[LandscapeBlocks::Stone].clone(),
        floor_block: demo_blocks[DemoBlocks::Road].clone(),
        lamp_block: demo_blocks[DemoBlocks::Lamp].clone(),
    };

    let maze = maze_generator::ellers_algorithm::EllersGenerator::new(None).generate(9, 9);

    let room_grid_bounds = Grid::new([0, 0, 0], [maze.size.0, 1, maze.size.1]);

    let space_bounds = dungeon_grid.minimum_space_for_rooms(room_grid_bounds);
    let mut space = Space::builder(space_bounds)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(space_bounds);
            // TODO: There should be a way to express "spawn with feet in this block",
            // independent of height.
            spawn.set_eye_position(
                dungeon_grid
                    .room_box_at(m2gp(maze.start))
                    .abut(Face::NY, 0)
                    .unwrap()
                    .center()
                    .map(FreeCoordinate::from)
                    + Vector3::new(0., 2.0, 0.),
            );
            spawn.set_flying(false);

            // Orient towards the first room's exit.
            let start_field = maze.get_field(&maze.start).unwrap();
            for direction in Direction::all() {
                if start_field.has_passage(&direction) {
                    spawn.set_look_direction(d2f(direction).normal_vector());
                    break;
                }
            }

            spawn
        })
        .build_empty();

    let start_wall = Block::from(rgb_const!(1.0, 0.0, 0.0));
    let goal_wall = Block::from(rgb_const!(0.0, 0.8, 0.0));
    for room_position in room_grid_bounds.interior_iter() {
        let interior = dungeon_grid.room_box_at(room_position);

        theme.plain_room(
            match maze.get_field(&gp2m(room_position)).unwrap().field_type {
                FieldType::Start => Some(&start_wall),
                FieldType::Goal => Some(&goal_wall),
                FieldType::Normal => None,
            },
            &mut space,
            interior,
        )?;
    }

    // Doorways
    for room in room_grid_bounds.interior_iter() {
        let coord = gp2m(room);
        let field = maze.get_field(&coord).unwrap();
        for direction in [Direction::East, Direction::South] {
            let face = d2f(direction);
            // get_field() check is to work around the maze generator sometimes producing
            // out-of-bounds passages.
            if field.has_passage(&direction) && maze.get_field(&coord.next(&direction)).is_some() {
                maze.get_field(&gp2m(room).next(&direction)).unwrap();
                theme.inside_doorway(&mut space, room, face)?;
            }
        }
    }

    Ok(space)
}

fn m2gp(p: maze_generator::prelude::Coordinates) -> GridPoint {
    GridPoint::new(p.x, 0, p.y)
}

fn gp2m(p: GridPoint) -> maze_generator::prelude::Coordinates {
    maze_generator::prelude::Coordinates { x: p.x, y: p.z }
}

fn d2f(direction: Direction) -> Face {
    match direction {
        Direction::North => Face::NZ,
        Direction::East => Face::PX,
        Direction::South => Face::PZ,
        Direction::West => Face::NX,
    }
}

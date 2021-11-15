// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use all_is_cubes::inv::Tool;
use maze_generator::prelude::{Direction, Field, FieldType, Generator};
use rand::{Rng, SeedableRng};

use all_is_cubes::block::{Block, BlockCollision, AIR};
use all_is_cubes::cgmath::{EuclideanSpace as _, InnerSpace as _, Vector3};
use all_is_cubes::character::Spawn;
use all_is_cubes::linking::{BlockModule, BlockProvider, GenError, InGenError};
use all_is_cubes::math::{
    Face, FaceMap, FreeCoordinate, GridCoordinate, GridPoint, GridRotation, GridVector, Rgb,
};
use all_is_cubes::rgb_const;
use all_is_cubes::space::{Grid, GridArray, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::dungeon::{build_dungeon, d2f, maze_to_array, DungeonGrid, Theme};
use crate::{four_walls, DemoBlocks, LandscapeBlocks};

struct DemoRoom {
    // TODO: remove dependency on maze gen entirely
    maze_field: Field,
    //windowed_faces: FaceMap<bool>,
    corridor_only: bool,
    lit: bool,
}

/// Data to use to construct specific dungeon rooms.
struct DemoTheme {
    dungeon_grid: DungeonGrid,
    /// Same coordinate system as `dungeon_grid.room_box`.
    /// Pick 2 out of 3 axes to define the bounds of a corridor/doorway on the third axis.
    corridor_box: Grid,
    blocks: BlockProvider<DungeonBlocks>,
    wall_block: Block,
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

        space.fill_uniform(interior.abut(Face::NY, 1).unwrap(), &self.blocks[FloorTile])?;
        space.fill_uniform(interior.abut(Face::PY, 1).unwrap(), wall_block)?;

        four_walls(
            interior.expand(FaceMap::repeat(1)),
            |_, _, _, wall_excluding_corners| {
                space.fill_uniform(wall_excluding_corners, wall_block)?;
                Ok::<(), InGenError>(())
            },
        )?;

        Ok(())
    }

    fn inside_doorway(
        &self,
        space: &mut Space,
        map: &GridArray<DemoRoom>,
        room_position: GridPoint,
        face: Face,
    ) -> Result<(), InGenError> {
        let passage_axis = face.axis_number().unwrap();

        let mut room_1_box = self.actual_room_box(room_position, &map[room_position]);
        let mut room_2_box = self.actual_room_box(
            room_position + face.normal_vector(),
            &map[room_position + face.normal_vector()],
        );
        if room_1_box.lower_bounds()[passage_axis] > room_2_box.lower_bounds()[passage_axis] {
            std::mem::swap(&mut room_1_box, &mut room_2_box);
        }

        let wall_parallel = GridRotation::CLOCKWISE.transform(face);
        let parallel_axis = wall_parallel.axis_number().unwrap();
        assert!(parallel_axis != 1);

        let doorway_box = {
            let corridor_box = self
                .corridor_box
                .translate(self.dungeon_grid.room_translation(room_position));
            // TODO: Add Grid operations to make this easier
            let mut lower = corridor_box.lower_bounds();
            let mut upper = corridor_box.upper_bounds();
            lower[passage_axis] = room_1_box.upper_bounds()[passage_axis];
            upper[passage_axis] = room_2_box.lower_bounds()[passage_axis];
            Grid::from_lower_upper(lower, upper)
        };

        // Cut doorway
        space.fill_uniform(doorway_box, &AIR)?;

        // Add floor and walls
        space.fill_uniform(
            doorway_box.abut(Face::NY, 1).unwrap(),
            &self.blocks[FloorTile],
        )?;
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

    /// Box that might be smaller than the DungeonGrid's box.
    /// TODO: Should we teach DungeonGrid to help with this?
    fn actual_room_box(&self, room_position: GridPoint, room_data: &DemoRoom) -> Grid {
        if room_data.corridor_only {
            self.corridor_box
                .translate(self.dungeon_grid.room_translation(room_position))
        } else {
            self.dungeon_grid.room_box_at(room_position)
        }
    }
}

impl Theme<DemoRoom> for DemoTheme {
    fn grid(&self) -> &DungeonGrid {
        &self.dungeon_grid
    }

    fn passes(&self) -> usize {
        2
    }

    fn place_room(
        &self,
        space: &mut Space,
        pass_index: usize,
        map: &GridArray<DemoRoom>,
        room_position: GridPoint,
        room_data: &DemoRoom,
    ) -> Result<(), InGenError> {
        // TODO: put in struct, or eliminate
        let start_wall = Block::from(rgb_const!(1.0, 0.0, 0.0));
        let goal_wall = Block::from(rgb_const!(0.0, 0.8, 0.0));

        let interior = self.actual_room_box(room_position, room_data);
        let wall_type = match room_data.maze_field.field_type {
            FieldType::Start => Some(&start_wall),
            FieldType::Goal => Some(&goal_wall),
            FieldType::Normal => None,
        };

        match pass_index {
            0 => {
                self.plain_room(wall_type, space, interior)?;

                if room_data.lit {
                    let top_middle = interior
                        .abut(Face::PY, -1)
                        .unwrap()
                        .center()
                        .map(|c| c as GridCoordinate);
                    space.set(
                        top_middle,
                        if room_data.corridor_only {
                            &self.blocks[CorridorLight]
                        } else {
                            &self.lamp_block
                        },
                    )?;
                }
            }
            1 => {
                for direction in [Direction::East, Direction::South] {
                    let face = d2f(direction);
                    let neighbor = room_position + face.normal_vector();
                    // contains_cube() check is to work around the maze generator sometimes producing
                    // out-of-bounds passages.
                    if room_data.maze_field.has_passage(&direction)
                        && map.grid().contains_cube(neighbor)
                    {
                        self.inside_doorway(space, map, room_position, face)?;
                    }
                }

                // Set spawn.
                // TODO: Don't unconditionally override spawn; instead communicate this out.
                if matches!(room_data.maze_field.field_type, FieldType::Start) {
                    let mut spawn = Spawn::default_for_new_space(space.grid());
                    // TODO: There should be a way to express "spawn with feet in this block",
                    // independent of height.
                    spawn.set_eye_position(
                        interior
                            .abut(Face::NY, 0)
                            .unwrap()
                            .center()
                            .map(FreeCoordinate::from)
                            + Vector3::new(0., 2.0, 0.),
                    );
                    spawn.set_inventory(vec![
                        Tool::RemoveBlock { keep: true }.into(),
                        Tool::Jetpack { active: false }.into(),
                    ]);

                    // Orient towards the first room's exit.
                    for direction in Direction::all() {
                        if room_data.maze_field.has_passage(&direction) {
                            spawn.set_look_direction(d2f(direction).normal_vector());
                            break;
                        }
                    }

                    *space.spawn_mut() = spawn;
                }
            }
            _ => unreachable!(),
        }
        Ok(())
    }
}

/// This function is called from `UniverseTemplate`.
pub(crate) async fn demo_dungeon(
    universe: &mut Universe,
    progress: YieldProgress,
) -> Result<Space, InGenError> {
    install_dungeon_blocks(universe)?;

    // TODO: reintroduce random elements separate from the maze.
    let mut rng = rand_xoshiro::Xoshiro256Plus::from_entropy();

    let dungeon_grid = DungeonGrid {
        room_box: Grid::new([0, 0, 0], [9, 5, 9]),
        room_wall_thickness: FaceMap::repeat(1),
        gap_between_walls: Vector3::new(1, 1, 1),
    };

    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    let theme = DemoTheme {
        dungeon_grid: dungeon_grid.clone(),
        corridor_box: Grid::new([3, 0, 3], [3, 3, 3]),
        blocks: BlockProvider::using(universe)?,
        // TODO: use more appropriate blocks
        wall_block: landscape_blocks[LandscapeBlocks::Stone].clone(),
        lamp_block: demo_blocks[DemoBlocks::Lamp].clone(),
    };

    let maze =
        maze_to_array(&maze_generator::ellers_algorithm::EllersGenerator::new(None).generate(9, 9));
    let dungeon_map = maze.map(|maze_field| DemoRoom {
        maze_field,
        //windowed_faces: FaceMap::repeat(rng.gen_bool(0.1)),
        corridor_only: rng.gen_bool(0.5),
        lit: rng.gen_bool(0.98),
    });

    let space_bounds = dungeon_grid.minimum_space_for_rooms(dungeon_map.grid());
    let mut space = Space::builder(space_bounds).build_empty();

    build_dungeon(&mut space, &theme, &dungeon_map, progress).await?;

    Ok(space)
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub(crate) enum DungeonBlocks {
    CorridorLight,
    FloorTile,
}
impl BlockModule for DungeonBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/dungeon-blocks"
    }
}
use DungeonBlocks::*;

/// Add [`DungeonBlocks`] to the universe.
pub fn install_dungeon_blocks(universe: &mut Universe) -> Result<(), GenError> {
    let resolution = 16;
    let resolution_g = GridCoordinate::from(resolution);
    let one_diagonal = GridVector::new(1, 1, 1);
    let center_point_doubled = GridPoint::from_vec(one_diagonal * resolution_g);

    let light_voxel = Block::from(Rgb::new(0.7, 0.7, 0.0));
    let stone_color = Rgb::new(0.5, 0.5, 0.5);
    let stone_floor_voxel = Block::from(stone_color);
    let stone_grout_1 = Block::from(stone_color * 0.8);
    let stone_grout_2 = Block::from(stone_color * 0.9);

    use DungeonBlocks::*;
    BlockProvider::<DungeonBlocks>::new(|key| {
        Ok(match key {
            CorridorLight => Block::builder()
                .display_name("Corridor Light")
                .light_emission(Rgb::new(8.0, 7.0, 0.7))
                .collision(BlockCollision::Recur)
                .voxels_fn(universe, resolution, |cube| {
                    let centered = cube * 2 - center_point_doubled;
                    if centered.y > centered.x.abs() && centered.y > centered.z.abs() {
                        &light_voxel
                    } else {
                        &AIR
                    }
                })?
                .build(),
            FloorTile => Block::builder()
                .display_name("Floor Tile")
                .voxels_fn(universe, resolution, |cube| {
                    let edges = cube
                        .to_vec()
                        .map(|c| (c == 0 || c == resolution_g - 1) as u8)
                        .dot(Vector3::new(1, 1, 1));
                    let bottom_edges = cube
                        .to_vec()
                        .map(|c| (c == 0) as u8)
                        .dot(Vector3::new(1, 1, 1));
                    if edges >= 2 && bottom_edges > 0 {
                        &stone_grout_1
                    } else if edges >= 2 {
                        &stone_grout_2
                    } else {
                        &stone_floor_voxel
                    }
                })?
                .build(),
        })
    })?
    .install(universe)?;

    Ok(())
}

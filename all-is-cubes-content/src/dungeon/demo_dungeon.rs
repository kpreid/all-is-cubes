#![allow(unused_qualifications)] // macro false positive

use core::f64::consts::TAU;
use core::mem;

use exhaust::Exhaust;
use rand::prelude::SliceRandom;
use rand::{Rng, SeedableRng};

use all_is_cubes::block::{Block, Resolution::*, RotationPlacementRule, AIR};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::load_image::space_from_image;
use all_is_cubes::content::palette;
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::euclid::Size3D;
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::{BlockModule, BlockProvider, GenError, InGenError};
use all_is_cubes::math::{
    Axis, Cube, Face6, FaceMap, GridAab, GridArray, GridCoordinate, GridRotation, GridSize,
    GridVector, Rgb, Rgba,
};
use all_is_cubes::space::{LightPhysics, Space};
use all_is_cubes::time;
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;
use all_is_cubes::{include_image, rgb_const};

use crate::alg::four_walls;
use crate::dungeon::{build_dungeon, generate_maze, DungeonGrid, MazeRoomKind, Theme};
use crate::{tree, DemoBlocks, LandscapeBlocks, TemplateParameters};

const WINDOW_PATTERN: [GridCoordinate; 3] = [-2, 0, 2];

#[derive(Clone, Debug)]
struct DemoRoom {
    maze_kind: MazeRoomKind,

    /// In a *relative* room coordinate system (1 unit = 1 room box),
    /// how big is this room? Occupying multiple rooms' space if this
    /// is not equal to `GridAab::ORIGIN_CUBE`.
    extended_bounds: GridAab,

    /// Which walls/faces have something on them.
    wall_features: FaceMap<WallFeature>,

    floor: FloorKind,
    corridor_only: bool,
    lit: bool,

    grants_item: Option<Block>,
}

impl DemoRoom {
    /// Area of dungeon-room-cubes this room data actually occupies
    fn extended_map_bounds(&self) -> GridAab {
        self.extended_bounds
    }
}

/// What kind of holes a wall has.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum WallFeature {
    /// Blank wall.
    Blank,
    /// Opening to a corridor.
    Passage { gate: bool, blocked: bool },
    /// Window to the outside world.
    Window,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum FloorKind {
    Solid,
    Chasm,
    Bridge,
}

/// Data to use to construct specific dungeon rooms.
struct DemoTheme {
    dungeon_grid: DungeonGrid,
    /// Same coordinate system as `dungeon_grid.room_box`.
    /// Pick 2 out of 3 axes to define the bounds of a corridor/doorway on the third axis.
    corridor_box: GridAab,
    blocks: BlockProvider<DungeonBlocks>,
    wall_block: Block,
    lamp_block: Block,
    /// TODO: replace window glass with openings that are too small to pass through
    window_glass_block: Block,
    item_pedestal: Block,
}

impl DemoTheme {
    fn plain_room(
        &self,
        wall_block: Option<&Block>,
        space: &mut Space,
        interior: GridAab,
    ) -> Result<(), InGenError> {
        let wall_block = wall_block.unwrap_or(&self.wall_block);

        crate::BoxStyle::from_whole_blocks_for_walls(
            Some(wall_block.clone()),
            Some(self.blocks[FloorTile].clone()),
            Some(wall_block.clone()),
            None,
        )
        .with_interior(Some(AIR))
        .create_box(interior.expand(FaceMap::repeat(1)))
        .execute(space, &mut transaction::no_outputs)?;

        Ok(())
    }

    fn inside_doorway(
        &self,
        space: &mut Space,
        map: &GridArray<Option<DemoRoom>>,
        room_position: Cube,
        face: Face6,
        has_gate: bool,
        blocked: bool,
    ) -> Result<(), InGenError> {
        let passage_axis = face.axis();

        let mut room_1_box = self.actual_room_box(
            room_position,
            map[room_position]
                .as_ref()
                .expect("passage led to nonexistent room"),
        );
        let mut room_2_box = self.actual_room_box(
            room_position + face.normal_vector(),
            map[room_position + face.normal_vector()]
                .as_ref()
                .expect("passage led to nonexistent room"),
        );
        if room_1_box.lower_bounds()[passage_axis] > room_2_box.lower_bounds()[passage_axis] {
            mem::swap(&mut room_1_box, &mut room_2_box);
        }

        let wall_parallel = GridRotation::CLOCKWISE.transform(face);
        let parallel_axis = wall_parallel.axis();
        assert!(parallel_axis != Axis::Y);

        let rotate_nz_to_face = GridRotation::from_to(Face6::NZ, face, Face6::PY).unwrap();

        let doorway_box = {
            let corridor_box = self
                .corridor_box
                .translate(self.dungeon_grid.room_translation(room_position));
            // TODO: Add GridAab operations to make this easier
            let mut lower = corridor_box.lower_bounds();
            let mut upper = corridor_box.upper_bounds();
            lower[passage_axis] = room_1_box.upper_bounds()[passage_axis];
            upper[passage_axis] = room_2_box.lower_bounds()[passage_axis];
            GridAab::from_lower_upper(lower, upper)
        };

        // Cut doorway
        space.fill_uniform(doorway_box, &AIR)?;

        // Add floor and walls
        space.fill_uniform(
            doorway_box.abut(Face6::NY, 1).unwrap(),
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
        space.fill_uniform(doorway_box.abut(Face6::PY, 1).unwrap(), &self.wall_block)?; // TODO: ceiling block

        // Gate
        if has_gate {
            let gate_box = doorway_box
                .abut(face, -1)
                .unwrap()
                .translate(face.opposite().normal_vector() * doorway_box.size()[face.axis()] / 2);
            let gate_side_1 = gate_box.abut(wall_parallel.opposite(), -1).unwrap();
            let gate_side_2 = gate_box
                .abut(wall_parallel, if blocked { -2 } else { -1 })
                .unwrap();
            space.fill_uniform(
                gate_side_2,
                &self.blocks[Gate].clone().rotate(rotate_nz_to_face),
            )?;
            space.fill_uniform(
                gate_side_1,
                &self.blocks[GatePocket].clone().rotate(rotate_nz_to_face),
            )?;
            // TODO: add opening/closing mechanism and make some of these outright blocked
        } else if blocked {
            // TODO: either implement this or change the schema so it can't happen
            panic!("can't block gateless passage");
        }

        Ok(())
    }

    /// Box of the room, in space coordinates, that might be smaller or bigger than the
    /// [`DungeonGrid`]'s box.
    /// TODO: Should we teach `DungeonGrid` to help with this?
    fn actual_room_box(&self, room_position: Cube, room_data: &DemoRoom) -> GridAab {
        if room_data.corridor_only {
            self.corridor_box
                .translate(self.dungeon_grid.room_translation(room_position))
        } else {
            let eb = room_data.extended_map_bounds();
            self.dungeon_grid
                .room_box_at(room_position + eb.lower_bounds().to_vector())
                .union(self.dungeon_grid.room_box_at(
                    room_position + eb.upper_bounds().to_vector() - GridVector::new(1, 1, 1),
                ))
                .unwrap()
        }
    }
}

impl Theme<Option<DemoRoom>> for DemoTheme {
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
        map: &GridArray<Option<DemoRoom>>,
        room_position: Cube,
        room_data: &Option<DemoRoom>,
    ) -> Result<(), InGenError> {
        let room_data = match room_data.as_ref() {
            Some(room_data) => room_data,
            None => return Ok(()),
        };

        // TODO: put in struct, or eliminate
        let start_wall = Block::from(rgb_const!(1.0, 0.0, 0.0));
        let goal_wall = Block::from(rgb_const!(0.0, 0.8, 0.0));

        let interior = self.actual_room_box(room_position, room_data);
        let wall_type = match room_data.maze_kind {
            MazeRoomKind::Start => Some(&start_wall),
            MazeRoomKind::Goal => Some(&goal_wall),
            MazeRoomKind::Path | MazeRoomKind::OffPath => None,
            MazeRoomKind::Unoccupied => unreachable!(),
        };
        let floor_layer = self
            .dungeon_grid
            .room_box_at(room_position)
            .abut(Face6::NY, 1)
            .unwrap();

        match pass_index {
            0 => {
                self.plain_room(wall_type, space, interior)?;

                // Spikes on the bottom of the pit
                // (TODO: revise this condition when staircase-ish rooms exist)
                if room_data.extended_map_bounds().lower_bounds().y < 0 {
                    assert!(!room_data.corridor_only, "{room_data:?}");
                    space.fill_uniform(
                        interior.abut(Face6::NY, -1).unwrap(),
                        &self.blocks[Spikes],
                    )?;
                }

                match room_data.floor {
                    FloorKind::Solid => {
                        space.fill_uniform(floor_layer, &self.blocks[FloorTile])?;
                    }
                    FloorKind::Chasm => { /* TODO: little platforms */ }
                    FloorKind::Bridge => {
                        let midpoint = Cube::containing(floor_layer.center()).unwrap();
                        for direction in [Face6::NX, Face6::NZ, Face6::PX, Face6::PZ] {
                            if room_data.wall_features[direction] != WallFeature::Blank {
                                let wall_cube = Cube::containing(
                                    floor_layer.abut(direction, -1).unwrap().center(),
                                )
                                .unwrap();
                                let bridge_box = GridAab::single_cube(midpoint)
                                    .union(GridAab::single_cube(wall_cube))
                                    .unwrap();
                                space.fill_uniform(bridge_box, &self.blocks[FloorTile])?;
                            }
                        }
                    }
                }

                if room_data.lit {
                    let top_middle =
                        Cube::containing(interior.abut(Face6::PY, -1).unwrap().center()).unwrap();
                    space.set(
                        top_middle,
                        if room_data.corridor_only {
                            &self.blocks[CorridorLight]
                        } else {
                            &self.lamp_block
                        },
                    )?;
                }

                // Windowed walls
                let window_y = self
                    .dungeon_grid
                    .room_box_at(room_position)
                    .lower_bounds()
                    .y
                    + 1;
                four_walls(
                    interior.expand(FaceMap::repeat(1)),
                    |origin, along_wall, length, wall_excluding_corners_box| {
                        let wall = GridRotation::CLOCKWISE.transform(along_wall); // TODO: make four_walls provide this in a nice name
                        if let WallFeature::Window = room_data.wall_features[wall] {
                            let midpoint = length / 2;
                            for step in WINDOW_PATTERN {
                                let mut window_pos =
                                    origin + along_wall.normal_vector() * (midpoint + step);
                                window_pos.y = window_y;
                                if let Some(window_box) =
                                    GridAab::from_lower_size(window_pos, [1, 3, 1])
                                        .intersection(wall_excluding_corners_box)
                                {
                                    space.fill_uniform(window_box, &self.window_glass_block)?;
                                }
                            }
                        }
                        Ok::<(), InGenError>(())
                    },
                )?;

                // Ceiling light port (not handled by four_walls above)
                if let WallFeature::Window = room_data.wall_features[Face6::PY] {
                    let midpoint =
                        Cube::containing(interior.abut(Face6::PY, 1).unwrap().center()).unwrap();
                    for x in WINDOW_PATTERN {
                        for z in WINDOW_PATTERN {
                            space.set(
                                midpoint + GridVector::new(x, 0, z),
                                &self.window_glass_block,
                            )?;
                        }
                    }
                }
            }
            1 => {
                for face in [Face6::PX, Face6::PZ] {
                    if let WallFeature::Passage { gate, blocked } = room_data.wall_features[face] {
                        self.inside_doorway(space, map, room_position, face, gate, blocked)?;
                    }
                }

                // Item.
                if let Some(block) = &room_data.grants_item {
                    // note that this is the nominal floor, not the possibly extended downward floor
                    let floor_middle =
                        Cube::containing(floor_layer.abut(Face6::PY, 1).unwrap().center()).unwrap();
                    space.set(floor_middle, &self.item_pedestal)?;
                    // TODO: This should be in pick-up-able form as opposed to placed,
                    // once such a distinction is actually implemented
                    space.set(floor_middle + GridVector::new(0, 1, 0), block)?;
                }

                // Set spawn.
                // TODO: Don't unconditionally override spawn; instead communicate this out.
                if matches!(room_data.maze_kind, MazeRoomKind::Start) {
                    let mut spawn = Spawn::default_for_new_space(space.bounds());
                    spawn.set_bounds(interior);
                    spawn.set_inventory(vec![
                        Tool::RemoveBlock { keep: true }.into(),
                        Tool::Jetpack { active: false }.into(),
                    ]);

                    // Orient towards the first room's exit.
                    for face in Face6::ALL {
                        if let WallFeature::Passage { .. } = room_data.wall_features[face] {
                            spawn.set_look_direction(face.normal_vector());
                            break;
                        }
                    }

                    space.set_spawn(spawn);
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
    mut progress: YieldProgress,
    params: TemplateParameters,
) -> Result<Space, InGenError> {
    let TemplateParameters {
        seed,
        size: requested_size,
    } = params;
    let seed = seed.unwrap_or(0);

    {
        let mut install_txn = UniverseTransaction::default();
        let blocks_progress = progress.start_and_cut(0.2, "dungeon blocks").await;
        install_dungeon_blocks(&mut install_txn, blocks_progress).await?;
        install_txn.execute(universe, &mut transaction::no_outputs)?;
    }

    let dungeon_grid = DungeonGrid {
        room_box: GridAab::from_lower_size([0, 0, 0], [9, 5, 9]),
        room_wall_thickness: FaceMap::repeat(1),
        gap_between_walls: Size3D::new(1, 1, 1),
    };
    let perimeter_margin = 30;

    let mut requested_rooms = Size3D::from(
        (requested_size.unwrap_or(Size3D::new(135, 40, 135))
            - Size3D::new(perimeter_margin, 0, perimeter_margin))
        .to_vector()
        .component_div(dungeon_grid.room_spacing().to_vector()),
    );
    if requested_rooms.width == 0 || requested_rooms.depth == 0 {
        return Err(InGenError::Other("Size too small".into()));
    }

    // TODO: Add 3D support (avoid collisions with tall rooms + generate stairs).
    // For now, ignore vertical size suggestions entirely
    requested_rooms.height = 1;

    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    let theme = DemoTheme {
        dungeon_grid: dungeon_grid.clone(),
        corridor_box: GridAab::from_lower_size([3, 0, 3], [3, 3, 3]),
        blocks: BlockProvider::using(universe)?,
        // TODO: use more appropriate blocks
        wall_block: landscape_blocks[LandscapeBlocks::Stone].clone(),
        lamp_block: demo_blocks[DemoBlocks::Lamp].clone(),
        window_glass_block: demo_blocks[DemoBlocks::GlassBlock].clone(),
        item_pedestal: demo_blocks[DemoBlocks::Pedestal].clone(),
    };
    // Random assortment of blocks to provide
    // TODO: make this things like keys for doors
    let grantable_items = [
        demo_blocks[DemoBlocks::Lamp].clone(),
        demo_blocks[DemoBlocks::Signboard].clone(),
        // TODO: can't do this until we have an "item" form: &demo_blocks[DemoBlocks::Explosion(0)],
        landscape_blocks[LandscapeBlocks::Leaves(tree::TreeGrowth::Block)].clone(),
        landscape_blocks[LandscapeBlocks::Grass].clone(),
        landscape_blocks[LandscapeBlocks::Dirt].clone(),
        landscape_blocks[LandscapeBlocks::Stone].clone(),
    ];

    // Construct dungeon map
    let maze_progress = progress.start_and_cut(0.1, "generating layout").await;
    let dungeon_map = generate_dungeon_map(seed, requested_rooms, &grantable_items);
    maze_progress.finish().await;

    // Construct space with initial bulk-filled contents
    let mut space = {
        let space_construction_progress = progress.start_and_cut(0.1, "filling the earth").await;
        let space_bounds = dungeon_grid
            .minimum_space_for_rooms(dungeon_map.bounds())
            .expand(FaceMap::symmetric([perimeter_margin, 1, perimeter_margin]));
        let mut space = Space::builder(space_bounds)
            .sky(crate::landscape::sky_with_grass(
                palette::DAY_SKY_COLOR * 2.0,
            ))
            .light_physics(LightPhysics::None) // temporary
            .build();

        // Fill in (under)ground areas
        space.fill_uniform(
            GridAab::from_ranges([space_bounds.x_range(), -1..0, space_bounds.z_range()]),
            &landscape_blocks[LandscapeBlocks::Grass],
        )?;
        space.fill_uniform(
            {
                let mut u = space_bounds.upper_bounds();
                u.y = -1;
                GridAab::from_lower_upper(space_bounds.lower_bounds(), u)
            },
            &landscape_blocks[LandscapeBlocks::Dirt],
        )?;
        space_construction_progress.finish().await;
        space
    };

    let build_progress = progress.start_and_cut(0.2, "building rooms").await;
    build_dungeon(&mut space, &theme, &dungeon_map, build_progress).await?;

    // Enable lighting
    let mut light_progress = progress;
    light_progress.set_label("lighting");
    light_progress.progress(0.0).await;
    let mut physics = space.physics().clone();
    physics.light = LightPhysics::Rays {
        // account for large rooms
        maximum_distance: (dungeon_grid.room_box.size().height * 4) as u16,
    };
    space.set_physics(physics);
    light_progress.progress(0.01).await;
    // Make a rough lighting pass so that we don't have completely black rooms on start.
    space.evaluate_light::<time::NoTime>(254, |_i| {
        // TODO: report progress
        // light_progress.progress(i.max_queue_priority as f32 / 255.0)
    });
    light_progress.finish().await;

    Ok(space)
}

/// Non-async map generation subsection of [`demo_dungeon()`].
fn generate_dungeon_map(
    seed: u64,
    requested_rooms: GridSize,
    grantable_items: &[Block],
) -> all_is_cubes::math::Vol<alloc::boxed::Box<[Option<DemoRoom>]>> {
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(seed);

    let maze = generate_maze(seed, requested_rooms);

    // Expand bounds to allow for extra-tall rooms.
    let expanded_bounds = maze.bounds().expand(FaceMap::symmetric([0, 1, 0]));

    GridArray::from_fn(expanded_bounds, |room_position| {
        let maze_room = maze.get(room_position)?;

        // Allow rooms that are not start or end to have more interesting properties.
        let is_not_end = match maze_room.kind {
            MazeRoomKind::Unoccupied => return None,
            MazeRoomKind::Start | MazeRoomKind::Goal => false,
            MazeRoomKind::Path | MazeRoomKind::OffPath => true,
        };

        let corridor_only = is_not_end && rng.gen_bool(0.5);

        let mut extended_bounds = GridAab::ORIGIN_CUBE;
        // Optional high ceiling
        if !corridor_only && rng.gen_bool(0.25) {
            extended_bounds = extended_bounds.expand(FaceMap::default().with(Face6::PY, 1));
        };
        // Floor pit
        let floor = if !corridor_only && is_not_end && rng.gen_bool(0.25) {
            extended_bounds = extended_bounds.expand(FaceMap::default().with(Face6::NY, 1));
            *[FloorKind::Chasm, FloorKind::Bridge, FloorKind::Bridge]
                .choose(&mut rng)
                .unwrap()
        } else {
            FloorKind::Solid
        };

        let wall_features = {
            FaceMap::from_fn(|face| -> WallFeature {
                let neighbor = room_position + face.normal_vector();
                let neighbor_in_bounds = maze.bounds().contains_cube(neighbor);

                if maze_room.has_passage(face) {
                    let gate = rng.gen_bool(0.25);
                    return WallFeature::Passage {
                        gate,
                        // TODO: generate gates that are actual puzzles with keys, rather than
                        // only permanently open or shut
                        blocked: gate
                            && (maze_room.kind == MazeRoomKind::OffPath
                                || maze[neighbor].kind == MazeRoomKind::OffPath)
                            && rng.gen_bool(0.5),
                    };
                }

                // Create windows only if they look into space outside the maze
                let have_window = if neighbor_in_bounds || corridor_only || face == Face6::NY {
                    false
                } else if face == Face6::PY {
                    // ceilings are more common overall and we want more internally-lit ones
                    rng.gen_bool(0.25)
                } else {
                    rng.gen_bool(0.75)
                };

                if have_window {
                    WallFeature::Window
                } else {
                    WallFeature::Blank
                }
            })
        };

        Some(DemoRoom {
            maze_kind: maze_room.kind,
            extended_bounds,
            wall_features,
            floor,
            corridor_only,
            lit: wall_features[Face6::PY] == WallFeature::Blank && rng.gen_bool(0.75),
            grants_item: (matches!(floor, FloorKind::Solid)
                && !corridor_only
                && is_not_end
                && rng.gen_bool(0.5))
            .then(|| Block::clone(grantable_items.choose(&mut rng).unwrap())),
        })
    })
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, Exhaust)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub(crate) enum DungeonBlocks {
    /// A dim light to attach to ceilings.
    CorridorLight,
    /// Normal flooring for the dungeon.
    FloorTile,
    /// Spikes for pit traps, facing upward.
    Spikes,
    /// Gate for blocking passage in the Z axis and to be possibly slid sideways.
    Gate,
    /// Receptacle for a moved `Gate`.
    GatePocket,
}
impl BlockModule for DungeonBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/dungeon-blocks"
    }
}
use DungeonBlocks::*;

/// Add [`DungeonBlocks`] to the universe.
pub async fn install_dungeon_blocks(
    txn: &mut UniverseTransaction,
    progress: YieldProgress,
) -> Result<(), GenError> {
    let resolution = R16;
    let resolution_g = GridCoordinate::from(resolution);
    let one_diagonal = GridVector::new(1, 1, 1);
    let center_point_doubled = (one_diagonal * resolution_g).to_point();

    let light_voxel = Block::builder()
        .color(Rgba::new(0.7, 0.7, 0.0, 1.0))
        .light_emission(Rgb::new(8.0, 7.0, 0.7) * 0.5)
        .build();
    let spike_metal = Block::from(palette::STEEL);

    BlockProvider::<DungeonBlocks>::new(progress, |key| {
        Ok(match key {
            CorridorLight => Block::builder()
                .display_name("Corridor Light")
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::PY })
                .voxels_fn(resolution, |cube| {
                    let centered = cube.lower_bounds() * 2 - center_point_doubled;
                    if centered.y > centered.x.abs() && centered.y > centered.z.abs() {
                        &light_voxel
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),

            FloorTile => {
                let resolution = R32;
                let space =
                    space_from_image(include_image!("floor.png"), GridRotation::RXZY, &|pixel| {
                        let block = Block::from(Rgba::from_srgb8(pixel));
                        VoxelBrush::with_thickness(block, 0..resolution.into())
                            .rotate(GridRotation::RXZY)
                    })?;
                Block::builder()
                    .display_name("Floor Tile")
                    .voxels_ref(resolution, txn.insert_anonymous(space))
                    .build()
            }

            Spikes => Block::builder()
                .display_name("Spikes")
                .voxels_fn(resolution, |Cube { x, y, z }| {
                    let resolution = f64::from(resolution);
                    let bsin = |x| (f64::from(x) * TAU / resolution * 2.0).sin();
                    if f64::from(y) / resolution < bsin(x) * bsin(z) {
                        &spike_metal
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),

            Gate => {
                let space =
                    space_from_image(include_image!("fence.png"), GridRotation::RXyZ, &|pixel| {
                        // Note that this produces selectable collidable transparent blocks --
                        // that's preferred here.
                        let block = Block::builder().color(Rgba::from_srgb8(pixel)).build();
                        VoxelBrush::with_thickness(block, 7..9)
                    })?;
                Block::builder()
                    .display_name("Gate")
                    .voxels_ref(R16, txn.insert_anonymous(space))
                    .build()
            }

            // TODO: improve this appearance
            GatePocket => {
                let space = space_from_image(
                    include_image!("fence-pocket.png"),
                    GridRotation::RXyZ,
                    &|pixel| {
                        let block = Block::builder().color(Rgba::from_srgb8(pixel)).build();
                        VoxelBrush::new([([0, 0, 6], block.clone()), ([0, 0, 9], block)])
                    },
                )?;
                Block::builder()
                    .display_name("Gate Pocket")
                    .voxels_ref(R16, txn.insert_anonymous(space))
                    .build()
            }
        })
    })
    .await?
    .install(txn)?;

    Ok(())
}

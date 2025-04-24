use alloc::boxed::Box;
use alloc::sync::Arc;
use core::mem;

use rand::prelude::IndexedRandom as _;
use rand::seq::IteratorRandom as _;
use rand::{Rng, SeedableRng};

use all_is_cubes::block::{self, AIR, Block, Resolution::*, text};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::{BoxPart, BoxStyle, palette};
use all_is_cubes::euclid::{Size3D, Vector3D, vec3};
use all_is_cubes::inv::{self, Tool};
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{
    Axis, Cube, Face6, FaceMap, GridAab, GridCoordinate, GridRotation, GridSize, GridSizeCoord,
    GridVector, Vol,
};
use all_is_cubes::space::{LightPhysics, Space};
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;
use all_is_cubes::{arcstr, op, time};

use crate::alg::four_walls;
use crate::dungeon::{
    DungeonBlocks, DungeonBlocks::*, DungeonGrid, MazeRoomKind, Theme, build_dungeon, generate_maze,
};
use crate::{DemoBlocks, LandscapeBlocks, TemplateParameters, tree};

// -------------------------------------------------------------------------------------------------

const WINDOW_PATTERN: [GridCoordinate; 3] = [-2, 0, 2];
const TORCH_PATTERN: [GridCoordinate; 2] = [-4, 4];

#[derive(Clone, Debug)]
struct DemoRoom {
    maze_kind: MazeRoomKind,
    position_on_path: Option<usize>,

    /// In a *relative* room coordinate system (1 unit = 1 room box),
    /// how big is this room? Occupying multiple rooms' space if this
    /// is not equal to `GridAab::ORIGIN_CUBE`.
    extended_bounds: GridAab,

    /// Which walls/faces have something on them.
    wall_features: FaceMap<WallFeature>,

    floor: FloorKind,

    /// If true, the room is no bigger than its attached corridors.
    corridor_only: bool,

    lit: bool,

    grants_item: Option<Tool>,
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
    Passage(Door),
    /// Window to the outside world.
    Window,
}

/// A possible obstruction in a passage.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Door {
    None,
    Open,
    Locked,
    /// Can be seen through but cannot be unlocked.
    Permanent,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum FloorKind {
    Solid,
    Chasm,
    Bridge,
}

/// Data for the size and theming of dungeon rooms.
struct DemoTheme {
    dungeon_grid: DungeonGrid,

    /// [`BoxStyle`] for a basic room’s walls, floor, and ceiling.
    ///
    /// May sometimes be overridden in part.
    room_style: BoxStyle,

    /// Same coordinate system as `dungeon_grid.room_box`.
    /// Pick 2 out of 3 axes to define the walkable bounds of a corridor/doorway on the third axis.
    corridor_box: GridAab,

    /// [`BoxStyle`]s with which to carve a corridor and create its walls, one per axis.
    ///
    /// The interior of each box should be [`AIR`] or at least passable, as should be the faces on
    /// the relevant axis.
    corridor_box_styles: Vector3D<BoxStyle, ()>,

    blocks: BlockProvider<DungeonBlocks>,

    // TODO: ought to be part of the BlockProvider
    locked_gate_block: Block,
    /// TODO: replace window glass with openings that are too small to pass through
    window_glass_block: Block,
    item_pedestal: Block,
}

// -------------------------------------------------------------------------------------------------

impl DemoTheme {
    fn plain_room(
        &self,
        wall_block: Option<&Block>,
        space: &mut Space,
        interior: GridAab,
    ) -> Result<(), InGenError> {
        let mut alt_style: BoxStyle;
        let room_style = if let Some(wall_block) = wall_block {
            alt_style = self.room_style.clone();
            alt_style[BoxPart::face(Face6::NX)] = Some(wall_block.clone());
            alt_style[BoxPart::face(Face6::PX)] = Some(wall_block.clone());
            alt_style[BoxPart::face(Face6::NZ)] = Some(wall_block.clone());
            alt_style[BoxPart::face(Face6::PZ)] = Some(wall_block.clone());
            alt_style[BoxPart::face(Face6::PY)] = Some(wall_block.clone());
            &alt_style
        } else {
            &self.room_style
        };

        room_style
            .create_box(interior.expand(FaceMap::splat(1)))
            .execute(space, &mut transaction::no_outputs)?;

        Ok(())
    }

    fn inside_doorway(
        &self,
        space: &mut Space,
        map: Vol<&[Option<DemoRoom>]>,
        mut room_1_position: Cube,
        face: Face6,
        door: Door,
    ) -> Result<(), InGenError> {
        let passage_axis = face.axis();
        let mut room_2_position = room_1_position + face.normal_vector();

        if face.is_negative() {
            mem::swap(&mut room_1_position, &mut room_2_position);
        }

        let room_1_data = map[room_1_position]
            .as_ref()
            .expect("passage led to nonexistent room");
        let room_2_data = map[room_2_position]
            .as_ref()
            .expect("passage led to nonexistent room");
        let room_1_box = self.actual_room_box(room_1_position, room_1_data);
        let room_2_box = self.actual_room_box(room_2_position, room_2_data);

        let wall_parallel = GridRotation::CLOCKWISE.transform(face);
        let parallel_axis = wall_parallel.axis();
        assert!(parallel_axis != Axis::Y);

        let rotate_nz_to_face = GridRotation::from_to(Face6::NZ, face, Face6::PY).unwrap();

        // This box is exactly the volume which would ordinarily be impassable if this corridor.
        // were not being added.
        let doorway_box = {
            let corridor_box = self
                .corridor_box
                .translate(self.dungeon_grid.room_translation(room_1_position));
            // TODO: Add GridAab operations to make this easier
            let mut lower = corridor_box.lower_bounds();
            let mut upper = corridor_box.upper_bounds();
            lower[passage_axis] = room_1_box.upper_bounds()[passage_axis];
            upper[passage_axis] = room_2_box.lower_bounds()[passage_axis];
            GridAab::from_lower_upper(lower, upper)
        };

        // Choose the box style used to carve/connect the doorway.
        // In particular, if either room is corridor_only, make its ends be featureless by copying
        // from the center.
        // TODO: we should probably precalculate these
        let unmodified_corridor_box_style = &self.corridor_box_styles[passage_axis];
        let corridor_box_style = unmodified_corridor_box_style.clone().map(|part, block| {
            if room_1_data.corridor_only && part.is_on_face(passage_axis.negative_face())
                || room_2_data.corridor_only && part.is_on_face(passage_axis.positive_face())
            {
                unmodified_corridor_box_style[part.centered_on(passage_axis)]
                    .clone()
                    .unwrap_or(AIR)
            } else {
                block
            }
        });

        // Place the doorway/corridor’s walls and cut it into the room walls.
        let doorway_box_for_box_style = doorway_box.expand({
            // Expand by 1 cube to form the exterior box, except...
            let mut v = [1; 3];
            // ...on the passage axis, it meets the room walls instead of sticking into the room.
            v[passage_axis] = 0;
            FaceMap::symmetric(v)
        });
        corridor_box_style
            .create_box(doorway_box_for_box_style)
            .execute(space, &mut transaction::no_outputs)
            .unwrap();

        // * If !gate_present, we don't generate a gate at all.
        // * If gate_movable, we generate a GatePocket for it to slide into; otherwise it is
        //   entirely fixed flat bars.
        // * If gate_movable && !gate_open, we generate a lock which allows the gate to be
        //   opened with a key.
        let (gate_present, gate_movable, gate_open) = match door {
            Door::None => (false, false, false),
            Door::Open => (true, true, true),
            Door::Locked => (true, true, false),
            Door::Permanent => (true, false, false),
        };
        if gate_present {
            let gate_box = doorway_box.abut(face, -1).unwrap().translate(
                face.opposite().normal_vector() * doorway_box.size().to_i32()[face.axis()] / 2,
            );
            let gate_side_1 = gate_box.abut(wall_parallel.opposite(), -1).unwrap();
            let gate_side_2 = gate_box
                .abut(wall_parallel, if gate_open { -1 } else { -2 })
                .unwrap();
            let lock_box = if gate_movable && !gate_open {
                gate_side_1
                    .abut(Face6::NY, -1) // one cube up from bottom
                    .unwrap()
                    .abut(Face6::PY, 1) // one cube high
                    .unwrap()
                    .abut(wall_parallel, 1) // one cube adjacent to the pocket (centered on 3x3)
                    .unwrap()
            } else {
                GridAab::ORIGIN_EMPTY
            };
            space.fill_uniform(
                gate_side_2,
                &self.blocks[Gate].clone().rotate(rotate_nz_to_face),
            )?;
            space.fill_uniform(
                gate_side_1,
                &self.blocks[if gate_movable { GatePocket } else { Gate }]
                    .clone()
                    .rotate(rotate_nz_to_face),
            )?;
            space.fill_uniform(
                lock_box,
                &self.locked_gate_block.clone().rotate(rotate_nz_to_face),
            )?;
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
                .union_box(self.dungeon_grid.room_box_at(
                    room_position + eb.upper_bounds().to_vector() - GridVector::new(1, 1, 1),
                ))
        }
    }

    // TODO: This should be a definition in the universe, but there's no way to do that with `Tool`
    // yet.
    fn make_key_tool(&self) -> Tool {
        let move_modifier = block::Modifier::Move(block::Move::new(Face6::NX, 0, 16));
        let move_gate = op::Operation::AddModifiers([move_modifier.clone()].into());
        let unlock_unrotated = op::Operation::Neighbors(
            [
                (
                    Cube::new(0, 0, 0),
                    op::Operation::Replace {
                        old: self.locked_gate_block.clone(),
                        new: self.blocks[Gate].clone().with_modifier(move_modifier),
                        conserved: true,
                        optional: false,
                    },
                ),
                // TODO: this should be a paired move + composite
                // that slides the gate into the pocket, but that isn't
                // supported by Composite yet
                (Cube::new(0, -1, 0), move_gate.clone()),
                (Cube::new(0, 1, 0), move_gate),
            ]
            .into(),
        );
        // TODO: there should be an operation-modifier that means "match this against any rotation"
        // instead of this approach of making rotated copies of the operation
        let unlock = op::Operation::Alt(
            GridRotation::CLOCKWISE
                .iterate()
                .map(|r| unlock_unrotated.clone().rotate(r))
                .collect::<Arc<_>>(),
        );

        Tool::Custom {
            op: unlock,

            icon: self.blocks[Key].clone(),
        }
    }
}

impl Theme<Option<DemoRoom>> for DemoTheme {
    fn passes(&self) -> usize {
        2
    }

    fn place_room(
        &self,
        space: &mut Space,
        pass_index: usize,
        map: Vol<&[Option<DemoRoom>]>,
        room_position: Cube,
        room_data: &Option<DemoRoom>,
    ) -> Result<(), InGenError> {
        let Some(room_data) = room_data.as_ref() else {
            return Ok(());
        };

        // TODO: put in struct, or eliminate
        let start_wall = block::from_color!(1.0, 0.0, 0.0);
        let goal_wall = block::from_color!(0.0, 0.8, 0.0);

        let interior = self.actual_room_box(room_position, room_data);
        let unmodified_room_box = self.dungeon_grid.room_box_at(room_position);

        let wall_type = match room_data.maze_kind {
            MazeRoomKind::Start => Some(&start_wall),
            MazeRoomKind::Goal => Some(&goal_wall),
            MazeRoomKind::Path | MazeRoomKind::OffPath => None,
            MazeRoomKind::Unoccupied => unreachable!(),
        };
        let floor_layer = unmodified_room_box.abut(Face6::NY, 1).unwrap();

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
                                let bridge_box =
                                    GridAab::single_cube(midpoint).union_cube(wall_cube);
                                space.fill_uniform(bridge_box, &self.blocks[FloorTile])?;
                            }
                        }
                    }
                }

                // Lit corridors get the magic pyramid light instead of putting a torch in your face
                if room_data.lit && room_data.corridor_only {
                    let top_middle =
                        Cube::containing(interior.abut(Face6::PY, -1).unwrap().center()).unwrap();
                    space.set(top_middle, &self.blocks[CorridorLight])?;
                }

                // Windowed walls and torches on walls
                let window_y = unmodified_room_box.lower_bounds().y + 1;
                let torch_y = unmodified_room_box.lower_bounds().y;
                four_walls(
                    interior.expand(FaceMap::splat(1)),
                    |origin, along_wall, length, wall_excluding_corners_box| {
                        let wall = GridRotation::CLOCKWISE.transform(along_wall); // TODO: make four_walls provide this in a nice name
                        let midpoint = (length / 2) as GridCoordinate;

                        if let WallFeature::Window = room_data.wall_features[wall] {
                            for step in WINDOW_PATTERN {
                                let mut window_pos =
                                    origin + along_wall.normal_vector() * (midpoint + step);
                                window_pos.y = window_y;
                                if let Some(window_box) =
                                    GridAab::from_lower_size(window_pos, [1, 3, 1])
                                        .intersection_cubes(wall_excluding_corners_box)
                                {
                                    space.fill_uniform(window_box, &self.window_glass_block)?;
                                }
                            }
                        } else if room_data.lit && !room_data.corridor_only {
                            for step in TORCH_PATTERN {
                                let mut torch_pos = origin
                                    + along_wall.normal_vector() * (midpoint + step)
                                    + wall.opposite().normal_vector();
                                torch_pos.y = torch_y;

                                space.set(torch_pos, &self.blocks[Brazier])?;
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
                    if let WallFeature::Passage(door) = room_data.wall_features[face] {
                        self.inside_doorway(space, map, room_position, face, door)?;
                    }
                }

                // Debug dump data for the room, so we can compare the data to what was
                // actually generated. This is spoilers for the maze, but it’s hard to read.
                if !room_data.corridor_only {
                    let description = arcstr::format!(
                        "{room_position:?}\n{kind:?}\ndop={position_on_path:?}",
                        kind = room_data.maze_kind,
                        position_on_path = room_data.position_on_path
                    );
                    let info_text_block = text::Text::builder()
                        .string(description)
                        .font(text::Font::SmallerBodyText)
                        .foreground(block::from_color!(palette::STEEL))
                        .resolution(R128)
                        .positioning(text::Positioning {
                            line_y: text::PositioningY::BodyTop,
                            ..text::Positioning::LOW
                        })
                        .build()
                        .single_block()
                        .rotate(GridRotation::IDENTITY);
                    space.set(
                        Cube::from(interior.lower_bounds() + vec3(1, 3, 0)),
                        info_text_block,
                    )?;
                }

                // Item.
                if let Some(item) = room_data.grants_item.clone() {
                    // note that this is the nominal floor, not the possibly extended downward floor
                    let floor_middle =
                        Cube::containing(floor_layer.abut(Face6::PY, 1).unwrap().center()).unwrap();
                    space.set(floor_middle, &self.item_pedestal)?;
                    space.set(
                        floor_middle + GridVector::new(0, 1, 0),
                        self.blocks[ItemHolder]
                            .clone()
                            .with_modifier(inv::Inventory::from_slots([item.into()])),
                    )?;
                }

                // Set spawn.
                // TODO: Don't unconditionally override spawn; instead communicate this out.
                if matches!(room_data.maze_kind, MazeRoomKind::Start) {
                    let mut spawn = Spawn::default_for_new_space(space.bounds());
                    spawn.set_bounds(interior);
                    spawn.set_inventory(vec![
                        Tool::Activate.into(),
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

// -------------------------------------------------------------------------------------------------

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
        super::install_dungeon_blocks(&mut install_txn, blocks_progress).await?;
        install_txn.execute(universe, &mut transaction::no_outputs)?;
    }

    let dungeon_grid = DungeonGrid {
        room_box: GridAab::from_lower_size([0, 0, 0], [9, 5, 9]),
        room_wall_thickness: FaceMap::splat(1),
        gap_between_walls: Size3D::new(1, 1, 1),
    };
    let perimeter_margin: GridSizeCoord = 30;

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
    let dungeon_blocks = BlockProvider::<DungeonBlocks>::using(universe)?;
    let theme = DemoTheme {
        dungeon_grid: dungeon_grid.clone(),
        room_style: BoxStyle::from_fn(|part| {
            if part == BoxPart::INTERIOR {
                Some(AIR)
            } else if part.is_on_face(Face6::NY) {
                Some(dungeon_blocks[FloorTile].clone())
            } else {
                // TODO: add wall-tile and ceiling-tile blocks
                Some(landscape_blocks[LandscapeBlocks::Stone].clone())
            }
        }),
        corridor_box: GridAab::from_lower_size([3, 0, 3], [3, 3, 3]),
        corridor_box_styles: {
            let basic_style = BoxStyle::from_fn(|part| {
                if part == BoxPart::INTERIOR {
                    Some(AIR)
                } else if part.is_on_face(Face6::NY) {
                    Some(dungeon_blocks[FloorTile].clone())
                } else {
                    // TODO: add wall-tile and ceiling-tile blocks
                    Some(landscape_blocks[LandscapeBlocks::Stone].clone())
                }
            });

            let corner_block = |facing_room: Face6, left: bool| {
                block::Composite::new(
                    dungeon_blocks[DoorwaySideMask].clone().rotate(
                        GridRotation::from_to(Face6::PZ, facing_room, Face6::PY).unwrap()
                            * if left {
                                GridRotation::IDENTITY
                            } else {
                                GridRotation::RxYZ
                            },
                    ),
                    block::CompositeOperator::In,
                )
                .reversed()
                .compose_or_replace(landscape_blocks[LandscapeBlocks::Stone].clone())
            };

            let on_horizontal_axis = |axis: Axis| {
                basic_style
                    .clone()
                    .with(BoxPart::face(axis.negative_face()), Some(AIR))
                    .with(BoxPart::face(axis.positive_face()), Some(AIR))
                    // TODO: way too repetitive and unclear
                    .with(
                        BoxPart::face(axis.positive_face())
                            .push(axis.positive_face().cross(Face6::PY).try_into().unwrap()),
                        Some(corner_block(axis.positive_face(), true)),
                    )
                    .with(
                        BoxPart::face(axis.positive_face())
                            .push(axis.positive_face().cross(Face6::NY).try_into().unwrap()),
                        Some(corner_block(axis.positive_face(), false)),
                    )
                    .with(
                        BoxPart::face(axis.negative_face())
                            .push(axis.negative_face().cross(Face6::PY).try_into().unwrap()),
                        Some(corner_block(axis.negative_face(), true)),
                    )
                    .with(
                        BoxPart::face(axis.negative_face())
                            .push(axis.negative_face().cross(Face6::NY).try_into().unwrap()),
                        Some(corner_block(axis.negative_face(), false)),
                    )
            };

            vec3(
                on_horizontal_axis(Axis::X),
                basic_style
                    .clone()
                    .with(BoxPart::face(Face6::NY), Some(AIR))
                    .with(BoxPart::face(Face6::PY), Some(AIR)),
                on_horizontal_axis(Axis::Z),
            )
        },
        locked_gate_block: dungeon_blocks[Gate]
            .clone()
            .with_modifier(block::Composite::new(
                dungeon_blocks[GateLock].clone(),
                block::CompositeOperator::Over,
            )),
        window_glass_block: demo_blocks[DemoBlocks::GlassBlock].clone(),
        item_pedestal: demo_blocks[DemoBlocks::Pedestal].clone(),
        blocks: dungeon_blocks,
    };
    // Random assortment of items to provide
    // TODO: make this things like keys for doors
    let grantable_items = [
        demo_blocks[DemoBlocks::Lamp(false)].clone(),
        demo_blocks[DemoBlocks::Signboard].clone(),
        // TODO: can't do this until we have an "item" form: &demo_blocks[DemoBlocks::Explosion(0)],
        landscape_blocks[LandscapeBlocks::Leaves(tree::TreeGrowth::Block)].clone(),
        landscape_blocks[LandscapeBlocks::Grass].clone(),
        landscape_blocks[LandscapeBlocks::Dirt].clone(),
        landscape_blocks[LandscapeBlocks::Stone].clone(),
    ]
    .map(Tool::Block);

    // Construct dungeon map
    let maze_progress = progress.start_and_cut(0.1, "generating layout").await;
    let dungeon_map = generate_dungeon_map(
        seed,
        requested_rooms,
        &grantable_items,
        &theme.make_key_tool(),
    );
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
    build_dungeon(&mut space, &theme, dungeon_map.as_ref(), build_progress).await?;

    // Enable lighting
    let mut light_progress = progress;
    light_progress.set_label("lighting");
    light_progress.progress(0.0).await;
    let mut physics = space.physics().clone();
    physics.light = LightPhysics::Rays {
        // account for large rooms
        maximum_distance: (dungeon_grid.room_box.size().height * 4) as u8,
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
///
/// This calls [`generate_maze()`], then elaborates on that process by adding gates, keys,
/// and random details.
fn generate_dungeon_map(
    seed: u64,
    requested_rooms: GridSize,
    grantable_items: &[Tool],
    key_item: &Tool,
) -> Vol<Box<[Option<DemoRoom>]>> {
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(seed);

    let (maze, path_length) = generate_maze(seed, requested_rooms);

    let [gain_key] = choose_key_locations(&mut rng, &maze, path_length);

    // Expand bounds to allow for extra-tall rooms.
    let expanded_bounds = maze.bounds().expand(FaceMap::symmetric([0, 1, 0]));

    let map = Vol::from_fn(expanded_bounds, |room_position| {
        let maze_room = maze.get(room_position)?;

        let must_grant_item: Option<Tool> =
            (room_position == gain_key.found_in_room).then(|| key_item.clone());
        // Allow rooms that are not start or end to have more interesting properties.
        let is_not_end = match maze_room.kind {
            MazeRoomKind::Unoccupied => return None,
            MazeRoomKind::Start | MazeRoomKind::Goal => false,
            MazeRoomKind::Path | MazeRoomKind::OffPath => true,
        };

        let corridor_only = is_not_end && must_grant_item.is_none() && rng.random_bool(0.5);

        let mut extended_bounds = GridAab::ORIGIN_CUBE;
        // Optional high ceiling
        if !corridor_only && rng.random_bool(0.25) {
            extended_bounds = extended_bounds.expand(FaceMap::default().with(Face6::PY, 1));
        }
        // Floor pit
        let floor =
            if !corridor_only && is_not_end && must_grant_item.is_none() && rng.random_bool(0.25) {
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
                    // If the two rooms are both on the path, then the path passes between them
                    // and it must be passable.
                    // Also, in the off-path section which has an important item, we must let the
                    // player reach it -- we don’t track what the exact path to items are, so
                    // instead we just make all rooms at that path-position passable.
                    let must_be_passable = !(maze_room.kind == MazeRoomKind::OffPath
                        || maze[neighbor].kind == MazeRoomKind::OffPath)
                        || maze_room.position_on_path == Some(gain_key.found_on_path_position);

                    let door = *if must_be_passable {
                        if gain_key.room_may_require_this(maze_room) {
                            // If the rooms are on the path and the player has a key,
                            // generate no door or a door that is can be opened.
                            &[
                                Door::None,
                                Door::None,
                                Door::Open,
                                Door::Locked,
                                Door::Locked,
                            ][..]
                        } else {
                            // If the rooms are on the path the player does *not* have a key,
                            // don't generate locked doors, only already-open ones.
                            &[Door::None, Door::None, Door::Open][..]
                        }
                    } else {
                        // If the rooms are not on the path, make it possibly entirely closed off,
                        // or possibly a red-herring locked door.
                        &[Door::None, Door::Open, Door::Locked, Door::Permanent][..]
                    }
                    .choose(&mut rng)
                    .unwrap();
                    return WallFeature::Passage(door);
                }

                // Create windows only if they look into space outside the maze
                let have_window = if neighbor_in_bounds || corridor_only || face == Face6::NY {
                    false
                } else if face == Face6::PY {
                    // ceilings are more common overall and we want more internally-lit ones
                    rng.random_bool(0.25)
                } else {
                    rng.random_bool(0.75)
                };

                if have_window {
                    WallFeature::Window
                } else {
                    WallFeature::Blank
                }
            })
        };

        let ok_to_grant_item = matches!(floor, FloorKind::Solid) && !corridor_only && is_not_end;
        assert!(must_grant_item.is_none() || ok_to_grant_item);
        let grants_item = must_grant_item.or_else(|| {
            (ok_to_grant_item && rng.random_bool(0.5))
                .then(|| grantable_items.choose(&mut rng).unwrap().clone())
        });

        Some(DemoRoom {
            maze_kind: maze_room.kind,
            position_on_path: maze_room.position_on_path,
            extended_bounds,
            wall_features,
            floor,
            corridor_only,
            // Light some rooms that might be dark, particularly if they have items in them.
            lit: wall_features[Face6::PY] == WallFeature::Blank
                && (grants_item.is_some() || rng.random_bool(0.75)),
            grants_item,
        })
    });

    map
}

fn choose_key_locations<const N: usize>(
    rng: &mut rand_xoshiro::Xoshiro256Plus,
    maze: &Vol<Box<[super::maze::MazeRoom]>>,
    path_length: usize,
) -> [KeyLocation; N] {
    // Pick positions along the path at which the player will gain items for use to access
    // future rooms. They should not be the start or end room.
    let Some(positions_on_path) =
        rand::seq::index::sample_array(rng, path_length - 2).map(|array| array.map(|i| i + 1))
    else {
        // TODO: handle this by having fewer obstacles instead of erroring out
        panic!("maze too small")
    };

    // Pick rooms.
    positions_on_path.map(|position_on_path| {
        let found_in_room: Cube = maze
            .iter()
            .filter(|(_, room)| room.position_on_path == Some(position_on_path))
            .choose(rng)
            .map(|(pos, _)| pos)
            .expect("no candidate room for key");

        KeyLocation {
            found_in_room,
            found_on_path_position: position_on_path,
        }
    })
}

#[derive(Debug)]
struct KeyLocation {
    found_in_room: Cube,
    found_on_path_position: usize,
}
impl KeyLocation {
    fn room_may_require_this(&self, maze_room: &super::maze::MazeRoom) -> bool {
        maze_room
            .position_on_path
            .is_some_and(|p| p > self.found_on_path_position)
    }
}

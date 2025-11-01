//! Generates a random abstract maze, for refinement into a dungeon elsewhere.

use alloc::boxed::Box;
use alloc::collections::VecDeque;

use rand::SeedableRng;
use rand::seq::{IteratorRandom as _, SliceRandom as _};

use all_is_cubes::math::{Cube, Face6, FaceMap, GridAab, GridSize, Vol};

// -------------------------------------------------------------------------------------------------

pub type Maze = Vol<Box<[MazeRoom]>>;

/// What role the room plays in the maze layout.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MazeRoomKind {
    /// Room to start in.
    Start,
    /// Room which is to be sought by the player.
    Goal,
    /// A room that is on the path to the goal.
    Path,
    /// A room that is not on the path to the goal, but is reachable.
    OffPath,
    /// A room that is not connected to the maze; empty space.
    Unoccupied,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MazeRoom {
    pub kind: MazeRoomKind,

    /// Which faces of the room must be connected to the adjacent room.
    passages: FaceMap<bool>,

    /// If this room is connected to the path from start to goal,
    /// then this is the position within the path of the closest on-path room.
    /// For example,
    ///
    /// * The starting room is always 0.
    ///     * Dead ends connected to the starting room are also 0.
    /// * The adjacent room leading towards the goal is always 1.
    ///     * Dead ends connected to that room are also 1.
    pub position_on_path: Option<usize>,
}

impl MazeRoom {
    pub fn has_passage(&self, direction: Face6) -> bool {
        self.passages[direction]
    }
}

fn open_passage(maze: &mut Maze, from: Cube, dir: Face6) {
    maze[from].passages[dir] = true;
    maze[from + dir.normal_vector()].passages[dir.opposite()] = true;
}

pub fn generate_maze(seed: u64, requested_rooms: GridSize) -> (Maze, usize) {
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(seed);

    let mut maze: Maze = Vol::repeat(
        GridAab::from_lower_size([0, 0, 0], requested_rooms),
        MazeRoom {
            kind: MazeRoomKind::Unoccupied,
            passages: FaceMap::splat(false),
            position_on_path: None,
        },
    );

    let start = Cube::new(0, 0, 0);
    maze[start].kind = MazeRoomKind::Start;

    let path_length = generate_path(&mut maze, &mut rng, Cube::new(0, 0, 0), MazeRoomKind::Goal);
    generate_dead_ends(&mut maze, &mut rng);
    fill_remaining_distances(&mut maze, start);

    (maze, path_length)
}

/// Create rooms and passages from the room `starting_cube` to a randomly chosen ending room.
///
/// Returns the length of the path, counting the starting room, the ending room, and
/// all rooms on the path in between them.
fn generate_path(
    maze: &mut Maze,
    rng: &mut rand_xoshiro::Xoshiro256Plus,
    starting_cube: Cube,
    end_kind: MazeRoomKind,
) -> usize {
    let mut path_cube = starting_cube;
    let mut position_on_path = 0;
    loop {
        maze[path_cube].position_on_path = Some(position_on_path);
        position_on_path += 1;

        let Some(next_direction) = Face6::ALL
            .into_iter()
            .filter(|dir| {
                let neighbor = path_cube + dir.normal_vector();
                maze.bounds().contains_cube(neighbor)
                    && maze[neighbor].kind == MazeRoomKind::Unoccupied
            })
            .choose(rng)
        else {
            // Painted ourselves into a corner...so call that the end
            if path_cube != starting_cube {
                maze[path_cube].kind = end_kind;
            } else {
                // TODO: signal "failed to move away from the starting cube" error,
                // but only if we weren't just a 1x1x1
            }
            // At this point position_on_path has been incremented but not written to any room,
            // so it is equal to the length.
            break position_on_path;
        };
        let neighbor = path_cube + next_direction.normal_vector();

        open_passage(maze, path_cube, next_direction);
        maze[neighbor].kind = MazeRoomKind::Path;

        path_cube = neighbor;
    }
}

/// Given a maze which already has start and end rooms, create dead-end paths to all the other
/// possible rooms.
fn generate_dead_ends(maze: &mut Maze, rng: &mut rand_xoshiro::Xoshiro256Plus) {
    let mut needs_filling: VecDeque<Cube> = maze
        .iter()
        .filter(|(_, cell)| cell.kind == MazeRoomKind::Unoccupied)
        .map(|(cube, _)| cube)
        .collect();
    needs_filling.make_contiguous().shuffle(rng);

    let mut checked_without_progress = 0;

    while let Some(cube_to_fill) = needs_filling.pop_front() {
        match Face6::ALL
            .into_iter()
            .filter(|dir| {
                let neighbor = cube_to_fill + dir.normal_vector();
                maze.bounds().contains_cube(neighbor)
                    && maze[neighbor].kind != MazeRoomKind::Unoccupied
            })
            .choose(rng)
        {
            None => {
                needs_filling.push_back(cube_to_fill);
                checked_without_progress += 1;
                if checked_without_progress > needs_filling.len() {
                    panic!("unable to make progress on dead ends");
                }
            }
            Some(dir) => {
                checked_without_progress = 0;
                maze[cube_to_fill].kind = MazeRoomKind::OffPath;
                open_passage(maze, cube_to_fill, dir);
            }
        }
    }
}

/// Flood-fill the maze to complete `position_on_path` info.
///
/// `room` should be a room that already has its position info.
fn fill_remaining_distances(maze: &mut Maze, starting_room: Cube) {
    // Use a vector as stack to keep track of what to do and keep this algorithm non-recursive.
    let mut needs_neighbors_filled = vec![starting_room];

    while let Some(here) = needs_neighbors_filled.pop() {
        let Some(here_position_on_path) = maze[here].position_on_path else {
            panic!("rooms in needs_neighbors_filled should have their positions");
        };
        for direction in Face6::ALL {
            if maze[here].has_passage(direction) {
                let neighbor = here + direction;
                let needs_fill = maze[neighbor].position_on_path.is_none();
                let needs_visit = needs_fill
                    || maze[neighbor].position_on_path.is_some_and(|np| np > here_position_on_path);

                if needs_fill {
                    // Note the number is unchanged.
                    // If this were “distance from the path” we'd add 1 instead.
                    maze[neighbor].position_on_path = Some(here_position_on_path);
                }

                if needs_visit {
                    needs_neighbors_filled.push(neighbor);
                }
            }
        }
    }
}

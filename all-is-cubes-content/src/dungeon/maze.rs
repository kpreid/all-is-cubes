use alloc::boxed::Box;
use alloc::collections::VecDeque;

use rand::seq::{IteratorRandom as _, SliceRandom as _};
use rand::SeedableRng;

use all_is_cubes::math::{Cube, Face6, FaceMap, GridAab, GridSize, Vol};

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
    /// A room that is not on the path to the goal.
    OffPath,
    /// A room that is not connected to the maze; empty space.
    Unoccupied,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MazeRoom {
    pub kind: MazeRoomKind,
    passages: FaceMap<bool>,
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

pub fn generate_maze(seed: u64, requested_rooms: GridSize) -> Maze {
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(seed);

    let mut maze: Maze = Vol::repeat(
        GridAab::from_lower_size([0, 0, 0], requested_rooms),
        MazeRoom {
            kind: MazeRoomKind::Unoccupied,
            passages: FaceMap::splat(false),
        },
    );

    let start = Cube::new(0, 0, 0);
    maze[start].kind = MazeRoomKind::Start;

    generate_path(&mut maze, &mut rng, Cube::new(0, 0, 0), MazeRoomKind::Goal);
    generate_dead_ends(&mut maze, &mut rng);

    maze
}

fn generate_path(
    maze: &mut Maze,
    rng: &mut rand_xoshiro::Xoshiro256Plus,
    starting_cube: Cube,
    end_kind: MazeRoomKind,
) {
    let mut path_cube = starting_cube;
    loop {
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
                // TODO: signal error
            }
            break;
        };
        let neighbor = path_cube + next_direction.normal_vector();

        open_passage(maze, path_cube, next_direction);
        maze[neighbor].kind = MazeRoomKind::Path;

        path_cube = neighbor;
    }
}

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

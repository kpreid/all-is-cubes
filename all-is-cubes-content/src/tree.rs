use core::fmt;

use itertools::Itertools as _;
use petgraph::visit::EdgeRef as _;
use rand::Rng as _;
use rand::seq::IndexedRandom as _;

use all_is_cubes::block::{self, AIR, Block};
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::math::{
    Cube, Face6, FaceMap, GridAab, GridCoordinate, GridRotation, GridVector, Vol,
};
use all_is_cubes::space::SpaceTransaction;

use crate::LandscapeBlocks::{self, Leaves, Log};

/// Tree segment sizes or growth stages.
///
/// TODO: make the public version of this a struct with private contents
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, exhaust::Exhaust)]
#[expect(
    unnameable_types,
    reason = "TODO: this should be public with an opaque struct wrapper"
)]
#[allow(clippy::module_name_repetitions)] // TODO: rename to Growth?
pub enum TreeGrowth {
    Sapling = 1, // radius = 1
    G2,          // radius = 2
    G3,
    G4,
    G5,
    G6,
    G7,
    Block,
}

impl fmt::Display for TreeGrowth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self as GridCoordinate)
    }
}

impl TreeGrowth {
    /// Convert a log radius (meaningful range 1 to 8) to the enum.
    /// Out-of-range values will be clamped/saturated.
    pub fn from_radius(index: GridCoordinate) -> Self {
        match index {
            i if i <= 1 => Self::Sapling,
            2 => Self::G2,
            3 => Self::G3,
            4 => Self::G4,
            5 => Self::G5,
            6 => Self::G6,
            7 => Self::G7,
            _ => Self::Block,
        }
    }

    pub fn radius(self) -> GridCoordinate {
        self as i32
    }
}

/// Construct a tree log/branch block whose faces each have the specified cross-section size,
/// or [`None`] for no branch in that direction.
pub(crate) fn make_log(
    blocks: &BlockProvider<LandscapeBlocks>,
    directions: FaceMap<Option<TreeGrowth>>,
    leaves: Option<TreeGrowth>,
    rng: &mut dyn rand::RngCore,
) -> Block {
    // TODO: this needs to canonicalize rotations so that we don't end up with
    // identical-looking but differently defined blocks.

    let wood: Block = block::Composite::stack(
        AIR,
        directions
            .iter()
            .map(|(face, &growth)| {
                let Some(growth) = growth else {
                    return AIR;
                };
                blocks[Log(growth)].clone().rotate(
                    GridRotation::from_to(Face6::NY, face, Face6::PX)
                        .or_else(|| GridRotation::from_to(Face6::NY, face, Face6::PZ))
                        .unwrap(),
                )
            })
            .map(|block| block::Composite::new(block, block::CompositeOperator::Over)),
    );

    if let Some(leaves_growth) = leaves {
        // Rotating (and mirroring) leaves gives less obvious repetition
        let leaves_rotation = *GridRotation::ALL.choose(rng).unwrap();
        wood.with_modifier(
            block::Composite::new(
                blocks[Leaves(leaves_growth)]
                    .clone()
                    .rotate(leaves_rotation),
                block::CompositeOperator::Over,
            )
            .reversed() // wood always wins
            .with_disassemblable(),
        )
    } else {
        wood
    }
}

/// Construct a tree whose lowest trunk piece is at `root` and which is contained within
/// `bounds`.
///
/// Panics if `root` is not within `bounds`.
pub(crate) fn make_tree(
    blocks: &BlockProvider<LandscapeBlocks>,
    rng: &mut dyn rand::RngCore,
    root: Cube,
    bounds: GridAab,
) -> SpaceTransaction {
    // Graph of which blocks are to be connected by logs.
    let mut graph = Growph::new(bounds.expand(FaceMap::default().with(Face6::NY, 1)));

    // Establish the root.
    *graph.edge_mut(root, Face6::NY).unwrap() = Some(TreeGrowth::Sapling);

    let not_on_surface = bounds
        .shrink(FaceMap::splat(1))
        .unwrap_or(GridAab::ORIGIN_EMPTY);

    let max_leaves_height_for_size = (i32::try_from(bounds.size().height).unwrap_or(1) / 3) + 1;
    let leaves_height = rng.random_range(1..=max_leaves_height_for_size);

    // Generate foliage (before the branches that lead to it)
    for cube in bounds
        .abut(Face6::PY, -leaves_height)
        .unwrap()
        .interior_iter()
    {
        if not_on_surface.contains_cube(cube) {
            // leaves don't hide behind other leaves
            continue;
        }
        if !rng.random_bool(0.99) {
            // chance of entirely missing cube
            continue;
        }
        if let Some(l) = graph.leaves_mut(cube) {
            let distance_from_top = (bounds.upper_bounds().y - cube.y) as f32 - 1.0;
            let max_growth = 8.5f32;
            let min_growth =
                max_growth * (1.0 - (distance_from_top / (leaves_height as f32 - 0.99)));

            *l = Some(TreeGrowth::from_radius(
                rng.random_range(min_growth..=8.5).floor() as i32,
            ));
        }
    }

    // Generate branches.
    // This is done by using A* pathfinding to find paths from the root to all leaves.
    let mut paths = 0;
    const MAX_STEP_COST: i32 = 30;
    const BASE_DISTANCE_COST: i32 = 10;
    while let Some((_, path)) = petgraph::algo::astar(
        &graph,
        root,
        |cube| {
            // Find a leaf block that is not already connected.
            cube != root
                && matches!(graph.leaves(cube), Some(Some(_)))
                && graph.neighbor_edges(cube) == FaceMap::default()
        },
        // Edge cost.
        |edge_ref| {
            let relative_y = ((f64::from(edge_ref.target().y - bounds.lower_bounds().y) + 0.5)
                / f64::from(bounds.size().height))
            .clamp(0.0, 1.0);
            let is_currently_on_existing_branch =
                graph.neighbor_edges(edge_ref.source()) != FaceMap::default();
            let would_enter_branch = graph.neighbor_edges(edge_ref.target()) != FaceMap::default();

            if !is_currently_on_existing_branch && would_enter_branch {
                // Highly penalize making a cyclic structure (branches splitting then rejoining).
                MAX_STEP_COST
            } else if edge_ref.growth.is_some() && rng.random_bool(1.0 - relative_y) {
                // Reusing existing branches is cheaper.
                // TODO: RNG in the cost function is probably a bad idea ...
                BASE_DISTANCE_COST / 2
            } else {
                BASE_DISTANCE_COST
            }
        },
        // Heuristic; must never return an underestimate of cost.
        |node| {
            let distance = node - root;
            (distance.x.abs() + distance.y.abs() + distance.z.abs()) * MAX_STEP_COST
        },
    ) {
        for (a, b) in path.into_iter().tuple_windows() {
            let face = Face6::try_from(b - a).unwrap();
            // TODO: cleverer algorithm (at least don't overwrite)
            *graph.edge_mut(a, face).unwrap() = Some(TreeGrowth::Sapling);

            // Count how many paths are through this branch
            // This will later be used to decide the growth size.
            // Note that we only update a, the rootward element of the path,
            // to avoid double-counting (and leaf nodes stay at zero harmlessly)
            graph.data[a].flow = graph.data[a].flow.saturating_add(1);
        }
        paths += 1;
        if paths > 50 {
            log::warn!("aborting branch building");
            break;
        }
    }

    // Patch up into-root flow so it will be properly sized
    graph.data[root + GridVector::new(0, -1, 0)].flow = graph.data[root].flow;

    // Adjust branch sizes based on flow data
    for cube in graph.bounds().interior_iter() {
        for face in Face6::ALL {
            let neighbor = cube + face.normal_vector();
            if graph.bounds().contains_cube(neighbor) {
                let flow1 = graph.data[cube].flow;
                let flow2 = graph.data[neighbor].flow;
                let flow = flow1.min(flow2);
                if let Some(edge_growth @ Some(_)) = graph.edge_mut(cube, face) {
                    let size = (f64::from(flow) * 1.1).sqrt() as GridCoordinate;
                    *edge_growth = Some(TreeGrowth::from_radius(size));
                }
            }
        }
    }

    // Convert graph into blocks. Skip the bottom layer that existed just to help the root.
    let mut txn = SpaceTransaction::default();
    for (cube, log) in graph.logs(blocks, rng) {
        if log != AIR && bounds.contains_cube(cube) {
            txn.at(cube).overwrite(log);
        }
    }
    txn
}

use graph::Growph;
mod graph {
    #![expect(clippy::option_option)]

    use super::*;
    use all_is_cubes::euclid::default::Vector3D;
    use all_is_cubes::math::Axis;
    use alloc::boxed::Box;
    use core::mem;

    /// A graph of connections between adjacent blocks of a tree we're going to grow.
    ///
    /// Every cube of the candidate region is considered a node of the graph, even if nothing
    /// is in it yet; this allows traversing the graph to find places to put branches.
    pub(crate) struct Growph {
        // TODO: more accessors instead of pub
        pub(crate) data: Vol<Box<[GrowphCell]>>,
    }
    impl Growph {
        pub fn new(bounds: GridAab) -> Self {
            Self {
                data: Vol::from_fn(bounds, |_| GrowphCell::new()),
            }
        }

        pub fn bounds(&self) -> GridAab {
            self.data.bounds()
        }

        pub fn edge(&self, cube: Cube, neighbor_face: Face6) -> Option<Option<TreeGrowth>> {
            if neighbor_face.is_negative() {
                // For any pair, data is stored in the cell with the lower coordinates.
                // TODO: could, theoretically, numeric overflow
                self.data
                    .get(cube + neighbor_face.normal_vector())
                    .map(|cell| cell.pos_neighbors[neighbor_face.axis()])
            } else {
                self.data
                    .get(cube)
                    .map(|cell| cell.pos_neighbors[neighbor_face.axis()])
            }
        }

        pub fn edge_mut(
            &mut self,
            cube: Cube,
            neighbor_face: Face6,
        ) -> Option<&mut Option<TreeGrowth>> {
            if neighbor_face.is_negative() {
                // For any pair, data is stored in the cell with the lower coordinates.
                // TODO: could, theoretically, numeric overflow
                self.data
                    .get_mut(cube + neighbor_face.normal_vector())
                    .map(|cell| &mut cell.pos_neighbors[neighbor_face.axis()])
            } else {
                // TODO: reject attempts to modify the extraneous edges exiting the upper bounds
                self.data
                    .get_mut(cube)
                    .map(|cell| &mut cell.pos_neighbors[neighbor_face.axis()])
            }
        }

        /// Return all the neighbor connections of this cube.
        /// Doesn't care if `cube` is out of bounds.
        pub fn neighbor_edges(&self, cube: Cube) -> FaceMap<Option<TreeGrowth>> {
            FaceMap::from_fn(|face| self.edge(cube, face).flatten())
        }

        pub fn leaves(&self, cube: Cube) -> Option<Option<TreeGrowth>> {
            self.data.get(cube).map(|cell| cell.leaves)
        }

        pub fn leaves_mut(&mut self, cube: Cube) -> Option<&mut Option<TreeGrowth>> {
            self.data.get_mut(cube).map(|cell| &mut cell.leaves)
        }

        pub fn logs<'a>(
            &'a self,
            blocks: &'a BlockProvider<LandscapeBlocks>,
            rng: &'a mut dyn rand::RngCore,
        ) -> impl Iterator<Item = (Cube, Block)> + 'a {
            self.bounds().interior_iter().map(|cube| {
                (
                    cube,
                    make_log(
                        blocks,
                        self.neighbor_edges(cube),
                        self.leaves(cube).flatten(),
                        rng,
                    ),
                )
            })
        }
    }

    #[derive(Copy, Clone, Debug)]
    pub(crate) struct GrowphCell {
        pos_neighbors: Vector3D<Option<TreeGrowth>>,
        leaves: Option<TreeGrowth>,
        pub(crate) flow: u16,
    }
    impl GrowphCell {
        fn new() -> Self {
            Self {
                pos_neighbors: Vector3D::new(None, None, None),
                leaves: None,
                flow: 0,
            }
        }
    }

    impl petgraph::visit::GraphBase for Growph {
        type NodeId = Cube;
        type EdgeId = (Cube, Axis);
    }
    impl petgraph::visit::Data for Growph {
        type NodeWeight = Option<TreeGrowth>;
        type EdgeWeight = Option<TreeGrowth>;
    }
    impl<'g> petgraph::visit::IntoEdgeReferences for &'g Growph {
        type EdgeRef = GrowphEdgeRef;

        type EdgeReferences = Box<dyn Iterator<Item = GrowphEdgeRef> + 'g>; // TODO custom type

        fn edge_references(self) -> Self::EdgeReferences {
            todo!()
        }
    }
    impl<'g> petgraph::visit::IntoEdges for &'g Growph {
        type Edges = Box<dyn Iterator<Item = GrowphEdgeRef> + 'g>; // TODO custom type

        fn edges(self, cube: Cube) -> Self::Edges {
            Box::new(
                self.neighbor_edges(cube)
                    .map(|face, growth| GrowphEdgeRef::from_face(cube, face, growth))
                    .into_values()
                    .into_iter()
                    .filter(|edge| {
                        // Filter out edges that exit the bounds of the graph
                        self.bounds().contains_cube(edge.source())
                            && self.bounds().contains_cube(edge.target())
                    }),
            )
        }
    }
    impl<'g> petgraph::visit::IntoNeighbors for &'g Growph {
        type Neighbors = Box<dyn Iterator<Item = Cube> + 'g>;

        fn neighbors(self, _node: Self::NodeId) -> Self::Neighbors {
            todo!()
        }
    }
    impl petgraph::visit::Visitable for Growph {
        type Map = VisitMap;

        fn visit_map(&self) -> Self::Map {
            VisitMap::new(self.bounds())
        }

        fn reset_map(&self, map: &mut Self::Map) {
            // TODO: optimize same-size reset
            *map = self.visit_map()
        }
    }

    /// Note that this is and must be a directed edge, even though the graph itself is undirected.
    #[derive(Clone, Copy, Debug)]
    pub(crate) struct GrowphEdgeRef {
        cube: Cube,
        face: Face6,
        pub growth: Option<TreeGrowth>,
    }
    impl GrowphEdgeRef {
        fn from_face(cube: Cube, face: Face6, growth: Option<TreeGrowth>) -> Self {
            Self { cube, face, growth }
        }
    }
    impl petgraph::visit::EdgeRef for GrowphEdgeRef {
        type NodeId = Cube;
        type EdgeId = (Cube, Axis);
        type Weight = Option<TreeGrowth>;

        fn source(&self) -> Self::NodeId {
            self.cube
        }
        fn target(&self) -> Self::NodeId {
            self.cube + self.face.normal_vector()
        }
        fn weight(&self) -> &Self::Weight {
            &self.growth
        }
        fn id(&self) -> Self::EdgeId {
            (
                // Edges are undirected but EdgeRef is directed, so when identifying them we must
                // convert.
                if self.face.is_negative() {
                    self.target()
                } else {
                    self.source()
                },
                self.face.axis(),
            )
        }
    }

    #[derive(Clone, Debug)]
    pub(crate) struct VisitMap(Vol<Box<[bool]>>);
    impl VisitMap {
        fn new(bounds: GridAab) -> VisitMap {
            Self(Vol::from_fn(bounds, |_| false))
        }
    }
    impl petgraph::visit::VisitMap<Cube> for VisitMap {
        fn visit(&mut self, node: Cube) -> bool {
            !mem::replace(
                self.0.get_mut(node).expect("node coordinates out of range"),
                true,
            )
        }

        fn is_visited(&self, &node: &Cube) -> bool {
            self.0[node]
        }
    }
}

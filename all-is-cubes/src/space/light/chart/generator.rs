#![allow(
    clippy::exhaustive_structs,
    missing_docs,
    missing_debug_implementations
)]

use alloc::boxed::Box;
use alloc::vec::Vec;
use core::num::NonZero;
use std::collections::HashMap;

use euclid::{Point3D, Vector3D};

use all_is_cubes_base::math::{Cube, Face6, FaceMap, FreePoint, FreeVector};
use all_is_cubes_base::raycast::Ray;

use super::{FlatNode, Weight};

// -------------------------------------------------------------------------------------------------
//
/// A `RayTreeNode` is a data structure which aggregates paths through the cube grid
/// such that paths sharing a common prefix share a common tree ancestor.
///
/// Before use, this is converted into [`FlatNode`] for denser storage.
#[doc(hidden)] // public only for debug visualizations
pub struct RayTreeNode {
    /// Cube that this node represents the rays hitting, relative to the origin of rays
    /// (the block we're computing light for).
    /// Thus, the root of the tree is always `[0, 0, 0]`.
    ///
    /// This field is, strictly speaking, redundant, because its information is also
    /// implicitly defined by the path from the root.
    pub relative_cube: Point3D<i8, Cube>,

    // Children indexed by the step direction they take.
    // If all empty, then this node is the last step in one or more raycasts.
    pub children: FaceMap<Option<Box<RayTreeNode>>>,

    /// Total weight of all rays that take this path.
    /// This is the sum of the weights of children plus the weight of rays that
    /// terminate in this cube.
    pub weight: FaceMap<Weight>,
}

// -------------------------------------------------------------------------------------------------

pub fn generate_flat_tree_chart() -> Vec<FlatNode> {
    tree_to_flat(&rays_to_tree(generate_light_ray_pattern()))
}

/// Generates the set of rays that we will build the chart from.
pub fn generate_light_ray_pattern() -> impl Iterator<Item = OneRay> {
    const RAY_DIRECTION_STEP: isize = 5;

    // TODO: octahedron instead of cube
    itertools::iproduct!(
        -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP,
        -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP,
        -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP
    )
    .filter_map(|(x, y, z)| {
        if x.abs() == RAY_DIRECTION_STEP
            || y.abs() == RAY_DIRECTION_STEP
            || z.abs() == RAY_DIRECTION_STEP
        {
            let direction = Vector3D::new(x as f32, y as f32, z as f32).normalize();

            let mut face_cosines = FaceMap::splat(0.0f32);
            for face in Face6::ALL {
                let unit_vector: FreeVector = face.normal_vector();
                let cosine = unit_vector.to_f32().dot(direction.to_f32()).max(0.0);
                face_cosines[face] = cosine;
            }
            Some(OneRay {
                direction,
                face_cosines,
            })
        } else {
            None
        }
    })
}

#[derive(Clone, Copy)]
pub struct OneRay {
    /// Unit direction vector specifying the ray direction.
    pub direction: Vector3D<f32, Cube>,
    pub face_cosines: FaceMap<f32>,
}

#[derive(Clone, Copy, Debug)]
pub struct Step {
    /// Cube we just hit, relative to the origin of rays
    /// (the block we're computing light for).
    pub relative_cube: Point3D<i8, Cube>,
}

fn ray_to_steps(info: OneRay) -> Vec<Step> {
    let maximum_distance = 127.0;

    let ray: Ray = Ray::new(FreePoint::splat(0.5), info.direction.map(f64::from));
    ray.cast()
        .take_while(|step| step.t_distance() <= maximum_distance)
        .map(|step| Step {
            relative_cube: step
                .cube_ahead()
                .lower_bounds()
                .map(|coord| i8::try_from(coord).expect("coordinate too big")),
        })
        .collect()
}

impl RayTreeNode {
    pub fn insert(&mut self, tail: &[Step], weight: FaceMap<Weight>) {
        self.weight += weight;
        if let &[Step { relative_cube, .. }, ref tail @ ..] = tail {
            let direction = Face6::try_from((relative_cube - self.relative_cube).to_i32()).unwrap();
            self.children[direction]
                .get_or_insert_with(|| {
                    Box::new(RayTreeNode {
                        relative_cube,
                        children: FaceMap::default(),
                        weight: FaceMap::splat(0.0),
                    })
                })
                .insert(tail, weight)
        }
    }

    #[allow(dead_code)]
    pub fn traverse_pre_mut(&mut self, f: &mut dyn FnMut(&mut Self)) {
        f(self);
        for child in self.children.iter_mut().filter_map(|(_, c)| c.as_mut()) {
            child.traverse_pre_mut(f)
        }
    }

    pub fn traverse_post(&self, f: &mut dyn FnMut(&Self)) {
        for child in self.children.values().filter_map(|c| c.as_ref()) {
            child.traverse_post(f)
        }
        f(self);
    }
}

pub fn rays_to_tree(rays: impl Iterator<Item = OneRay>) -> RayTreeNode {
    let mut tree = RayTreeNode {
        relative_cube: Point3D::default(),
        children: FaceMap::default(),
        weight: FaceMap::splat(0.0),
    };

    // let mut length_histogram: BTreeMap<usize, usize> = BTreeMap::new();

    for ray_info in rays {
        let weight = ray_info.face_cosines;
        let steps = ray_to_steps(ray_info);
        // *length_histogram.entry(steps.len()).or_default() += 1;
        // skip first step because it is always the root
        tree.insert(&steps[1..], weight);
    }

    tree
}

/// Flatten the `RayTreeNode` tree into a vector whose indices are used to identify nodes.
///
/// This is the representation which is actually used.
pub fn tree_to_flat(root: &RayTreeNode) -> Vec<FlatNode> {
    let len = {
        let mut len = 0;
        root.traverse_post(&mut |_| len += 1);
        len
    };
    let mut table: Vec<FlatNode> = vec![FlatNode::default(); len];

    // Fill the Vec from the tree, *backwards*.
    // This allows us to place the root at 0 while always knowing the index of each child.
    let mut next_index = len - 1;
    let mut cube_to_index: HashMap<Point3D<i8, Cube>, NonZero<u32>> = HashMap::new();
    root.traverse_post(&mut |node| {
        let this_index = next_index;
        if this_index != 0 {
            next_index -= 1;

            cube_to_index.insert(
                node.relative_cube,
                NonZero::new(u32::try_from(this_index).expect("index doesn’t fit in u32"))
                    .expect("index is not zero"),
            );
        } else {
            assert_eq!(node.relative_cube, Point3D::origin());
        }

        table[this_index] = FlatNode::new(
            node.weight,
            FaceMap::from_fn(|face| {
                node.children[face]
                    .as_ref()
                    .map(|child| cube_to_index[&child.relative_cube])
            }),
        );
    });

    assert_eq!(next_index, 0);

    table.shrink_to_fit();
    table
}

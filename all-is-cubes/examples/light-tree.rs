#![allow(warnings, reason = "TODO: stubbed-out ex-prototype code")]
//! Prototype and visualization of new light-ray processing.
//!
//! Note: This is *not* a code sample to be imitated, as it uses unstable/pseudo-private APIs.
//! It is listed as an “example” because it is a program that only makes sense to run manually.

use std::collections::HashMap;
use std::num::NonZero;

use euclid::{Point3D, Vector3D};

use all_is_cubes::math::{Cube, Face6, FaceMap, FreePoint, FreeVector};
use all_is_cubes::raycast::Ray;

use all_is_cubes::rerun_glue as rg;
/// Private — do not use.
use all_is_cubes::space::light::chart::generator;

fn main() {
    let destination = rg::RootDestination::wrap_and_initialize(
        rg::RecordingStreamBuilder::new("all-is-cubes/light-tree")
            .default_enabled(true)
            .connect_grpc()
            .unwrap(),
    )
    .get(rg::Stem::World);

    // let num_rays = generator::generate_light_ray_pattern().count();
    // eprintln!(
    //     "num ray steps = {}",
    //     generator::generate_light_ray_pattern()
    //         .flat_map(generator::ray_to_steps)
    //         .count()
    // );
    let tree = generator::rays_to_tree(generator::generate_light_ray_pattern());
    eprintln!("num tree nodes = {}", {
        let mut n = 0;
        tree.traverse_post(&mut |_| n += 1);
        n
    });

    let table = generator::tree_to_flat(&tree);
    // dbg!(&table[0..20]);

    let mut deduped_arrows: HashMap<(Point3D<i8, Cube>, Face6), FaceMap<f32>> = HashMap::new();
    tree.traverse_post(&mut |node| {
        for (child_dir, child) in node.children.iter() {
            if let Some(child) = child {
                let key = (node.relative_cube, child_dir);
                *deduped_arrows.entry(key).or_default() += child.weight;
            }
        }
    });

    //let weight_scale = 3.0 / (num_rays as f32);
    let weight_scale = 0.01;
    let mut arrow_origins: Vec<rg::components::Position3D> = Vec::new();
    let mut arrow_offsets: Vec<rg::components::Vector3D> = Vec::new();
    let mut arrow_radii: Vec<rg::components::Radius> = Vec::new();
    for ((origin, dir), weight) in deduped_arrows {
        arrow_origins.push(rg::convert_point(origin));
        arrow_offsets.push(rg::convert_vec(dir.normal_vector::<f32, ()>()).into());
        arrow_radii.push((weight.px * weight_scale).min(1.0).into())
        //arrow_radii.push((weight.px.log2() * 0.1).into())
    }

    destination.log(
        &rg::entity_path!("arrows"),
        &rg::archetypes::Arrows3D::from_vectors(arrow_offsets)
            .with_origins(arrow_origins)
            .with_radii(arrow_radii),
    );

    destination.stream.flush_blocking();
}

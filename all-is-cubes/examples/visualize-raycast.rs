//! Visualize the information produced by [`Raycaster`] as it steps through
//! multiple levels of voxel grids.
//!
//! Note: This is *not* a code sample to be imitated, as it uses unstable/pseudo-private APIs.
//! It is listed as an “example” because it is a program that only makes sense to run manually.

use all_is_cubes::block::Resolution::{self, R8};
use all_is_cubes::math::{Aab, FreeVector, GridAab, GridVector};
use all_is_cubes::raycast::{Ray, Raycaster};

/// Private — do not use.
use all_is_cubes::rerun_glue as rg;

fn main() {
    let destination = rg::Destination {
        stream: rg::RecordingStreamBuilder::new("all-is-cubes/visualize-raycast")
            .default_enabled(true)
            .connect_grpc()
            .unwrap(),
        path: rg::entity_path![],
    };
    destination.log_initialization();

    for ray in [
        Ray::new([0.0, 0.5, 0.5], [1.0, 1. / 16., 1. / 4.]),
        Ray::new([0.0, 0.75, 0.5], [1.0, 0.0, 0.0]),
    ] {
        show_top_level(destination.clone(), ray, R8);
    }

    destination.stream.flush_blocking();
}

fn show_top_level(destination: rg::Destination, ray: Ray, resolution: Resolution) {
    show(
        destination,
        ray.cast()
            .within(GridAab::from_lower_upper([-10, -10, -10], [11, 11, 11])),
        ray,
        &mut 0,
        Recurse::Outer { resolution },
    )
}

#[allow(clippy::needless_pass_by_value)]
fn show(
    destination: rg::Destination,
    raycaster: Raycaster,
    ray: Ray,
    global_t: &mut i64,
    recurse: Recurse,
) {
    let mut previous_point = None;
    for (i, step) in (0..1000).zip(raycaster) {
        destination
            .stream
            .set_time_sequence("global_step", *global_t);
        *global_t += 1;

        let (transform, box_color) = match recurse {
            Recurse::Outer { resolution: _ } => (
                euclid::Transform3D::identity(),
                rg::components::Color::from_rgb(188, 133, 17),
            ),
            Recurse::Inner {
                resolution,
                translation,
                box_color,
            } => {
                let scale = f64::from(resolution).recip();
                (
                    euclid::Transform3D::scale(scale, scale, scale)
                        .then_translate(translation.to_f64()),
                    box_color,
                )
            }
        };

        let intersection_point = rg::convert_vec(
            transform
                .transform_point3d(step.intersection_point(ray))
                .unwrap()
                .to_vector(),
        );
        if let Some(previous_point) = previous_point {
            destination.log(
                &rg::entity_path!["segment"],
                &rg::archetypes::LineStrips3D::new([rg::components::LineStrip3D(vec![
                    previous_point,
                    intersection_point,
                ])]),
            );
        }
        destination.log(
            &rg::entity_path!["intersection_point"],
            &rg::archetypes::Points3D::new([intersection_point]).with_radii([1. / 256.]),
        );
        destination.log(
            &rg::entity_path!["cube_ahead"],
            &rg::convert_aabs(
                [Aab::from_lower_upper(
                    transform
                        .transform_point3d(step.cube_ahead().lower_bounds().to_f64())
                        .unwrap(),
                    transform
                        .transform_point3d(step.cube_ahead().upper_bounds().to_f64())
                        .unwrap(),
                )],
                FreeVector::zero(),
            )
            .with_labels([format!(
                "{i}. Hit {face:?}\nt = {t}",
                face = step.face(),
                t = step.t_distance()
            )])
            .with_colors([box_color]),
        );
        previous_point = Some(intersection_point);

        match recurse {
            Recurse::Outer { resolution } => {
                let (r_raycaster, r_ray) =
                    step.recursive_raycast(ray, resolution, GridAab::for_block(resolution));
                let r_destination = destination.child(&rg::entity_path!["recursive"]);
                // r_destination.log(
                //     &rg::entity_path![],
                //     &rg::archetypes::Transform3D::from_translation_rotation_scale(
                //         rg::convert_vec(step.cube_ahead().lower_bounds().to_vector()),
                //         rg::datatypes::Rotation3D::IDENTITY,
                //         rg::datatypes::Scale3D::Uniform(f32::from(resolution).recip()),
                //     ),
                // );
                show(
                    r_destination,
                    r_raycaster,
                    r_ray,
                    global_t,
                    Recurse::Inner {
                        resolution,
                        translation: step.cube_ahead().lower_bounds().to_vector(),
                        box_color: [
                            rg::components::Color::from_rgb(50, 107, 133),
                            rg::components::Color::from_rgb(60, 80, 100),
                        ][i % 2],
                    },
                );
            }
            Recurse::Inner { .. } => {}
        }
    }
}

enum Recurse {
    Outer {
        resolution: Resolution,
    },
    Inner {
        resolution: Resolution,
        translation: GridVector,
        box_color: rg::components::Color,
    },
}

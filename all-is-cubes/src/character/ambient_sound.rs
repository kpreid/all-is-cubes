//! Analyzes the scene around a character to produce a [`SpatialAmbient`] background sound.
//!
//! See also [`super::exposure`], which collects light instead of sound in a similar way.

use alloc::boxed::Box;

use bevy_ecs::prelude as ecs;
use euclid::{Point3D, Vector3D};
use rand::SeedableRng as _;
use rand_distr::Distribution as _;

use crate::camera;
use crate::math::{Cube, ps32};
use crate::raycast::Ray;
use crate::sound::{Ambient, SpatialAmbient};
use crate::space;

// -------------------------------------------------------------------------------------------------

const STORED_SAMPLE_COUNT: usize = 200;
const SAMPLES_PER_FRAME_COUNT: usize = 100;

#[derive(Clone, ecs::Component)]
pub(crate) struct State {
    samples: Box<[SpatialAmbient; STORED_SAMPLE_COUNT]>,

    /// Last written element of `sound_samples`.
    last_sample_index: usize,

    /// RNG used for random direction sampling
    rng: rand_xoshiro::Xoroshiro128Plus,

    output: SpatialAmbient,
}

impl Default for State {
    fn default() -> Self {
        Self {
            samples: vec![SpatialAmbient::SILENT; STORED_SAMPLE_COUNT].try_into().unwrap(),
            last_sample_index: 0,
            rng: rand_xoshiro::Xoroshiro128Plus::seed_from_u64(0),
            output: SpatialAmbient::SILENT,
        }
    }
}

impl State {
    pub(crate) fn step(&mut self, space: &space::Read<'_>, vt: camera::ViewTransform, dt: f64) {
        if dt == 0. {
            return;
        }

        self.sample_sound(space, vt);
        self.output.set_to_weighted_sum_of(ps32(1.0), self.samples.iter());

        // TODO: impose soft limiting on sound output level for safety and comfort
    }

    fn sample_sound(&mut self, space: &space::Read<'_>, vt: camera::ViewTransform) {
        type HeadVector = Vector3D<f64, camera::Eye>;

        let vt: euclid::Transform3D<f64, camera::Eye, Cube> = vt.to_transform();
        let ray_origin = vt.transform_point3d(Point3D::origin()).unwrap();

        for _ray in 0..SAMPLES_PER_FRAME_COUNT {
            let index = (self.last_sample_index + 1).rem_euclid(self.samples.len());
            self.last_sample_index = index;
            let sample = &mut self.samples[self.last_sample_index];

            // Design note: Using pseudorandomly selected rays means that this the long-term
            // behavior of this system is not deterministic / doesn't reach a steady state.
            // However, for audio, I think that’s probably a good (or at least neutral) thing,
            // because any kind of temporally repeating pattern might be noticeable.

            let local_direction = HeadVector::from(rand_distr::UnitSphere.sample(&mut self.rng));
            let global_direction = vt.transform_vector3d(local_direction);
            let ray = Ray::new(ray_origin, global_direction);
            let sound = sound_from_one_ray(ray, space);

            sample.set_to_weighted_sum_of(ps32(1.0), [&sound.pan(local_direction)].into_iter());
        }
    }

    pub(crate) fn sound_average(&self) -> &SpatialAmbient {
        &self.output
    }
}

fn sound_from_one_ray(ray: Ray, space: &space::Read<'_>) -> Ambient {
    let max_steps = 20; // TODO: controllability

    let bounds = space.bounds();
    for step in ray.cast().within(bounds, false).take(max_steps) {
        if !bounds.contains_cube(step.cube_ahead()) {
            // TODO: sky should define sound too
            return Ambient::SILENT;
        } else {
            let ev = space.get_evaluated(step.cube_ahead());
            // TODO: sound behavior shouldn't be conditional on visibility but on an independent sound-opacity
            if ev.visible() {
                return ev.attributes().ambient_sound.clone();
            }
        }
    }
    // If we got here, nothing was hit
    Ambient::SILENT
}

// TODO: unit tests, e.g. furnace test

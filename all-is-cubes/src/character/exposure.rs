//! Analyzes the scene around a character to choose a camera “exposure” (brightness scale) value.

use euclid::{Point3D, vec3};

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

/// Acts as polyfill for float methods
#[cfg(not(any(feature = "std", test)))]
#[allow(unused_imports)]
use crate::math::Euclid as _;

use crate::camera;
use crate::math::FreeCoordinate;
use crate::raycast::Ray;
use crate::space::{LightPhysics, Space};

/// What average luminance of the exposed scene to try to match
const TARGET_LUMINANCE: f32 = 0.9;
/// Proportion by which we apply the exposure adjustment rather than not
/// (0.0 = none, 1.0 = perfect adaptation). This is less than 1 so that
/// dark areas stay dark.
/// TODO: this should be an adjustable game rule + graphics option.
const ADJUSTMENT_STRENGTH: f32 = 0.375;
const EXPOSURE_CHANGE_RATE: f32 = 2.0;

#[derive(Clone)]
pub(crate) struct State {
    /// Incrementally updated samples of neighboring light levels, used for
    /// determining exposure / eye adaptation.
    luminance_samples: [f32; 100],

    /// Last written element of [`Self::luminance_samples`].
    luminance_sample_index: usize,

    /// Natural log of the computed camera exposure value based on the samples..
    exposure_log: f32,
}

impl Default for State {
    fn default() -> Self {
        Self {
            luminance_samples: [1.0; _],
            luminance_sample_index: 0,
            exposure_log: 0.0,
        }
    }
}

impl State {
    pub fn exposure(&self) -> f32 {
        self.exposure_log.exp()
    }

    pub(crate) fn step(&mut self, space: &Space, vt: camera::ViewTransform, dt: f64) {
        #![allow(clippy::cast_lossless, reason = "lossiness depends on size of usize")]

        if dt == 0. {
            return;
        }

        // Sample surrounding light.
        // TODO: This is *yet another* implementation of "trace rays and do something with them";
        // we should reduce the redundancy and get better implementation by reusing code from the
        // raytracing renderer and/or the light updator.
        {
            let max_steps = match space.physics().light {
                LightPhysics::None => 0,
                LightPhysics::Rays { maximum_distance } => {
                    usize::from(maximum_distance).saturating_mul(2)
                }
            };
            let vt = vt.to_transform();
            let sqrtedge = (self.luminance_samples.len() as FreeCoordinate).sqrt();
            let ray_origin = vt.transform_point3d(Point3D::origin()).unwrap();
            'rays: for _ray in 0..10 {
                // TODO: better idea for what ray count should be
                let index =
                    (self.luminance_sample_index + 1).rem_euclid(self.luminance_samples.len());
                self.luminance_sample_index = index;
                let indexf = index as FreeCoordinate;
                let ray = Ray::new(
                    ray_origin,
                    // Fixed 90° FOV
                    vt.transform_vector3d(vec3(
                        (indexf).rem_euclid(sqrtedge) / sqrtedge * 2. - 1.,
                        (indexf).div_euclid(sqrtedge) / sqrtedge * 2. - 1.,
                        -1.0,
                    )),
                );
                // TODO: this should be something more like the light-propagation raycast.
                let bounds = space.bounds();
                for step in ray.cast().within(bounds, false).take(max_steps) {
                    // Require hitting a visible surface and checking behind it, because if we
                    // just take the first valid value, then we'll trivially pick the same cube
                    // every time if our eye is within a cube with valid light.
                    if !bounds.contains_cube(step.cube_ahead()) {
                        self.luminance_samples[self.luminance_sample_index] =
                            space.physics().sky.sample(ray.direction).luminance();
                        continue 'rays;
                    } else if space.get_evaluated(step.cube_ahead()).visible() {
                        let l = space.get_lighting(step.cube_behind());
                        if l.valid() {
                            self.luminance_samples[self.luminance_sample_index] =
                                l.value().luminance();
                            continue 'rays;
                        }
                    }
                }
                // If we got here, nothing was hit
                self.luminance_samples[self.luminance_sample_index] =
                    space.physics().sky.sample(ray.direction).luminance();
            }
        }

        let target_exposure = compute_target_exposure(self.luminance_average());
        if target_exposure.is_finite() {
            let delta_log = target_exposure.ln() - self.exposure_log;
            self.exposure_log += delta_log * dt as f32 * EXPOSURE_CHANGE_RATE;
        }
    }

    /// Combine the light samples.
    fn luminance_average(&self) -> f32 {
        self.luminance_samples.iter().copied().sum::<f32>()
            * (self.luminance_samples.len() as f32).recip()
    }
}

/// Compute the target exposure value (which [`State::exposure()`] is moving towards)
/// from the observed scene luminance.
fn compute_target_exposure(luminance: f32) -> f32 {
    let derived_exposure = (TARGET_LUMINANCE / luminance).clamp(0.1, 4.);
    // Lerp between full adjustment and no adjustment according to ADJUSTMENT_STRENGTH
    derived_exposure * ADJUSTMENT_STRENGTH + 1. * (1. - ADJUSTMENT_STRENGTH)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::character::Character;
    use crate::character::eye::CharacterEye;
    use crate::math::{GridAab, Rgb};
    use crate::time;
    use crate::universe::Universe;
    use euclid::point3;

    /// [`compute_target_exposure`] is what actually determines the exposure under
    /// steady-state conditions, so test it in isolation with some examples, because
    /// nothing else anywhere in the codebase is going to do that. (In particular, rendering
    /// tests will use fixed exposure.)
    #[test]
    fn target_exposure() {
        assert_eq!(
            [0.0, 0.01, 0.5, 1.0, 2.0, 100.0, 1000.0].map(compute_target_exposure),
            [2.125, 2.125, 1.3, 0.9625, 0.79375, 0.6625, 0.6625]
        );
    }

    /// End-to-end test for character exposure calculation,
    /// which incidentally tests some of the pieces too.
    ///
    /// This test does NOT test the *choice* of exposure (that is, it does not test
    /// [`compute_target_exposure`]), just how it's updated in a [`Character`].
    #[test]
    fn e2e() {
        // Put a character in a uniformly lit box.
        let mut universe = Universe::new();
        universe.set_clock(time::Clock::new(time::TickSchedule::per_second(10), 0));
        let luminance = 3.;
        let light = Rgb::new(luminance, luminance, luminance);

        // TODO: Also test a version with blocks instead of sky only:
        // let light_block = Block::builder()
        //     .color(Rgba::BLACK)
        //     .light_emission(light)
        //     .build();

        let space = universe.insert_anonymous({
            let mut space = Space::builder(GridAab::from_lower_size([0, 0, 0], [10, 10, 10]))
                .sky_color(light)
                .build();

            // space.fill_uniform(space.bounds(), light_block).unwrap();
            // space
            //     .fill_uniform(space.bounds().shrink(FaceMap::splat(1)), AIR)
            //     .unwrap();

            space.evaluate_light(0, drop);
            space
        });
        let mut character = Character::spawn_default(universe.read_ticket(), space);
        character.body.set_position(point3(5., 5., 5.));
        let character = universe.insert("character".into(), character).unwrap();

        // Let exposure sampling reach steady state
        for i in 0..100 {
            {
                let exposure = &character
                    .query::<CharacterEye>(universe.read_ticket())
                    .unwrap()
                    .exposure;
                eprintln!(
                    "{i:3} {exp_log} {exp}",
                    exp_log = exposure.exposure_log,
                    exp = exposure.exposure(),
                );
            }
            universe.step(false, time::Deadline::Whenever);
        }

        // Done running; examine results.

        let exposure = &character
            .query::<CharacterEye>(universe.read_ticket())
            .unwrap()
            .exposure;

        // Luminance sampling should match the scene we set up.
        eprintln!("{:?}", exposure.luminance_samples);
        assert_eq!(exposure.luminance_average(), luminance);

        // Exposure produced by the stateful process should closely approach the value from
        // compute_target_exposure.
        let expected = compute_target_exposure(luminance);
        let expected_actual_ratio_difference = exposure.exposure() / expected - 1.0;
        assert!(
            expected_actual_ratio_difference.abs() < 0.001,
            "actual {actual} != expected {expected} (ratio {expected_actual_ratio_difference})",
            actual = exposure.exposure()
        );
    }
}

//! Analyzes the scene around a character to choose a camera “exposure” (brightness scale) value.

use euclid::{vec3, Point3D};

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use crate::math::Euclid as _;

use crate::camera;
use crate::math::FreeCoordinate;
use crate::raycast::Ray;
use crate::space::{LightPhysics, Space};

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
            luminance_samples: [1.0; 100],
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
        #![allow(clippy::cast_lossless)] // lossiness depends on size of usize

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
                for step in ray.cast().within(bounds).take(max_steps) {
                    // Require hitting a visible surface and checking behind it, because if we
                    // just take the first valid value, then we'll trivially pick the same cube
                    // every time if our eye is within a cube with valid light.
                    if !bounds.contains_cube(step.cube_ahead()) {
                        self.luminance_samples[self.luminance_sample_index] =
                            space.physics().sky.sample(ray.direction).luminance();
                        continue 'rays;
                    } else if space.get_evaluated(step.cube_ahead()).visible {
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

        /// What average luminance of the exposed scene to try to match
        const TARGET_LUMINANCE: f32 = 0.9;
        /// Proportion by which we apply the exposure adjustment rather than not
        /// (0.0 = none, 1.0 = perfect adaptation). This is less than 1 so that
        /// dark areas stay dark.
        /// TODO: this should be an adjustable game rule + graphics option.
        const ADJUSTMENT_STRENGTH: f32 = 0.5;
        const EXPOSURE_CHANGE_RATE: f32 = 2.0;

        // Combine the light rays into an exposure value update.
        let luminance_average: f32 = self.luminance_samples.iter().copied().sum::<f32>()
            * (self.luminance_samples.len() as f32).recip();
        let derived_exposure = (TARGET_LUMINANCE / luminance_average).clamp(0.1, 10.);
        // Lerp between full adjustment and no adjustment according to ADJUSTMENT_STRENGTH
        let derived_exposure =
            derived_exposure * ADJUSTMENT_STRENGTH + 1. * (1. - ADJUSTMENT_STRENGTH);
        if derived_exposure.is_finite() {
            let delta_log = derived_exposure.ln() - self.exposure_log;
            self.exposure_log += delta_log * dt as f32 * EXPOSURE_CHANGE_RATE;
        }
    }
}

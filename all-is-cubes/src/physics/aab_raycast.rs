/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::math::{Aab, Face7, FreeCoordinate, FreePoint, FreeVector, Octant};
use crate::raycast::{Ray, Raycaster};

// -------------------------------------------------------------------------------------------------

/// Given a ray describing movement of the origin of an AAB, perform a raycast to find
/// the positions where the AAB moves into new cubes. The returned ray steps' `t_distance`
/// values report how far to move the AAB to meet the edge.
///
/// If `reversed` is true, find positions where it leaves cubes (which will be the
/// [`RaycastStep::cube_behind()`] of each step).
///
/// Note that due to the nature of floating-point arithmetic, it is uncertain whether the
/// resulting new AAB position will have the AAB's forward face land before, after, or
/// exactly on the boundary. The caller must compute an appropriate nudge (using TODO:
/// provide a function for this) to serve its needs.
///
/// TODO(crush): This function does not give enough information to avoid rounding errors resulting
/// in disagreement about what cubes the AAB actually touches. Replace all uses of this
/// with `aab_raycast()` to fix that.
///
/// [`RaycastStep`]: crate::raycast::RaycastStep
#[deprecated]
#[allow(dead_code, reason = "TODO: remove")]
pub(crate) fn aab_raycast_legacy(aab: Aab, origin_ray: Ray, reversed: bool) -> Raycaster {
    let leading_corner = aab.corner_point(Octant::from_vector(if reversed {
        -origin_ray.direction
    } else {
        origin_ray.direction
    }));
    let leading_ray = origin_ray.translate(leading_corner.to_vector());
    leading_ray.cast()
}

/// Given an AAB in absolute coordinates, and a translation vector to move linearly along,
/// returns a raycasting iterator for moving that AAB through a grid and detecting when the
/// forward-facing faces of the AAB start to intersect cubes they did not previously intersect.
///
/// If `reversed` is true, instead find positions where the rear-facing faces of the AAB stop
/// intersecting cubes they previously intersect.
///
/// In either case, it is guaranteed that the relevant face of the produced AAB lies exactly on the
/// relevant cube face; the other faces may be subject to rounding error, and in particular, the
/// size of the produced AAB may not be identical to the original AAB’s size.
/// Thus, this function does the best it can to produce AABs useful for exact collision detection.
///
/// The first returned step always has a `t_distance` of 0.
/// It may be used for detecting “already intersecting” situations.
///
/// The second element of the returned tuple is the special “last” step,
/// which always has a `t_distance` of 1,
/// and corresponds to the completion of a movement without collision.
/// (It is not returned as part of the iterator so that callers can depend on it always existing
/// and treat it specially.)
pub(crate) fn aab_raycast(
    absolute_aab: Aab,
    translation: FreeVector,
    reversed: bool,
) -> (impl Iterator<Item = AabRaycastStep>, AabRaycastStep) {
    // Find which corner of the AAB is the one adjacent to all of the forward-moving faces
    // (or the backward-moving faces if `reversed`), because that corner’s raycast steps correspond
    // to the steps we’re computing for the whole AAB.
    let leading_corner: Octant =
        Octant::from_vector(if reversed { -translation } else { translation });

    // Compute the relative position of the opposite corner, which we will use to construct new
    // AABs for each step.
    let offset_to_trailing_corner: FreeVector =
        leading_corner.reflect(-absolute_aab.size().to_vector());

    let leading_corner_ray = Ray {
        origin: absolute_aab.corner_point(leading_corner),
        direction: translation,
    };

    let last_step = {
        // TODO: make sure rounding error in this addition cannot result in the AAB advancing
        // past touching a face that should have been reported as a previous step.
        let leading_corner_this_step = leading_corner_ray.unit_endpoint();
        AabRaycastStep {
            t_distance: 1.0,
            // TODO(crush): “within” doesn’t quite describe this accurately, because it is
            // sometimes taken as describing only a preexisting collision.
            // We may need a different data type for the best possible handling.
            aab_face: Face7::Within,
            translated_aab: Aab::from_two_points(
                leading_corner_this_step,
                leading_corner_this_step + offset_to_trailing_corner,
            ),
        }
    };

    let iterator = leading_corner_ray
        .cast()
        .take_while(|ray_step| ray_step.t_distance() < 1.0)
        .map(move |ray_step| {
            // Note that `intersection_point()` guarantees that, while the returned point necessarily
            // has rounding error, it will never be outside of the cube face the raycast crossed.
            // Therefore, when we use it to construct the AAB, that corner of the AAB will also be
            // in the right place.
            let leading_corner_this_step: FreePoint =
                ray_step.intersection_point(leading_corner_ray);

            let translated_aab = Aab::from_two_points(
                leading_corner_this_step,
                // Unlike the leading corner, this corner point has rounding error (specifically, two
                // roundings, one from the size() calculation and one from this addition).
                leading_corner_this_step + offset_to_trailing_corner,
            );

            AabRaycastStep {
                t_distance: ray_step.t_distance(),
                aab_face: if reversed {
                    ray_step.face()
                } else {
                    ray_step.face().opposite()
                },
                translated_aab,
            }
        });
    (iterator, last_step)
}

/// Step output of [`aab_raycast()`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct AabRaycastStep {
    /// The distance traveled so far, in multiples of `direction.length()`.
    pub t_distance: FreeCoordinate,

    /// The input AAB translated to the position described by this step.
    ///
    /// The face `self.face` of this [`Aab`] will always be an exact integer.
    pub translated_aab: Aab,

    /// The face of the AAB which is touching a new cube on the grid.
    /// (This is the opposite of the new cube’s face.)
    pub aab_face: Face7,
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::{Face, ps64};
    use alloc::{vec, vec::Vec};
    use euclid::vec3;
    use rand::SeedableRng as _;
    use rand_distr::Distribution as _;
    use std::eprintln;

    fn aab_raycast_collect(
        (iterator, last): (impl Iterator<Item = AabRaycastStep>, AabRaycastStep),
    ) -> (Vec<AabRaycastStep>, AabRaycastStep) {
        const LIMIT: usize = 101;
        let steps: Vec<AabRaycastStep> = iterator.take(LIMIT).collect();
        if steps.len() == LIMIT {
            panic!("too many items");
        }
        (steps, last)
    }

    /// Non-randomized test of a concrete example of what [`aab_raycast()`] is supposed to do,
    /// with simple numbers instead of ones that will provoke rounding errors.
    #[test]
    fn aab_raycast_simple() {
        let aab = Aab::from_radius(ps64(0.25));
        let absolute_aab = aab.translate(vec3(0.5, 0.5, 0.5));
        let translation = vec3(2.0, 0.0, 0.0);

        pretty_assertions::assert_eq!(
            aab_raycast_collect(aab_raycast(absolute_aab, translation, false)),
            (
                vec![
                    AabRaycastStep {
                        t_distance: 0.0,
                        aab_face: Face7::Within,
                        translated_aab: absolute_aab,
                    },
                    AabRaycastStep {
                        t_distance: 1. / 8.,
                        aab_face: Face7::PX,
                        translated_aab: aab.translate(vec3(0.75, 0.5, 0.5)),
                    },
                    AabRaycastStep {
                        t_distance: 5. / 8.,
                        aab_face: Face7::PX,
                        translated_aab: aab.translate(vec3(1.75, 0.5, 0.5)),
                    },
                ],
                AabRaycastStep {
                    t_distance: 1.0,
                    aab_face: Face7::Within,
                    translated_aab: aab.translate(vec3(2.5, 0.5, 0.5)),
                },
            )
        );
    }

    #[cfg_attr(miri, ignore = "slow under Miri")]
    #[macro_rules_attribute::apply(crate::util::cartesian_product_test)]
    fn aab_raycast_aligned_with_grid(
        #[case(not_reversed = false)]
        #[case(reversed = true)]
        reversed: bool,
    ) {
        // not-round-in-base-2 numbers to provoke rounding misbehavior
        let absolute_aab = Aab::from_lower_upper([0.3, 0.6, 0.9], [1.3, 1.6, 1.9]);

        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
        for case_number in 0..1000 {
            let direction: FreeVector = rand_distr::UnitSphere.sample(&mut rng).into();
            let translation: FreeVector = direction * 5.0; // must be shorter than the take()

            // recomputing leading corner because it's not part of the algorithm's own outputs -- TODO dubious
            let leading_corner: Octant =
                Octant::from_vector(if reversed { -translation } else { translation });

            eprintln!("\n#{case_number} with inputs:");
            eprintln!("  translation: {translation:?}");

            let (iter, last) = aab_raycast(absolute_aab, translation, reversed);
            let mut iter = iter.take(100);

            let first_step = iter.next().expect("should have at least one step");
            assert_eq!(first_step.t_distance, 0.0);
            assert_eq!(first_step.aab_face, Face7::Within);
            assert_eq!(
                first_step.translated_aab.corner_point(leading_corner),
                absolute_aab.corner_point(leading_corner)
            );

            for step in iter {
                std::dbg!(step);
                let face = Face::try_from(step.aab_face).expect("should see no more Within");
                let face_coordinate = step.translated_aab.face_coordinate_on_axis(face);
                assert_eq!(face_coordinate, face_coordinate.round());
            }

            assert_eq!(
                last.aab_face,
                Face7::Within,
                "should have a last step that is Within"
            );
            // TODO: make assertions about the properties of the last step, such as that it does not
            // touch any new cubes.
        }
    }
}

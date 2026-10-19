/// Acts as polyfill for float methods
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use num_traits::float::Float as _;

use arrayvec::ArrayVec;
use euclid::{Vector3D, vec3};

#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use crate::math::Euclid as _;

use crate::math::{Aab, Axis, FaceMap, FreeCoordinate, FreeVector, PositiveSign};
use crate::physics::{Body, StopAt, collision::collide_along_ray};
use crate::space;

// -------------------------------------------------------------------------------------------------

/// If [`Body::occupying`] is smaller than [`Body::collision_box`], and there is room to grow it,
/// then do so. This undoes the effect of [`crush_if_colliding()`], and is performed near the
/// beginning of each body physics step.
//
// TODO: implement integrated push-out: move the body position when it would allow further
// uncrushing away from the opposite direction.
pub(in crate::physics) fn uncrush(body: &mut Body, space: &space::Read<'_>) -> UncrushInfo {
    const TRACE_UNCRUSH: bool = false;

    let uncrushed = body.uncrushed_collision_box_abs();
    if uncrushed == body.occupying {
        return UncrushInfo::NotNeeded;
    }

    let mut successful_expansions = ArrayVec::<Axis, 3>::new();

    // Try to find up to 3 axes to expand on, one at a time.
    for _ in 0..3 {
        if TRACE_UNCRUSH {
            log::trace!(
                "uncrush() attempt {i} starting",
                i = successful_expansions.len() + 1
            );
        }
        let current_volume = body.occupying.volume();

        let mut collided_with_anything = false;

        // `self.occupying` modified to be expanded on exactly one axis.
        let single_axis_expansions: Vector3D<Aab, ()> = {
            vec3(
                body.occupying.with_axis_range_from(Axis::X, uncrushed),
                body.occupying.with_axis_range_from(Axis::Y, uncrushed),
                body.occupying.with_axis_range_from(Axis::Z, uncrushed),
            )
        };

        // Position on each axis which `occupying` could be expanded to touch without colliding with
        // anything. Note that this
        // * Tracks expansions that are valid on a *single axis only*, but might collide if multiple
        //   axes are done simultaneously.
        // * Has flipped negative coordinates as per `Aab::face_coordinate_outward()`.
        let mut clear_space: FaceMap<FreeCoordinate> =
            FaceMap::from_fn(|face| uncrushed.face_coordinate_outward(face));

        // Find what we would collide with if `body.occupying` were expanded.
        // Note that this is a single collision search which is then used to compute
        // 3 different possible single-axis expansions.
        collide_along_ray(
            space,
            FreeVector::zero(),
            uncrushed,
            |contact| {
                collided_with_anything = true;
                let contact_aab = contact.aab();

                for axis in Axis::ALL {
                    // The contact only counts if it overlaps in the axes perpendicular to the
                    // axis we are considering expanding on.
                    // TODO: it would be more efficient to test only the perpendicular axes
                    // and not compute `single_axis_expansions` at all.
                    if !single_axis_expansions[axis].intersects(contact_aab) {
                        continue;
                    }

                    // Decide which side of the body this contact counts as obstructing
                    let contact_lb = contact_aab.lower_bounds_p()[axis];
                    let contact_ub = contact_aab.upper_bounds_p()[axis];
                    let body_position_on_axis = body.position()[axis];
                    let face = if contact_ub <= body_position_on_axis {
                        axis.negative_face()
                    } else if contact_lb >= body_position_on_axis {
                        axis.positive_face()
                    } else {
                        // This contact overlaps the body on this axis.
                        // In this case, don't try to uncrush on this axis,
                        // because the body is somewhere it should be escaping from,
                        // and uncrushing might make it harder to escape.
                        for face in [axis.negative_face(), axis.positive_face()] {
                            clear_space[face] = body.occupying.face_coordinate_outward(face);
                        }
                        continue;
                    };

                    // Reduce the clear space to that which does not overlap with `contact`.
                    if TRACE_UNCRUSH {
                        log::trace!(
                            "reducing: {:?} {:?} {:?}",
                            face,
                            clear_space[face],
                            contact_aab.face_coordinate_outward(face.opposite())
                        );
                    }
                    clear_space[face] = clear_space[face]
                        .min(-contact_aab.face_coordinate_outward(face.opposite()));
                }
            },
            StopAt::Anything,
        );

        if !collided_with_anything {
            // No collisions at all means we can immediately jump to full uncrush.
            body.occupying = uncrushed;
            return UncrushInfo::Complete;
        }

        // Find which expansion on a single axis gives us the most additional volume.
        let best_expansion: Option<(Axis, Aab, PositiveSign<FreeCoordinate>)> = Axis::ALL
            .into_iter()
            .filter_map(|axis| {
                let expanded_on_axis = body.occupying.with_axis_range(
                    axis,
                    -clear_space[axis.negative_face()],
                    clear_space[axis.positive_face()],
                )?;
                if !expanded_on_axis.contains(body.position()) {
                    // Shouldn’t happen, but definitely don’t let occupying stop containing
                    // the body’s position.
                    if TRACE_UNCRUSH {
                        log::trace!(
                            "rejecting {axis:?} {expanded_on_axis:?} \
                            for not containing the body's position"
                        );
                    }
                    return None;
                }
                let expansion_volume = expanded_on_axis.volume().saturating_sub(current_volume);
                if expansion_volume > 0.0 {
                    // Return the expansion only if it actually made some improvement
                    Some((axis, expanded_on_axis, expansion_volume))
                } else {
                    if TRACE_UNCRUSH {
                        log::trace!(
                            "rejecting {axis:?} {expanded_on_axis:?} \
                            for not being an increase in volume"
                        );
                    }
                    None
                }
            })
            .max_by_key(|&(_, _, volume)| volume);

        if let Some((axis, expanded_on_axis, volume)) = best_expansion {
            if TRACE_UNCRUSH {
                log::trace!("best axis {axis:?} expands by {volume} to {expanded_on_axis:?}");
            }
            body.occupying = expanded_on_axis;
            successful_expansions.push(axis);
        } else {
            break;
        }
    }

    let info = match *successful_expansions.as_slice() {
        [] => UncrushInfo::NotPossible,
        [first] => UncrushInfo::Partial {
            first,
            second: None,
            third: None,
        },
        [first, second] => UncrushInfo::Partial {
            first,
            second: Some(second),
            third: None,
        },
        [first, second, third] => UncrushInfo::Partial {
            first,
            second: Some(second),
            third: Some(third),
        },
        [..] => unreachable!(),
    };
    if TRACE_UNCRUSH {
        log::trace!("uncrush() -> {info:?}");
    }
    info
}

/// Result of [`uncrush()`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub(in crate::physics) enum UncrushInfo {
    /// Body’s `occupying` is already the right size.
    NotNeeded,
    /// No room for expansion was found.
    NotPossible,
    /// Body’s `occupying` is now the right size.
    Complete,
    /// Body’s `occupying` has been expanded fully or partially on one or more axes.
    Partial {
        first: Axis,
        second: Option<Axis>,
        third: Option<Axis>,
    },
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use crate::math::{Aab, GridAab, ps64};
    use crate::space::Space;

    fn test_uncrush(
        mut body: Body,
        space: &Space,
        initial_occupying: Aab,
        expected_occupying: Aab,
        expected_info: UncrushInfo,
    ) {
        body.occupying = initial_occupying;
        let info = uncrush(&mut body, &space.read());
        assert_eq!((info, body.occupying), (expected_info, expected_occupying));
    }

    #[test]
    fn uncrush_not_needed() {
        let body = Body::new_minimal([0., 1.25, 0.], Aab::from_radius(ps64(0.5)));
        let occupying = body.occupying;
        test_uncrush(
            body,
            &Space::empty_positive(1, 1, 1),
            occupying,
            occupying,
            UncrushInfo::NotNeeded,
        );
    }

    #[test]
    fn uncrush_unobstructed() {
        let expected_occupying = Aab::from_lower_upper([-0.5, 0.75, -0.5], [0.5, 1.75, 0.5]);
        test_uncrush(
            Body::new_minimal([0., 1.25, 0.], Aab::from_radius(ps64(0.5))),
            &Space::empty_positive(1, 1, 1),
            expected_occupying.expand_or_shrink(FaceMap::splat(0.1)).unwrap(),
            expected_occupying,
            UncrushInfo::Complete,
        );
    }

    /// When colliding with a fully overlapping cube, uncrush shouldn’t do anything.
    #[test]
    fn uncrush_impossible_intersecting() {
        let [block] = make_some_blocks();
        // pick a shrunk occupying box which should not expand
        let expected_occupying = Aab::from_lower_upper([0.25, 0.25, 0.25], [0.75, 0.75, 0.75]);
        test_uncrush(
            // body that intersects the block
            Body::new_minimal([0.5, 0.5, 0.5], Aab::from_radius(ps64(0.5))),
            // space that has a block
            &Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
                .filled_with(block.clone())
                .build(),
            expected_occupying,
            expected_occupying,
            UncrushInfo::NotPossible,
        );
    }

    #[test]
    fn uncrush_partial_success() {
        let [block] = make_some_blocks();
        test_uncrush(
            // body whose lower 0.25 intersects the block
            Body::new_minimal([0.5, 1.25, 0.5], Aab::from_radius(ps64(0.5))),
            // space that has a block
            &Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
                .filled_with(block.clone())
                .build(),
            // initial occupying has been crushed by a hypothetical obstacle
            // (Y lower is greater than 1.0, the block top, and greater than 0.75, the
            // fully uncrushed position)
            Aab::from_lower_upper([0.25, 1.125, 0.25], [0.75, 1.75, 0.75]),
            // final occupying should touch the block
            Aab::from_lower_upper([0., 1.0, 0.], [1., 1.75, 1.0]),
            UncrushInfo::Partial {
                // note: Z and X are equally good and Z coming first is accidental
                first: Axis::Z,
                second: Some(Axis::X),
                third: Some(Axis::Y),
            },
        );
    }

    // TODO: test uncrush when fully enclosed by solid surfaces
}

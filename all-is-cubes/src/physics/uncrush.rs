/// Acts as polyfill for float methods
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use num_traits::float::Float as _;

use arrayvec::ArrayVec;
use descriptive_unwrap::OptionExt as _;
use euclid::{Size3D, Vector3D, vec3};

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

/// If [`Body::occupying`] is smaller than [`Body::collision_box`] (“crushed”), then fix that as
/// much as possible.
///
/// If there is room to grow `occupying` to its normal size, that is done.
/// The body’s position may also change, to become its normal position within that box.
///
/// This undoes the effect of [`crush_if_colliding()`], and is performed near the
/// beginning of each body physics step.
pub(in crate::physics) fn uncrush(body: &mut Body, space: &space::Read<'_>) -> UncrushInfo {
    const TRACE_UNCRUSH: bool = false;

    let uncrushed_and_unmoved = body.uncrushed_collision_box_abs();
    if uncrushed_and_unmoved == body.occupying {
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
        let current_crushed_length_per_axis =
            (uncrushed_and_unmoved.size() - body.occupying.size()).max(Size3D::zero());

        // Compute the region around the current `body.occupying` that we *might* want to expand
        // into, and thus should be checked for obstacles.
        let region_to_check_for_collision = body
            .occupying
            .expand_or_shrink(FaceMap::from_fn(|face| {
                current_crushed_length_per_axis[face.axis()]
            }))
            .none_is_unreachable()
            // protection against rounding error in the subtraction followed by addition;
            // ensures uncrushed_and_unmoved will always be part of the checked region
            .union(uncrushed_and_unmoved);

        // `self.occupying` modified to be expanded on exactly one axis;
        // used to determine which axes a contact is relevant to.
        let single_axis_check_regions: Vector3D<Aab, ()> = {
            vec3(
                body.occupying.with_axis_range_from(Axis::X, region_to_check_for_collision),
                body.occupying.with_axis_range_from(Axis::Y, region_to_check_for_collision),
                body.occupying.with_axis_range_from(Axis::Z, region_to_check_for_collision),
            )
        };

        let mut collided_with_anything = false;

        // After the `collide_along_ray()` completes, this variable holds the region on each axis
        // which `occupying` could be expanded to touch without colliding with anything. Note that this
        // * Tracks expansions that are valid on a *single axis only*, but might collide if multiple
        //   axes are done simultaneously.
        // * Has flipped negative coordinates as per `Aab::face_coordinate_outward()`.
        // * Is potentially bigger than `uncrushed_and_unmoved`.
        let mut clear_space: FaceMap<FreeCoordinate> =
            FaceMap::from_fn(|face| region_to_check_for_collision.face_coordinate_outward(face));

        // Find what the body could collide with if `body.occupying` were to be expanded and
        // possibly moved.
        // Note that this is a single collision search which is then used to compute
        // 3 different possible single-axis expansions.
        collide_along_ray(
            space,
            FreeVector::zero(),
            region_to_check_for_collision,
            |contact| {
                collided_with_anything = true;
                let contact_aab = contact.aab();

                for axis in Axis::ALL {
                    // The contact only counts if it overlaps in the axes perpendicular to the
                    // axis we are considering expanding on.
                    // TODO: it would be more efficient to test only the perpendicular axes
                    // and not compute `single_axis_check_regions` at all.
                    if !single_axis_check_regions[axis].intersects(contact_aab) {
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
            body.occupying = uncrushed_and_unmoved;
            return UncrushInfo::Complete;
        }

        // Find which expansion on a single axis gives us the most additional volume.
        let best_expansion: Option<(Axis, Aab, PositiveSign<FreeCoordinate>)> = Axis::ALL
            .into_iter()
            .filter_map(|axis| {
                let clear_space_min = -clear_space[axis.negative_face()];
                let clear_space_max = clear_space[axis.positive_face()];

                let is_constrained_low =
                    clear_space_min > uncrushed_and_unmoved.lower_bounds_p()[axis];
                let is_constrained_high =
                    clear_space_max < uncrushed_and_unmoved.upper_bounds_p()[axis];

                // Determine how much expansion, in which direction, we actually propose to do
                // on this axis.
                let new_occupying = match (is_constrained_low, is_constrained_high) {
                    // We can fully expand without any translation.
                    (false, false) => {
                        body.occupying.with_axis_range_from(axis, uncrushed_and_unmoved)
                    }

                    // We must translate the body in the positive direction
                    (true, false) => body.occupying.with_axis_range(
                        axis,
                        clear_space_min,
                        (clear_space_min + uncrushed_and_unmoved.size()[axis])
                            .clamp(clear_space_min, clear_space_max),
                    )?,

                    // We must translate the body in the negative direction
                    (false, true) => body.occupying.with_axis_range(
                        axis,
                        (clear_space_max - uncrushed_and_unmoved.size()[axis])
                            .clamp(clear_space_min, clear_space_max),
                        clear_space_max,
                    )?,

                    // The clear space is less than the goal size, so the body cannot expand fully
                    // and will just take all available space.
                    (true, true) => {
                        body.occupying.with_axis_range(axis, clear_space_min, clear_space_max)?
                    }
                };

                if !new_occupying.contains(body.position()) {
                    // Shouldn’t happen, but definitely don’t let occupying stop containing
                    // the body’s position.
                    if TRACE_UNCRUSH {
                        log::trace!(
                            "rejecting {axis:?} {new_occupying:?} \
                            for not containing the body's position"
                        );
                    }
                    return None;
                }
                let expansion_volume = new_occupying.volume().saturating_sub(current_volume);
                if expansion_volume > 0.0 {
                    // Return the expansion only if it actually made some improvement
                    Some((axis, new_occupying, expansion_volume))
                } else {
                    if TRACE_UNCRUSH {
                        log::trace!(
                            "rejecting {axis:?} {new_occupying:?} \
                            for not being an increase in volume"
                        );
                    }
                    None
                }
            })
            .max_by_key(|&(_, _, volume)| volume);

        if let Some((axis, new_occupying, volume)) = best_expansion {
            if TRACE_UNCRUSH {
                log::trace!("best axis {axis:?} expands by {volume} to {new_occupying:?}");
            }

            // Compute the position which makes the new crushedness equal on both sides.
            let mut new_position = body.position();
            new_position[axis] =
                // position of new occupying box,
                new_occupying.lower_bounds_p()[axis]
                // translated to where the body position should be relative to it,
                - body.collision_box_rel().lower_bounds_p()[axis]
                // but scaled to what the new occupying box size is
                * (new_occupying.size()[axis] / body.collision_box_rel().size()[axis]);

            body.set_position_occupying(new_position, new_occupying);
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
    /// Body’s `occupying` is now the right size, and no translation was needed.
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
    use crate::math::{Aab, FreePoint, GridAab, ps64};
    use crate::space::Space;
    use euclid::point3;

    fn test_uncrush(
        mut body: Body,
        space: &Space,
        initial_occupying: Aab,
        expected_position: FreePoint,
        expected_occupying: Aab,
        expected_info: UncrushInfo,
    ) {
        body.occupying = initial_occupying;
        let info = uncrush(&mut body, &space.read());
        assert_eq!(
            (info, body.position(), body.occupying),
            (expected_info, expected_position, expected_occupying)
        );
    }

    #[test]
    fn uncrush_not_needed() {
        let position = point3(0., 1.25, 0.);
        let body = Body::new_minimal(position, Aab::from_radius(ps64(0.5)));
        let occupying = body.occupying;
        test_uncrush(
            body,
            &Space::empty_positive(1, 1, 1),
            occupying,
            position,
            occupying,
            UncrushInfo::NotNeeded,
        );
    }

    #[test]
    fn uncrush_unobstructed() {
        let position = point3(0., 1.25, 0.);
        let expected_occupying = Aab::from_lower_upper([-0.5, 0.75, -0.5], [0.5, 1.75, 0.5]);
        test_uncrush(
            Body::new_minimal(position, Aab::from_radius(ps64(0.5))),
            &Space::empty_positive(1, 1, 1),
            expected_occupying.expand_or_shrink(FaceMap::splat(0.1)).unwrap(),
            position,
            expected_occupying,
            UncrushInfo::Complete,
        );
    }

    /// When colliding with a fully overlapping cube, uncrush shouldn’t do anything.
    #[test]
    fn uncrush_impossible_intersecting() {
        let position = point3(0.5, 0.5, 0.5);
        let [block] = make_some_blocks();
        // pick a shrunk occupying box which should not expand
        let expected_occupying = Aab::from_lower_upper([0.25, 0.25, 0.25], [0.75, 0.75, 0.75]);
        test_uncrush(
            // body that intersects the block
            Body::new_minimal(position, Aab::from_radius(ps64(0.5))),
            // space that has a block
            &Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
                .filled_with(block.clone())
                .build(),
            expected_occupying,
            position,
            expected_occupying,
            UncrushInfo::NotPossible,
        );
    }

    #[test]
    fn uncrush_partial_success() {
        let position = point3(0.5, 1.25, 0.5);
        let [block] = make_some_blocks();
        test_uncrush(
            // body whose lower 0.25 intersects the block
            Body::new_minimal(position, Aab::from_radius(ps64(0.5))),
            // space that has a block
            &Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
                .filled_with(block.clone())
                .build(),
            // initial occupying has been crushed by a hypothetical obstacle
            // (Y lower is greater than 1.0, the block top, and greater than 0.75, the
            // fully uncrushed position)
            Aab::from_lower_upper([0.25, 1.125, 0.25], [0.75, 1.75, 0.75]),
            // position moved up
            point3(0.5, 1.5, 0.5),
            // final occupying should touch the block
            Aab::from_lower_upper([0., 1., 0.], [1., 2., 1.]),
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

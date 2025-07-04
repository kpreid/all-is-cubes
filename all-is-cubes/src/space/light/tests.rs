//! Tests for the behavior of light in a [`Space`].

use pretty_assertions::assert_eq;

use super::{LightUpdatesInfo, PackedLight, Priority, data::LightStatus};
use crate::block::{self, AIR, Block};
use crate::listen::{Listen as _, Listener, Log};
use crate::math::{Cube, Face6, FaceMap, GridPoint, Rgb, Rgba, rgb_const};
use crate::space::{CubeTransaction, GridAab, LightPhysics, Sky, Space, SpaceChange, SpacePhysics};
use crate::time;
use crate::universe::{ReadTicket, Universe};

#[test]
fn initial_value_in_empty_space() {
    let space = Space::builder(GridAab::ORIGIN_CUBE).build();
    std::dbg!(&space);
    assert_eq!(PackedLight::NO_RAYS, space.get_lighting([0, 0, 0]));
}

#[test]
fn initial_value_in_filled_space() {
    let space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(block::from_color!(Rgba::WHITE))
        .build();
    assert_eq!(PackedLight::OPAQUE, space.get_lighting([0, 0, 0]));
}

#[test]
fn initial_value_initialized_after_creation() {
    // First, initialize the space empty and without light data.
    // Note that we must use a space bigger than 1x1x1 because
    // the set_physics() will do a fast_evaluate_light(), not just all-Uninitialized.
    let mut space = Space::builder(GridAab::from_lower_size([0, 0, 0], [3, 3, 3]))
        .light_physics(LightPhysics::None)
        .build();
    assert_eq!(PackedLight::ONE, space.get_lighting([0, 0, 0]));

    // Put a block in it so this is not a trivial case, and activate lighting.
    space
        .mutate(ReadTicket::stub(), |m| {
            m.set([1, 1, 1], block::from_color!(1.0, 0.0, 0.0, 1.0))
        })
        .unwrap();
    space.set_physics(SpacePhysics {
        light: LightPhysics::Rays {
            maximum_distance: 10,
        },
        ..space.physics().clone()
    });

    // Note: this is pretty specific to the `fast_evaluate_light()` algorithm
    // and will probably break when we improve it.
    assert_eq!(
        space.light.block_sky.in_direction(Face6::PY),
        space.get_lighting([1, 2, 1]),
        "sky above obstacle"
    );
    assert_eq!(
        PackedLight::UNINITIALIZED_AND_BLACK,
        space.get_lighting([1, 0, 1]),
        "unknown below obstacle"
    );

    // TODO: also test what happens on updates?
}

#[test]
fn out_of_bounds_lighting_value() {
    let space = Space::builder(GridAab::ORIGIN_CUBE)
        .sky(Sky::Octants([
            Rgb::ONE * 2.0,
            Rgb::ONE * 3.0,
            Rgb::ONE * 5.0,
            Rgb::ONE * 7.0,
            Rgb::ONE * 11.0,
            Rgb::ONE * 13.0,
            Rgb::ONE * 17.0,
            Rgb::ONE * 19.0,
        ]))
        .build();
    for face in Face6::ALL {
        assert_eq!(
            space.physics().sky.for_blocks().in_direction(face),
            space.get_lighting(Cube::ORIGIN + face.normal_vector())
        );
    }
}

#[test]
fn step() {
    let mut universe = Universe::new();
    let color = Rgb::new(1.0, 0.0, 0.0);
    let space = universe
        .insert(
            "space".into(),
            Space::builder(GridAab::from_lower_upper([0, 0, 0], [3, 1, 1]))
                .sky_color(color)
                .build(),
        )
        .unwrap();
    let sky_light = PackedLight::from(color);

    universe
        .execute_1(
            &space,
            &CubeTransaction::replacing(None, Some(block::from_color!(Rgb::ONE))).at(Cube::ORIGIN),
        )
        .unwrap();
    // Not changed yet... except for the now-opaque block
    {
        let space = space.read(universe.read_ticket()).unwrap();
        assert_eq!(space.get_lighting([0, 0, 0]), PackedLight::OPAQUE);
        assert_eq!(space.get_lighting([1, 0, 0]), PackedLight::NO_RAYS);
        assert_eq!(space.get_lighting([2, 0, 0]), PackedLight::NO_RAYS);
    }

    let info = universe.step(false, time::Deadline::Whenever);
    assert_eq!(
        info.space_step.light,
        LightUpdatesInfo {
            update_count: 1,
            max_update_difference: sky_light.difference_priority(PackedLight::NO_RAYS),
            queue_count: 0,
            max_queue_priority: Priority::MIN
        }
    );

    {
        let space = space.read(universe.read_ticket()).unwrap();
        assert_eq!(space.get_lighting([0, 0, 0]), PackedLight::OPAQUE); // opaque
        assert_eq!(space.get_lighting([1, 0, 0]), sky_light); // updated
        assert_eq!(space.get_lighting([2, 0, 0]), PackedLight::NO_RAYS); // not updated/not relevant
    }
}

#[test]
fn evaluate_light() {
    let mut space = Space::empty_positive(3, 1, 1);
    assert_eq!(0, space.evaluate_light(0, |_| {}));
    space
        .mutate(ReadTicket::stub(), |m| {
            m.set([1, 0, 0], block::from_color!(Rgb::ONE))
        })
        .unwrap();
    assert_eq!(2, space.evaluate_light(0, |_| {}));
    assert_eq!(0, space.evaluate_light(0, |_| {}));
    // This is just a smoke test, "is it plausible that it's working".
    // Ideally we'd confirm identical results from repeated step() and single evaluate_light().
}

// TODO: test evaluate_light's epsilon parameter

/// There's a special case for setting cubes to opaque. That case must do the usual
/// light update and notification.
#[test]
fn set_cube_opaque_notification() {
    let mut space = Space::empty_positive(1, 1, 1);
    let log: Log<SpaceChange> = Log::new();
    space.listen(log.listener().filter(|change| match change {
        SpaceChange::CubeLight { cube: _ } => Some(change.clone()),
        _ => None,
    }));
    // Self-test that the initial condition is not trivially the answer we're looking for
    assert_ne!(space.get_lighting([0, 0, 0]), PackedLight::OPAQUE);

    space
        .mutate(ReadTicket::stub(), |m| {
            m.set([0, 0, 0], block::from_color!(Rgb::ONE))
        })
        .unwrap();

    assert_eq!(space.get_lighting([0, 0, 0]), PackedLight::OPAQUE);
    assert_eq!(
        log.drain(),
        vec![SpaceChange::CubeLight {
            cube: Cube::new(0, 0, 0)
        }]
    );
}

fn light_source_test_space(block: Block) -> Space {
    let mut space = Space::builder(GridAab::from_lower_upper([0, 0, 0], [3, 3, 3]))
        .sky_color(Rgb::ZERO)
        .build();
    space
        .mutate(ReadTicket::stub(), |m| m.set([1, 1, 1], block))
        .unwrap();
    space.evaluate_light(0, |_| ());
    space
}

#[test]
fn light_source_self_illumination_transparent() {
    let light = Rgb::new(0.5, 1.0, 2.0);
    let alpha = 0.125;
    let block = Block::builder()
        .color(Rgba::new(1.0, 0.0, 0.0, alpha)) // irrelevant except maybe for alpha
        .light_emission(light)
        .build();

    let space = light_source_test_space(block);
    assert_eq!(space.get_lighting([1, 1, 1]), light.into());
}

#[test]
fn light_source_self_illumination_opaque() {
    let light = Rgb::new(0.5, 1.0, 2.0);
    let block = Block::builder()
        .color(Rgba::new(1.0, 1.0, 1.0, 1.0)) // irrelevant except for alpha
        .light_emission(light)
        .build();

    let space = light_source_test_space(block);
    assert_eq!(space.get_lighting([1, 1, 1]), light.into());
    let adjacents = FaceMap::from_fn(|face| {
        space
            .get_lighting(GridPoint::new(1, 1, 1) + face.normal_vector())
            .value()
    });
    assert_eq!(
        adjacents,
        // TODO: make this test less fragile. The asymmetry isn't even wanted;
        // I think it's probably due to exactly diagonal rays having to make a choice
        // of which neighbors to pass through.
        // Some of the values also differ due to our current choice of discarding
        // light updates with priority 1.
        FaceMap {
            nx: Rgb::new(0.13631347, 0.27262694, 0.5452539),
            ny: Rgb::new(0.16928194, 0.3385639, 0.6771278),
            nz: Rgb::new(0.2102241, 0.4204482, 0.8408964),
            px: Rgb::new(0.13631347, 0.27262694, 0.5452539),
            py: Rgb::new(0.16928194, 0.3385639, 0.6771278),
            pz: Rgb::new(0.2102241, 0.4204482, 0.8408964),
        },
    );
}

/// Check that an animation hint causes a block and its neighbors to be lit even if
/// it isn't visible, to be prepared for changes.
#[test]
fn animation_treated_as_visible() {
    fn eval_mid_block(block: Block) -> [LightStatus; 2] {
        let mut space = Space::empty_positive(3, 3, 3);
        space
            .mutate(ReadTicket::stub(), |m| m.set([1, 1, 1], block))
            .unwrap();
        space.evaluate_light(0, |_| {});
        [
            space.get_lighting([1, 1, 1]).status(),
            space.get_lighting([0, 1, 1]).status(),
        ]
    }
    let no_block = eval_mid_block(AIR);
    let visible_block = eval_mid_block(block::from_color!(1.0, 1.0, 1.0, 0.5));
    let invisible_but_animated = eval_mid_block(
        Block::builder()
            .color(Rgba::TRANSPARENT)
            .animation_hint(block::AnimationHint::redefinition(
                block::AnimationChange::Shape,
            ))
            .build(),
    );
    assert_eq!(
        [no_block, visible_block, invisible_but_animated,],
        [
            [LightStatus::NoRays, LightStatus::NoRays],
            [LightStatus::Visible, LightStatus::Visible],
            [LightStatus::Visible, LightStatus::Visible],
        ]
    );
}

#[test]
fn reflectance_is_clamped() {
    let over_unity_block = block::from_color!(16.0, 1.0, 0.0, 1.0);
    let sky_color = rgb_const!(0.5, 0.5, 0.5);
    let mut space = Space::builder(GridAab::from_lower_size([0, 0, 0], [5, 3, 3]))
        .sky_color(sky_color)
        .build_and_mutate(|m| {
            m.set([1, 1, 1], &over_unity_block).unwrap();
            m.set([3, 1, 1], &over_unity_block).unwrap();
            Ok(())
        })
        .unwrap();
    space.evaluate_light(0, |_| {});

    let light = space.get_lighting([2, 1, 1]).value();
    dbg!(light);
    assert!(light.red() <= sky_color.red());
}

/// Helper to construct a space with `LightPhysics` set to None
fn space_with_disabled_light() -> Space {
    let mut space = Space::empty_positive(1, 1, 1);
    space.set_physics(SpacePhysics {
        light: LightPhysics::None,
        ..SpacePhysics::default()
    });
    space
}

#[test]
fn disabled_lighting_returns_one_always() {
    assert_eq!(
        space_with_disabled_light().get_lighting([0, 0, 0]),
        PackedLight::ONE
    );
}

#[test]
fn disabled_lighting_does_not_update() {
    let mut universe = Universe::new();
    let mut space = space_with_disabled_light();
    space
        .light
        .light_needs_update(Cube::new(0, 0, 0), Priority::UNINIT);
    universe.insert_anonymous(space);
    assert_eq!(
        universe
            .step(false, time::Deadline::Whenever)
            .space_step
            .light,
        LightUpdatesInfo::default()
    );
    // TODO: this test could really easily fail to report anything meaningful; also test non-disabled lighting.
}

// TODO: test sky lighting propagation onto blocks after quiescing

// TODO: test a single semi-transparent block will receive and diffuse light

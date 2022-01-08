// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Tests for the behavior of light in a [`Space`].

use crate::apps::Tick;
use crate::block::{AnimationHint, Block, AIR};
use crate::listen::{Listener, Sink};
use crate::math::{FaceMap, GridPoint, Rgb, Rgba};
use crate::space::{
    Grid, LightPhysics, LightStatus, LightUpdatesInfo, PackedLight, Space, SpaceChange,
    SpacePhysics,
};
use pretty_assertions::assert_eq;

#[test]
fn initial_lighting_value() {
    let space = Space::empty_positive(1, 1, 1);
    assert_eq!(PackedLight::NO_RAYS, space.get_lighting((0, 0, 0)));
}

#[test]
fn out_of_bounds_lighting_value() {
    let space = Space::empty_positive(1, 1, 1);
    assert_eq!(
        PackedLight::from(space.physics().sky_color),
        space.get_lighting((-1, 0, 0))
    );
}

#[test]
fn step() {
    let mut space = Space::empty_positive(3, 1, 1);
    space.set_physics(SpacePhysics {
        sky_color: Rgb::new(1.0, 0.0, 0.0),
        ..SpacePhysics::default()
    });
    let sky_light = PackedLight::from(space.physics().sky_color);

    space.set((0, 0, 0), Rgb::ONE).unwrap();
    // Not changed yet... except for the now-opaque block
    assert_eq!(space.get_lighting((0, 0, 0)), PackedLight::OPAQUE);
    assert_eq!(space.get_lighting((1, 0, 0)), PackedLight::NO_RAYS);
    assert_eq!(space.get_lighting((2, 0, 0)), PackedLight::NO_RAYS);

    let (info, _) = space.step(None, Tick::arbitrary());
    assert_eq!(
        info.light,
        LightUpdatesInfo {
            update_count: 1,
            max_update_difference: sky_light.difference_priority(PackedLight::NO_RAYS),
            queue_count: 0,
            max_queue_priority: 0
        }
    );

    assert_eq!(space.get_lighting((0, 0, 0)), PackedLight::OPAQUE); // opaque
    assert_eq!(space.get_lighting((1, 0, 0)), sky_light); // updated
    assert_eq!(space.get_lighting((2, 0, 0)), PackedLight::NO_RAYS); // not updated/not relevant
}

#[test]
fn evaluate_light() {
    let mut space = Space::empty_positive(3, 1, 1);
    assert_eq!(0, space.evaluate_light(0, |_| {}));
    space.set([1, 0, 0], Rgb::ONE).unwrap();
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
    let sink = Sink::new();
    space.listen(
        sink.listener()
            .filter(|change| matches!(change, SpaceChange::Lighting(_)).then(|| change)),
    );
    // Self-test that the initial condition is not trivially the answer we're looking for
    assert_ne!(space.get_lighting([0, 0, 0]), PackedLight::OPAQUE);

    space.set([0, 0, 0], Block::from(Rgb::ONE)).unwrap();

    assert_eq!(space.get_lighting([0, 0, 0]), PackedLight::OPAQUE);
    assert_eq!(
        sink.drain(),
        vec![SpaceChange::Lighting(GridPoint::new(0, 0, 0))]
    );
}

fn light_source_test_space(block: Block) -> Space {
    let mut space = Space::empty_positive(3, 3, 3);
    space.set_physics(SpacePhysics {
        sky_color: Rgb::ZERO,
        ..Default::default()
    });
    space.set([1, 1, 1], block).unwrap();
    space.evaluate_light(0, |_| ());
    space
}

#[test]
fn light_source_self_illumination_transparent() {
    let light = Rgb::new(0.5, 1.0, 2.0);
    let alpha = 0.125;
    let block = Block::builder()
        .light_emission(light)
        .color(Rgba::new(1.0, 0.0, 0.0, alpha)) // irrelevant except for alpha
        .build();

    let space = light_source_test_space(block);
    // TODO: Arguably the coverage/alpha shouldn't affect light emission.
    // Perhaps we should multiply the emission value by the coverage.
    assert_eq!(space.get_lighting([1, 1, 1]).value(), light * alpha);
}

#[test]
fn light_source_self_illumination_opaque() {
    let light = Rgb::new(0.5, 1.0, 2.0);
    let block = Block::builder()
        .light_emission(light)
        .color(Rgba::new(1.0, 1.0, 1.0, 1.0)) // irrelevant except for alpha
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
        // I think it's probably due to exactly diagonal rays.
        // Some of the values also differ due to our current choice of discarding
        // light updates with priority 1.
        FaceMap {
            within: light,
            nx: Rgb::new(0.13053422, 0.26106843, 0.52213687),
            ny: Rgb::new(0.16210495, 0.3242099, 0.6484198),
            nz: Rgb::new(0.20131129, 0.40262258, 0.80524516),
            px: Rgb::new(0.13053422, 0.26106843, 0.52213687),
            py: Rgb::new(0.16210495, 0.3242099, 0.6484198),
            pz: Rgb::new(0.20131129, 0.40262258, 0.80524516),
        },
    );
}

/// Check that an animation hint causes a block and its neighbors to be lit even if
/// it isn't visible, to be prepared for changes.
#[test]
fn animation_treated_as_visible() {
    fn eval_mid_block(block: Block) -> [LightStatus; 2] {
        let mut space = Space::empty_positive(3, 3, 3);
        space.set([1, 1, 1], block).unwrap();
        space.evaluate_light(0, |_| {});
        [
            space.get_lighting([1, 1, 1]).status(),
            space.get_lighting([0, 1, 1]).status(),
        ]
    }
    let no_block = eval_mid_block(AIR);
    let visible_block = eval_mid_block(Block::from(rgba_const!(1.0, 1.0, 1.0, 0.5)));
    let invisible_but_animated = eval_mid_block(
        Block::builder()
            .color(Rgba::TRANSPARENT)
            .animation_hint(AnimationHint::CONTINUOUS)
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
    let over_unity_block = Block::from(rgba_const!(16.0, 1.0, 0.0, 1.0));
    let sky_color = rgb_const!(0.5, 0.5, 0.5);
    let mut space = Space::builder(Grid::new([0, 0, 0], [5, 3, 3]))
        .sky_color(sky_color)
        .build_empty();
    space.set([1, 1, 1], &over_unity_block).unwrap();
    space.set([3, 1, 1], &over_unity_block).unwrap();
    space.evaluate_light(0, |_| {});

    let light = space.get_lighting([2, 1, 1]).value();
    dbg!(light);
    assert!(light.red() <= sky_color.red());
}

/// Helper to construct a space with LightPhysics set to None
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
    let mut space = space_with_disabled_light();
    space.light_needs_update(GridPoint::new(0, 0, 0), u8::MAX);
    assert_eq!(
        space.step(None, Tick::arbitrary()).0.light,
        LightUpdatesInfo::default()
    );
}

// TODO: test sky lighting propagation onto blocks after quiescing

// TODO: test a single semi-transparent block will receive and diffuse light

use euclid::{point2, point3, vec2, vec3, Point3D, Rotation3D, Size2D};
use pretty_assertions::assert_eq;
use rand::SeedableRng;

use crate::camera::{
    look_at_y_up, Camera, ExposureOption, FrustumPoints, GraphicsOptions, LightingOption,
    ViewTransform, Viewport,
};
use crate::math::{ps32, ps64, rgba_const, Aab};

#[test]
fn camera_bad_viewport_doesnt_panic() {
    Camera::new(
        GraphicsOptions::default(),
        Viewport {
            nominal_size: Size2D::new(0.0, 0.0),
            framebuffer_size: Size2D::new(0, 0),
        },
    );
}

#[test]
fn set_options_updates_matrices() {
    let mut camera = Camera::new(GraphicsOptions::default(), Viewport::ARBITRARY);
    let matrix = camera.projection_matrix();
    camera.set_options({
        let mut g = camera.options().clone();
        g.fov_y = ps64(30.);
        g
    });
    assert_ne!(matrix, camera.projection_matrix());
}

#[test]
fn camera_view_position() {
    // This test used to be less trivial when the transform was taken as a matrix
    let mut camera = Camera::new(GraphicsOptions::default(), Viewport::ARBITRARY);
    let pos = Point3D::new(1.0, 2.0, 3.0);
    camera.set_view_transform(ViewTransform {
        rotation: Rotation3D::identity(),
        translation: pos.to_vector().cast_unit(),
    });
    assert_eq!(camera.view_position(), pos);
}

/// Test that the range of depth values produced by the projection matrix is as expected.
#[test]
fn projection_depth() {
    let camera = Camera::new(
        GraphicsOptions::default(),
        Viewport::with_scale(1.0, [4, 3]),
    );
    let mat = dbg!(camera.projection_matrix());
    let world_depths = [camera.near_plane_distance(), camera.view_distance()];
    let expected_ndc_depths = [0., 1.];
    let actual_ndc_depths = world_depths.map(|z| {
        let eye = point3(0., 0., -f64::from(z));
        let clip = mat.transform_point3d_homogeneous(eye);
        // doesn't reject z=0 like euclid's to_point3d() does
        clip.z / clip.w
    });

    dbg!(expected_ndc_depths, actual_ndc_depths);
    assert!(actual_ndc_depths
        .into_iter()
        .zip(expected_ndc_depths)
        .all(|(a, e)| (a - e).abs() < 1e-8));
}

#[test]
fn view_frustum() {
    let camera = Camera::new(
        GraphicsOptions {
            view_distance: ps64(10f64.powi(2)),
            fov_y: ps64(90.),
            ..GraphicsOptions::default()
        },
        Viewport::with_scale(1.0, [10, 5]),
    );
    // TODO: approximate comparison instead of equals
    let x_near = 0.062499999999999986;
    let y_near = 0.031249999999999993;
    let z_near = -0.03125;
    let x_far = 200.00000000003973;
    let y_far = 100.00000000001987;
    let z_far = -100.0000000000199;
    assert_eq!(
        camera.view_frustum,
        FrustumPoints {
            lbn: point3(-x_near, -y_near, z_near),
            ltn: point3(-x_near, y_near, z_near),
            rbn: point3(x_near, -y_near, z_near),
            rtn: point3(x_near, y_near, z_near),
            lbf: point3(-x_far, -y_far, z_far),
            ltf: point3(-x_far, y_far, z_far),
            rbf: point3(x_far, -y_far, z_far),
            rtf: point3(x_far, y_far, z_far),
            bounds: Aab::new(-x_far, x_far, -y_far, y_far, z_far, z_near),
        }
    );
}

#[test]
fn post_process() {
    let mut options = GraphicsOptions::default();
    let mut camera = Camera::new(options.clone(), Viewport::ARBITRARY);

    // A camera with all default options should pass colors unchanged.
    let color = rgba_const!(0.1, 0.2, 0.3, 0.4);
    assert_eq!(camera.post_process_color(color), color);

    // Try exposure
    options.exposure = ExposureOption::Fixed(ps32(0.5));
    camera.set_options(options);
    assert_eq!(
        camera.post_process_color(color),
        color.map_rgb(|rgb| rgb * 0.5)
    );
}

#[test]
fn exposure_automatic_active() {
    let mut camera = Camera::new(
        GraphicsOptions {
            exposure: ExposureOption::Automatic,
            lighting_display: LightingOption::Smooth,
            ..GraphicsOptions::default()
        },
        Viewport::ARBITRARY,
    );

    camera.set_measured_exposure(7.0);
    assert_eq!(camera.exposure(), ps32(7.0));
}

#[test]
fn exposure_automatic_disabled_when_lighting_is_disabled() {
    let mut camera = Camera::new(
        GraphicsOptions {
            exposure: ExposureOption::Automatic,
            lighting_display: LightingOption::None,
            ..GraphicsOptions::default()
        },
        Viewport::ARBITRARY,
    );

    camera.set_measured_exposure(7.0);
    assert_eq!(camera.exposure(), ps32(1.0)); // ignoring measured
}

#[test]
fn look_at_identity() {
    let id = ViewTransform::identity();
    assert_eq!(look_at_y_up(point3(0., 0., 0.), point3(0., 0., -10.)), id);
}

#[test]
fn look_at_direction_consistency() {
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(253789);
    for _ in 0..100 {
        let Some(direction) = Aab::new(-1., 1., -1., 1., -1., 1.)
            .random_point(&mut rng)
            .to_vector()
            .try_normalize()
        else {
            continue;
        };
        let rotation = look_at_y_up(point3(0., 0., 0.), direction.to_point());
        let rotated_eye_vector = rotation
            .to_transform()
            .transform_vector3d(vec3(0., 0., -1.));

        let difference = rotated_eye_vector - direction;
        assert!(
            difference.length() < 1e-4,
            "{direction:?} -> {rotated_eye_vector:?}"
        );
    }
}

#[test]
fn viewport_is_empty() {
    assert!(Viewport::with_scale(1.0, vec2(0, 1)).is_empty());
    assert!(Viewport::with_scale(1.0, vec2(1, 0)).is_empty());
    assert!(Viewport::with_scale(1.0, vec2(100, 0)).is_empty());

    assert!(!Viewport::with_scale(1.0, vec2(100, 1)).is_empty());
    assert!(!Viewport::with_scale(1.0, vec2(10, 10)).is_empty());

    // nominal size does not matter
    assert!(!Viewport::with_scale(0.0, vec2(10, 10)).is_empty());
}

#[test]
fn project_ndc_into_world_edge_cases() {
    let camera = Camera::new(GraphicsOptions::default(), Viewport::ARBITRARY);
    {
        let ray = dbg!(camera.project_ndc_into_world(point2(f64::NAN, 0.0)));
        assert!(ray.origin.x.is_nan());
        assert!(ray.direction.x.is_nan());
    }
    {
        let ray = dbg!(camera.project_ndc_into_world(point2(f64::INFINITY, 0.0)));
        assert!(ray.origin.x.is_nan());
        assert!(ray.direction.x.is_nan());
    }
}

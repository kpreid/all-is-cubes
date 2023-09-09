use super::*;
use euclid::Rotation3D;
use pretty_assertions::assert_eq;
use rand::SeedableRng;

#[test]
fn camera_bad_viewport_doesnt_panic() {
    Camera::new(
        GraphicsOptions::default(),
        Viewport {
            nominal_size: Vector2D::new(0.0, 0.0),
            framebuffer_size: Vector2D::new(0, 0),
        },
    );
}

#[test]
fn set_options_updates_matrices() {
    let mut camera = Camera::new(GraphicsOptions::default(), Viewport::ARBITRARY);
    let matrix = camera.projection();
    camera.set_options({
        let mut g = camera.options().clone();
        g.fov_y = NotNan::from(30);
        g
    });
    assert_ne!(matrix, camera.projection());
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

#[test]
fn view_frustum() {
    let camera = Camera::new(
        GraphicsOptions {
            view_distance: NotNan::from(10i32.pow(2)),
            fov_y: NotNan::from(90),
            ..GraphicsOptions::default()
        },
        Viewport::with_scale(1.0, [10, 5]),
    );
    // TODO: approximate comparison instead of equals
    let x_near = 0.062499999999999986;
    let y_near = 0.031249999999999993;
    let z_near = -0.03125;
    let x_far = 199.99999999996868;
    let y_far = 99.99999999998434;
    let z_far = -99.99999999998437;
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
    options.exposure = ExposureOption::Fixed(notnan!(0.5));
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
    assert_eq!(camera.exposure(), notnan!(7.0));
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
    assert_eq!(camera.exposure(), notnan!(1.0)); // ignoring measured
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

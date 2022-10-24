use super::*;
use pretty_assertions::assert_eq;

#[test]
fn camera_bad_viewport_doesnt_panic() {
    Camera::new(
        GraphicsOptions::default(),
        Viewport {
            nominal_size: Vector2::new(0.0, 0.0),
            framebuffer_size: Vector2::new(0, 0),
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
    let pos = Point3::new(1.0, 2.0, 3.0);
    camera.set_view_transform(Decomposed {
        scale: 1.0,
        rot: Basis3::one(),
        disp: pos.to_vec(),
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
        Viewport::with_scale(1.0, Vector2::new(10, 5)),
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
            lbn: Point3::new(-x_near, -y_near, z_near),
            ltn: Point3::new(-x_near, y_near, z_near),
            rbn: Point3::new(x_near, -y_near, z_near),
            rtn: Point3::new(x_near, y_near, z_near),
            lbf: Point3::new(-x_far, -y_far, z_far),
            ltf: Point3::new(-x_far, y_far, z_far),
            rbf: Point3::new(x_far, -y_far, z_far),
            rtf: Point3::new(x_far, y_far, z_far),
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

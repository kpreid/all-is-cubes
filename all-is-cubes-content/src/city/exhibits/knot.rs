use super::prelude::*;

#[exhibit(
    name: "Knot",
    subtitle: "Complex voxel shape",
    placement: Placement::Surface,
)]
fn KNOT(ctx: Context<'_>) {
    let mut txn = ExhibitTransaction::default();
    let footprint = GridAab::from_lower_size([-2, -2, -1], [5, 5, 3]);
    let resolution = R32;
    let resf = FreeCoordinate::from(resolution);
    let toroidal_radius = resf * 1.5;
    let knot_split_radius = resf * 0.5625;
    let strand_radius = resf * 0.25;
    let twists = 2.5;

    let paint1 = block::from_color!(0.7, 0.7, 0.7, 1.0);
    let paint2 = block::from_color!(0.1, 0.1, 0.9, 1.0);
    let paint3 = block::from_color!(0.9, 0.7, 0.1, 1.0);
    let drawing_space = Space::builder(footprint.multiply(resolution.into()))
        .read_ticket(ctx.universe.read_ticket())
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build_and_mutate(|m| {
            m.fill_all(|p| {
                // Measure from midpoint of odd dimension space
                let p = p - Vector3D::new(1, 1, 1) * (GridCoordinate::from(resolution) / 2);
                // Work in floating point
                let p = p.lower_bounds().map(FreeCoordinate::from);

                let cylindrical = Vector2D::<_, Cube>::new(p.to_vector().xy().length(), p.z);
                let torus_cross_section = cylindrical - Vector2D::new(toroidal_radius, 0.);
                let knot_center_angle = p.xy().to_vector().angle_from_x_axis();
                let rotated_cross_section = Rotation2D::new(knot_center_angle * twists)
                    .transform_vector(torus_cross_section);

                let angle_if_within_strand = |offset: Vector2D<f64, Cube>| {
                    let knot_center = rotated_cross_section
                        .component_mul(Vector2D::new(1.0, 2.0_f64.sqrt().recip()))
                        + offset;
                    if knot_center.length() < strand_radius {
                        // Add center angle to add twist relative to the strands.
                        Some(knot_center.x.atan2(knot_center.y) + knot_center_angle.radians)
                    } else {
                        None
                    }
                };

                // Compute stripe pattern
                // Note that the second strand is rotated by PI so they join up
                if let Some(strand_radial_angle) =
                    angle_if_within_strand(Vector2D::new(-knot_split_radius, 0.)).or_else(|| {
                        angle_if_within_strand(Vector2D::new(knot_split_radius, 0.)).map(|a| a + PI)
                    })
                {
                    let unit_range = (strand_radial_angle / (PI * 2.)).rem_euclid(1.0);
                    Some(if unit_range < 0.25 {
                        &paint2
                    } else if (0.5..0.75).contains(&unit_range) {
                        &paint3
                    } else {
                        &paint1
                    })
                } else {
                    None
                }
            })?;
            Ok(())
        })?;
    let space = space_to_blocks(
        txn.insert_anonymous(drawing_space),
        txn.read_ticket(),
        resolution,
        &mut |block| block.with_modifier(block::SetAttribute::DisplayName(ctx.exhibit.name.into())),
    )?;
    Ok((space, txn))
}

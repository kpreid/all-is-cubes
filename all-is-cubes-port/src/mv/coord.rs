use all_is_cubes::math::{GridAab, GridCoordinate, GridRotation, GridVector, Gridgid};

// -------------------------------------------------------------------------------------------------

pub(crate) const MV_TO_AIC_ROTATION: GridRotation = GridRotation::RXzY;

/// Coordinate transform which converts the coordinate system handedness and “up”
/// direction conventional for MagicaVoxel to the one conventional for All is Cubes.
///
/// The input size should be in the original MagicaVoxel coordinate system.
pub(crate) fn mv_to_aic_coordinate_transform(mv_size: dot_vox::Size) -> Gridgid {
    // Coordinates are Z-up right-handed compared to our Y-up right-handed,
    // so swap Z into Y and invert Y as Z.
    // (This is not a `GridRotation::to_positive_octant_matrix()` because the `sizes` are
    // not necessarily equal.)
    Gridgid {
        // Unwrap OK-ish because the actual allowed data size is limited to much smaller values
        // (1024?). Still, TODO: make this an import error instead.
        translation: GridVector::new(0, 0, GridCoordinate::try_from(mv_size.y).unwrap()),
        rotation: MV_TO_AIC_ROTATION,
    }
}

/// Inverse of [`mv_to_aic_coordinate_transform`].
///
/// Also translates coordinates so that the lower bounds are zero, since the dot-vox format
/// does not support arbitrary lower bounds.
#[cfg(feature = "export")]
pub(crate) fn aic_to_mv_coordinate_transform(aic_bounds: GridAab) -> Gridgid {
    let rotated_size = MV_TO_AIC_ROTATION
        .inverse()
        .transform_size(aic_bounds.size());
    let mv_size = dot_vox::Size {
        x: rotated_size.width,
        y: rotated_size.height,
        z: rotated_size.depth,
    };
    mv_to_aic_coordinate_transform(mv_size).inverse()
        * Gridgid::from_translation(-aic_bounds.lower_bounds().to_vector())
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::math::Cube;

    #[test]
    fn coordinate_transform() {
        let t = mv_to_aic_coordinate_transform(dot_vox::Size {
            x: 100,
            y: 200,
            z: 300,
        });

        assert_eq!(
            t.transform_cube(Cube::new(10, 20, 30)),
            Cube::new(10, 30, 179)
        );

        assert_eq!(
            t.inverse(),
            aic_to_mv_coordinate_transform(GridAab::from_lower_size([0, 0, 0], [100, 300, 200]))
        );
    }
}

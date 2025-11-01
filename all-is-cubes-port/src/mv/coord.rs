#![cfg_attr(
    not(all(feature = "import", feature = "export")),
    allow(dead_code, unused_imports)
)]

use all_is_cubes::math::{
    Face6, GridAab, GridCoordinate, GridRotation, GridSize, GridVector, Gridgid,
};

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
pub(crate) fn aic_to_mv_coordinate_transform(aic_bounds: GridAab) -> Gridgid {
    let rotated_size = MV_TO_AIC_ROTATION.inverse().transform_size(aic_bounds.size());
    let mv_size = dot_vox::Size {
        x: rotated_size.width,
        y: rotated_size.height,
        z: rotated_size.depth,
    };
    mv_to_aic_coordinate_transform(mv_size).inverse()
        * Gridgid::from_translation(-aic_bounds.lower_bounds().to_vector())
}

/// Convert size types, with [`MV_TO_AIC_ROTATION`] baked in.
pub(crate) fn mv_to_aic_size(size: dot_vox::Size) -> GridSize {
    GridSize::new(size.x, size.z, size.y)
}

// TODO: this name is now confusing vs. the other functions which are about a *fixed* rotation
pub(crate) fn mv_to_aic_rotation(rotation: dot_vox::Rotation) -> GridRotation {
    GridRotation::from_basis(rotation.to_cols_array_2d().map(|col| {
        let col = GridVector::from(col.map(|el| el as i32));
        Face6::try_from(col).unwrap()
    }))
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

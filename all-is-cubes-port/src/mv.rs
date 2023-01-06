//! Import of MagicaVoxel `.vox` files.

use all_is_cubes::block::Block;
use all_is_cubes::cgmath::{Point3, Vector3};
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{GridAab, GridCoordinate, GridMatrix, GridRotation, Rgb, Rgba};
use all_is_cubes::space::{LightPhysics, SetCubeError, Space};
use all_is_cubes::universe::{Name, Universe, UniverseIndex};
use all_is_cubes::util::YieldProgress;

pub(crate) async fn load_dot_vox(
    p: YieldProgress,
    bytes: &[u8],
) -> Result<Universe, DotVoxConversionError> {
    let dot_vox::DotVoxData {
        version,
        models,
        palette,
        materials,
    } = dot_vox::load_bytes(bytes).map_err(DotVoxConversionError::Parse)?;
    // TODO: have a better path for reporting this kind of info
    log::info!(
        "Loaded MagicaVoxel .vox format: version {}, {} models, {} ignored materials",
        version,
        models.len(),
        materials.len(),
    );
    p.progress(0.15).await;

    let palette = convert_dot_vox_palette(&palette);
    let p = p.finish_and_cut(0.3).await;

    let mut universe = Universe::new();

    let models_progress = p.split_evenly(models.len());
    for ((i, model), model_progress) in models.into_iter().enumerate().zip(models_progress) {
        let mut space = convert_dot_vox_model(&palette, model)?;
        space.fast_evaluate_light();

        let name = Name::from(&*format!("model_{i}"));
        let space_ref = universe
            .insert(name, space)
            .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?;

        if i == 0 {
            universe
                .insert("character".into(), Character::spawn_default(space_ref))
                .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?;
        }

        model_progress.progress(1.0).await;
    }

    Ok(universe)
}

fn convert_dot_vox_palette(palette: &[u32]) -> Vec<Block> {
    palette
        .iter()
        .enumerate()
        .map(|(index, rgba)| {
            // TODO: Which endianness / component ordering should we expect?
            Block::builder()
                .display_name(index.to_string())
                .color(Rgba::from_srgb8(rgba.to_le_bytes()))
                .build()
        })
        .collect()
}

/// TODO: Document and allow control over the metadata choices like spawn and physics.
fn convert_dot_vox_model(
    palette_blocks: &[Block],
    model: dot_vox::Model,
) -> Result<Space, DotVoxConversionError> {
    let transform = mv_to_aic_coordinate_transform(model.size);
    let bounds = GridAab::from_lower_size(
        [0, 0, 0],
        [
            model.size.x as i32,
            model.size.y as i32,
            model.size.z as i32,
        ],
    )
    .transform(transform)
    .expect("TODO: return error");

    let mut space = Space::builder(bounds)
        .spawn({
            let mut spawn = Spawn::looking_at_space(bounds, Vector3::new(-1., 1., 1.));
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .light_physics(LightPhysics::Rays {
            maximum_distance: bounds.y_range().len() as u16,
        })
        .sky_color(Rgb::ONE)
        .build();

    for v in model.voxels {
        let converted_cube: Point3<GridCoordinate> = Point3 {
            x: v.x,
            y: v.y,
            z: v.z,
        }
        .map(i32::from);
        let transformed_cube = transform.transform_cube(converted_cube);

        #[allow(clippy::unnecessary_lazy_evaluations)] // dubious positive
        let block = palette_blocks.get(v.i as usize).ok_or_else(|| {
            DotVoxConversionError::PaletteTooShort {
                len: palette_blocks.len(),
                index: v.i,
            }
        })?;

        space
            .set(transformed_cube, block)
            .map_err(DotVoxConversionError::SetCube)?;
    }

    Ok(space)
}

/// Note: This is not a well-designed error enum (yet)
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub(crate) enum DotVoxConversionError {
    #[error("{0}")]
    Parse(&'static str),
    #[error("palette of {len} colors too short to contain index {index}")]
    PaletteTooShort { len: usize, index: u8 },
    #[error("failed to place block")]
    SetCube(#[source] SetCubeError),
    #[error("unexpected error")]
    Unexpected(#[source] InGenError),
}

impl From<DotVoxConversionError> for InGenError {
    fn from(error: DotVoxConversionError) -> Self {
        InGenError::other(error)
    }
}

/// Coordinate transform which converts the coordinate system handedness and “up”
/// direction conventional for MagicaVoxel to the one conventional for All is Cubes.
///
/// The input size should be in the original MagicaVoxel coordinate system.
fn mv_to_aic_coordinate_transform(mv_size: dot_vox::Size) -> GridMatrix {
    // Coordinates are Z-up right-handed compared to our Y-up right-handed,
    // so swap Z into Y and invert Y as Z.
    // (This is not a `GridRotation::to_positive_octant_matrix()` because the `sizes` are
    // not necessarily equal.)
    GridMatrix::from_translation([0, 0, mv_size.y as i32]) * GridRotation::RXzY.to_rotation_matrix()
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::math::GridPoint;

    #[test]
    #[ignore]
    fn print_many_transforms() {
        let gbox = GridAab::from_lower_size([0, 0, 0], [2, 2, 2]);
        let transform = mv_to_aic_coordinate_transform(dot_vox::Size { x: 2, y: 2, z: 2 });
        for point in gbox.interior_iter() {
            let tmat = transform.transform_cube(point);

            println!("{point:?} -> {tmat:?}");
        }
        panic!(); // cause output to be displayed
    }

    #[test]
    fn coordinate_transform() {
        let t = mv_to_aic_coordinate_transform(dot_vox::Size {
            x: 100,
            y: 100,
            z: 100,
        });
        assert_eq!(
            t.transform_cube(GridPoint::new(10, 20, 30)),
            GridPoint::new(10, 30, 79)
        );
    }

    #[tokio::test]
    async fn invalid_file_error() {
        let error = load_dot_vox(YieldProgress::noop(), &[]).await.unwrap_err();
        assert!(
            matches!(
                error,
                DotVoxConversionError::Parse("Not a valid MagicaVoxel .vox file")
            ),
            "{error:?}"
        );
    }

    // TODO: add tests of loading valid files (we will need to create test data files)
}

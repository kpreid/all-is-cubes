//! Import and export of MagicaVoxel `.vox` files.

use all_is_cubes::block::{self, Block};
use all_is_cubes::cgmath::{EuclideanSpace as _, Point3, Transform as _, Vector3};
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{GridAab, GridCoordinate, GridMatrix, GridRotation, Rgb, Rgba};
use all_is_cubes::space::{LightPhysics, SetCubeError, Space};
use all_is_cubes::universe::{self, Name, Universe};
use all_is_cubes::util::{ConciseDebug, CustomFormat, YieldProgress};

use crate::{ExportError, ExportSet};

pub(crate) async fn load_dot_vox(
    p: YieldProgress,
    bytes: &[u8],
) -> Result<Universe, DotVoxConversionError> {
    dot_vox_data_to_universe(
        p,
        &dot_vox::load_bytes(bytes).map_err(DotVoxConversionError::Parse)?,
    )
    .await
}

pub(crate) async fn export_dot_vox(
    p: YieldProgress,
    source: ExportSet,
    mut destination: impl std::io::Write,
) -> Result<(), crate::ExportError> {
    let data: dot_vox::DotVoxData = export_to_dot_vox_data(p, source).await?;
    data.write_vox(&mut destination)?;
    Ok(())
}

pub(crate) async fn dot_vox_data_to_universe(
    p: YieldProgress,
    data: &dot_vox::DotVoxData,
) -> Result<Universe, DotVoxConversionError> {
    let dot_vox::DotVoxData {
        version,
        models,
        palette,
        materials,
        scenes,
        layers,
    } = data;
    // TODO: have a better path for reporting this kind of info
    log::info!(
        "Loaded MagicaVoxel .vox format: version {}, {} models, {} ignored materials, {} ignored scenes, {} ignored layers",
        version,
        models.len(),
        materials.len(),
        scenes.len(),
        layers.len(),
    );
    p.progress(0.15).await;

    let palette = dot_vox_palette_to_blocks(palette);
    let p = p.finish_and_cut(0.3).await;

    let mut universe = Universe::new();

    let models_progress = p.split_evenly(models.len());
    for ((i, model), model_progress) in models.iter().enumerate().zip(models_progress) {
        let mut space = dot_vox_model_to_space(&palette, model)?;
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

        model_progress.finish().await;
    }

    Ok(universe)
}

/// Create [`DotVoxData`] from a collection of [`Space`]s.
///
/// TODO: also support exporting [`BlockDef`]s.
///
/// TODO: report export flaws (space too big, too many blocks)
///
pub(crate) async fn export_to_dot_vox_data(
    p: YieldProgress,
    source: ExportSet,
) -> Result<dot_vox::DotVoxData, ExportError> {
    let ExportSet {
        block_defs,
        spaces: to_export,
    } = source;

    // If block def list is nonempty, fail.
    if let Some(first) = block_defs.get(0) {
        return Err(ExportError::NotRepresentable {
            name: Some(first.name()),
            reason: "Exporting BlockDefs to .vox is not yet supported".into(),
        });
    }

    let mut palette: Vec<dot_vox::Color> = Vec::new();
    let mut models: Vec<dot_vox::Model> = Vec::with_capacity(to_export.len());

    for (mut p, space_ref) in p.split_evenly(to_export.len()).zip(to_export.into_iter()) {
        p.set_label(format!("Exporting space {}", space_ref.name()));
        models.push(space_to_dot_vox_model(&space_ref, &mut palette)?);
        p.finish().await
    }

    Ok(dot_vox::DotVoxData {
        version: 150, // TODO: magic number taken from examples; may not be right
        models,
        palette,
        scenes: Vec::new(),
        layers: Vec::new(),
        materials: Vec::new(),
    })
}

fn dot_vox_palette_to_blocks(palette: &[dot_vox::Color]) -> Vec<Block> {
    palette
        .iter()
        .enumerate()
        .map(|(index, &dot_vox::Color { r, g, b, a })| {
            Block::builder()
                .display_name(index.to_string())
                .color(Rgba::from_srgb8([r, g, b, a]))
                .build()
        })
        .collect()
}
fn block_to_dot_vox_palette_entry(evaluated: &block::EvaluatedBlock) -> Option<dot_vox::Color> {
    // TODO: should we compare identity or color?
    if *evaluated == block::AIR_EVALUATED {
        None
    } else {
        let [r, g, b, a] = evaluated.color.to_srgb8();
        Some(dot_vox::Color { r, g, b, a })
    }
}

/// TODO: Document and allow control over the metadata choices like spawn and physics,
/// and the choice of coordinate transform.
fn dot_vox_model_to_space(
    palette_blocks: &[Block],
    model: &dot_vox::Model,
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

    for v in model.voxels.iter() {
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
fn space_to_dot_vox_model(
    space_ref: &universe::URef<Space>,
    palette: &mut Vec<dot_vox::Color>,
) -> Result<dot_vox::Model, ExportError> {
    let space = space_ref.read()?;
    let bounds = space.bounds();
    if bounds.size().x > 256 || bounds.size().y > 256 || bounds.size().z > 256 {
        return Err(ExportError::NotRepresentable {
            name: Some(space_ref.name()),
            reason: format!(
                "space of size {} is too large to export to .vox; must be 256 or less in each axis",
                bounds.size().custom_format(ConciseDebug)
            ),
        });
    }

    let transform = aic_to_mv_coordinate_transform(bounds);
    let block_index_to_palette_index: Vec<Option<u8>> = space
        .block_data()
        .iter()
        .map(|data| {
            if let Some(entry) = block_to_dot_vox_palette_entry(data.evaluated()) {
                if let Ok(index) = u8::try_from(palette.len()) {
                    palette.push(entry);
                    return Some(index);
                }
            }
            None
        })
        .collect();

    let mut voxels: Vec<dot_vox::Voxel> = Vec::new();
    for cube in bounds.interior_iter() {
        if let Some(i) =
            block_index_to_palette_index[usize::from(space.get_block_index(cube).unwrap())]
        {
            let transformed_cube = transform.transform_cube(cube);
            voxels.push(dot_vox::Voxel {
                // We previously checked that the size is not too big.
                x: transformed_cube.x as u8,
                y: transformed_cube.y as u8,
                z: transformed_cube.z as u8,
                i,
            });
        } else {
            // Else the cube is empty space and should not be exported explicitly.
        }
    }

    Ok(dot_vox::Model {
        size: {
            let Vector3 { x, y, z } = transform
                .transform_vector(space.bounds().size())
                .map(i32::abs) // vector rotation might make it negative
                .cast::<u32>()
                .unwrap(); // conversion from positive i32 to u32 cannot fail
            dot_vox::Size { x, y, z }
        },
        voxels,
    })
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

/// Inverse of [`mv_to_aic_coordinate_transform`].
///
/// Also translates coordinates so that the lower bounds are zero, since the dot-vox format
/// does not support arbitrary lower bounds.
fn aic_to_mv_coordinate_transform(aic_bounds: GridAab) -> GridMatrix {
    let aic_size = aic_bounds.size().cast::<u32>().expect("negative sizes");
    let mv_size = dot_vox::Size {
        // Note axis swap! We can't just delegate this to the matrix because the matrix doesn't exist.
        // (TODO: But we could delegate it to the GridRotation)
        x: aic_size.x,
        y: aic_size.z,
        z: aic_size.y,
    };
    mv_to_aic_coordinate_transform(mv_size)
        .inverse_transform()
        .unwrap()
        * GridMatrix::from_translation(-aic_bounds.lower_bounds().to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::BlockDef;
    use all_is_cubes::math::GridPoint;
    use all_is_cubes::raytracer::print_space;
    use all_is_cubes::universe::URef;
    use either::Either;

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
            y: 200,
            z: 300,
        });

        assert_eq!(
            t.transform_cube(GridPoint::new(10, 20, 30)),
            GridPoint::new(10, 30, 179)
        );

        assert_eq!(
            t.inverse_transform().unwrap(),
            aic_to_mv_coordinate_transform(GridAab::from_lower_size([0, 0, 0], [100, 300, 200]))
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

    async fn roundtrip(
        export_universe: &Universe,
    ) -> Result<Universe, Either<ExportError, DotVoxConversionError>> {
        // TODO: also roundtrip through bytes, for maximum rigor
        let data = export_to_dot_vox_data(
            YieldProgress::noop(),
            ExportSet::all_of_universe(export_universe),
        )
        .await
        .map_err(Either::Left)?;
        dot_vox_data_to_universe(YieldProgress::noop(), &data)
            .await
            .map_err(Either::Right)
    }

    #[tokio::test]
    async fn export_import_space() {
        // Data expected to be preserved:
        // Irregular and negative dimensions to check that the coordinate transform worked.
        let bounds = GridAab::from_lower_size([-20, -30, -40], [1, 2, 3]);
        let block1 = Block::builder()
            .color(Rgba::WHITE)
            .display_name("a")
            .build();
        let block2 = Block::builder()
            .color(Rgba::WHITE)
            .display_name("b")
            .build();

        // Construct universe for export.
        let mut export_universe = Universe::new();
        let mut export_space = Space::builder(bounds).build();
        export_space.set([-20, -30 + 1, -40 + 2], &block1).unwrap();
        export_space.set([-20, -30 + 1, -40], &block2).unwrap();
        let export_space = export_universe.insert_anonymous(export_space);
        print_space(&export_space.read().unwrap(), [1., 1., 1.]);

        // Export and import.
        let import_universe = roundtrip(&export_universe).await.expect("roundtrip failed");
        // then find the one space in it.
        let import_space: URef<Space> = import_universe.iter_by_type().next().unwrap().1;

        print_space(&import_space.read().unwrap(), [1., 1., 1.]);

        // Compare.
        // (The lower bounds will be zero.)
        let s = import_space.read().unwrap();
        assert_eq!(
            s.bounds(),
            GridAab::from_lower_size([0, 0, 0], bounds.size())
        );
        // Block metadata is not preserved. The display name is filled with the palette index.
        assert_eq!(
            s[[0, 1, 2]],
            Block::builder()
                .color(block1.color())
                .display_name("0")
                .build()
        );
        assert_eq!(
            s[[0, 1, 0]],
            Block::builder()
                .color(block1.color())
                .display_name("1")
                .build()
        );
        // TODO: make more assertions about the data?
    }

    /// [`dot_vox`] only supports coordinates from 0-255
    #[tokio::test]
    async fn export_too_large_space() {
        let mut universe = Universe::new();
        let space = universe.insert_anonymous(
            Space::builder(GridAab::from_lower_size([0, 0, 0], [257, 1, 1])).build(),
        );

        let error =
            export_to_dot_vox_data(YieldProgress::noop(), ExportSet::from_spaces(vec![space]))
                .await
                .unwrap_err();
        assert!(matches!(error, ExportError::NotRepresentable { .. }));
    }

    #[tokio::test]
    async fn export_block_def() {
        let mut universe = Universe::new();
        universe
            .insert("x".into(), BlockDef::new(block::AIR))
            .unwrap();

        let error =
            export_to_dot_vox_data(YieldProgress::noop(), ExportSet::all_of_universe(&universe))
                .await
                .unwrap_err();
        assert!(matches!(
            error,
            ExportError::NotRepresentable {
                name: Some(name),
                ..
            }
         if name == "x".into()));
    }

    // TODO: add tests of loading valid files (we will need to create test data files)
}

//! Import and export of MagicaVoxel `.vox` files.

#![cfg_attr(
    not(all(feature = "import", feature = "export")),
    allow(unused_imports)
)]

use all_is_cubes::block::{self, Block};
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::euclid::{Point3D, vec3};
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{
    Cube, GridAab, GridCoordinate, GridRotation, GridVector, Gridgid, Rgb, Rgba,
};
use all_is_cubes::space::{LightPhysics, SetCubeError, Space};
use all_is_cubes::universe::{self, Name, ReadTicket, Universe};
use all_is_cubes::util::{ConciseDebug, Refmt, YieldProgress};

#[cfg(feature = "export")]
use crate::{ExportError, ExportSet, Format};

#[cfg(feature = "import")]
pub(crate) async fn load_dot_vox(
    p: YieldProgress,
    bytes: &[u8],
) -> Result<Box<Universe>, DotVoxConversionError> {
    dot_vox_data_to_universe(
        p,
        &dot_vox::load_bytes(bytes).map_err(DotVoxConversionError::Parse)?,
    )
    .await
}

#[cfg(feature = "import")]
async fn dot_vox_data_to_universe(
    p: YieldProgress,
    data: &dot_vox::DotVoxData,
) -> Result<Box<Universe>, DotVoxConversionError> {
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

    let mut universe = Box::new(Universe::new());

    let models_progress = p.split_evenly(models.len());
    for ((i, model), model_progress) in models.iter().enumerate().zip(models_progress) {
        let mut space = dot_vox_model_to_space(&palette, model)?;
        space.fast_evaluate_light();

        let name = Name::from(format!("model_{i}"));
        let space_handle = universe
            .insert(name, space)
            .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?;

        if i == 0 {
            universe
                .insert(
                    "character".into(),
                    Character::spawn_default(universe.read_ticket(), space_handle),
                )
                .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?;
        }

        model_progress.finish().await;
    }

    Ok(universe)
}

/// Read the given [`ExportSet`] to produce in-memory [`DotVoxData`].
///
/// Use [`DotVoxData::write_vox()`] to produce the actual bytes from this.
///
/// TODO: also support exporting [`BlockDef`]s.
///
/// TODO: report export flaws (space too big, too many blocks)
///
#[cfg(feature = "export")]
pub(crate) async fn export_to_dot_vox_data(
    p: YieldProgress,
    read_ticket: ReadTicket<'_>,
    mut source: ExportSet,
) -> Result<dot_vox::DotVoxData, ExportError> {
    let to_export = source.contents.extract_type::<Space>();
    source.reject_unsupported(Format::DotVox)?;

    let mut palette: Vec<dot_vox::Color> = Vec::new();
    let mut models: Vec<dot_vox::Model> = Vec::with_capacity(to_export.len());

    #[expect(clippy::shadow_unrelated)]
    for (mut p, space_handle) in p.split_evenly(to_export.len()).zip(to_export) {
        p.set_label(format!("Exporting space {}", space_handle.name()));
        models.push(space_to_dot_vox_model(
            read_ticket,
            &space_handle,
            &mut palette,
        )?);
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

#[cfg(feature = "import")]
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
#[cfg(feature = "export")]
fn block_to_dot_vox_palette_entry(evaluated: &block::EvaluatedBlock) -> Option<dot_vox::Color> {
    // TODO: should we compare identity or color?
    if *evaluated == block::AIR_EVALUATED {
        None
    } else {
        let [r, g, b, a] = evaluated.color().to_srgb8();
        Some(dot_vox::Color { r, g, b, a })
    }
}

/// TODO: Document and allow control over the metadata choices like spawn and physics,
/// and the choice of coordinate transform.
#[cfg(feature = "import")]
fn dot_vox_model_to_space(
    palette_blocks: &[Block],
    model: &dot_vox::Model,
) -> Result<Space, DotVoxConversionError> {
    let transform = mv_to_aic_coordinate_transform(model.size);
    let bounds =
        GridAab::from_lower_size([0, 0, 0], vec3(model.size.x, model.size.y, model.size.z))
            .transform(transform)
            .expect("TODO: return error");

    let mut space = Space::builder(bounds)
        .spawn({
            let mut spawn = Spawn::looking_at_space(bounds, vec3(-1., 1., 1.));
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .light_physics(LightPhysics::Rays {
            maximum_distance: u8::try_from(bounds.y_range().len()).unwrap_or(u8::MAX),
        })
        .sky_color(Rgb::ONE)
        .build();

    space.mutate(ReadTicket::stub(), |m| {
        for v in model.voxels.iter() {
            let converted_cube: Cube = Cube::from(Point3D::new(v.x, v.y, v.z).map(i32::from));
            let transformed_cube = transform.transform_cube(converted_cube);

            let block = palette_blocks.get(v.i as usize).ok_or_else(|| {
                DotVoxConversionError::PaletteTooShort {
                    len: palette_blocks.len(),
                    index: v.i,
                }
            })?;

            m.set(transformed_cube, block)
                .map_err(DotVoxConversionError::SetCube)?;
        }
        Ok::<(), DotVoxConversionError>(())
    })?;

    Ok(space)
}
#[cfg(feature = "export")]
fn space_to_dot_vox_model(
    read_ticket: ReadTicket<'_>,
    space_handle: &universe::Handle<Space>,
    palette: &mut Vec<dot_vox::Color>,
) -> Result<dot_vox::Model, ExportError> {
    let space = space_handle.read(read_ticket)?;
    let bounds = space.bounds();
    if bounds.size().width > 256 || bounds.size().height > 256 || bounds.size().depth > 256 {
        return Err(ExportError::NotRepresentable {
            format: Format::DotVox,
            name: Some(space_handle.name()),
            reason: format!(
                "space of size {} is too large to export to .vox; must be 256 or less in each axis",
                bounds.size().refmt(&ConciseDebug)
            ),
        });
    }

    let transform = aic_to_mv_coordinate_transform(bounds);
    let block_index_to_palette_index: Vec<Option<u8>> = space
        .block_data()
        .iter()
        .map(|data| {
            if let Some(entry) = block_to_dot_vox_palette_entry(data.evaluated())
                && let Ok(index) = u8::try_from(palette.len())
            {
                palette.push(entry);
                Some(index)
            } else {
                None
            }
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
            let size = transform.rotation.transform_size(space.bounds().size());
            dot_vox::Size {
                x: size.width,
                y: size.height,
                z: size.depth,
            }
        },
        voxels,
    })
}

/// Note: This is not a well-designed error enum (yet)
#[cfg(feature = "import")]
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

#[cfg(feature = "import")]
impl From<DotVoxConversionError> for InGenError {
    fn from(error: DotVoxConversionError) -> Self {
        InGenError::other(error)
    }
}

const MV_TO_AIC_ROTATION: GridRotation = GridRotation::RXzY;

/// Coordinate transform which converts the coordinate system handedness and “up”
/// direction conventional for MagicaVoxel to the one conventional for All is Cubes.
///
/// The input size should be in the original MagicaVoxel coordinate system.
fn mv_to_aic_coordinate_transform(mv_size: dot_vox::Size) -> Gridgid {
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
fn aic_to_mv_coordinate_transform(aic_bounds: GridAab) -> Gridgid {
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

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::BlockDef;
    use all_is_cubes::universe::Handle;
    use all_is_cubes::util::yield_progress_for_testing;
    use all_is_cubes_render::raytracer::print_space;
    use either::Either;

    #[test]
    #[ignore = "debug tool, not a test"]
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
            t.transform_cube(Cube::new(10, 20, 30)),
            Cube::new(10, 30, 179)
        );

        assert_eq!(
            t.inverse(),
            aic_to_mv_coordinate_transform(GridAab::from_lower_size([0, 0, 0], [100, 300, 200]))
        );
    }

    #[cfg(feature = "import")]
    #[tokio::test]
    async fn invalid_file_error() {
        let error = load_dot_vox(yield_progress_for_testing(), &[])
            .await
            .unwrap_err();
        assert!(
            matches!(
                error,
                DotVoxConversionError::Parse("Not a valid MagicaVoxel .vox file")
            ),
            "{error:?}"
        );
    }

    #[cfg(all(feature = "export", feature = "import"))]
    async fn roundtrip(
        export_universe: &Universe,
    ) -> Result<Box<Universe>, Either<ExportError, DotVoxConversionError>> {
        // TODO: also roundtrip through bytes, for maximum rigor
        let data = export_to_dot_vox_data(
            yield_progress_for_testing(),
            export_universe.read_ticket(),
            ExportSet::all_of_universe(export_universe),
        )
        .await
        .map_err(Either::Left)?;
        dot_vox_data_to_universe(yield_progress_for_testing(), &data)
            .await
            .map_err(Either::Right)
    }

    #[cfg(all(feature = "export", feature = "import"))]
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
        let export_space = Space::builder(bounds)
            .build_and_mutate(|m| {
                m.set([-20, -30 + 1, -40 + 2], &block1)?;
                m.set([-20, -30 + 1, -40], &block2)?;
                Ok(())
            })
            .unwrap();
        let export_space = export_universe.insert_anonymous(export_space);
        print_space(
            &export_space.read(export_universe.read_ticket()).unwrap(),
            [1., 1., 1.],
        );

        // Export and import.
        let import_universe = roundtrip(&export_universe).await.expect("roundtrip failed");
        // then find the one space in it.
        let import_space: Handle<Space> = import_universe.iter_by_type().next().unwrap().1;

        print_space(
            &import_space.read(import_universe.read_ticket()).unwrap(),
            [1., 1., 1.],
        );

        // Compare.
        // (The lower bounds will be zero.)
        let s = import_space.read(import_universe.read_ticket()).unwrap();
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
    #[cfg(feature = "export")]
    #[tokio::test]
    async fn export_too_large_space() {
        let mut universe = Universe::new();
        let space = universe.insert_anonymous(
            Space::builder(GridAab::from_lower_size([0, 0, 0], [257, 1, 1])).build(),
        );

        let error = export_to_dot_vox_data(
            yield_progress_for_testing(),
            universe.read_ticket(),
            ExportSet::from_spaces(vec![space]),
        )
        .await
        .unwrap_err();
        assert!(matches!(error, ExportError::NotRepresentable { .. }));
    }

    #[cfg(feature = "export")]
    #[tokio::test]
    async fn export_block_def() {
        let mut universe = Universe::new();
        universe
            .insert(
                "x".into(),
                BlockDef::new(universe.read_ticket(), block::AIR),
            )
            .unwrap();

        let error = export_to_dot_vox_data(
            yield_progress_for_testing(),
            universe.read_ticket(),
            ExportSet::all_of_universe(&universe),
        )
        .await
        .unwrap_err();
        assert!(matches!(
            error,
            ExportError::MemberTypeNotRepresentable {
                name,
                ..
            }
         if name == "x".into()));
    }

    // TODO: add tests of loading valid files (we will need to create test data files)
}

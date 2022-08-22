use all_is_cubes::block::Block;
use all_is_cubes::cgmath::Vector3;
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{GridAab, GridPoint, Rgb, Rgba};
use all_is_cubes::space::{LightPhysics, SetCubeError, Space};
use all_is_cubes::universe::{Name, Universe, UniverseIndex};
use all_is_cubes::util::YieldProgress;
use all_is_cubes_content::free_editing_starter_inventory;

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

        let name = Name::from(&*format!("model_{}", i));
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
    let extent = GridAab::from_lower_size(
        [0, 0, 0],
        [
            model.size.x as i32,
            model.size.z as i32,
            model.size.y as i32,
        ],
    );
    let mut space = Space::builder(extent)
        .spawn({
            let mut spawn = Spawn::looking_at_space(extent, Vector3::new(-1., 1., 1.));
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .light_physics(LightPhysics::Rays {
            maximum_distance: extent.y_range().len() as u16,
        })
        .sky_color(Rgb::ONE)
        .build();
    for v in model.voxels {
        // Coordinates are Z-up right-handed compared to our Y-up right-handed,
        // so swap Z into Y and invert Y as Z.
        let cube = GridPoint::new(
            i32::from(v.x),
            i32::from(v.z),
            (model.size.y as i32) - 1 - i32::from(v.y),
        );
        #[allow(clippy::unnecessary_lazy_evaluations)] // dubious positive
        let block = palette_blocks.get(v.i as usize).ok_or_else(|| {
            DotVoxConversionError::PaletteTooShort {
                len: palette_blocks.len(),
                index: v.i,
            }
        })?;
        space
            .set(cube, block)
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

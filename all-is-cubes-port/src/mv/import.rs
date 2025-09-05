use std::sync::Arc;

use all_is_cubes::arcstr;
use all_is_cubes::block::{self, Block};
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::GridAab;
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, Name, ReadTicket, Universe};
use all_is_cubes::util::YieldProgress;

use crate::mv;
use crate::mv::error::DotVoxConversionError;

// TODO: change signature so we can drop the bytes after they are parsed
pub(crate) async fn load_dot_vox(
    mut progress: YieldProgress,
    bytes: &[u8],
) -> Result<Box<Universe>, DotVoxConversionError> {
    let parse_bytes_progress = progress.start_and_cut(0.1, "Parsing .vox file").await;
    let data = dot_vox::load_bytes(bytes).map_err(DotVoxConversionError::Parse)?;
    parse_bytes_progress.finish().await;

    dot_vox_data_to_universe(progress, Arc::new(data)).await
}

pub(crate) async fn dot_vox_data_to_universe(
    mut progress: YieldProgress,
    data: Arc<dot_vox::DotVoxData>,
) -> Result<Box<Universe>, DotVoxConversionError> {
    let dot_vox::DotVoxData {
        version,
        models,
        palette,
        materials,
        scenes,
        layers,
    } = &*data;
    // Use Yoke to make owned field pointers from the Arc
    let models_yoked =
        yoke::Yoke::<&[dot_vox::Model], _>::attach_to_cart(data.clone(), |data| &data.models);

    // TODO: have a better path for reporting this kind of info
    log::info!(
        "Loaded MagicaVoxel .vox format: version {}, \
        {} models, {} ignored materials, {} ignored scene nodes, {} ignored layers",
        version,
        models_yoked.get().len(),
        materials.len(),
        scenes.len(),
        layers.len(),
    );

    // Convert the palette to `Block`s.
    let palette_progress = {
        progress.start_and_cut(
            0.1,
            format_args!("Importing {} .vox palette entries", palette.len()),
        )
    }
    .await;
    let palette: Arc<[Block]> = mv::palette::dot_vox_palette_to_blocks(palette);
    palette_progress.finish().await;

    // Universe is a large struct and boxing it promptly helps us not have large stack usage
    // and large futures.
    let mut universe = Box::new(Universe::new());

    // Convert the models to `Space`s.
    let models_progress = progress
        .start_and_cut(0.9, "Importing {} .vox models")
        .await;
    let model_count = models.len();
    let model_spaces: Vec<Result<Space, DotVoxConversionError>> = crate::util::maybe_parallelize(
        models_progress,
        0..model_count,
        move |model_index| format!("Importing model {model_index}/{model_count}"),
        move |model_index| {
            let mut space = mv::model::to_space(&palette, &models_yoked.get()[model_index])?;
            space.fast_evaluate_light();
            Ok(space)
        },
    )
    .await;

    progress.set_label("Storing models");
    progress.progress(0.0).await;
    let mut model_space_handles: Vec<Handle<Space>> = Vec::new();
    for (i, space_result) in model_spaces.into_iter().enumerate() {
        let space = space_result?;
        let name = Name::from(format!("model_{i}"));
        let space_handle = universe
            .insert(name, space)
            .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?;
        model_space_handles.push(space_handle);
    }

    // If there is one model space, put the character in it.
    // If there is more than one, build blocks from all the models.
    // TODO: Import the actual scene as blocks.
    let viewed_space_handle = if let [space_handle] = model_space_handles.as_slice() {
        space_handle.clone()
    } else {
        progress.set_label("Creating model preview");
        progress.progress(0.5).await;
        universe
            .insert(
                "model_previews".into(),
                view_all_models_as_blocks(universe.read_ticket(), models, model_space_handles)?,
            )
            .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?
    };

    universe
        .insert(
            "character".into(),
            Character::spawn_default(universe.read_ticket(), viewed_space_handle),
        )
        .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?;

    progress.finish().await;

    Ok(universe)
}

/// Construct a [`Space`] where all models in the file can be seen as independent blocks.
#[cfg(feature = "import")]
fn view_all_models_as_blocks(
    read_ticket: ReadTicket<'_>,
    models: &[dot_vox::Model],
    space_handles: Vec<Handle<Space>>,
) -> Result<Space, DotVoxConversionError> {
    let row_length = space_handles.len().isqrt();
    let row_length_g = i32::try_from(row_length).unwrap();
    let bounds = GridAab::from_lower_size(
        [0, 0, 0],
        [
            (row_length * 2 + 1) as u32,
            1,
            (space_handles.len().div_ceil(row_length) * 2 + 1) as u32,
        ],
    );
    Space::builder(bounds)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_eye_position([-1.0, 2.0, -1.0]);
            spawn.set_look_direction([1.0, -0.5, 1.0]);
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .read_ticket(read_ticket)
        .build_and_mutate(|m| {
            for ((i, space), model) in (0i32..).zip(space_handles).zip(models) {
                let max_size = model.size.x.max(model.size.y).max(model.size.z);
                let mut resolution = block::Resolution::R1;
                while u32::from(resolution) < max_size
                    && let Some(d) = resolution.double()
                    && d <= block::Resolution::R64
                {
                    resolution = d;
                }

                m.set(
                    [
                        i.rem_euclid(row_length_g) * 2,
                        0,
                        i.div_euclid(row_length_g) * 2,
                    ],
                    Block::builder()
                        .voxels_handle(resolution, space)
                        .display_name(arcstr::format!("Model #{i}"))
                        .build(),
                )?;
            }
            Ok(())
        })
        .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))
}

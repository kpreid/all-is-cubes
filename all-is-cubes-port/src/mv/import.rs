use std::sync::Arc;

use all_is_cubes::arcstr;
use all_is_cubes::block::{self, Block, Resolution};
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::euclid::vec3;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{GridAab, GridPoint, GridVector};
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

/// How to treat the models and scene nodes being imported.
#[derive(Clone, Copy, Debug)]
enum ImportMode {
    /// There is only one model and it shall become the [`Space`] where the character is.
    OneModelAsSpace,
    /// All models shall be viewed as a collection of blocks.
    /// The individual blocks do not get light calculation.
    ManyModelsAsBlocks,
    /// There is a scene, which shall be imported as a [`Space`] containing the models as blocks.
    Scene,
}
impl ImportMode {
    /// When importing models, whether to configure their [`Space`]s as appropriate for being
    /// blocks rather than being the space the character occupies.
    fn treat_models_as_blocks(self) -> bool {
        match self {
            ImportMode::OneModelAsSpace => false,
            ImportMode::ManyModelsAsBlocks => true,
            ImportMode::Scene => true,
        }
    }
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

    // TODO: this should be a user-selectable option, not solely automatic
    let mode = match (scenes.len(), models.len()) {
        (1.., _) => ImportMode::Scene,
        (0, 1) => ImportMode::OneModelAsSpace,
        (0, 2..) => ImportMode::ManyModelsAsBlocks,
        (0, 0) => {
            return Err(DotVoxConversionError::FileEmpty);
        }
    };

    let model_phase_cost_relative_to_scene_phase = match mode {
        ImportMode::OneModelAsSpace => 1.0,
        ImportMode::ManyModelsAsBlocks => 0.5, // even split assumed
        ImportMode::Scene => {
            (models.len() as f32) / (models.len() as f32).mul_add(0.5, scenes.len().max(1) as f32)
        }
    };

    // TODO: have a better path for reporting this kind of info about the results of the import
    log::info!(
        "Loaded MagicaVoxel .vox format: version {version}, \
        {models} models, {materials} materials, {scenes} scene nodes, {layers} ignored layers. \
        Selected import method: {mode:?}.",
        models = models.len(),
        materials = materials.len(),
        scenes = scenes.len(),
        layers = layers.len(),
    );

    // Convert the palette to `Block`s.
    let palette_progress = {
        progress.start_and_cut(
            0.1,
            format_args!("Importing {} .vox palette entries", palette.len()),
        )
    }
    .await;
    let palette: Arc<[Block]> = mv::palette::dot_vox_palette_to_blocks(palette, materials);
    palette_progress.finish().await;

    let mut universe = Universe::new();

    // Convert the models to `Space`s.
    let models_progress = progress
        .start_and_cut(
            model_phase_cost_relative_to_scene_phase,
            "Importing {} .vox models",
        )
        .await;
    let model_count = models.len();
    let model_spaces: Vec<Result<Space, DotVoxConversionError>> = crate::util::maybe_parallelize(
        models_progress,
        0..model_count,
        move |model_index| format!("Importing model {model_index}/{model_count}"),
        move |model_index| {
            let mut space = mv::model::to_space(
                &palette,
                &models_yoked.get()[model_index],
                mode.treat_models_as_blocks(),
            )?;
            space.mutate(ReadTicket::stub(), |m| m.fast_evaluate_light());
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
    let progress = progress.finish_and_cut(0.1).await;

    let [mut view_progress, final_progress] = progress.split(0.99);
    let viewed_space_handle = match mode {
        ImportMode::OneModelAsSpace => {
            let [space_handle] = model_space_handles.as_slice() else {
                todo!("can't happen now, but should have an error")
            };
            view_progress.finish().await;
            space_handle.clone()
        }
        ImportMode::Scene => universe
            .insert(
                "scene".into(),
                mv::scene::scene_to_space(
                    view_progress,
                    data,
                    universe.read_ticket(),
                    model_space_handles,
                )
                .await?,
            )
            .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?,
        ImportMode::ManyModelsAsBlocks => {
            view_progress.set_label("Creating model preview");
            view_progress.progress(0.0).await;
            let space =
                view_all_models_as_blocks(universe.read_ticket(), models, model_space_handles)?;
            view_progress.finish().await;
            universe
                .insert("model_previews".into(), space)
                .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?
        }
    };

    universe
        .insert(
            "character".into(),
            Character::spawn_default(universe.read_ticket(), viewed_space_handle)
                .map_err(|e| DotVoxConversionError::Unexpected(InGenError::other(e)))?,
        )
        .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))?;

    final_progress.finish().await;

    Ok(universe)
}

/// Construct a [`Space`] where all models in the file can be seen as independent blocks.
fn view_all_models_as_blocks(
    read_ticket: ReadTicket<'_>,
    models: &[dot_vox::Model],
    space_handles: Vec<Handle<Space>>,
) -> Result<Space, DotVoxConversionError> {
    assert!(!models.is_empty(), "zero models not supported");

    let row_length = space_handles.len().isqrt();
    let row_length_g =
        i32::try_from(row_length).map_err(|_| DotVoxConversionError::TransformOverflow)?;
    let maximum_size_of_model_in_blocks = 4u32;
    let spacing_between_models = 4u8;
    let bounds = GridAab::from_lower_size(
        GridPoint::splat(-maximum_size_of_model_in_blocks.cast_signed()),
        [
            (row_length * usize::from(spacing_between_models)) as u32
                + maximum_size_of_model_in_blocks * 2,
            maximum_size_of_model_in_blocks * 2,
            (space_handles.len().div_ceil(row_length) * usize::from(spacing_between_models)) as u32
                + maximum_size_of_model_in_blocks * 2,
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
                let mut resolution = Resolution::R1;
                while u32::from(resolution) < max_size
                    && let Some(d) = resolution.double()
                    && d <= Resolution::R64
                {
                    resolution = d;
                }

                for (rel_cube, block) in mv::scene::model_space_to_blocks(
                    model,
                    space,
                    {
                        let mut a = block::BlockAttributes::default();
                        a.display_name = arcstr::format!("Model #{i}");
                        a
                    },
                    resolution,
                    GridVector::zero(),
                ) {
                    m.set(
                        rel_cube
                            + vec3(
                                i.rem_euclid(row_length_g) * i32::from(spacing_between_models),
                                0,
                                i.div_euclid(row_length_g) * i32::from(spacing_between_models),
                            ),
                        block,
                    )?;
                }
            }
            Ok(())
        })
        .map_err(|e| DotVoxConversionError::Unexpected(InGenError::from(e)))
}

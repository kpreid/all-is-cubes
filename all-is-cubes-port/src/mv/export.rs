use std::collections::HashMap;

use itertools::Itertools as _;

use all_is_cubes::block::Resolution;
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::{Handle, ReadTicket};
use all_is_cubes::util::YieldProgress;

use crate::{ExportError, ExportSet, Format, mv};

/// Read the given [`ExportSet`] to produce in-memory [`DotVoxData`].
///
/// Use [`DotVoxData::write_vox()`] to produce the actual bytes from this.
///
/// TODO: report export flaws (space too big, too many blocks)
///
pub(crate) async fn export_to_dot_vox_data(
    mut progress: YieldProgress,
    read_ticket: ReadTicket<'_>,
    mut source: ExportSet,
) -> Result<dot_vox::DotVoxData, ExportError> {
    let spaces_to_export = source.contents.extract_type::<Space>();
    let blocks_to_export = source.contents.extract_type::<all_is_cubes::block::BlockDef>();
    source.reject_unsupported(Format::DotVox)?;

    // TODO: this value is not used except in logging, but will be used with export_space_as_scene()
    let maximum_resolution_in_all_spaces: (Option<&Handle<Space>>, Resolution) = spaces_to_export
        .iter()
        .map(
            |handle| -> Result<(Option<&Handle<Space>>, Resolution), ExportError> {
                Ok((
                    Some(handle),
                    maximum_resolution_of_all_blocks(&handle.read(read_ticket)?),
                ))
            },
        )
        .process_results(|resolutions| resolutions.max_by_key(|&(_, resolution)| resolution))?
        .unwrap_or((None, Resolution::R1));

    log::info!(
        ".vox: given {b} blocks and {s} spaces to export; \
        maximum resolution in spaces is {resolution}",
        b = blocks_to_export.len(),
        s = spaces_to_export.len(),
        resolution = maximum_resolution_in_all_spaces.1,
    );

    let mut palette: Vec<dot_vox::Color> = Vec::new();
    let mut models: Vec<dot_vox::Model> =
        Vec::with_capacity(spaces_to_export.len().saturating_add(blocks_to_export.len()));
    #[expect(clippy::shadow_unrelated)]
    for (mut progress, space_handle) in progress
        .start_and_cut(0.5, "")
        .await
        .split_evenly(spaces_to_export.len())
        .zip(spaces_to_export)
    {
        progress.set_label(format!("Exporting space {}", space_handle.name()));
        models.push(mv::model::from_space(
            read_ticket,
            &space_handle,
            &mut palette,
        )?);
        progress.finish().await
    }

    #[expect(clippy::shadow_unrelated)]
    for (mut progress, block_def_handle) in
        progress.split_evenly(blocks_to_export.len()).zip(blocks_to_export)
    {
        progress.set_label(format!("Exporting block {}", block_def_handle.name()));
        models.push(mv::model::from_block(
            block_def_handle.name(),
            &block_def_handle
                .read(read_ticket)?
                .evaluate()
                .map_err(|error| ExportError::Eval {
                    name: block_def_handle.name(),
                    error,
                })?,
            &mut palette,
        )?);
        progress.finish().await
    }

    // TODO: When more than one model is exported, create a scene including all the models

    Ok(dot_vox::DotVoxData {
        version: 150, // TODO: magic number taken from examples; may not be right
        models,
        palette,
        scenes: Vec::new(),
        layers: Vec::new(),
        materials: Vec::new(),
    })
}

/// Rather than exporting a space as a voxel model, this exports a space as a scene containing
/// a model per block.
#[expect(dead_code, reason = "TODO: This is not functional and not used yet.")]
async fn export_space_as_scene(
    progress: YieldProgress,
    read_ticket: ReadTicket<'_>,
    space_handle: &Handle<Space>,
    resolution: Resolution,
    models_out: &mut Vec<dot_vox::Model>,
    palette_out: &mut Vec<dot_vox::Color>,
    scene_out: &mut Vec<dot_vox::SceneNode>,
) -> Result<(), ExportError> {
    let space = space_handle.read(read_ticket)?;
    let [convert_blocks_progress, convert_space_progress] = progress.split(0.5);

    let mut block_index_to_model_index: HashMap<space::BlockIndex, u32> = HashMap::new();
    for ((mut p, block_data), block_index) in convert_blocks_progress
        .split_evenly(space.block_data().len())
        .zip(space.block_data())
        .zip(0..=space::BlockIndex::MAX)
    {
        p.set_label(format!("Exporting block model {block_index}"));
        p.progress(0.0).await;

        block_index_to_model_index.insert(block_index, u32::try_from(models_out.len()).unwrap());
        models_out.push(mv::model::from_block(
            space_handle.name(), // TODO: use block's Indirect handle name if it has one.
            block_data.evaluated(),
            palette_out,
        )?);

        p.finish().await;
    }

    // the root must exist at index 0
    scene_out.push(dot_vox::SceneNode::Group {
        attributes: dot_vox::Dict::default(),
        children: Vec::new(),
    });

    for cube in space.bounds().interior_iter() {
        let block_index = space.get_block_index(cube).unwrap();
        if let Some(&model_id) = block_index_to_model_index.get(&block_index) {
            let index_of_transform_node = u32::try_from(scene_out.len()).unwrap();

            let translation = cube.lower_bounds() * i32::from(resolution);

            scene_out.push(dot_vox::SceneNode::Transform {
                attributes: dot_vox::Dict::default(),
                frames: vec![dot_vox::Frame {
                    attributes: dot_vox::Dict::from([
                        (
                            String::from("_t"),
                            format!("{} {} {}", translation.x, translation.y, translation.z),
                        ),
                        (
                            String::from("_r"),
                            String::from("2"), // identity matrix -- TODO: confirm this is the right value
                        ),
                    ]),
                }],
                child: index_of_transform_node + 1, // shape node
                layer_id: 0,
            });
            scene_out.push(dot_vox::SceneNode::Shape {
                attributes: dot_vox::Dict::default(),
                models: vec![dot_vox::ShapeModel {
                    model_id,
                    attributes: dot_vox::Dict::new(),
                }],
            });

            let dot_vox::SceneNode::Group {
                children: ref mut root_children,
                ..
            } = scene_out[0]
            else {
                unreachable!()
            };
            root_children.push(index_of_transform_node); // transform node
        }
    }
    convert_space_progress.finish().await;

    Ok(())
}

fn maximum_resolution_of_all_blocks(space: &space::Read<'_>) -> Resolution {
    space
        .block_data()
        .iter()
        .map(|bd| bd.evaluated().resolution())
        .max()
        .unwrap_or(Resolution::R1)
}

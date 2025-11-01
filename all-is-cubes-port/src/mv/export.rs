use all_is_cubes::space::Space;
use all_is_cubes::universe::ReadTicket;
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
            &block_def_handle.read(read_ticket)?.evaluate(read_ticket).map_err(|error| {
                ExportError::Eval {
                    name: block_def_handle.name(),
                    error,
                }
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

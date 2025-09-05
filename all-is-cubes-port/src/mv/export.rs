use all_is_cubes::space::Space;
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::util::YieldProgress;

use crate::{ExportError, ExportSet, Format, mv};

/// Read the given [`ExportSet`] to produce in-memory [`DotVoxData`].
///
/// Use [`DotVoxData::write_vox()`] to produce the actual bytes from this.
///
/// TODO: also support exporting [`BlockDef`]s.
///
/// TODO: report export flaws (space too big, too many blocks)
///
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
        models.push(mv::model::from_space(
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

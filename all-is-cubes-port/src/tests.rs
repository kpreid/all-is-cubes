use std::error::Error as _;
use std::sync::Arc;

use all_is_cubes::block;
use all_is_cubes::util::{assert_send_sync, yield_progress_for_testing};

use crate::file::NonDiskFile;
use crate::{
    load_universe_from_file, BlockDef, ExportError, ExportSet, ImportError, Path, PathBuf, Universe,
};

#[test]
fn errors_are_send_sync() {
    assert_send_sync::<ImportError>();
    assert_send_sync::<ExportError>();
}

#[tokio::test]
async fn import_unknown_format() {
    let error = load_universe_from_file(
        yield_progress_for_testing(),
        Arc::new(NonDiskFile::from_name_and_data_source("foo".into(), || {
            Ok(b"nonsense".to_vec())
        })),
    )
    .await
    .unwrap_err();

    assert_eq!(error.to_string(), "failed to import 'foo'");
    assert_eq!(
        error.source().unwrap().to_string(),
        "the data is not in a recognized format"
    );
}

#[test]
fn member_export_path() {
    let mut universe = Universe::new();
    let foo = universe
        .insert("foo".into(), BlockDef::new(block::AIR))
        .unwrap();
    let _bar = universe
        .insert("bar".into(), BlockDef::new(block::AIR))
        .unwrap();

    assert_eq!(
        ExportSet::all_of_universe(&universe)
            .member_export_path(Path::new("/export/data.ext"), &foo),
        PathBuf::from("/export/data-foo.ext"),
    );
    assert_eq!(
        ExportSet::from_block_defs(vec![foo.clone()])
            .member_export_path(Path::new("/export/data.ext"), &foo),
        PathBuf::from("/export/data.ext"),
    );
}

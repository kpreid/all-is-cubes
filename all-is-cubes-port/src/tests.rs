use std::error::Error as _;
use std::fs;
use std::sync::Arc;

use pretty_assertions::assert_eq;
use serde_json::json;

use all_is_cubes::block::AIR;
use all_is_cubes::util::{assert_send_sync, yield_progress_for_testing};

use crate::file::NonDiskFile;
use crate::{
    export_to_path, load_universe_from_file, BlockDef, ExportError, ExportFormat, ExportSet,
    ImportError, Path, PathBuf, Universe,
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
    let foo = universe.insert("foo".into(), BlockDef::new(AIR)).unwrap();
    let _bar = universe.insert("bar".into(), BlockDef::new(AIR)).unwrap();

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

#[tokio::test]
async fn port_whence_load_then_save() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let path: PathBuf = tmp_dir.path().join("foo.alliscubesjson");

    // Write initial state.
    export_to_path(
        yield_progress_for_testing(),
        ExportFormat::AicJson,
        ExportSet::all_of_universe(&Universe::new()),
        path.clone(),
    )
    .await
    .unwrap();

    // Load it again, producing a universe containing `PortWhence`.
    let mut universe =
        load_universe_from_file(yield_progress_for_testing(), Arc::new(path.clone()))
            .await
            .unwrap();

    // Make a change.
    universe.insert("hello".into(), BlockDef::new(AIR)).unwrap();

    // Save it.
    universe
        .whence
        .save(&universe, yield_progress_for_testing())
        .await
        .unwrap();

    // Check the saved result
    assert_eq!(
        serde_json::from_reader::<_, serde_json::Value>(fs::File::open(path).unwrap()).unwrap(),
        json!({
            "type": "UniverseV1",
            "members": [
                {
                    "name": {"Specific": "hello"},
                    "member_type": "Block",
                    "value": {"type": "BlockV1", "primitive": {"type": "AirV1"}},
                }
            ]
        })
    );
}

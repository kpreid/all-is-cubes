use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use all_is_cubes::block;
use all_is_cubes::universe::{Name, URef};
use all_is_cubes::util::yield_progress_for_testing;

use crate::{export_to_path, load_universe_from_file, ExportSet};

#[tokio::test]
async fn import_export_native_format() {
    let import_path = PathBuf::from(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/src/native/tests/native-test.alliscubesjson"
    ));
    let universe =
        load_universe_from_file(yield_progress_for_testing(), Arc::new(import_path.clone()))
            .await
            .unwrap();

    assert_eq!(
        universe.whence.document_name(),
        Some(String::from("native-test"))
    );

    // This is *not* a thorough test of `Universe` deserialization.
    // It is just enough to prove that we ran the deserialization code and not something else.
    let uref: URef<block::BlockDef> = universe.get(&Name::from("foo")).unwrap();
    assert_eq!(**uref.read().unwrap(), block::AIR);

    // Export again.
    let destination_dir = tempfile::tempdir().unwrap();
    let destination: PathBuf = destination_dir.path().join("foo.alliscubesjson");
    export_to_path(
        yield_progress_for_testing(),
        crate::ExportFormat::AicJson,
        ExportSet::all_of_universe(&universe),
        destination.to_path_buf(),
    )
    .await
    .unwrap();

    // Compare JSON structure (by value so that we don't have prettyprinting differences)
    let expected_value: serde_json::Value =
        serde_json::from_reader(fs::File::open(import_path).unwrap()).unwrap();
    let actual_value: serde_json::Value =
        serde_json::from_reader(fs::File::open(&destination).unwrap()).unwrap();
    pretty_assertions::assert_eq!(expected_value, actual_value);
}

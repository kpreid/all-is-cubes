#![cfg_attr(
    not(all(feature = "import", feature = "export")),
    allow(unused_imports)
)]
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use all_is_cubes::block;
use all_is_cubes::universe::{Handle, Name};
use all_is_cubes::util::{async_test, yield_progress_for_testing};

#[cfg(feature = "import")]
use crate::load_universe_from_file;
#[cfg(feature = "export")]
use crate::{ExportSet, export_to_path};

#[cfg(all(feature = "export", feature = "import"))]
#[async_test]
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
    let handle: Handle<block::BlockDef> = universe.get(&Name::from("foo")).unwrap();
    assert_eq!(
        handle.read(universe.read_ticket()).unwrap().block(),
        &block::AIR
    );

    // Export again.
    let destination_dir = tempfile::tempdir().unwrap();
    let destination: PathBuf = destination_dir.path().join("foo.alliscubesjson");
    export_to_path(
        yield_progress_for_testing(),
        universe.read_ticket(),
        crate::Format::AicJson,
        ExportSet::all_of_universe(&universe),
        destination.clone(),
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

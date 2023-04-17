use std::error::Error as _;

use all_is_cubes::block;
use all_is_cubes::universe::{Name, URef, UniverseIndex as _};
use all_is_cubes::util::assert_send_sync;

use crate::file::NonDiskFile;
use crate::{
    load_universe_from_file, BlockDef, ExportError, ExportSet, ImportError, Path, PathBuf,
    Universe, YieldProgress,
};

#[test]
fn errors_are_send_sync() {
    assert_send_sync::<ImportError>();
    assert_send_sync::<ExportError>();
}

#[tokio::test]
async fn import_native_format() {
    let universe = load_universe_from_file(
        YieldProgress::noop(),
        Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/tests/native-test.json"
        )),
    )
    .await
    .unwrap();

    // This is *not* a thorough test of `Universe` deserialization.
    // It is just enough to prove that we ran the deserialization code and not something else.
    let uref: URef<BlockDef> = universe.get(&Name::from("foo")).unwrap();
    assert_eq!(**uref.read().unwrap(), block::AIR);
}

#[tokio::test]
async fn import_unknown_format() {
    let error = load_universe_from_file(
        YieldProgress::noop(),
        &NonDiskFile::from_name_and_data_source("foo".into(), || Ok(b"nonsense".to_vec())),
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

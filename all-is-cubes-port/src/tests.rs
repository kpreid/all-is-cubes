#[cfg(all(feature = "import", feature = "export", feature = "native"))]
#[all_is_cubes::util::async_test]
async fn port_whence_load_then_save() {
    use std::fs;
    use std::sync::Arc;

    use pretty_assertions::assert_eq;
    use serde_json::json;

    use all_is_cubes::block::AIR;
    use all_is_cubes::block::BlockDef;
    use all_is_cubes::universe::Universe;
    use all_is_cubes::util::yield_progress_for_testing;

    use crate::{ExportSet, Format, export_to_path, load_universe_from_file};
    use std::path::PathBuf;

    let tmp_dir = tempfile::tempdir().unwrap();
    let path: PathBuf = tmp_dir.path().join("foo.alliscubesjson");

    // Write initial state.
    {
        let init_universe = Universe::new();
        export_to_path(
            yield_progress_for_testing(),
            init_universe.read_ticket(),
            Format::AicJson,
            ExportSet::all_of_universe(&init_universe),
            path.clone(),
        )
        .await
        .unwrap();
    }

    // Load it again, producing a universe containing `PortWhence`.
    let mut universe =
        load_universe_from_file(yield_progress_for_testing(), Arc::new(path.clone()))
            .await
            .unwrap();

    // Make a change.
    universe
        .insert("hello".into(), BlockDef::new(universe.read_ticket(), AIR))
        .unwrap();

    // Save it.
    universe.whence.save(&universe, yield_progress_for_testing()).await.unwrap();

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

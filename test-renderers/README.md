`test-renderers` is an unpublished package which contains image-comparison-based test cases for:

* All is Cubes’ different renderers, including those which use a GPU.
* User interface elements.

Organization
------------

* The individual renderer test cases, or scenes to render, are located in `src/test_cases.rs`.
* `tests/*-render.rs` for each renderer being tested.
* `tests/ui.rs` for UI tests.
* `src/lib.rs` and its modules contain the shared test infrastructure.
* `expected/` contains the expected rendered outputs.
* The tests write to `../target/test-renderers-output/` the actual outputs and a report `index.html`.

These tests use a custom test harness to enable cleanly skipping all tests when no GPU is present and generating combined reports.

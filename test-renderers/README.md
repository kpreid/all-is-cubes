`test-renderers` and its children are unpublished packages which contains image-comparison-based test cases for:

* All is Cubes’ different renderers, including those which use a GPU.
* User interface elements.

Organization
------------

* `tests/*-render.rs` test targets for each renderer being tested.
* `tests/ui.rs` test target for UI tests.
* `cases/` contains the renderer test cases (scenes to render).
* `runner/` contains the test runner, which implements the `main()` logic for all test targets.
* `types/` contains code that `runner` and `cases` both need.
* `expected/` contains the expected rendered outputs.
* The tests write to `../target/test-renderers-output/` the actual outputs and a report `index.html`.

These tests use a custom test harness to enable cleanly skipping all tests when no GPU is present and generating combined reports.

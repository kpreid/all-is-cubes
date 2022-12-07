`test-renderers` is an unpublished package which contains image-comparison-based test cases for All is Cubes’ different renderers, including those which use a GPU.

Organization
------------

* The individual test cases, or scenes to render, are located in `src/test_cases.rs`.
* `tests/` contains one file (test target) for each renderer being tested.
* `src/lib.rs` and its modules contain the shared test infrastructure.
* `expected/` contains the expected rendered outputs.
* The tests write to `../target/test-renderers-output/` the actual outputs and a report `index.html`.

These tests use a custom test harness to enable cleanly skipping all tests when no GPU is present and generating combined reports.

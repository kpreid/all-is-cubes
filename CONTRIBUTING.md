Contributing changes to All is Cubes
====================================

All is Cubes is a one-person hobby project with idiosyncratic goals.
However, its components are intended to be theoretically usable as library code,
and I am open to contributions which make it a better library or are just nifty in some way.

This file contains information on the project’s development practices
which may be useful when preparing a patch/PR.

Testing
-------

To test your changes, run `cargo xtask test` and `cargo xtask lint`.
No test failures, compilation errors, or warnings should be present.

If you have added or modified any public items,
check if their documentation is correct and comprehensible when viewed as `rustdoc` pages.
(`cargo xtask lint` will generate the documentation along with its other checks.) 

Consider running `cargo xtask fuzz` to fuzz-test changes to modules that have fuzz test coverage.

Code style
----------

*   Public items should follow the
    [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/),
    except as otherwise specified.

    *   Function documentation is encouraged to describe the circumstances under which the function
        may panic, but this information may be presented as “Panics if…” rather than using
        a “`# Panics`” heading.

*   Misbehaviors due to internal numeric overflow, or due to other forms of out-of-bounds input,
    are considered bugs.
    Code should panic, or return errors, rather than allowing invariants to be broken
    or misinterpreting input.

*   Panics due to user input are considered bugs.
    That is, library code should be designed to be usable even when aborting is unacceptable and
    `catch_unwind()` is unavailable.
    (No, I don't mean high-reliability embedded; I mean `wasm32-unknown-unknown` web applications.)

*   Use “pure Rust” code, without any dependencies upon non-Rust libraries, whenever practical.
    Consult each crate’s documentation for specific restrictions it may obey;
    for example, the `all-is-cubes` library is `no_std` compatible.

*   `unsafe` code is permitted but should be used sparingly.
    Prefer using existing libraries whenever possible, as long as this does not trade off against
    soundness.
    Use small modules to keep the `unsafe` code's dependencies clear.

*   Avoid adding large amounts of data to version control.
    For example, image files for image-comparison testing should be kept as small as practical
    (by keeping the image dimensions low, and by using compression tools like
    `oxipng -o max` or `pngcrush -brute`)
    and should not require frequent updates.

Code formatting
---------------

All Rust code is to be automatically formatted with `rustfmt`.
Format-on-save is encouraged, but you may also run `cargo xtask fmt` to reformat the entire project.
Special cases:

*   Doc comments currently must be manually formatted.
    Their lines must be wrapped to at most 100 columns.
    Shorter wrapping such as to approximately 90 columns is encouraged,
    to reduce churn upon reindentation and to avoid wasting mental effort on precise manual line wrapping.

*   “Semantic” line breaks (such as at the end of sentences within a paragraph)
    are encouraged in doc comments and other Markdown.

*   Non-documentation comments may be of arbitrary length when they are `TODO`s or similar.
    Comments expected to exist indefinitely should be wrapped to 100 columns or shorter.

*   `#[rustfmt::skip]` may be used to manually format code which benefits from tabular formatting.

Version control practices
-------------------------

Each Git commit should be self-contained and of a comprehensible size.

*   Code should successfully compile and pass tests both before and after the commit.
    *   It is acceptable for a commit to introduce warnings if they are fixed promptly
        by a later commit.
*   When broad refactoring changes are necessary, avoid mixing them with other changes.
    For example, “Introduce `foo()`” and “Use `foo()` instead of `messy().alternative()`”,
    should be kept as two separate commits when the latter is large.
*   Follow-up changes in a pull request (in response to review or test failures)
    should be squashed into the relevant original commit unless they would make
    sense by themselves.

Each commit message should:

*   Begin with a single line which summarizes the change to a level of detail that is useful when it appears in a list of commits, without being longer than necessary.

    The summary line may begin with a prefix describing the area or aspect it affects, if there is such a single area, such as:

    *   A crate name, with the “`all-is-cubes-`” prefix removed, e.g. “`mesh:`” or
        “`xtask:`”.
    *   A module name within `all-is-cubes` itself, e.g. “`transaction:`” or “`raycast:`”.
    *   “`Deps:`” for dependency maintenance.
    *   “`CI:`” for CI configuration maintenance.
    *   “`Rerun:`” for changes to Rerun/`re_sdk` integration.
    *   New prefixes which are _not_ crate or module names should be capitalized,
        or otherwise spelled in a way which does not resemble a crate or module name
        (e.g. “`glTF:`”).

*   Be wrapped to 72 columns, except for the first line.

*   Explain *why* the change is being made, insofar as that is not obvious.
    (“Fix bug” requires no justification for why it's *worth* fixing,
    but you still must explain what the bug is.)

*   Not contain bare GitHub issue IDs.
    All references to the current project hosting must be spelled out as URLs.
    (They may contain bare commit IDs, since those are part of the repository itself.)

Do not use merge commits, except when merging separate lines of development that
already existed. When pull requests need to be updated, rebase instead of merging.

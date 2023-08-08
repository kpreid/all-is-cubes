This `tools/` directory contains files relating to development of All is Cubes
rather than the code of the packages themselves, to reduce clutter in the
top-level directory.

## `about.*`

These are data for `cargo-about` to compile license information that is used by
binary distributions.

## `all-is-cubes.xcodeproj`

If you wish to run Xcode’s Metal Debugger upon `all-is-cubes`, you can use this
project to do it.
Note that you will have to create a Scheme and choose the executable location
yourself; I have not found a way to convince Xcode to store a relative path to
the executable that Cargo builds.

## `xtask`

Rust package implementing the `cargo xtask` command providing custom helpers
for building and testing `all-is-cubes`. Run `cargo xtask --help` for more
information.

(This works because `../.cargo/config.toml` defines a command alias named
`xtask` for `cargo run --bin xtask`.)

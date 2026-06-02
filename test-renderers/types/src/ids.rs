use core::fmt;

use clap::builder::PossibleValue;

// -------------------------------------------------------------------------------------------------

/// A suite is a group of tests which get their own image directories, report files, and
/// test targets.
#[derive(
    Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize,
)]
#[expect(clippy::exhaustive_enums)]
pub enum SuiteId {
    Renderers,
    Ui,
}
impl SuiteId {
    fn as_str(self) -> &'static str {
        match self {
            SuiteId::Renderers => "renderers",
            SuiteId::Ui => "ui",
        }
    }
}
impl clap::ValueEnum for SuiteId {
    fn value_variants<'a>() -> &'a [Self] {
        &[SuiteId::Renderers, SuiteId::Ui]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        Some(PossibleValue::new(self.as_str()))
    }
}
impl fmt::Display for SuiteId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(
    Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize,
)]
#[expect(clippy::exhaustive_structs)]
pub struct TestId {
    pub suite: SuiteId,

    /// Name of the test function and parameters, used as part of the image file name.
    pub test: String,
}

impl fmt::Display for TestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { suite, test } = self;
        write!(f, "{suite}/{test}")
    }
}

// -------------------------------------------------------------------------------------------------

/// Uniquely identifies each distinct image produced/consumed by the renderer test suite.
///
/// There is one image-comparison assertion to be performed per [`ImageId`] value.
/// That is, this does not distinguish “expected” from “actual”, but does distinguish
/// between different expected/actual pairs.
// TODO: better name
#[derive(Clone, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
#[expect(clippy::exhaustive_structs)]
pub struct ImageId {
    pub test_id: TestId,
    pub renderer: RendererId,
    /// Serial numbers start at 1 and increase whenever a single test case compares
    /// more than one image.
    pub serial_number: u64,
}

impl fmt::Debug for ImageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            test_id,
            renderer,
            serial_number,
        } = self;
        write!(f, "{{{test_id} in {renderer} #{serial_number}}}")
    }
}

// -------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
#[expect(clippy::exhaustive_enums)]
pub enum RendererId {
    /// A single expected output expected to be equal for all renderers.
    /// No renderer uses this value.
    All,

    /// Used by tests/gltf-render.rs
    Gltf,
    /// Used by tests/ray-render.rs
    Raytracer,
    /// Used by tests/wgpu-render.rs
    Wgpu,
}
impl RendererId {
    fn as_str(self) -> &'static str {
        match self {
            RendererId::All => "all",
            RendererId::Gltf => "gltf",
            RendererId::Raytracer => "ray",
            RendererId::Wgpu => "wgpu",
        }
    }
}
impl clap::ValueEnum for RendererId {
    fn value_variants<'a>() -> &'a [Self] {
        &[
            RendererId::All,
            RendererId::Gltf,
            RendererId::Raytracer,
            RendererId::Wgpu,
        ]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        Some(PossibleValue::new(self.as_str()))
    }
}
impl fmt::Display for RendererId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

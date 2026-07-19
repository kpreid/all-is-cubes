use all_is_cubes::linking::InGenError;
use all_is_cubes::math::GridAab;
use all_is_cubes::space::SetCubeError;

/// Errors that may occur while importing `.vox` data.
#[derive(Debug, displaydoc::Display)]
#[allow(clippy::doc_markdown)]
#[non_exhaustive]
pub(crate) enum DotVoxConversionError {
    /// {0}
    Parse(&'static str),

    /// file contains no models or scenes to import
    FileEmpty,

    /// file refers to scene node with ID {0} but does not define it"
    MissingSceneNode(u32),

    /// scene graph contains cycle (involving node with ID {0})
    SceneGraphCycle(u32),

    /// scene graph is too complex
    SceneGraphRecursion,

    // TODO: better error, include size
    /// scene size would allocate too much space
    SceneTooLarge(GridAab),

    /// attribute “{attribute}” of scene node with ID {scene_index} is invalid
    SceneAttributeParse {
        scene_index: u32,
        attribute: &'static str,
    },

    /// file refers to model with ID {0} but does not define it
    MissingModel(u32),

    // TODO: don't use Debug formatting
    /// model’s size is impossibly large: {0:?}
    ModelSizeInvalid(dot_vox::Size),

    /// palette of {len} colors too short to contain index {index}
    PaletteTooShort { len: usize, index: u8 },

    /// position/transform too large
    TransformOverflow,

    /// failed to place block
    SetCube(SetCubeError),

    /// unexpected error
    Unexpected(InGenError),
}

impl core::error::Error for DotVoxConversionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use DotVoxConversionError as E;
        match self {
            E::Parse(_) => None,
            E::FileEmpty => None,
            E::MissingSceneNode(_) => None,
            E::SceneGraphCycle(_) => None,
            E::SceneGraphRecursion => None,
            E::SceneTooLarge(_) => None,
            E::SceneAttributeParse {
                scene_index: _,
                attribute: _,
            } => None,
            E::MissingModel(_) => None,
            E::ModelSizeInvalid(_) => None,
            E::PaletteTooShort { len: _, index: _ } => None,
            E::TransformOverflow => None,
            E::SetCube(error) => Some(error),
            E::Unexpected(error) => Some(error),
        }
    }
}

impl From<SetCubeError> for DotVoxConversionError {
    fn from(error: SetCubeError) -> Self {
        DotVoxConversionError::SetCube(error)
    }
}
impl From<InGenError> for DotVoxConversionError {
    fn from(error: InGenError) -> Self {
        DotVoxConversionError::Unexpected(error)
    }
}

#[cfg(feature = "import")]
impl From<DotVoxConversionError> for InGenError {
    fn from(error: DotVoxConversionError) -> Self {
        InGenError::other(error)
    }
}

pub(crate) fn warn_extra_attributes(
    thing: core::fmt::Arguments<'_>,
    attributes: &dot_vox::Dict,
    expected_attributes: &[&'static str],
) {
    let unexpected = Vec::from_iter(
        attributes.keys().filter(|key| !expected_attributes.contains(&key.as_str())),
    );
    if !unexpected.is_empty() {
        // TODO: have a better path for reporting this kind of info about the results of the import
        log::info!("{thing} contains unknown attributes {unexpected:?}");
    }
}

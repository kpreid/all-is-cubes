use all_is_cubes::linking::InGenError;
use all_is_cubes::math::GridAab;
use all_is_cubes::space::SetCubeError;

/// Errors that may occur while importing `.vox` data.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub(crate) enum DotVoxConversionError {
    #[error("{0}")]
    Parse(&'static str),

    #[error("file contains no models or scenes to import")]
    FileEmpty,

    #[error("file refers to scene node with ID {0} but does not define it")]
    MissingSceneNode(u32),

    #[error("scene graph contains cycle (involving node with ID {0})")]
    SceneGraphCycle(u32),

    #[error("scene graph is too complex")]
    SceneGraphRecursion,

    // TODO: better error, include size
    #[error("scene size would allocate too much space")]
    SceneTooLarge(GridAab),

    #[error("attribute “{attribute}” of scene node with ID {scene_index} is invalid")]
    SceneAttributeParse {
        scene_index: u32,
        attribute: &'static str,
    },

    #[error("file refers to model with ID {0} but does not define it")]
    MissingModel(u32),

    // TODO: don't use Debug formatting
    #[error("model’s size is impossibly large: {0:?}")]
    ModelSizeInvalid(dot_vox::Size),

    #[error("palette of {len} colors too short to contain index {index}")]
    PaletteTooShort { len: usize, index: u8 },

    #[error("position/transform too large")]
    TransformOverflow,

    #[error("failed to place block")]
    SetCube(#[source] SetCubeError),

    #[error("unexpected error")]
    Unexpected(#[source] InGenError),
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

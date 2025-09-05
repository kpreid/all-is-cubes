use all_is_cubes::linking::InGenError;
use all_is_cubes::space::SetCubeError;

/// Note: This is not a well-designed error enum (yet)
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub(crate) enum DotVoxConversionError {
    #[error("{0}")]
    Parse(&'static str),
    #[error("palette of {len} colors too short to contain index {index}")]
    PaletteTooShort { len: usize, index: u8 },
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

use luminance::framebuffer::FramebufferError;
use luminance::pipeline::PipelineError;
use luminance::tess::TessError;
use luminance::texture::TextureError;

use crate::GraphicsResourceError;

// Conversions for [`GraphicsResourceError`]
impl From<FramebufferError> for GraphicsResourceError {
    fn from(source: FramebufferError) -> Self {
        GraphicsResourceError::new(source)
    }
}
impl From<PipelineError> for GraphicsResourceError {
    fn from(source: PipelineError) -> Self {
        GraphicsResourceError::new(source)
    }
}
impl From<TessError> for GraphicsResourceError {
    fn from(source: TessError) -> Self {
        GraphicsResourceError::new(source)
    }
}
impl From<TextureError> for GraphicsResourceError {
    fn from(source: TextureError) -> Self {
        GraphicsResourceError::new(source)
    }
}

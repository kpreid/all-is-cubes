// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::fmt;

use instant::Duration;

use all_is_cubes::apps::Layers;
use all_is_cubes::mesh::chunked_mesh::CsmUpdateInfo;
use all_is_cubes::util::{CustomFormat, StatusText};

/// Performance info about drawing an entire scene.
///
/// This is intended to be displayed to the user as real-time diagnostic information.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct RenderInfo {
    pub(crate) frame_time: Duration,
    pub(crate) prepare_time: Duration,
    pub(crate) draw_time: Layers<Duration>,
    pub(crate) draw_info: Layers<SpaceRenderInfo>,
}

impl CustomFormat<StatusText> for RenderInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(
            fmt,
            "Frame time: {} (prep {}, draw world {}, ui {})\n\n",
            self.frame_time.custom_format(StatusText),
            self.prepare_time.custom_format(StatusText),
            self.draw_time.world.custom_format(StatusText),
            self.draw_time.ui.custom_format(StatusText),
        )?;
        write!(
            fmt,
            "WORLD:\n{}\n\n",
            self.draw_info.world.custom_format(StatusText)
        )?;
        write!(fmt, "UI:\n{}", self.draw_info.ui.custom_format(StatusText))?;
        Ok(())
    }
}

/// Performance info about drawing a [`Space`].
///
/// This is intended to be displayed to the user as real-time diagnostic information,
/// part of [`RenderInfo`].
///
/// [`Space`]: all_is_cubes::space::Space
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct SpaceRenderInfo {
    /// Status of the block and chunk meshes.
    pub chunk_info: CsmUpdateInfo,
    pub chunks_drawn: usize,
    /// How many squares (quadrilaterals; sets of 2 triangles = 6 vertices) were used
    /// to draw this frame.
    pub squares_drawn: usize,
    /// Status of the texture atlas.
    pub texture_info: BlockTextureInfo,
    /// Time taken to upload light data.
    pub light_update_time: Duration,
    /// Number of light cubes updated
    pub light_update_count: usize,
}

impl Default for SpaceRenderInfo {
    fn default() -> Self {
        Self {
            chunk_info: Default::default(),
            chunks_drawn: 0,
            squares_drawn: 0,
            texture_info: Default::default(),
            light_update_time: Duration::ZERO,
            light_update_count: 0,
        }
    }
}

impl CustomFormat<StatusText> for SpaceRenderInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: StatusText) -> fmt::Result {
        let Self {
            chunk_info,
            chunks_drawn,
            squares_drawn,
            texture_info,
            light_update_time,
            light_update_count,
        } = self;

        let light_update_time = light_update_time.custom_format(format_type);

        writeln!(fmt, "{}", chunk_info.custom_format(format_type))?;
        writeln!(
            fmt,
            "Chunks drawn: {chunks_drawn:3} Quads drawn: {squares_drawn:7}  \
            Light: {light_update_count:3} cubes in {light_update_time}",
        )?;
        write!(fmt, "{:#?}", texture_info.custom_format(StatusText))?;
        Ok(())
    }
}

/// Performance info about [`Block`] texture management.
///
/// [`Block`]: all_is_cubes::block::Block
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BlockTextureInfo {
    pub(crate) flushed: usize,
    pub(crate) flush_time: Duration,
    pub(crate) in_use_tiles: usize,
    pub(crate) in_use_texels: usize,
    pub(crate) capacity_texels: usize,
}

impl Default for BlockTextureInfo {
    fn default() -> Self {
        BlockTextureInfo {
            flushed: 0,
            flush_time: Duration::ZERO,
            in_use_tiles: 0,
            in_use_texels: 0,
            capacity_texels: 0,
        }
    }
}

impl CustomFormat<StatusText> for BlockTextureInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: StatusText) -> fmt::Result {
        write!(
            fmt,
            "Textures: {} tiles, {} texels ({}%) used, {:2} flushed in {}",
            self.in_use_tiles,
            self.in_use_texels,
            (self.in_use_texels as f32 / self.capacity_texels as f32 * 100.0).ceil() as usize,
            self.flushed,
            self.flush_time.custom_format(format_type)
        )
    }
}

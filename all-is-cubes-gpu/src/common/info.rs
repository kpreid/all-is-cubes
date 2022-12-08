use std::fmt;

use instant::Duration;

use all_is_cubes::apps::Layers;
use all_is_cubes::camera::Flaws;
use all_is_cubes::mesh::chunked_mesh::CsmUpdateInfo;
use all_is_cubes::util::{CustomFormat, StatusText};

/// Performance info about drawing an entire scene.
///
/// This is intended to be displayed to the user as real-time diagnostic information.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct RenderInfo {
    pub(crate) update: UpdateInfo,
    pub(crate) draw: DrawInfo,
    pub flaws: Flaws,
}

/// Info about the “update” operation, where fresh scene information is gathered,
/// processed (e.g. mesh generation), and copied to GPU memory.
///
/// All of the timings here are CPU time, so they do not account for GPU activity
/// unless blocking occurs.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct UpdateInfo {
    /// Known flaws detected at update time.
    ///
    /// This should include all flaws reported within `spaces` too.
    pub(crate) flaws: Flaws,
    /// Start-to-finish time for the update operation.
    pub(crate) total_time: Duration,
    /// Time taken on miscellaneous preparatory actions such as calculating the current
    /// camera state.
    pub(crate) prep_time: Duration,
    /// Time taken on gathering cursor and debug line vertices.
    pub(crate) lines_time: Duration,
    /// Time taken on submitting accumulated information to the GPU.
    /// `None` if the graphics API does not expose this as a step.
    pub(crate) submit_time: Option<Duration>,
    /// Per-space details, including time taken.
    pub(crate) spaces: Layers<SpaceUpdateInfo>,
}

impl UpdateInfo {
    #[doc(hidden)] // unstable
    pub fn flaws(&self) -> Flaws {
        self.flaws
    }
}

#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct DrawInfo {
    /// Total time taken by drawing each layer.
    pub(crate) times: Layers<Duration>,
    pub(crate) space_info: Layers<SpaceDrawInfo>,
    /// Time taken on submitting accumulated information to the GPU.
    /// `None` if the graphics API does not expose this as a step.
    pub(crate) submit_time: Option<Duration>,
}

impl CustomFormat<StatusText> for RenderInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        let &Self {
            update:
                UpdateInfo {
                    flaws: _, // flaws are aggregated up
                    total_time: update_time,
                    prep_time: update_prep_time,
                    lines_time,
                    submit_time: update_submit_time,
                    spaces: ref update_spaces,
                },
            draw:
                DrawInfo {
                    times: draw_time,
                    space_info: ref draw_spaces,
                    submit_time,
                },
            flaws,
        } = self;

        let total_time = update_time
            .saturating_add(draw_time.world)
            .saturating_add(draw_time.ui);

        // Overall summary line
        write!(
            fmt,
            // TODO: adjust this format to account for more pieces
            "Frame time: {} (update {}, draw world {}, ui {}",
            total_time.custom_format(StatusText),
            update_time.custom_format(StatusText),
            draw_time.world.custom_format(StatusText),
            draw_time.ui.custom_format(StatusText),
        )?;
        if let Some(t) = submit_time {
            write!(fmt, ", submit {}", t.custom_format(StatusText))?;
        }
        writeln!(fmt, ")")?;

        // UpdateInfo details
        write!(
            fmt,
            "Update breakdown: prep {}, world mesh {}, ui mesh {}, lines {}",
            update_prep_time.custom_format(StatusText),
            update_spaces.world.total_time.custom_format(StatusText),
            update_spaces.ui.total_time.custom_format(StatusText),
            lines_time.custom_format(StatusText),
        )?;
        if let Some(t) = update_submit_time {
            write!(fmt, ", submit {}", t.custom_format(StatusText))?;
        }

        // Spaces
        write!(
            fmt,
            "\n\nWORLD:\n{}\n{}\n\n",
            update_spaces.world.custom_format(StatusText),
            draw_spaces.world.custom_format(StatusText)
        )?;
        write!(
            fmt,
            "UI:\n{}\n{}",
            update_spaces.ui.custom_format(StatusText),
            draw_spaces.ui.custom_format(StatusText)
        )?;

        write!(fmt, "\nRender flaws: {flaws:?}")?;
        Ok(())
    }
}

/// Performance info about copying [`Space`] data into the renderer per-frame.
///
/// This is intended to be displayed to the user as real-time diagnostic information,
/// part of [`RenderInfo`].
///
/// [`Space`]: all_is_cubes::space::Space
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
pub struct SpaceUpdateInfo {
    pub(crate) total_time: Duration,

    /// Status of the block and chunk meshes.
    pub(crate) chunk_info: CsmUpdateInfo,
    /// Status of the texture atlas.
    pub(crate) texture_info: BlockTextureInfo,

    /// Time taken to upload light data.
    pub(crate) light_update_time: Duration,
    /// Number of light cubes updated
    pub(crate) light_update_count: usize,
}

impl SpaceUpdateInfo {
    pub(crate) fn flaws(&self) -> Flaws {
        self.chunk_info.flaws
    }
}

impl CustomFormat<StatusText> for SpaceUpdateInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: StatusText) -> fmt::Result {
        let &SpaceUpdateInfo {
            total_time: _, // we print this as summary info from the parent only
            ref chunk_info,
            ref texture_info,
            light_update_time,
            light_update_count,
        } = self;

        let light_update_time = light_update_time.custom_format(format_type);

        writeln!(fmt, "{}", chunk_info.custom_format(format_type))?;
        writeln!(
            fmt,
            "Light: {light_update_count:3} cubes in {light_update_time}"
        )?;
        write!(fmt, "{:#?}", texture_info.custom_format(StatusText))?;
        Ok(())
    }
}

/// Performance info about actually drawing a [`Space`] (excluding data updates).
///
/// Depending on the asynchrony of the renderer implementation, this may not be a
/// complete accounting of time spent; in particular it is not guaranteed to include
/// time spent by the GPU or waiting for the GPU.
///
/// This is intended to be displayed to the user as real-time diagnostic information,
/// part of [`RenderInfo`].
///
/// [`Space`]: all_is_cubes::space::Space
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
pub struct SpaceDrawInfo {
    /// Time taken to set up for drawing the space.
    pub(crate) draw_init_time: Duration,
    /// Time taken to draw chunks' opaque geometry
    /// (and determine if they are visible to be drawn).
    pub(crate) draw_opaque_time: Duration,
    /// Time taken to draw chunks' transparent geometry
    /// (and determine if they are visible to be drawn).
    pub(crate) draw_transparent_time: Duration,

    /// Number of chunk meshes drawn.
    pub(crate) chunks_drawn: usize,
    /// How many squares (quadrilaterals; sets of 2 triangles = 6 vertices) were used
    /// to draw this frame.
    pub(crate) squares_drawn: usize,

    pub(crate) flaws: Flaws,
}

impl CustomFormat<StatusText> for SpaceDrawInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: StatusText) -> fmt::Result {
        let &SpaceDrawInfo {
            draw_init_time,
            draw_opaque_time,
            draw_transparent_time,
            chunks_drawn,
            squares_drawn,
            flaws: _, // TODO: include or exclude?
        } = self;

        let draw_init_time = draw_init_time.custom_format(format_type);
        let draw_opaque_time = draw_opaque_time.custom_format(format_type);
        let draw_transparent_time = draw_transparent_time.custom_format(format_type);

        writeln!(
            fmt,
            "Draw init: {draw_init_time}  opaque: {draw_opaque_time}  transparent: {draw_transparent_time}",
        )?;
        writeln!(
            fmt,
            "Chunks drawn: {chunks_drawn:3} Quads drawn: {squares_drawn:7}",
        )?;
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

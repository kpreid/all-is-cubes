use std::time::Duration;
use std::{fmt, ops};

use all_is_cubes::camera::{Flaws, Layers};
#[cfg(feature = "rerun")]
use all_is_cubes::rerun_glue as rg;
use all_is_cubes::util::{Fmt, Refmt, StatusText};
use all_is_cubes_mesh::dynamic::CsmUpdateInfo;

/// Performance info about drawing an entire scene.
///
/// This is intended to be displayed to the user as real-time diagnostic information.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct RenderInfo {
    pub(crate) waiting_for_gpu: Duration,
    pub(crate) update: UpdateInfo,
    pub(crate) draw: DrawInfo,
    /// [Flaws] in the rendering.
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
pub(crate) struct DrawInfo {
    /// Total time taken by drawing each layer.
    pub(crate) times: Layers<Duration>,
    pub(crate) space_info: Layers<SpaceDrawInfo>,
    /// Time taken on submitting accumulated information to the GPU.
    /// `None` if the graphics API does not expose this as a step.
    pub(crate) submit_time: Option<Duration>,
}
impl DrawInfo {
    pub(crate) fn flaws(&self) -> Flaws {
        self.space_info.world.flaws | self.space_info.ui.flaws
    }
}

impl Fmt<StatusText> for RenderInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        let &Self {
            waiting_for_gpu,
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

        let total_time = waiting_for_gpu
            .saturating_add(update_time)
            .saturating_add(draw_time.world)
            .saturating_add(draw_time.ui);

        // Overall summary line
        write!(
            fmt,
            // TODO: adjust this format to account for more pieces
            "Frame time: {} (GPU wait {}, update {}, draw world {}, ui {}",
            total_time.refmt(&StatusText),
            waiting_for_gpu.refmt(&StatusText),
            update_time.refmt(&StatusText),
            draw_time.world.refmt(&StatusText),
            draw_time.ui.refmt(&StatusText),
        )?;
        if let Some(t) = submit_time {
            write!(fmt, ", submit {}", t.refmt(&StatusText))?;
        }
        writeln!(fmt, ")")?;

        // UpdateInfo details
        write!(
            fmt,
            "Update breakdown: prep {}, world mesh {}, ui mesh {}, lines {}",
            update_prep_time.refmt(&StatusText),
            update_spaces.world.total_time.refmt(&StatusText),
            update_spaces.ui.total_time.refmt(&StatusText),
            lines_time.refmt(&StatusText),
        )?;
        if let Some(t) = update_submit_time {
            write!(fmt, ", submit {}", t.refmt(&StatusText))?;
        }

        // Spaces
        write!(
            fmt,
            "\n\nWORLD:\n{}\n{}\n\n",
            update_spaces.world.refmt(&StatusText),
            draw_spaces.world.refmt(&StatusText)
        )?;
        write!(
            fmt,
            "UI:\n{}\n{}",
            update_spaces.ui.refmt(&StatusText),
            draw_spaces.ui.refmt(&StatusText)
        )?;

        write!(fmt, "\nRender flaws: {flaws}")?;
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

    #[cfg(feature = "rerun")]
    pub(crate) fn write_to_rerun(&self, destination: &rg::Destination) {
        destination.log(
            &"info".into(),
            &rg::archetypes::TextDocument::new(self.refmt(&StatusText).to_string())
                .with_media_type(rg::components::MediaType::TEXT),
        );

        // TODO: include every useful number we can; make names more consistent
        let &Self {
            total_time,
            chunk_info:
                CsmUpdateInfo {
                    total_time: chunk_total_time,
                    ..
                },
            texture_info: _,
            light_update_time,
            light_update_count: _,
        } = self;

        destination.log(&"update_total_time".into(), &rg::milliseconds(total_time));
        destination.log(
            &"chunk_total_time".into(),
            &rg::milliseconds(chunk_total_time),
        );
        destination.log(
            &"light_update_time".into(),
            &rg::milliseconds(light_update_time),
        );
    }
}

impl Fmt<StatusText> for SpaceUpdateInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        let &SpaceUpdateInfo {
            total_time: _, // we print this as summary info from the parent only
            ref chunk_info,
            ref texture_info,
            light_update_time,
            light_update_count,
        } = self;

        let light_update_time = light_update_time.refmt(fopt);

        writeln!(fmt, "{}", chunk_info.refmt(fopt))?;
        writeln!(
            fmt,
            "Light: {light_update_count:3} cubes in {light_update_time}"
        )?;
        write!(fmt, "{:#?}", texture_info.refmt(fopt))?;
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
    pub(crate) draw_opaque_chunks_time: Duration,
    /// Time taken to draw instanced blocks' opaque geometry.
    pub(crate) draw_opaque_blocks_time: Duration,
    /// Time taken to draw chunks' transparent geometry
    /// (and determine if they are visible to be drawn).
    pub(crate) draw_transparent_time: Duration,
    /// Time taken to finish rendering (currently this means dropping the render pass and writing
    /// the instance buffer).
    pub(crate) finalize_time: Duration,

    /// Number of chunk meshes drawn.
    pub(crate) chunks_drawn: usize,
    /// Number of instanced block meshes drawn.
    pub(crate) blocks_drawn: usize,
    /// How many squares (quadrilaterals; sets of 2 triangles = 6 vertices) were used
    /// to draw this frame.
    pub(crate) squares_drawn: usize,

    pub(crate) flaws: Flaws,
}

impl Fmt<StatusText> for SpaceDrawInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        let &SpaceDrawInfo {
            draw_init_time,
            draw_opaque_chunks_time,
            draw_opaque_blocks_time,
            draw_transparent_time,
            finalize_time,
            chunks_drawn,
            blocks_drawn,
            squares_drawn,
            flaws: _, // TODO: include or exclude?
        } = self;

        let draw_init_time = draw_init_time.refmt(fopt);
        let draw_opaque_chunks_time = draw_opaque_chunks_time.refmt(fopt);
        let draw_opaque_blocks_time = draw_opaque_blocks_time.refmt(fopt);
        let draw_transparent_time = draw_transparent_time.refmt(fopt);
        let finalize_time = finalize_time.refmt(fopt);

        writeln!(
            fmt,
            "Draw init: {draw_init_time}  \
            opaque chunks: {draw_opaque_chunks_time}  \
            opaque blocks: {draw_opaque_blocks_time}  \
            transparent chunks: {draw_transparent_time}  \
            finalize: {finalize_time}",
        )?;
        writeln!(
            fmt,
            "Chunks drawn: {chunks_drawn:3} Block insts drawn: {blocks_drawn:3} Quads drawn: {squares_drawn:7}",
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

impl ops::Add for BlockTextureInfo {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        BlockTextureInfo {
            flushed: self.flushed + rhs.flushed,
            flush_time: self.flush_time + rhs.flush_time,
            in_use_tiles: self.in_use_tiles + rhs.in_use_tiles,
            in_use_texels: self.in_use_texels + rhs.in_use_texels,
            capacity_texels: self.capacity_texels + rhs.capacity_texels,
        }
    }
}

impl Fmt<StatusText> for BlockTextureInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        write!(
            fmt,
            "Textures: {} tiles, {} texels ({}% of {}) used, {:2} flushed in {}",
            self.in_use_tiles,
            self.in_use_texels,
            (self.in_use_texels as f32 / self.capacity_texels as f32 * 100.0).ceil() as usize,
            self.capacity_texels,
            self.flushed,
            self.flush_time.refmt(fopt)
        )
    }
}

use core::time::Duration;
use core::{fmt, ops};

#[cfg(feature = "rerun")]
use all_is_cubes::rerun_glue as rg;
use all_is_cubes::util::{Fmt, Refmt, ShowStatus, StatusText};
use all_is_cubes_mesh::dynamic::CsmUpdateInfo;
use all_is_cubes_render::{Flaws, camera::Layers};

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

    /// CPU time taken on screen-space postprocessing and info text rendering.
    ///
    /// This information is stored and reported delayed by 1 frame because otherwise it would not be
    /// available while drawing the text that incorporates itself.
    pub(crate) previous_postprocess_time: Duration,
}
impl DrawInfo {
    pub(crate) fn flaws(&self) -> Flaws {
        self.space_info.world.flaws | self.space_info.ui.flaws
    }
}

impl Fmt<StatusText> for RenderInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
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
                    previous_postprocess_time,
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
            total_time.refmt(fopt),
            waiting_for_gpu.refmt(fopt),
            update_time.refmt(fopt),
            draw_time.world.refmt(fopt),
            draw_time.ui.refmt(fopt),
        )?;
        if let Some(t) = submit_time {
            write!(fmt, ", submit {}", t.refmt(fopt))?;
        }
        writeln!(
            fmt,
            ", prev post {})",
            previous_postprocess_time.refmt(fopt)
        )?;

        // UpdateInfo details
        write!(
            fmt,
            "Update breakdown: prep {}, world mesh {}, ui mesh {}, lines {}",
            update_prep_time.refmt(fopt),
            update_spaces.world.total_time.refmt(fopt),
            update_spaces.ui.total_time.refmt(fopt),
            lines_time.refmt(fopt),
        )?;
        if let Some(t) = update_submit_time {
            write!(fmt, ", submit {}", t.refmt(fopt))?;
        }

        // Spaces
        if fopt.show.contains(ShowStatus::WORLD) {
            write!(
                fmt,
                "\n\nWORLD:\n{}\n{}",
                update_spaces.world.refmt(fopt),
                draw_spaces.world.refmt(fopt)
            )?;
        }
        if fopt.show.contains(ShowStatus::UI) {
            write!(
                fmt,
                "\n\nUI:\n{}\n{}",
                update_spaces.ui.refmt(fopt),
                draw_spaces.ui.refmt(fopt)
            )?;
        }

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
        use alloc::string::ToString as _;

        destination.log(
            &"info".into(),
            &rg::archetypes::TextDocument::new(self.refmt(&StatusText::ALL).to_string())
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

    /// Number of chunk meshes (not instances) drawn.
    pub(crate) chunk_meshes_drawn: usize,
    /// Number of chunks that contributed instances.
    pub(crate) chunks_with_instances_drawn: usize,
    /// Number of instanced block meshes drawn.
    pub(crate) blocks_drawn: usize,
    /// How many triangles were used to draw this frame.
    pub(crate) triangles_drawn: usize,

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
            chunk_meshes_drawn,
            chunks_with_instances_drawn,
            blocks_drawn,
            triangles_drawn,
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
            "Chunk meshes drawn: {chunk_meshes_drawn:3}  \
            Block insts drawn: {blocks_drawn:3} in {chunks_with_instances_drawn:3} chunks  \
            Triangles drawn: {triangles_drawn:7}",
        )?;
        Ok(())
    }
}

/// Performance info about [`Block`] texture management.
///
/// [`Block`]: all_is_cubes::block::Block
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct BlockTextureInfo {
    pub(crate) flushed: usize,
    pub(crate) flush_time: Duration,
    pub(crate) in_use_tiles: usize,
    pub(crate) texels: crate::octree_alloc::Info,
}

impl ops::Add for BlockTextureInfo {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        BlockTextureInfo {
            flushed: self.flushed + rhs.flushed,
            flush_time: self.flush_time + rhs.flush_time,
            in_use_tiles: self.in_use_tiles + rhs.in_use_tiles,
            texels: self.texels + rhs.texels,
        }
    }
}

impl Fmt<StatusText> for BlockTextureInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        write!(
            fmt,
            "Textures: {}  Atlas: {}  Flushed: {:2} in {}",
            self.in_use_tiles,
            self.texels.refmt(fopt),
            self.flushed,
            self.flush_time.refmt(fopt)
        )
    }
}

//! Glue for GPU timestamp queries to gather performance information.

use alloc::sync::Arc;
use core::fmt;
use core::time::Duration;

use strum::EnumCount;

use all_is_cubes::util::{Refmt as _, StatusText};

use crate::glue::buffer_size_of;

// -------------------------------------------------------------------------------------------------

/// Typed indices for queries in [`Queries`].
#[derive(Debug, Clone, Copy, EnumCount)]
pub(crate) enum Query {
    BeginWorldRenderPass,
    EndWorldRenderPass,
    BeginUiRenderPass,
    EndUiRenderPass,
    BeginReprojectRenderPass,
    EndReprojectRenderPass,
    BeginGapFill,
    EndGapFill,
    BeginBloom,
    EndBloom,
    BeginPostprocess,
    EndPostprocess,
}

const QUERY_BUFFER_SIZE: u64 = buffer_size_of::<u64>().get() * Query::COUNT as u64;

impl Query {
    pub fn index(self) -> u32 {
        self as u32
    }
}

// -------------------------------------------------------------------------------------------------

/// Information needed to correctly interpret the timestamps obtained from [`Queries`].
#[derive(Debug)]
pub(crate) struct TimestampInterpretation {
    pub bloom_present: bool,
    pub reprojecting: bool,
}

// -------------------------------------------------------------------------------------------------

/// Resources for timestamp queries.
#[derive(Debug)]
pub(crate) struct Queries {
    shared: Arc<Shared>,

    timestamp_to_seconds: f64,

    pub(crate) query_set: wgpu::QuerySet,

    /// Intermediate buffer used as destination of `resolve_query_set()` operation, transferring
    /// data from `query_set`.
    resolve_buffer: wgpu::Buffer,

    // Buffers used cyclically to transfer data from `resolve_buffer` to CPU memory.
    // Each buffer in this channel is unused.
    read_buffer_rx: flume::Receiver<wgpu::Buffer>,

    // Latest data copied out of `resolve_buffer`.
    read_data_rx: flume::Receiver<[u64; Query::COUNT]>,

    latest_received_data: Option<GpuTimes>,
}

/// Part of [`Queries`] shared with the buffer mapping callback.
#[derive(Debug)]
struct Shared {
    read_buffer_tx: flume::Sender<wgpu::Buffer>,
    read_data_tx: flume::Sender<[u64; Query::COUNT]>,
}

impl Queries {
    pub(crate) fn new(device: &wgpu::Device, timestamp_period: f32) -> Self {
        let timestamp_to_seconds = f64::from(timestamp_period) / 1e9 /* ns to s */;
        let (read_buffer_tx, read_buffer_rx) = flume::bounded(20);
        let (read_data_tx, read_data_rx) = flume::bounded(20);
        Self {
            timestamp_to_seconds,
            query_set: device.create_query_set(&wgpu::QuerySetDescriptor {
                label: Some("Queries::query_set"),
                ty: wgpu::QueryType::Timestamp,
                count: const { Query::COUNT as u32 },
            }),
            resolve_buffer: device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("Queries::resolve_buffer"),
                size: QUERY_BUFFER_SIZE,
                usage: wgpu::BufferUsages::COPY_SRC | wgpu::BufferUsages::QUERY_RESOLVE,
                mapped_at_creation: false,
            }),
            read_buffer_rx,
            read_data_rx,
            latest_received_data: None,

            shared: Arc::new(Shared {
                read_buffer_tx,
                read_data_tx,
            }),
        }
    }

    pub(crate) fn resolve_and_fetch(
        &self,
        device: &wgpu::Device,
        encoder: &mut wgpu::CommandEncoder,
    ) {
        let shared = self.shared.clone();

        // Transfer query results from `query_set` to `resolve_buffer`.
        encoder.resolve_query_set(
            &self.query_set,
            (0..Query::COUNT as u32).into(),
            &self.resolve_buffer,
            0,
        );

        // Obtain or create a temporary buffer for GPU-to-CPU transfer.
        let read_buffer = self.read_buffer_rx.try_recv().unwrap_or_else(|_| {
            device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("Queries::read_buffer"),
                size: QUERY_BUFFER_SIZE,
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
                mapped_at_creation: false,
            })
        });

        // Copy from resolve_+buffer.
        encoder.copy_buffer_to_buffer(&self.resolve_buffer, 0, &read_buffer, 0, QUERY_BUFFER_SIZE);

        // Asynchronously copy from the temporary buffer to CPU memory.
        encoder.map_buffer_on_submit(
            &read_buffer.clone(),
            wgpu::MapMode::Read,
            0..QUERY_BUFFER_SIZE,
            move |result| {
                match result {
                    Ok(()) => {
                        let mut data = [0u64; Query::COUNT];
                        bytemuck::bytes_of_mut(&mut data)
                            .copy_from_slice(&read_buffer.get_mapped_range(0..QUERY_BUFFER_SIZE));
                        read_buffer.unmap();

                        // Send data to be collected for display later
                        let (Ok(()) | Err(_)) = shared.read_data_tx.try_send(data);

                        // Send buffer to be reused later
                        let (Ok(()) | Err(_)) = shared.read_buffer_tx.try_send(read_buffer);
                    }
                    Err(e) => {
                        log::error!("error from timestamp query map_buffer_on_submit(): {e}");
                    }
                }
            },
        );
    }

    pub(crate) fn latest(&mut self, interpretation: TimestampInterpretation) -> Option<GpuTimes> {
        let mut new_data = None;
        while let Ok(data) = self.read_data_rx.try_recv() {
            new_data = Some(data);
        }
        if let Some(latest) = new_data {
            self.latest_received_data = Some(GpuTimes::from_timestamp_queries(
                latest,
                self.timestamp_to_seconds,
                interpretation,
            ));
        }
        self.latest_received_data
    }
}

// -------------------------------------------------------------------------------------------------

/// Time spent by the GPU to render a frame.
///
/// Part of [`crate::DrawInfo`]. Produced by [`Queries::latest()`].
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct GpuTimes {
    world_render_pass: Duration,
    ui_render_pass: Duration,
    rt_reproject: Option<Duration>,
    rt_gap_fill: Option<Duration>,
    bloom: Option<Duration>,
    postprocess: Duration,
}

impl all_is_cubes::util::Fmt<StatusText> for GpuTimes {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        let Self {
            world_render_pass,
            ui_render_pass,
            rt_reproject,
            rt_gap_fill,
            bloom,
            postprocess,
        } = self;
        write!(
            fmt,
            "draw world {}, ui {}, ",
            world_render_pass.refmt(fopt),
            ui_render_pass.refmt(fopt),
        )?;
        if let Some(rt_reproject) = rt_reproject {
            write!(fmt, "reproject {}, ", rt_reproject.refmt(fopt))?;
        }
        if let Some(rt_gap_fill) = rt_gap_fill {
            write!(fmt, "gap fill {}, ", rt_gap_fill.refmt(fopt))?;
        }
        if let Some(bloom) = bloom {
            write!(fmt, "bloom {}, ", bloom.refmt(fopt))?;
        }
        write!(fmt, "postprocess {}", postprocess.refmt(fopt))?;
        Ok(())
    }
}

impl GpuTimes {
    #[allow(clippy::needless_pass_by_value)]
    fn from_timestamp_queries(
        timestamps: [u64; Query::COUNT],
        timestamp_to_seconds: f64,
        interpretation: TimestampInterpretation,
    ) -> Self {
        let fetch = |beginning: Query, end: Query| -> Duration {
            Duration::try_from_secs_f64(
                timestamps[end as usize].saturating_sub(timestamps[beginning as usize]) as f64
                    * timestamp_to_seconds,
            )
            .unwrap_or(Duration::MAX) // TODO: better error indicator
        };

        Self {
            world_render_pass: fetch(Query::BeginWorldRenderPass, Query::EndWorldRenderPass),
            ui_render_pass: fetch(Query::BeginUiRenderPass, Query::EndUiRenderPass),
            rt_reproject: interpretation.reprojecting.then(|| {
                fetch(
                    Query::BeginReprojectRenderPass,
                    Query::EndReprojectRenderPass,
                )
            }),
            rt_gap_fill: interpretation
                .reprojecting
                .then(|| fetch(Query::BeginGapFill, Query::EndGapFill)),
            bloom: interpretation.bloom_present.then(|| fetch(Query::BeginBloom, Query::EndBloom)),
            postprocess: fetch(Query::BeginPostprocess, Query::EndPostprocess),
        }
    }
}

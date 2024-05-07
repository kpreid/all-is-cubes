//! Types and data pertaining to the pattern of rays that are cast from a block to potential
//! light sources. Used by the algorithms in [`crate::space::light::updater`].

use crate::space::light::chart_schema::{self, OneRay};

/// Precalculated data about how light propagates through the cube grid,
/// used to traverse a `Space` to determine what light falls on a single block.
#[derive(Clone, Copy)]
pub(crate) struct LightChart {
    info: &'static [chart_schema::IndirectSteps],
    all_steps: &'static [chart_schema::Step],
}

impl LightChart {
    /// `bytemuck::cast_slice()` can't be const, so we have to write a function,
    /// but this should all compile to a noop.
    pub fn get() -> Self {
        const INFO_BYTES_LEN: usize =
            include_bytes!(concat!(env!("OUT_DIR"), "/light_chart_info.bin")).len();
        const STEPS_BYTES_LEN: usize =
            include_bytes!(concat!(env!("OUT_DIR"), "/light_chart_steps.bin")).len();

        // Ensure the data is sufficiently aligned
        #[repr(C)]
        struct AlignInfo {
            _aligner: [chart_schema::IndirectSteps; 0],
            data: [u8; INFO_BYTES_LEN],
        }
        #[repr(C)]
        struct AlignStep {
            _aligner: [chart_schema::Step; 0],
            data: [u8; STEPS_BYTES_LEN],
        }

        static INFO_BYTES: AlignInfo = AlignInfo {
            _aligner: [],
            data: *include_bytes!(concat!(env!("OUT_DIR"), "/light_chart_info.bin")),
        };
        static STEPS_BYTES: AlignStep = AlignStep {
            _aligner: [],
            data: *include_bytes!(concat!(env!("OUT_DIR"), "/light_chart_steps.bin")),
        };

        LightChart {
            info: bytemuck::cast_slice(&INFO_BYTES.data),
            all_steps: bytemuck::cast_slice(&STEPS_BYTES.data),
        }
    }

    pub fn rays(
        self,
        maximum_distance: u8,
    ) -> impl Iterator<Item = (OneRay, impl Iterator<Item = chart_schema::Step>)> {
        self.info.iter().map(move |ist| {
            let &chart_schema::IndirectSteps {
                info,
                relative_cube_sequence: [start, end],
            } = ist;
            (
                info,
                self.all_steps[start..end]
                    .iter()
                    .filter(move |step| step.distance <= maximum_distance)
                    .copied(),
            )
        })
    }
}

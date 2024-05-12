use crate::space::light::chart;

/// Precalculated data about how light propagates through the cube grid,
/// used to traverse a `Space` to determine what light falls on a single block.
#[derive(Clone, Copy)]
pub(crate) struct LightChart {
    info: &'static [chart::IndirectSteps],
    all_steps: &'static [chart::Step],
}

impl LightChart {
    /// Returns the precalculated light propagation chart.
    ///
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
            _aligner: [chart::IndirectSteps; 0],
            data: [u8; INFO_BYTES_LEN],
        }
        #[repr(C)]
        struct AlignStep {
            _aligner: [chart::Step; 0],
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
    ) -> impl Iterator<Item = (chart::OneRay, impl Iterator<Item = chart::Step>)> {
        self.info.iter().map(move |ist| {
            let &chart::IndirectSteps {
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

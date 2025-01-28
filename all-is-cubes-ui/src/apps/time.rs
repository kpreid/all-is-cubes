use all_is_cubes::math::{PositiveSign, ZeroOne, zo64};
use all_is_cubes::time::{Duration, Instant, TickSchedule};
#[cfg(doc)]
use all_is_cubes::universe::Universe;

/// Algorithm for deciding how to execute simulation and rendering frames.
/// Platform-independent; does not consult any clocks, only makes decisions
/// given the provided information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FrameClock<I> {
    schedule: TickSchedule,

    last_absolute_time: Option<I>,

    /// Whether there was a step and we should therefore draw a frame.
    /// TODO: This might go away in favor of actual dirty-notifications.
    render_dirty: bool,

    accumulated_step_time: Duration,

    draw_fps_counter: FpsCounter<I>,
}

impl<I> FrameClock<I> {
    /// Number of steps per frame to permit.
    /// This sets how low the frame rate can go below the step length before game time
    /// slows down.
    pub(crate) const CATCH_UP_STEPS: u32 = 2;

    /// Constructs a new [`FrameClock`].
    ///
    /// This operation is independent of the system clock.
    pub fn new(schedule: TickSchedule) -> Self {
        Self {
            schedule,
            last_absolute_time: None,
            render_dirty: true,
            accumulated_step_time: Duration::ZERO,
            draw_fps_counter: FpsCounter::default(),
        }
    }

    #[doc(hidden)] // TODO: Decide whether we want FpsCounter in our public API
    pub fn draw_fps_counter(&self) -> &FpsCounter<I> {
        &self.draw_fps_counter
    }

    fn step_length(&self) -> Duration {
        self.schedule.delta_t()
    }
}

impl<I: Instant> FrameClock<I> {
    /// Advance the clock using a source of absolute time.
    ///
    /// This cannot be meaningfully used in combination with
    /// [`FrameClock::request_frame()`] or [`FrameClock::advance_by()`].
    pub fn advance_to(&mut self, instant: I) {
        if let Some(last_absolute_time) = self.last_absolute_time {
            let delta = instant.saturating_duration_since(last_absolute_time);
            self.accumulated_step_time += delta;
            self.cap_step_time();
        }
        self.last_absolute_time = Some(instant);
    }

    /// Advance the clock using a source of relative time.
    pub fn advance_by(&mut self, duration: Duration) {
        self.accumulated_step_time += duration;
        self.cap_step_time();
    }

    /// Reacts to a callback from the environment requesting drawing a frame ASAP if
    /// we're going to (i.e. `requestAnimationFrame` on the web). Drives the simulation
    /// clock based on this input (it will not advance if no requests are made).
    ///
    /// Returns whether a frame should actually be rendered now. The caller should also
    /// consult [`FrameClock::should_step()`] afterward to schedule game state steps.
    ///
    /// This cannot be meaningfully used in combination with [`FrameClock::advance_to()`].
    #[must_use]
    pub fn request_frame(&mut self, time_since_last_frame: Duration) -> bool {
        let result = self.should_draw();
        self.did_draw();

        self.advance_by(time_since_last_frame);

        result
    }

    /// Returns the next time at which [`FrameClock::should_step()`], and then
    /// [`FrameClock::should_draw()`], should be consulted.
    ///
    /// [`FrameClock::advance_to()`] must have previously been called to give an absolute
    /// time reference.
    pub fn next_step_or_draw_time(&self) -> Option<I> {
        Some(self.last_absolute_time? + self.step_length())
    }

    /// Indicates whether a new frame should be drawn, given the amount of time that this
    /// [`FrameClock`] has been informed has passed.
    ///
    /// When a frame *is* drawn, [`FrameClock::did_draw`]] must be called; otherwise, this
    /// will always return true.
    pub fn should_draw(&self) -> bool {
        self.render_dirty
    }

    /// Informs the [`FrameClock`] that a frame was just drawn.
    pub fn did_draw(&mut self) {
        self.render_dirty = false;
        self.draw_fps_counter.record_frame();
    }

    /// Indicates whether [`Universe::step()`] should be performed,
    /// given the amount of time that this [`FrameClock`] has been informed has passed.
    ///
    /// When a step *is* performd, [`FrameClock::did_step`] must be called; otherwise, this
    /// will always return true.
    pub fn should_step(&self) -> bool {
        self.accumulated_step_time >= self.step_length()
    }

    /// Informs the [`FrameClock`] that a step was just performed.
    ///
    /// The caller must also provide an updated schedule, in case it has changed.
    pub fn did_step(&mut self, schedule: TickSchedule) {
        self.accumulated_step_time -= self.step_length();
        self.render_dirty = true;
        self.schedule = schedule;
    }

    fn cap_step_time(&mut self) {
        let cap = self.step_length() * Self::CATCH_UP_STEPS;
        if self.accumulated_step_time > cap {
            self.accumulated_step_time = cap;
        }
    }
}

/// Counts frame time / frames-per-second against real time as defined by [`Instant::now`].
#[derive(Clone, Debug, Eq, PartialEq)]
#[doc(hidden)] // TODO: Decide whether we want FpsCounter in our public API
pub struct FpsCounter<I> {
    average_frame_time_seconds: Option<PositiveSign<f64>>,
    last_frame: Option<I>,
}

impl<I> FpsCounter<I> {
    pub const fn new() -> Self {
        Self {
            average_frame_time_seconds: None,
            last_frame: None,
        }
    }

    pub fn period_seconds(&self) -> f64 {
        match self.average_frame_time_seconds {
            Some(nnt) => nnt.into_inner(),
            None => f64::NAN,
        }
    }

    pub fn frames_per_second(&self) -> f64 {
        self.period_seconds().recip()
    }
}

impl<I: Instant> FpsCounter<I> {
    pub fn record_frame(&mut self) {
        let this_frame = I::now();

        let this_seconds = self
            .last_frame
            .and_then(|l| {
                if this_frame > l {
                    Some(this_frame.saturating_duration_since(l))
                } else {
                    None
                }
            })
            .and_then(|duration| PositiveSign::try_from(duration.as_secs_f64()).ok());
        if let Some(this_seconds) = this_seconds {
            self.average_frame_time_seconds = Some(
                if let Some(previous) = self.average_frame_time_seconds.filter(|v| v.is_finite()) {
                    const MIX: ZeroOne<f64> = zo64(1.0 / 8.0);
                    this_seconds * MIX + previous * MIX.complement()
                } else {
                    // recover from any weirdness or initial state
                    this_seconds
                },
            );
        }
        self.last_frame = Some(this_frame);
    }
}

impl<I> Default for FpsCounter<I> {
    fn default() -> Self {
        Self::new()
    }
}

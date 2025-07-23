//! Output produced by xtask.

use std::fmt;
use std::mem;
use std::time::Duration;
use std::time::Instant;

// -------------------------------------------------------------------------------------------------

/// Describe how long a sub-task took.
pub(crate) struct Timing {
    label: String,
    time: Duration,
}

impl fmt::Display for Timing {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { label, time } = self;
        write!(f, "{time:5.1?} s  {label}", time = time.as_secs_f64())
    }
}

// -------------------------------------------------------------------------------------------------

/// Measure the time from creation to drop. Also prints text to mark these spans.
pub(crate) struct CaptureTime<'a> {
    label: String,
    start: Instant,
    output: &'a mut Vec<Timing>,
}

impl<'a> CaptureTime<'a> {
    pub fn new(time_log: &'a mut Vec<Timing>, label: impl Into<String>) -> Self {
        let label = label.into();
        if std::env::var("GITHUB_ACTIONS").is_ok() {
            eprintln!("::group::{label}");
        } else {
            eprintln!("------ [xtask] START: {label} ------");
        }
        Self {
            label,
            start: Instant::now(),
            output: time_log,
        }
    }
}

impl Drop for CaptureTime<'_> {
    fn drop(&mut self) {
        let label = mem::take(&mut self.label);
        let time = Instant::now().duration_since(self.start);
        if std::env::var("GITHUB_ACTIONS").is_ok() {
            eprintln!("::endgroup::");
        } else {
            eprintln!(
                "------ [xtask] END: {label} ({time:.1?} s) ------",
                time = time.as_secs_f64()
            );
        }
        self.output.push(Timing { label, time });
    }
}

// -------------------------------------------------------------------------------------------------

pub(crate) struct WithCommas(pub u64);

impl fmt::Display for WithCommas {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut in_leading_zeroes = true;
        for i in (0..4).rev() {
            let scale = 1000u64.pow(i);
            let digits_in_group = (self.0 / scale) % 1000;
            if in_leading_zeroes {
                if digits_in_group != 0 {
                    in_leading_zeroes = false;
                    write!(f, "{digits_in_group:3}")?;
                    if i > 0 {
                        write!(f, ",")?;
                    }
                } else {
                    // pad the absent group for constant width
                    write!(f, "    ")?;
                }
            } else {
                write!(f, "{digits_in_group:03}")?;
                if i > 0 {
                    write!(f, ",")?;
                }
            }
        }
        Ok(())
    }
}

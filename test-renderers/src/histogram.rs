use std::collections::BTreeMap;
use std::fmt;

use itertools::Itertools;

/// A histogram of luminance difference values.
///
/// For example, the zero element of the array contains a count of the pixels
/// that were considered equal.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct Histogram(pub [usize; 256]);

impl Histogram {
    pub const ZERO: Self = Self([0; 256]);
}

impl fmt::Debug for Histogram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Histogram({})",
            self.0
                .into_iter()
                .enumerate()
                .filter(|&(delta, count)| count > 0 && delta > 0)
                .map(|(delta, count)| format!("Δ{delta} ×{count}"))
                .join(", ")
        )
    }
}

/// A bound upon pixel differences observed in a [`DiffResult`](crate::DiffResult).
///
/// Each entry means “There may be up to &lt;value&gt; differences of magnitude
/// &lt;key&gt; or less”. Lower magnitudes that are accepted by a lower entry don't count
/// towards the limit at a higher magnitude. Differences of zero are always accepted.
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct Threshold(pub BTreeMap<u8, usize>);

impl Threshold {
    pub fn new(data: impl IntoIterator<Item = (u8, usize)>) -> Self {
        Self(
            data.into_iter()
                .map(|kv @ (key, _)| {
                    assert!(key > 0, "putting 0 ({kv:?}) in Threshold is redundant");
                    assert!(
                        key < 255,
                        "putting 255 ({kv:?}) in Threshold will make the comparison always pass"
                    );
                    kv
                })
                .collect(),
        )
    }

    /// Allow any number of pixel differences not exceeding `level`.
    pub fn no_bigger_than(level: u8) -> Self {
        if level == 0 {
            Self::new([])
        } else {
            Self::new([(level, usize::MAX)])
        }
    }

    // TODO: rename
    pub fn allows(&self, histogram: Histogram) -> bool {
        // Skip the first entry and always accept any number of zero-value differences.
        let mut checked_up_to = 1;
        // Loop over the thresholds, always in ascending order.
        for (&level, &count) in &self.0 {
            // Add 1 because the level value *includes* differences of that level, i.e.
            // level 1 should include checking histogram[1].
            let new_checked_up_to = usize::from(level) + 1;
            assert!(new_checked_up_to > checked_up_to);
            let new_differences = histogram.0[checked_up_to..new_checked_up_to]
                .iter()
                .sum::<usize>();
            if new_differences > count {
                // TODO: Instead of failing immediately, buffer this and allow a later-checked
                // higher-difference entry to also permit lower differences.
                return false;
            }
            checked_up_to = new_checked_up_to;
        }

        // Finally, reject differences greater than any accepted.
        let remaining_differences = histogram.0[checked_up_to..].iter().sum::<usize>();
        if remaining_differences > 0 {
            return false;
        }

        true
    }
}

impl From<u8> for Threshold {
    fn from(level: u8) -> Self {
        Self::no_bigger_than(level)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const H1: Histogram = {
        let mut h = [0; 256];
        h[0] = 1000;
        h[1] = 30;
        h[10] = 5;
        h[50] = 1;
        h[100] = 1;
        Histogram(h)
    };

    #[test]
    fn simple_threshold() {
        assert_eq!(
            (
                Threshold::no_bigger_than(99).allows(H1),
                Threshold::no_bigger_than(100).allows(H1)
            ),
            (false, true)
        );
    }

    #[test]
    fn exact_fit() {
        assert!(Threshold::new([(1, 30), (10, 5), (50, 1), (100, 1)]).allows(H1));
    }

    #[test]
    fn almost_exact_fit() {
        // fails because not allowing two in the 50-100 range
        assert!(!Threshold::new([(1, 30), (10, 5), (100, 1)]).allows(H1));
    }

    #[test]
    fn total_count() {
        assert_eq!(
            (
                Threshold::new([(100, 36)]).allows(H1),
                Threshold::new([(100, 37)]).allows(H1)
            ),
            (false, true)
        );
    }
}

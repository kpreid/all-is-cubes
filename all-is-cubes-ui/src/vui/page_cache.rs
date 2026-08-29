use core::fmt;

use descriptive_unwrap::OptionExt as _;
use hashbrown::HashMap;

use crate::vui::{Page, PageInst};

// -------------------------------------------------------------------------------------------------

/// Storage of the resources required to display individual UI [`Page`]s.
///
/// # Generic parameters
///
/// * `K` is a key used to identify pages for reuse.
// TODO: this or its successor should be in the public API, but isn’t clean yet.
pub(crate) struct PageCache<K> {
    /// Storage for keys with [`PageCacheRetention::Forever`].
    //---
    // TODO: Separately store the pages and the spaces / PageInstCaches, so that we can
    // efficiently flush the cache of spaces without losing the page definitions?
    forever: HashMap<K, PageInst>,

    /// Single-element storage for keys with [`PageCacheRetention::WhenUnused`].
    once: Option<(K, PageInst)>,
}

impl<K: PageCacheKey> PageCache<K> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            forever: HashMap::new(),
            once: None,
        }
    }

    #[must_use]
    pub fn get_mut(&mut self, key: K, factory: impl FnOnce() -> Page) -> &mut PageInst {
        match key.retention() {
            PageCacheRetention::Forever => {
                self.forever.entry(key).or_insert_with(|| PageInst::new(factory()))
            }
            PageCacheRetention::WhenUnused => {
                match self.once {
                    Some((ref cached_key, _)) if *cached_key == key => {
                        // TODO: when the new borrow checker Polonius Alpha is stable (see
                        // <https://blog.rust-lang.org/2026/08/04/enabling-polonius-alpha-on-nightly/>),
                        // we can borrow from the above pattern match instead of unwrapping.
                        &mut self.once.as_mut().none_is_unreachable().1
                    }
                    _ => &mut self.once.insert((key, PageInst::new(factory()))).1,
                }
            }
        }
    }
}

impl<K> PageCache<K> {
    fn len(&self) -> usize {
        self.forever.len().saturating_add(self.once.iter().len())
    }
}

impl<K: fmt::Debug> fmt::Debug for PageCache<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            forever: _,
            once: _,
        } = self;
        f.debug_struct("PageCache").field("len", &self.len()).finish_non_exhaustive()
    }
}

pub(crate) trait PageCacheKey: Eq + core::hash::Hash {
    /// Controls how long the page and space displaying the page will be kept around.
    ///
    /// This value must be consistent for each key (in the same way as hashes must be).
    fn retention(&self) -> PageCacheRetention;
}

pub(crate) enum PageCacheRetention {
    /// Keep this page forever.
    Forever,

    /// Keep this page until it is no longer visible.
    /// (More precisely, until another page of the same retention is requested.)
    WhenUnused,
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
    enum TestKey {
        FOne,
        WuOne,
        WuTwo,
    }

    impl PageCacheKey for TestKey {
        fn retention(&self) -> PageCacheRetention {
            match self {
                TestKey::FOne => PageCacheRetention::Forever,
                TestKey::WuOne | TestKey::WuTwo => PageCacheRetention::WhenUnused,
            }
        }
    }

    fn dummy_factory() -> Page {
        Page::empty()
    }

    #[test]
    fn when_unused_does_not_accumulate() {
        let mut cache = PageCache::new();
        _ = cache.get_mut(TestKey::FOne, dummy_factory);
        assert_eq!(cache.len(), 1);
        _ = cache.get_mut(TestKey::WuOne, dummy_factory);
        assert_eq!(cache.len(), 2);
        _ = cache.get_mut(TestKey::WuTwo, dummy_factory);
        assert_eq!(cache.len(), 2, "still two, not three");
        _ = cache.get_mut(TestKey::FOne, dummy_factory);
        assert_eq!(cache.len(), 2);
    }
}

/// Single-entry cache.
#[derive(Clone, Debug)]
pub(crate) struct Memo<K, V> {
    data: Option<(K, V)>,
}

impl<K: Eq, V> Memo<K, V> {
    pub fn new() -> Self {
        Self { data: None }
    }

    pub fn get_or_insert(&mut self, key: K, value_fn: impl FnOnce() -> V) -> &mut V
    where
        K: Copy,
    {
        match &mut self.data {
            Some((k, v)) => {
                if *k == key {
                    v
                } else {
                    *v = value_fn();
                    *k = key;
                    v
                }
            }
            d @ None => {
                let (_k, v) = d.insert((key, value_fn()));
                v
            }
        }
    }

    pub(crate) fn get(&self) -> Option<&V> {
        self.data.as_ref().map(|(_k, v)| v)
    }

    /// Drop the stored value, if any.
    #[cfg_attr(not(feature = "rerun"), expect(unused))] // currently not otherwise used
    pub fn clear(&mut self) {
        self.data = None;
    }
}

impl<K, V> Default for Memo<K, V> {
    fn default() -> Self {
        Self { data: None }
    }
}

use std::collections::{BTreeMap, btree_map};

use async_fn_traits::{AsyncFn1, AsyncFn2};
use futures_core::future::BoxFuture;
use itertools::Itertools as _;

use crate::{RenderTestContext, UniverseFuture};

// -------------------------------------------------------------------------------------------------

#[expect(missing_debug_implementations)]
pub struct TestCaseCollector<'a>(pub &'a mut BTreeMap<String, TestCase>);

impl TestCaseCollector<'_> {
    #[track_caller]
    pub fn insert<F>(
        &mut self,
        name: &str,
        universe_source: Option<UniverseFuture>,
        test_function: F,
    ) where
        F: AsyncFn1<RenderTestContext, Output = (), OutputFuture: Send>
            + Send
            + Sync
            + Clone
            + 'static,
    {
        match self.0.entry(name.to_owned()) {
            btree_map::Entry::Vacant(e) => {
                let boxed_test_function = Box::new(move |context| {
                    let test_function = test_function.clone(); // usually a fn pointer
                    let boxed_future: BoxFuture<'static, ()> = Box::pin(async move {
                        test_function(context).await;
                    });
                    boxed_future
                });
                let test_case = TestCase {
                    function: boxed_test_function,
                    universe_source,
                };
                e.insert(test_case);
            }
            btree_map::Entry::Occupied(_) => {
                panic!("Duplicate test name {name:?}")
            }
        }
    }

    /// Generate an independent test case for each item of `values`.
    /// The items must serialize to strings.
    #[allow(clippy::ref_option)]
    pub fn insert_variants<I, F>(
        &mut self,
        name: &str,
        universe_source: &Option<UniverseFuture>,
        test_function: F,
        values: I,
    ) where
        I: IntoIterator<Item: serde::Serialize + Clone + Send + Sync + 'static>,
        F: AsyncFn2<RenderTestContext, <I as IntoIterator>::Item, Output = (), OutputFuture: Send>
            + Send
            + Sync
            + Clone
            + 'static,
    {
        for variant_value in values {
            let test_function = test_function.clone();
            // TODO: serde_json is overkill -- consider using some of the stuff Settings uses
            let variant_serialized: serde_json::Value =
                serde_json::to_value(&variant_value).unwrap();
            let variant_string = stringify_variant(&variant_serialized);
            self.insert(
                &format!("{name}-{variant_string}"),
                universe_source.clone(),
                move |context| {
                    let variant_value = variant_value.clone();
                    test_function(context, variant_value)
                },
            );
        }
    }
}

/// Convert test variant data to a string.
///
/// It may not contain any JSON objects, and the result does not preserve
/// nested array structure or strings containing "-" versus separate strings.
fn stringify_variant(variant: &serde_json::Value) -> String {
    use serde_json::Value;
    match variant {
        Value::Null | Value::Bool(_) | Value::Number(_) => variant.to_string(),
        Value::String(s) => s.clone(),
        Value::Array(a) => a.iter().map(stringify_variant).join("-"),
        Value::Object(_) => panic!("objects not allowed in stringify_variant()"),
    }
}

// -------------------------------------------------------------------------------------------------

type BoxedTestFn = Box<dyn Fn(RenderTestContext) -> BoxFuture<'static, ()> + Send + Sync>;

/// Implementation of a particular test case (unique [`TestId`] stored externally).
pub struct TestCase {
    pub function: BoxedTestFn,
    pub universe_source: Option<UniverseFuture>,
}

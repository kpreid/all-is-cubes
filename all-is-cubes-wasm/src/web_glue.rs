//! DOM and JS environment manipulation that isn't application-specific.

use std::sync::LazyLock;
use std::sync::Mutex;

use futures_core::future::BoxFuture;
use js_sys::{Error, Function};
use wasm_bindgen::JsCast; // dyn_into()
use wasm_bindgen::prelude::Closure;
use web_sys::{AddEventListenerOptions, Document, Element, Event, EventTarget, Text};
use web_time::{Duration, Instant};

// -------------------------------------------------------------------------------------------------

/// Generate a uniform random [`u64`] (without incurring a `getrandom` dependency).
pub fn pseudorandom_u64() -> u64 {
    // Math::random() will get us 56 random bits per call, but we want 64, so call it twice.
    let [high, low]: [u64; 2] =
        core::array::from_fn(|_| (js_sys::Math::random() * f64::from(u32::MAX)) as u64);
    (high << 32) ^ low
}

// -------------------------------------------------------------------------------------------------

pub fn get_mandatory_element<E: JsCast>(document: &Document, id: &'static str) -> Result<E, Error> {
    document
        .get_element_by_id(id)
        .ok_or_else(|| Error::new(&format!("missing element {id:?}")))?
        .dyn_into::<E>()
        .map_err(|_| {
            Error::new(&format!(
                "element {id:?} was not a {:?}",
                std::any::type_name::<E>()
            ))
        })
}

pub fn replace_children_with_one_text_node(element: &Element) -> Text {
    let existing_string: Option<String> = element.text_content();
    let text = Text::new().unwrap();
    if let Some(s) = existing_string {
        text.set_data(&s);
    }
    element.set_text_content(None);
    element.append_child(text.as_ref()).unwrap();
    text
}

pub fn add_event_listener<E, F>(
    target: &EventTarget,
    event_type: &str,
    listener: F,
    options: &AddEventListenerOptions,
) where
    E: JsCast,
    F: Fn(E) + 'static,
{
    let closure: Closure<dyn Fn(Event)> = Closure::wrap(Box::new(move |event: Event| {
        listener(event.dyn_into::<E>().unwrap())
    }));
    target
        .add_event_listener_with_callback_and_add_event_listener_options(
            event_type,
            closure.as_ref().unchecked_ref(),
            options,
        )
        .expect("addEventListener failure");
    closure.forget(); // TODO: Instead return the closure or some other kind of handle
}

// -------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub(crate) struct Executor;

impl all_is_cubes::util::Executor for Executor {
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> BoxFuture<'static, ()>) {
        wasm_bindgen_futures::spawn_local(task_factory())
    }

    fn yield_now(&self) -> BoxFuture<'static, ()> {
        Box::pin(yield_to_event_loop())
    }
}

// -------------------------------------------------------------------------------------------------

/// Yield to the browser's event loop (if significant time has passed since the last call).
pub(crate) async fn yield_to_event_loop() {
    // Check whether it's worth yielding.
    {
        let now = Instant::now();
        let mut next = NEXT_YIELD_INSTANT.lock().unwrap();
        if now > *next {
            // Set the next yield time.
            // TODO: Make this configurable. Right now, we only use this for initial startup,
            // but later there might be uses for during gameplay in which case we want much finer
            // scheduling
            *next = now + Duration::from_millis(32);
        } else {
            // Don't yield.
            return;
        }
    }

    // Scope to force non-Send things to be dropped.
    let receiver = {
        let (sender, receiver) = futures_channel::oneshot::channel();

        let send_closure: Function = Closure::once_into_js(Box::new(move || {
            let _ = sender.send(());
        }))
        .dyn_into()
        .unwrap();

        // TODO: setTimeout is a lousy way to yield because it has minimum delays. Build a better one.
        web_sys::window()
            .unwrap()
            .set_timeout_with_callback_and_timeout_and_arguments_0(&send_closure, 0)
            .unwrap();

        receiver
    };

    let _ = receiver.await;
}

/// Time used by [`yield_to_event_loop`] to decude whether to actually yield.
/// TODO: A thread-local would be a better expression of intent here.
static NEXT_YIELD_INSTANT: LazyLock<Mutex<Instant>> = LazyLock::new(|| Mutex::new(Instant::now()));

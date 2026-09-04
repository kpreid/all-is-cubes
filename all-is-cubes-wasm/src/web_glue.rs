//! DOM and JS environment manipulation that isn't application-specific.

use std::fmt;
use std::sync::LazyLock;
use std::sync::Mutex;

use futures_core::future::BoxFuture;
use js_sys::{Error, Function, JsString};
use wasm_bindgen::prelude::{Closure, wasm_bindgen};
use wasm_bindgen::{JsCast, JsValue}; // dyn_into()
use web_sys::{
    AddEventListenerOptions, Document, Element, Event, EventTarget, Text, Window, console,
};
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

/// Install an event listener which downcasts the event to the JavaScript type `E`.
pub fn add_event_listener<E, F>(
    target: &EventTarget,
    event_type: &'static str,
    listener: F,
    options: &AddEventListenerOptions,
) where
    E: JsCast,
    F: Fn(E) + 'static,
{
    let closure: Closure<dyn Fn(Event)> =
        Closure::wrap(Box::new(move |event: Event| match event.dyn_into::<E>() {
            Ok(event) => listener(event),
            // In the event that, for some reason, we receive an event that does not cast to the
            // expected type, don’t panic; instead, discard it. This way, the application can continue
            // functioning if a non-critical event misbehaves.
            //
            // We use `console` instead of `log` in order to allow JS object inspection instead of
            // stringifying it.
            Err(event) => console::error_2(
                &JsString::from(format!(
                    "event listener for {event_type:?} received event that \
                        does not cast to {expected_type}: %o",
                    expected_type = std::any::type_name::<E>(),
                )),
                &event,
            ),
        }));
    match target.add_event_listener_with_callback_and_add_event_listener_options(
        event_type,
        closure.as_ref().unchecked_ref(),
        options,
    ) {
        Ok(()) => {}
        Err(error) => {
            // Not sure why this would ever fail, but log it if it does.
            console::error_3(
                &JsString::from(format!(
                    "failed to install event listener for {event_type:?} on target %o: %o",
                )),
                target,
                &error,
            )
        }
    }
    closure.forget(); // TODO: Instead return the closure or some other kind of handle
}

// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub(crate) struct Executor {
    pub(crate) window: Window,
}

impl all_is_cubes::util::Executor for Executor {
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> BoxFuture<'static, ()>) {
        wasm_bindgen_futures::spawn_local(task_factory())
    }

    fn yield_now(&self) -> BoxFuture<'static, ()> {
        Box::pin(yield_to_event_loop(self.window.clone()))
    }
}

// -------------------------------------------------------------------------------------------------

/// Yield to the browser's event loop (if significant time has passed since the last call).
///
/// Design note: This function could take `&Window`, but the current limitations of the
/// `yield-progress` library, due to the current limitations of expressing async functions
/// with boxed futures in stable Rust, mean that this function must not borrow anything.
pub(crate) async fn yield_to_event_loop(window: Window) {
    // Check whether it's worth yielding.
    {
        let now = Instant::now();
        let mut next = match NEXT_YIELD_INSTANT.lock() {
            Ok(guard) => guard,
            // state cannot be insterestingly corrupted, so ignore poison
            Err(poison) => poison.into_inner(),
        };
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

    // Scope to keep non-Send things out of the future state.
    let receiver = {
        let (sender, receiver) = futures_channel::oneshot::channel();

        let send_closure: Function = Closure::once_into_js(Box::new(move || {
            let _ = sender.send(());
        }))
        .unchecked_into();

        // TODO: setTimeout is a lousy way to yield because it has minimum delays. Build a better one.
        window
            .set_timeout_with_callback_and_timeout_and_arguments_0(&send_closure, 0)
            .expect("setTimeout should not error as we are using it");

        receiver
    };

    let _ = receiver.await;
}

/// Time used by [`yield_to_event_loop`] to decude whether to actually yield.
/// TODO: A thread-local would be a better expression of intent here.
static NEXT_YIELD_INSTANT: LazyLock<Mutex<Instant>> = LazyLock::new(|| Mutex::new(Instant::now()));

// -------------------------------------------------------------------------------------------------

/// Attach context to a JS exception object and make it into a Rust error.
pub(crate) fn excontext(context: &'static str) -> impl Fn(JsValue) -> ErrorFromJs {
    move |exception| ErrorFromJs { context, exception }
}

/// Wrapper for a JS exception that we want to handle non-fatally.
///
/// This error type does not have an [`Error::source()`]; all details are in its own message.
#[derive(Clone, Debug)]
pub(crate) struct ErrorFromJs {
    /// What we were doing. Should make sense preceded by “while”.
    context: &'static str,
    /// The exception caught.
    exception: JsValue,
}

impl fmt::Display for ErrorFromJs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "caught exception while {context}: {exception}",
            context = self.context,
            exception = js_stringify(&self.exception),
        )
    }
}

impl core::error::Error for ErrorFromJs {}

#[wasm_bindgen(inline_js = "export function js_stringify(x) { return String(x); }")]
extern "C" {
    fn js_stringify(value: &JsValue) -> JsString;
}

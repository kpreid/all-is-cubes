// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::future::Future;

use js_sys::{Error, Function};
use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast; // dyn_into()
use web_sys::{AddEventListenerOptions, Document, Event, EventTarget};

pub fn get_mandatory_element<E: JsCast>(document: &Document, id: &'static str) -> Result<E, Error> {
    document
        .get_element_by_id(id)
        .ok_or_else(|| Error::new(&format!("missing element {:?}", id)))?
        .dyn_into::<E>()
        .map_err(|_| {
            Error::new(&format!(
                "element {:?} was not a {:?}",
                id,
                std::any::type_name::<E>()
            ))
        })
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

pub fn yield_to_event_loop() -> impl Future<Output = ()> + Send + Sync {
    let (sender, receiver) = futures_channel::oneshot::channel();

    let send_closure: Function = Closure::once_into_js(Box::new(move || {
        let _ = sender.send(());
    }))
    .dyn_into().unwrap();

    // TODO: setTimeout is a lousy way to yield because it has minimum delays. Build a better one.
    web_sys::window()
        .unwrap()
        .set_timeout_with_callback_and_timeout_and_arguments_0(&send_closure, 0)
        .unwrap();

    async move {
        let _ = receiver.await;
    }
}

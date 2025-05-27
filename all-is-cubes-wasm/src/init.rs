//! Startup; going from a JS function call to a running session.

use std::sync::Arc;

use js_sys::Error;
use send_wrapper::SendWrapper;
use wasm_bindgen::prelude::{JsValue, wasm_bindgen};

use web_sys::{Document, console};

use all_is_cubes_gpu::in_wgpu;

use crate::js_bindings::{GuiHelpers, make_all_static_gui_helpers};
use crate::url_params::{OptionsInUrl, RendererOption, options_from_query_string};
use crate::web_glue::yield_to_event_loop;
use crate::web_session::{StaticDom, WebRenderer, WebSession, create_session};

/// Entry point for normal game-in-a-web-page operation.
#[wasm_bindgen]
pub async fn start_game() -> Result<(), JsValue> {
    // Note: This used to be in a `#[wasm_bindgen(start)]` function, but that stopped working.
    // Rather than stop to figure out what went wrong even though I Didn't Change Anything,
    // I moved it here since this is our sole entry point in practice.
    console_error_panic_hook::set_once();

    // Initialize logging via the `log` crate's interface.
    // We use `console_log` to perform the actual logging, but it doesn't offer a message source
    // filter, so we have to do that ourselves.
    log::set_logger({
        struct FilteredWebLogger;
        impl log::Log for FilteredWebLogger {
            fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
                let t = metadata.target();
                // Trace is the finest level, so no need to check it
                /* metadata.level() <= log::LevelFilter::Trace && */
                !t.starts_with("wgpu") && !t.starts_with("winit") && !t.starts_with("naga")
            }
            fn log(&self, record: &log::Record<'_>) {
                if self.enabled(record.metadata()) {
                    console_log::log(record);
                }
            }
            fn flush(&self) {}
        }
        &FilteredWebLogger
    })
    .unwrap();
    log::set_max_level(log::LevelFilter::Trace);

    let window = web_sys::window().expect("missing `window`");
    let document = window.document().expect("missing `document`");

    // TODO: StaticDom and GuiHelpers are the same kind of thing. Merge them?
    let gui_helpers = make_all_static_gui_helpers(window, document.clone());
    let static_dom = StaticDom::new(&document)?;
    {
        let list = static_dom.app_root.class_list();
        list.remove_1("state-script-not-loaded").unwrap();
        list.add_1("state-loading").unwrap();
    }

    // This function split is basically a `try {}` if that were stable.
    match start_game_with_dom(document, gui_helpers, &static_dom).await {
        Ok(()) => Ok(()),
        // TODO: log errors with details into the console and loading_log.
        Err(error) => {
            let formatted = format!(
                "\n--- ERROR WHILE LOADING ---\n{error}",
                error = all_is_cubes::util::ErrorChain(&*error)
            );
            static_dom.append_to_loading_log(&formatted);
            console::error_1(&JsValue::from(formatted));
            Err(Error::new(&format!(
                "Error occurred during loading (further details may be in log): {error}"
            ))
            .into())
        }
    }
}

/// Inner helper of `start_game` which returns an error to be nicely logged, instead of
/// raw [`JsValue`] exception type.
async fn start_game_with_dom(
    document: Document,
    gui_helpers: GuiHelpers,
    static_dom: &StaticDom,
) -> Result<(), Box<dyn core::error::Error>> {
    let progress = yield_progress::Builder::new()
        .yield_using(|_| yield_to_event_loop())
        .progress_using({
            let progress_bar = SendWrapper::new(static_dom.progress_bar.clone());
            // TODO: hook up label
            move |info| progress_bar.set_value(info.fraction().into())
        })
        .build();
    let [app_progress, progress] = progress.split(0.1);
    let [universe_progress, post_universe_progress] = progress.split(0.98);

    let query_string: String = document
        .location()
        .map_or_else(String::new, |q| q.search().unwrap_or_default());
    let OptionsInUrl {
        template,
        seed,
        renderer: renderer_option,
    } = options_from_query_string(query_string.trim_start_matches('?').as_bytes());

    static_dom.append_to_loading_log("\nInitializing application...");
    app_progress.progress(0.2).await;
    let settings = crate::settings::load_settings_from_local_storage_if_possible();
    let (session, viewport_cell, fullscreen_cell) = create_session(&gui_helpers, settings).await;

    static_dom.append_to_loading_log("\nInitializing graphics...");
    app_progress.progress(0.4).await;

    let cameras = session.create_cameras(viewport_cell.as_source());
    let renderer = match renderer_option {
        RendererOption::Wgpu => {
            let canvas = gui_helpers.canvas_helper().canvas();
            let (surface, adapter) = match init_wgpu(&canvas, wgpu::Backends::all()).await {
                Ok(sa) => Ok(sa),
                Err(e) => {
                    // Explicitly retrying with WebGL works around a Firefox quirk where
                    // WebGPU appears to exist but getContext() returns null.
                    // In principle, wgpu should handle this for us, but it's nontrivial.
                    // <https://github.com/gfx-rs/wgpu/issues/5332>
                    log::debug!(
                        "failed to create surface with Backends::all(); trying WebGL only.\n\
                        error: {e}"
                    );
                    init_wgpu(&canvas, wgpu::Backends::GL).await
                }
            }?;
            log::debug!("adapter: {:?}", adapter.get_info());
            let renderer = in_wgpu::SurfaceRenderer::new(
                cameras,
                surface,
                adapter,
                Arc::new(crate::web_glue::Executor),
            )
            .await?;
            WebRenderer::Wgpu(renderer)
        }
    };

    static_dom.append_to_loading_log("\nStarting game loop...");
    app_progress.progress(0.8).await;
    let root = WebSession::new(
        gui_helpers,
        static_dom.clone(),
        session,
        renderer,
        viewport_cell,
        fullscreen_cell,
    );
    root.start_loop();

    static_dom.append_to_loading_log("\nConstructing universe...");
    app_progress.finish().await;
    let universe = template
        .build::<crate::AdaptedInstant>(
            universe_progress,
            all_is_cubes_content::TemplateParameters {
                seed: Some(seed.unwrap_or_else(|| {
                    let rand_seed: u64 = crate::web_glue::pseudorandom_u64();
                    log::info!("Randomly chosen universe seed: {rand_seed}");
                    rand_seed
                })),
                size: None,
            },
        )
        .await
        .expect("universe template error");
    root.set_universe(*universe);

    // Explicitly keep the game loop alive.
    Box::leak(Box::new(root));

    // Do the final UI cleanup going from "loading" to "running".
    post_universe_progress.finish().await;
    {
        // TODO: make this part the WebSession's responsibility? Move the class list manip to StaticDom?
        let list = static_dom.app_root.class_list();
        list.remove_1("state-loading").unwrap();
        list.add_1("state-fully-loaded").unwrap();
    }
    console::log_1(&JsValue::from_str("start_game() completed."));
    static_dom.loading_log.set_data("");
    Ok(())
}

async fn init_wgpu(
    canvas: &web_sys::HtmlCanvasElement,
    backends: wgpu::Backends,
) -> Result<(wgpu::Surface<'static>, wgpu::Adapter), Box<dyn core::error::Error>> {
    let (surface, request_adapter_future) = {
        let wgpu_instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
            backends,
            ..Default::default()
        });
        let surface = wgpu_instance
            .create_surface(wgpu::SurfaceTarget::Canvas(canvas.clone()))
            .map_err(|e| format!("requesting {backends:?} context failed: {e:?}"))?;
        let request_adapter_future = wgpu_instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        });
        (surface, request_adapter_future)
    };
    let adapter = request_adapter_future.await?;
    Ok((surface, adapter))
}

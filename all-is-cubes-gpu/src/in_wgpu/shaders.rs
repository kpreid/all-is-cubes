//! Hot-reloadable shader loading.

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::string::String;
use alloc::sync::Arc;
use core::task;
use std::sync::LazyLock as Lazy;

use futures_core::future::BoxFuture;

use all_is_cubes::listen;

use crate::Identified;
use crate::reloadable::{Reloadable, reloadable_str};

/// All shaders that are built into the source code of this crate.
pub(crate) struct Shaders {
    pub(crate) blocks_and_lines: ReloadableShader,
    pub(crate) resampling: ReloadableShader,
    pub(crate) rt_copy: ReloadableShader,
    pub(crate) postprocess: ReloadableShader,
    #[cfg(feature = "rerun")]
    pub(crate) rerun_copy: ReloadableShader,

    modules_changed: listen::Notifier<()>,
}

static BLOCKS_AND_LINES: Lazy<Reloadable> =
    Lazy::new(|| reloadable_str!("src/in_wgpu/shaders/blocks-and-lines.wgsl"));
static RESAMPLING: Lazy<Reloadable> =
    Lazy::new(|| reloadable_str!("src/in_wgpu/shaders/resampling.wgsl"));
static RT_COPY: Lazy<Reloadable> =
    Lazy::new(|| reloadable_str!("src/in_wgpu/shaders/rt-copy.wgsl"));
static POSTPROCESS: Lazy<Reloadable> =
    Lazy::new(|| reloadable_str!("src/in_wgpu/shaders/postprocess.wgsl"));
#[cfg(feature = "rerun")]
static RERUN_COPY: Lazy<Reloadable> =
    Lazy::new(|| reloadable_str!("src/in_wgpu/shaders/rerun-copy.wgsl"));

impl Shaders {
    pub fn new(device: &wgpu::Device) -> Self {
        Self {
            blocks_and_lines: ReloadableShader::new(
                device,
                "blocks_and_lines".into(),
                BLOCKS_AND_LINES.as_source(),
            ),
            resampling: ReloadableShader::new(device, "resampling".into(), RESAMPLING.as_source()),
            rt_copy: ReloadableShader::new(device, "rt_copy".into(), RT_COPY.as_source()),
            postprocess: ReloadableShader::new(
                device,
                "postprocess".into(),
                POSTPROCESS.as_source(),
            ),
            #[cfg(feature = "rerun")]
            rerun_copy: ReloadableShader::new(device, "rerun_copy".into(), RERUN_COPY.as_source()),

            modules_changed: listen::Notifier::new(),
        }
    }

    pub fn update(&mut self, device: &wgpu::Device) {
        // TODO: use a shared dirty flag to reduce work?

        let Self {
            blocks_and_lines,
            resampling,
            rt_copy,
            postprocess,
            #[cfg(feature = "rerun")]
            rerun_copy,
            modules_changed,
        } = self;
        let mut changed = false;

        changed |= blocks_and_lines.update(device);
        changed |= resampling.update(device);
        changed |= rt_copy.update(device);
        changed |= postprocess.update(device);
        #[cfg(feature = "rerun")]
        {
            changed |= rerun_copy.update(device);
        }

        if changed {
            modules_changed.notify(&());
        }
    }
}

impl listen::Listen for Shaders {
    type Msg = ();
    type Listener = <listen::Notifier<Self::Msg> as listen::Listen>::Listener;
    fn listen_raw(&self, listener: Self::Listener) {
        self.modules_changed.listen_raw(listener)
    }
}

/// Builds on [`Reloadable`] by compiling a shader module and catching errors.
///
/// The code on initial creation must be valid or creation will panic.
pub(crate) struct ReloadableShader {
    label: String,
    source: listen::DynSource<Arc<str>>,
    dirty: listen::Flag,
    current_module: Identified<wgpu::ShaderModule>,
    next_module: Option<BoxFuture<'static, Result<wgpu::ShaderModule, wgpu::Error>>>,
}

impl ReloadableShader {
    fn new(device: &wgpu::Device, label: String, wgsl_source: listen::DynSource<Arc<str>>) -> Self {
        let dirty = listen::Flag::listening(false, &wgsl_source);
        let current_module =
            Identified::new(device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some(&label),
                source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(&*wgsl_source.get())),
            }));

        Self {
            label,
            source: wgsl_source,
            dirty,
            current_module,
            next_module: None,
        }
    }

    pub fn get(&self) -> &Identified<wgpu::ShaderModule> {
        &self.current_module
    }

    /// For shader testing.
    pub(crate) fn get_source_text(&self) -> Arc<str> {
        self.source.get()
    }

    /// Returns `true` if the current module changed.
    fn update(&mut self, device: &wgpu::Device) -> bool {
        if self.dirty.get_and_clear() {
            device.push_error_scope(wgpu::ErrorFilter::Validation);
            let new_module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some(&self.label),
                source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(&*self.source.get())),
            });
            let error_future = device.pop_error_scope();

            let module_future = async {
                match error_future.await {
                    None => Ok(new_module),
                    Some(error) => Err(error),
                }
            };

            // If on wasm, wgpu's types aren't Send but we won't be using threads
            #[cfg(target_family = "wasm")]
            let module_future = send_wrapper::SendWrapper::new(module_future);

            self.next_module = Some(Box::pin(module_future));
        }

        if let Some(f) = self.next_module.as_mut()
            && let task::Poll::Ready(result) =
                f.as_mut().poll(&mut task::Context::from_waker(task::Waker::noop()))
        {
            self.next_module = None;
            match result {
                Ok(new_module) => {
                    self.current_module = Identified::new(new_module);
                    true
                }
                Err(e) => {
                    log::error!(
                        "Error encountered while reloading shader:\n{}",
                        all_is_cubes::util::ErrorChain(&e)
                    );
                    false
                }
            }
        } else {
            false
        }
    }
}

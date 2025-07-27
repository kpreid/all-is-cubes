//! [`send_wrapper::SendWrapper`], but only on wasm where no threads currently exist.
//!
//! On wasm, [`wgpu`] is not [`Send`], but for the same reason, we never use any other threads,
//! so a `SendWrapper` will let the GPU resources be [`Send`].

cfg_select! {
    target_family = "wasm" => {
        use send_wrapper::SendWrapper;

        pub(crate) type Msw<T> = SendWrapper<T>;
    }
    _ => {
        /// A substitute for [`send_wrapper::SendWrapper`] which doesn't have any of its
        /// special properties or overhead — suitable to use when the inner type *is* [`Send`].
        #[derive(Clone, Copy, Debug, Default)]
        pub(crate) struct Msw<T>(T);

        impl<T> Msw<T> {
            pub const fn new(value: T) -> Self {
                Self(value)
            }
        }

        impl<T> core::ops::Deref for Msw<T> {
            type Target = T;
            fn deref(&self) -> &T {
                &self.0
            }
        }
        impl<T> core::ops::DerefMut for Msw<T> {
            fn deref_mut(&mut self) -> &mut T {
                &mut self.0
            }
        }
    }
}

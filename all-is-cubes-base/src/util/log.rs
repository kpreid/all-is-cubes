/// Provides the recommended log filter for programs which want to exclude particularly noisy
/// details of All is Cubes’s dependencies.
///
/// The guiding principle for this filtering is that at [`log::Level::Debug`] or lower level,
/// there should be no messages produced every frame/step unless something is wrong.
#[allow(clippy::missing_inline_in_public_items)]
pub fn standard_filter(metadata: &log::Metadata<'_>) -> bool {
    let target = metadata.target();

    // TODO: These filters may be stronger than necessary, because they used to be implemented
    // as what `simplelog` offered as configuration.
    // In particular, we should consider making the filtering dependent on log level rather than
    // filtering by module regardless of level.

    !(target.starts_with("tracing::span")  // logs every single tracing span
        || target.starts_with("wgpu") // noisy
        || target.starts_with("naga::") // noisy
        || target.starts_with("winit::") // noisy at Debug level since 0.30
        || target.starts_with("h2::") // used by Rerun and noisy at Debug level since 0.23
        || target.starts_with("re")) // Rerun
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn filter() {
        assert!(!standard_filter(
            &log::Metadata::builder().target("tracing::span::active").build()
        ));
        assert!(standard_filter(
            &log::Metadata::builder()
                .target("all_is_cubes_mesh::dynamic::chunked_mesh")
                .build()
        ));
        assert!(standard_filter(
            &log::Metadata::builder().target("some_random_crate").build()
        ));
    }
}

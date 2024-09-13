//! URL parameter interpretation.
//!
//! This module is designed to be compilable for non-web targets for
//! testability.

use std::borrow::Borrow;
use std::borrow::Cow;
use std::collections::BTreeMap;

use all_is_cubes_content::UniverseTemplate;
use all_is_cubes_render::camera::GraphicsOptions;

#[derive(Clone, Debug, PartialEq)]
#[expect(clippy::derive_partial_eq_without_eq)]
pub struct OptionsInUrl {
    pub template: UniverseTemplate,
    pub graphics_options: GraphicsOptions,
    pub renderer: RendererOption,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum RendererOption {
    #[default]
    Wgpu,
}

/// Parse the given URL query string (without leading "?") to obtain configuration parameters.
pub fn options_from_query_string(query_string: &[u8]) -> OptionsInUrl {
    let params: BTreeMap<Cow<'_, str>, Cow<'_, str>> =
        form_urlencoded::parse(query_string).collect();
    options_from_pairs(&params)
}

/// Given key-value pairs from the URL query string, return game options.
pub fn options_from_pairs<S>(params: &BTreeMap<S, S>) -> OptionsInUrl
where
    S: Ord + Borrow<str>,
{
    OptionsInUrl {
        template: params
            .get("template")
            .and_then(|s| {
                let s = s.borrow();
                let t = s.parse::<UniverseTemplate>();
                if t.is_err() {
                    log::warn!("Unrecognized value for template=: {:?}", s);
                }
                t.ok()
            })
            .unwrap_or_default(),
        graphics_options: GraphicsOptions::default(), // TODO: offer graphics options
        renderer: params
            .get("renderer")
            .and_then(|s| {
                let s = s.borrow();
                match s {
                    "wgpu" => Some(RendererOption::Wgpu),
                    _ => {
                        log::warn!("Unrecognized value for renderer=: {:?}", s);
                        None
                    }
                }
            })
            .unwrap_or_default(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_no_params() {
        assert_eq!(
            options_from_query_string(b""),
            OptionsInUrl {
                template: UniverseTemplate::default(),
                graphics_options: GraphicsOptions::default(),
                renderer: RendererOption::Wgpu,
            },
        )
    }

    #[test]
    fn parse_specified_template() {
        assert_eq!(
            options_from_query_string(b"template=cornell-box").template,
            UniverseTemplate::CornellBox,
        )
    }

    #[test]
    fn parse_specified_renderer() {
        assert_eq!(
            options_from_query_string(b"renderer=wgpu").renderer,
            RendererOption::Wgpu,
        )
    }
}

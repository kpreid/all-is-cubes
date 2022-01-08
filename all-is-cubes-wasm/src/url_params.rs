// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! URL parameter interpretation.
//!
//! This module is designed to be compilable for non-web targets for
//! testability.

use std::borrow::Borrow;
use std::borrow::Cow;
use std::collections::BTreeMap;

use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes_content::UniverseTemplate;

#[derive(Clone, Debug, PartialEq)]
pub struct OptionsInUrl {
    pub template: UniverseTemplate,
    pub graphics_options: GraphicsOptions,
}

/// Parse the given URL query string (without leading "?") to obtain configuration parameters.
pub fn options_from_query_string(query_string: &[u8]) -> OptionsInUrl {
    let params: BTreeMap<Cow<'_, str>, Cow<'_, str>> =
        form_urlencoded::parse(query_string).collect();
    options_from_pairs(params)
}

/// Given key-value pairs from the URL query string, return game options.
pub fn options_from_pairs<S>(params: BTreeMap<S, S>) -> OptionsInUrl
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
}

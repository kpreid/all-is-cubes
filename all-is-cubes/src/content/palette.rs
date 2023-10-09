//! Colors to use in the UI and default content.
//!
//! This module exists to be a place where we can review the different colors in use
//! and tweak them to go well together, and avoid introducing extra slightly different
//! hardcoded colors if possible.
//!
//! TODO: Split "system UI" colors and "demo content" colors.

// Implementation notes:
//
// Most of these colors are defined in sRGB because that way, they can be directly viewed
// in the documentation (using only macro_rules for the generation). However, All is Cubes
// internally uses strictly linear color space.
//
// 0xBB is the sRGB value approximating linear value 0.5.

use crate::math::Rgb;

/// Define a color constant and preview it in the documentation.
macro_rules! palette_entry {
    (
        $( #[doc = $doc:literal] )*
        $name:ident = srgb[$r:literal $g:literal $b:literal]
    ) => {
        #[doc = concat!(
            "<svg width='3.2em' height='3em' style='vertical-align: top; float: left; clear: left; padding-right: .2em;'>",
            "<rect width='100%' height='100%' fill='rgb(", $r, ",", $g, ",", $b, ")' /></svg>"
        )]
        $( #[doc = $doc] )*
        pub const $name: $crate::math::Rgb = $crate::math::Rgb::from_srgb8([$r, $g, $b]);
    };
    (
        $( #[doc = $doc:literal] )*
        $name:ident = srgb[$r:literal $g:literal $b:literal $a:literal]
    ) => {
        // TODO: visualize alpha
        #[doc = concat!
            ("<svg width='3.2em' height='3em' style='vertical-align: top; float: left; clear: left; padding-right: .2em;'>",
            "<rect width='100%' height='100%' fill='rgb(", $r, ",", $g, ",", $b, ")' /></svg>"
        )]
        $( #[doc = $doc] )*
        pub const $name: $crate::math::Rgba = $crate::math::Rgba::from_srgb8([$r, $g, $b, $a]);
    };
}

/// Define many color constants. In the future, this might start defining an enum; I haven't decided.
macro_rules! palette {
    ( $(
        $( #[doc = $doc:literal] )*
        $name:ident = srgb $data:tt ;
    )* ) => {
        $( palette_entry! {
            $( #[doc = $doc] )*
            $name = srgb $data
        } )*
    }
}

palette! {
    /// Default sky color for new [`Space`](crate::space::Space)s.
    DAY_SKY_COLOR = srgb[243 243 255];

    /// Used in texture atlases to mark areas that should not be visible.
    UNALLOCATED_TEXELS_ERROR = srgb[0xFF 0x00 0xBB 0xFF];

    /// Used as a placeholder appearance when a block definition fails to evaluate.
    BLOCK_EVAL_ERROR = srgb[0xFF 0xBB 0x00 0xFF];

    /// Used when a recursive block definition should have provided a voxel color but did not.
    MISSING_VOXEL_ERROR = srgb[0xBB 0x00 0xFF 0xFF];

    /// Fill color to draw when a renderer does not have any [`Space`](crate::space::Space)
    /// to define a sky color.
    NO_WORLD_TO_SHOW = srgb[0xBC 0xBC 0xBC 0xFF];
}

palette! {
    // Physical material colors.
    /// A realistic value for typical black materials, which do not absorb all light.
    ALMOST_BLACK = srgb[0x3d 0x3d 0x3d];
    ///
    GRASS = srgb[0x61 0xAA 0x31];
    ///
    DIRT = srgb[0x6C 0x50 0x44];
    /// Generic unspecified some-kind-of-stone...
    STONE = srgb[0xD9 0xD7 0xD5];
    /// TODO: Not actually exercised in demo content yet
    TREE_BARK = srgb[0x93 0x5C 0x32];
    /// TODO: Not actually exercised in demo content yet
    TREE_LEAVES = srgb[0x61 0xAA 0x31];
    /// Some kind of metallic structure.
    ///
    /// TODO: not taken from real references
    STEEL = srgb[0xAA 0xAA 0xAA];
    /// Some kind of prepared wood.
    ///
    /// TODO: not taken from real references
    PLANK = srgb[0xE8 0xCC 0x95];
}

palette! {
    // All is Cubes logo components
    // TODO: Decide what we want *actual* logo(type) colors to be.
    LOGO_FILL = srgb[0xC7 0x33 0x78];
    LOGO_STROKE = srgb[0x33 0x33 0x33];
}

palette! {
    // UI elements
    CURSOR_OUTLINE = srgb[0x00 0x00 0x00 0xFF];
    /// Illumination color in the HUD.
    HUD_SKY = srgb[0xFF 0xFF 0xFF];
    HUD_TEXT_FILL = srgb[0x00 0x00 0x00 0xFF];
    HUD_TEXT_STROKE = srgb[0xFF 0xFF 0xFF 0xFF];
    HUD_TOOLBAR_BACK = srgb[0x7E 0x7E 0x7E 0xFF];
    HUD_TOOLBAR_FRAME = srgb[0xDD 0xDD 0xDD 0xFF];
    MENU_BACK = srgb[0xBC 0xBC 0xBC 0xFF];
    MENU_FRAME = srgb[0xFA 0xFA 0xFA 0xFF];
    BUTTON_FRAME = srgb[0x3d 0x3d 0x3d 0xFF];
    BUTTON_BACK = srgb[0xBC 0xBC 0xBC 0xFF];
    BUTTON_LABEL = srgb[0x3d 0x3d 0x3d 0xFF];
    BUTTON_ACTIVATED_BACK = srgb[0xE1 0xE1 0xE1 0xFF];
    BUTTON_ACTIVATED_LABEL = srgb[0x63 0x63 0x63 0xFF];
}
pub const BUTTON_ACTIVATED_GLOW: Rgb = rgb_const!(4.0, 0.8, 0.8); // not representable as integer srgb

palette! {
    // In-world debug UI elements (all wireframe)
    // TODO: these have no reason to be public
    DEBUG_BEHAVIOR_BOUNDS = srgb[0x00 0x70 0x00 0xFF];
    DEBUG_COLLISION_BOX = srgb[0x00 0x00 0xFF 0xFF];
    DEBUG_COLLISION_CUBES = srgb[0xFF 0x00 0x00 0xFF];
    DEBUG_CHUNK_MAJOR = srgb[0x00 0x00 0xE8 0xFF];
    DEBUG_CHUNK_MINOR = srgb[0x00 0xE8 0xE8 0xFF];
}

palette! {
    UNIFORM_LUMINANCE_RED = srgb[0x9E 0x00 0x00];
    UNIFORM_LUMINANCE_GREEN = srgb[0x00 0x59 0x00];
    UNIFORM_LUMINANCE_BLUE = srgb[0x00 0x00 0xFF];
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::Rgba;
    use exhaust::Exhaust as _;

    #[test]
    fn uniform_luminance_check() {
        fn optimize(channel: usize) -> [u8; 4] {
            // Blue is the primary color whose maximum intensity is darkest;
            // therefore it is the standard by which we check the other.
            let reference_luminance = UNIFORM_LUMINANCE_BLUE.luminance();
            let (_color, srgb, luminance_difference) = u8::exhaust()
                .map(|srgb_byte| {
                    let mut srgb = [0, 0, 0, 255];
                    srgb[channel] = srgb_byte;
                    let color = Rgba::from_srgb8(srgb);
                    (color, srgb, (color.luminance() - reference_luminance).abs())
                })
                .min_by(|a, b| a.2.total_cmp(&b.2))
                .unwrap();
            println!("best luminance difference = {luminance_difference}");
            srgb
        }

        println!("red:");
        assert_eq!(
            UNIFORM_LUMINANCE_RED.with_alpha_one().to_srgb8(),
            optimize(0)
        );
        println!("green:");
        assert_eq!(
            UNIFORM_LUMINANCE_GREEN.with_alpha_one().to_srgb8(),
            optimize(1)
        );
    }
}

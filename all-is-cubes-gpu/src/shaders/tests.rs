//! Tests of shader functions.

#![allow(trivial_casts)]

use naga_rust_embed::rt::{self, texture::ONE};

use all_is_cubes::euclid::{size2, vec2};
use all_is_cubes::math::{Rgb, ps32};
use all_is_cubes_render::camera::{self, Camera, GraphicsOptions, Viewport};

use super::{blocks_and_lines, postprocess};

// -------------------------------------------------------------------------------------------------

/// Build a resource struct adequate to execute [`blocks_and_lines`] shader functions.
fn bl_resources(camera: &Camera) -> blocks_and_lines::Resources<'static> {
    let dummy_3d = const {
        rt::texture::Texture3d {
            dimensions: rt::Vec3::new(ONE, ONE, ONE),
            mip_levels: ONE,
            data: &rt::texture::Constant::new(rt::Vec4::<f32>::ZERO) as _,
        }
    };
    let dummy_cube = const {
        rt::texture::TextureCube {
            dimensions: rt::Vec2::new(ONE, ONE),
            mip_levels: ONE,
            data: &rt::texture::Constant::new(rt::Vec4::<f32>::ZERO) as _,
        }
    };
    let dummy_3d_u32 = const {
        rt::texture::Texture3d {
            dimensions: rt::Vec3::new(ONE, ONE, ONE),
            mip_levels: ONE,
            data: &rt::texture::Constant::new(rt::Vec4::<u32>::ZERO) as _,
        }
    };
    let dummy_2d = const {
        rt::texture::Texture2d {
            dimensions: rt::Vec2::new(ONE, ONE),
            mip_levels: ONE,
            data: &rt::texture::Constant::new(rt::Vec4::<f32>::ZERO) as _,
        }
    };
    blocks_and_lines::Resources {
        camera: blocks_and_lines::ShaderSpaceCamera::from_camera(camera),
        light_texture: dummy_3d_u32,
        block_g0_reflectance: dummy_3d,
        block_g1_reflectance: dummy_3d,
        block_g1_emission: dummy_3d,
        skybox_texture: dummy_cube,
        skybox_sampler: rt::texture::Sampler,
        debug_font_texture: dummy_2d,
        block_linear_sampler: rt::texture::Sampler,
    }
}

/// Build a resource struct adequate to execute [`postprocess`] shader functions.
fn postprocess_resources() -> postprocess::Resources<'static> {
    let dummy_2d = const {
        rt::texture::Texture2d {
            dimensions: rt::Vec2::new(ONE, ONE),
            mip_levels: ONE,
            data: &rt::texture::Constant::new(rt::Vec4::<f32>::ZERO) as _,
        }
    };
    let dummy_2d_u = const {
        rt::texture::Texture2d {
            dimensions: rt::Vec2::new(ONE, ONE),
            mip_levels: ONE,
            data: &rt::texture::Constant::new(rt::Vec4::<u32>::ZERO) as _,
        }
    };

    postprocess::Resources {
        camera: postprocess::PostprocessUniforms::from_options(
            &GraphicsOptions::default(),
            Viewport::ARBITRARY,
            ps32(1.0),
            wgpu::SurfaceColorSpace::Srgb,
            vec2(1.0, 1.0),
            &crate::text::GpuFontMetrics {
                atlas_cell_size: size2(10, 10),
                cell_margin: 1,
            },
        ),
        linear_scene_texture: dummy_2d,
        scene_sampler: rt::Sampler,
        text_texture: dummy_2d_u,
        text_sampler: rt::Sampler,
        bloom_texture: dummy_2d,
        bloom_sampler: rt::Sampler,
        font_texture: dummy_2d,
    }
}

// -------------------------------------------------------------------------------------------------

#[test]
fn modulo_consistency() {
    for (dividend, divisor) in [(10.0_f32, 4.0), (-0.5, 4.0), (10.125, 1.0), (-1.0, 1.0)] {
        assert_eq!(
            dividend.rem_euclid(divisor),
            bl_resources(&Camera::new(
                GraphicsOptions::default(),
                Viewport::ARBITRARY
            ))
            .modulo(dividend, divisor),
            "{dividend} % {divisor}",
        )
    }
}

#[test]
fn scale_to_integer_step_consistency() {
    let camera = Camera::new(GraphicsOptions::default(), Viewport::ARBITRARY);
    let resources = bl_resources(&camera);

    for case @ (s, ds) in [(0.5f32, 0.25), (0.0, 0.25), (0.5, -0.125)] {
        assert_eq!(
            all_is_cubes::raycast::scale_to_integer_step(f64::from(s), f64::from(ds)) as f32,
            resources.scale_to_integer_step(s, ds),
            "{case:?}",
        )
    }
}

#[test]
fn fog_limits() {
    for fog_mode in [
        camera::FogOption::Abrupt,
        camera::FogOption::Compromise,
        camera::FogOption::Physical,
        camera::FogOption::None, // lack of fog is implemented separately
    ] {
        let mut options = GraphicsOptions::default();
        options.fog = fog_mode;
        let camera = Camera::new(options, Viewport::ARBITRARY);
        let resources = bl_resources(&camera);

        assert_eq!(resources.fog_combo(0.0), 0.0);
        assert_eq!(resources.fog_combo(1.0), 1.0);
    }
}

#[test]
fn encode_srgb_consistency() {
    let resources = postprocess_resources();

    for linear in [0.0_f32, 1.0, 0.5, 0.25, 0.125] {
        let shader_encoded = resources.encode_srgb(rt::Vec3::splat(linear)).x;
        let reference_encoded = Rgb::from_luminance(linear).with_alpha_one().to_srgb_float()[0];
        assert!(
            (shader_encoded - reference_encoded).abs() < 1e-6,
            "{linear:?} {shader_encoded:?} {reference_encoded:?}",
        )
    }
}

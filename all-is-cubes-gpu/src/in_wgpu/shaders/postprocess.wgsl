// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

// --- Interface declarations --------------------------------------------------

// Mirrors `struct ShaderPostprocessCamera` on the Rust side.
struct ShaderPostprocessCamera {
    [[location(8)]] tone_mapping_id: i32;
};


// This group is named postprocess_bind_group_layout in the code.
[[group(0), binding(0)]]
var text_texture: texture_2d<f32>;
[[group(0), binding(1)]]
var text_sampler: sampler;
[[group(0), binding(2)]]
var linear_scene_texture: texture_2d<f32>;
[[group(0), binding(3)]]
var<uniform> camera: ShaderPostprocessCamera;

// --- Vertex shader -----------------------------------------------------------

struct VertexOutput {
    [[builtin(position)]] clip_position: vec4<f32>;
    [[location(0)]] tc: vec4<f32>;
};

[[stage(vertex)]]
fn postprocess_vertex(
    [[builtin(vertex_index)]] in_vertex_index: u32,
) -> VertexOutput {
    /// Full-screen triangle
    let position = vec4<f32>(
        mix(-1.0, 3.0, f32(in_vertex_index == 1u)),
        mix(-1.0, 3.0, f32(in_vertex_index == 2u)),
        0.0,
        1.0
    );
    return VertexOutput(position, position);
}

// --- Fragment shader; doing the actual postprocessing work -------------------

fn luminance(linear_rgb: vec3<f32>) -> f32 {
  return dot(linear_rgb, vec3<f32>(0.2126, 0.7152, 0.0722));
}

fn tone_map(linear_rgb: vec3<f32>) -> vec3<f32> {
    switch (camera.tone_mapping_id) {
        default: { // or case 0
            // Clamp (implicitly)
            return linear_rgb;
        }
        case 1: {
            // Reinhard
            // TODO: Explain exactly which Reinhard, citation, etc
            return linear_rgb / (1.0 + luminance(linear_rgb));
        }
    }
}

[[stage(fragment)]]
fn postprocess_fragment(in: VertexOutput) -> [[location(0)]] vec4<f32> {
    // scale clip coordinates to 0.1 coordinates and flip Y
    let texcoord: vec2<f32> = in.tc.xy * vec2<f32>(0.5, -0.5) + 0.5;

    let scene_color: vec4<f32> = textureSampleLevel(
        linear_scene_texture,
        text_sampler,
        texcoord,
        0.0
    );
    // apply tone mapping, respecting premultiplied alpha
    let tone_mapped_scene = vec4<f32>(tone_map(scene_color.rgb * scene_color.a) / scene_color.a, scene_color.a);

    let derivatives = vec2<f32>(dpdx(texcoord.x), dpdy(texcoord.y));

    var shadowing: f32 = 0.0;
    let radius: i32 = 2;
    for (var dx: i32 = -radius; dx <= radius; dx = dx + 1) {
        for (var dy: i32 = -radius; dy <= radius; dy = dy + 1) {
            let offset: vec2<f32> = vec2<f32>(vec2<i32>(dx, dy));
            let offset_alpha: f32 = textureSampleLevel(
                text_texture,
                text_sampler,
                texcoord + offset * derivatives,
                0.0
            ).a;
            let weight: f32 = 0.2 / max(1.0, length(offset));
            shadowing = shadowing + offset_alpha * weight;
        }
    }
    shadowing = clamp(shadowing, 0.0, 0.5);
    shadowing = pow(shadowing, 0.45);  // TODO: kludge for gamma, or sensible visual tweak?

    let foreground_texel = textureSampleLevel(text_texture, text_sampler, texcoord, 0.0);

    // Final compositing:
    // 1. Shadow layer on scene
    var color: vec4<f32> = mix(tone_mapped_scene, vec4<f32>(vec3<f32>(0.0), 1.0), shadowing);
    // 2. Blend foreground layer (ignoring texture color on shadow)
    color = mix(color, vec4<f32>(vec3<f32>(1.0), 1.0), foreground_texel.a);
    return color;
}

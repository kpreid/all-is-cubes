// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

struct VertexOutput {
    [[builtin(position)]] clip_position: vec4<f32>;
    [[location(0)]] tc: vec4<f32>;
};

[[group(0), binding(0)]]
var text_texture: texture_2d<f32>;
[[group(0), binding(1)]]
var text_sampler: sampler;

[[stage(vertex)]]
fn info_text_vertex(
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

[[stage(fragment)]]
fn info_text_fragment(in: VertexOutput) -> [[location(0)]] vec4<f32> {
    // scale clip coordinates to 0.1 coordinates and flip Y
    let texcoord: vec2<f32> = in.tc.xy * vec2<f32>(0.5, -0.5) + 0.5;

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

    // Shadow layer
    var color: vec4<f32> = mix(vec4<f32>(0.0), vec4<f32>(vec3<f32>(0.0), 1.0), shadowing);
    // Blend foreground layer (ignoring texture color)
    color = mix(color, vec4<f32>(vec3<f32>(1.0), 1.0), foreground_texel.a);
    
    //return vec4<f32>(texcoord.x, texcoord.y, 0.0, 1.0);
    return color;
}

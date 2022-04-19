// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

// Mirrors `struct WgpuCamera` on the Rust side.
struct WgpuCamera {
    [[location(0)]] projection: mat4x4<f32>;
    [[location(1)]] view_matrix: mat4x4<f32>;
    [[location(2)]] view_position: vec3<f32>;
    [[location(3)]] light_lookup_offset: vec3<i32>;
    [[location(4)]] fog_color: vec3<f32>;
    [[location(5)]] fog_mode_blend: f32;
    [[location(6)]] fog_distance: f32;
    [[location(7)]] exposure: f32;
};

// Mirrors `struct WgpuBlockVertex` on the Rust side.
struct WgpuBlockVertex {
    [[location(0)]] position: vec3<f32>;
    [[location(1)]] cube: vec3<f32>;
    [[location(2)]] normal: vec3<f32>;
    [[location(3)]] color_or_texture: vec4<f32>;
    [[location(4)]] clamp_min: vec3<f32>;
    [[location(5)]] clamp_max: vec3<f32>;
};

// Communication from vertex to fragment stage
struct VertexOutput {
    [[builtin(position)]] clip_position: vec4<f32>;
    [[location(0)]] cube: vec3<f32>;
    [[location(1)]] normal: vec3<f32>;
    [[location(2)]] color_or_texture: vec4<f32>;
    [[location(3)]] clamp_min: vec3<f32>;
    [[location(4)]] clamp_max: vec3<f32>;
};

// This group is named camera_bind_group_layout in the code.
[[group(0), binding(0)]]
var<uniform> camera: WgpuCamera;

// This group is named space_texture_bind_group_layout in the code.
[[group(1), binding(0)]]
var block_texture: texture_3d<f32>;
[[group(1), binding(1)]]
var block_sampler: sampler;
[[group(1), binding(2)]]
var light_texture: texture_3d<u32>;


[[stage(vertex)]]
fn block_vertex_main(
    input: WgpuBlockVertex,
) -> VertexOutput {
    return VertexOutput(
        camera.projection * camera.view_matrix * vec4<f32>(input.position, 1.0),
        input.cube,
        input.normal,
        input.color_or_texture,
        input.clamp_min,
        input.clamp_max,
    );
}

// --- Fragment shading stuff --------------------------------------------------

// Given integer cube coordinates, fetch and unpack a light_texture RGB value.
// The alpha component corresponds to the `LightStatus` enum on the Rust side,
// but indirectly in a way that is useful for blending:
//
// LightStatus::Uninitialized = -1
// LightStatus::Opaque = 0
// LightStatus::NoRays = -1
// LightStatus::Visible = 1
// 
// This encoding allows use of the 0-1 range for smooth lighting's blending
// excluding opaque blocks, while the -1 value indicates values that should be
// truly ignored.
fn light_texture_fetch(fragment_position: vec3<f32>) -> vec4<f32> {
    var lookup_position = vec3<i32>(floor(fragment_position)) + camera.light_lookup_offset;
    
    // Implement wrapping (not automatic since we're not using a sampler).
    // Wrapping is used to handle sky light and in the future will be used for
    // circular buffering of the local light in an unbounded world.
    var size: vec3<i32> = textureDimensions(light_texture, 0);
    lookup_position = (lookup_position % size + size) % size;

    var texel: vec4<u32> = textureLoad(light_texture, lookup_position, 0);
    var packed_light = vec4<i32>(texel.rgb);

    // Decode logarithmic representation.
    // Exception: A texel value of exactly 0 is taken as 0, not the lowest power of 2.
    var not_zero: vec3<bool> = packed_light > vec3<i32>(0);
    var unpacked_light: vec3<f32> =
        pow(vec3<f32>(2.0), vec3<f32>(packed_light - 128) / 16.0)
        * vec3<f32>(not_zero);

    // See all_is_cubes::space::LightStatus for the value this is interpreting.
    // The enum values are grouped into approximately {0, 128, 255}, so multiplying by 2 and
    // rounding produces -1, 0, and 1 without any conditionals.
    // TODO: Now that we're reading integer values, this is unnecessarily circuitous
    var status: f32 = round((f32(texel.a) * 255.0) * 2.0 - 1.0);

    // TODO: Return a struct instead
    return vec4<f32>(unpacked_light, status);
}

// Simple directional lighting used to give corners extra definition.
// Note that this algorithm is also implemented in the Rust code.
fn fixed_directional_lighting(normal: vec3<f32>) -> f32 {
  var light_1_direction = vec3<f32>(0.4, -0.1, 0.0);
  var light_2_direction = vec3<f32>(-0.4, 0.35, 0.25);
  return (1.0 - 1.0 / 16.0) + 0.25 * (max(0.0, dot(light_1_direction, normal)) + max(0.0, dot(light_2_direction, normal)));
}

// Compute light intensity applying to the fragment.
fn lighting(in: VertexOutput) -> vec3<f32> {
    // TODO: implement different lighting options
    var origin = in.cube + in.normal + vec3<f32>(0.5);
    var local_light = light_texture_fetch(origin).rgb;
    return fixed_directional_lighting(in.normal) * local_light;
}

// Get the vertex color or texel value to display
fn get_diffuse_color(in: VertexOutput) -> vec4<f32> {
    if (in.color_or_texture[3] < -0.5) {
        // Texture coordinates.
        var texcoord: vec3<f32> =
            clamp(in.color_or_texture.xyz, in.clamp_min, in.clamp_max);
        return textureSampleLevel(block_texture, block_sampler, texcoord, 0.0);

        // TODO: implement DEBUG_TEXTURE_EDGE
    } else {
        // Solid color.
        return in.color_or_texture;
    }
}

[[stage(fragment)]]
fn block_fragment_opaque(in: VertexOutput) -> [[location(0)]] vec4<f32> {
    var diffuse_color: vec4<f32> = get_diffuse_color(in);
    
    // Lighting
    var lit_color = diffuse_color * vec4<f32>(lighting(in), 1.0);

    // Exposure/eye adaptation
    var exposed_color = vec4<f32>(lit_color.rgb * camera.exposure, lit_color.a);

    return exposed_color;
}

[[stage(fragment)]]
fn block_fragment_transparent(in: VertexOutput) -> [[location(0)]] vec4<f32> {
    var diffuse_color: vec4<f32> = get_diffuse_color(in);
    
    // Lighting
    var lit_color = diffuse_color * vec4<f32>(lighting(in), 1.0);

    // Exposure/eye adaptation
    var exposed_color = vec4<f32>(lit_color.rgb * camera.exposure, lit_color.a);

    // Multiply color channels by alpha because our blend function choice is premultiplied alpha.
    return vec4<f32>(exposed_color.rgb * exposed_color.a, exposed_color.a);
}

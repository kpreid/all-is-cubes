// --- Interface declarations --------------------------------------------------

// This group is named bloom_bind_group_layout in the code.
@group(0) @binding(0) var input: texture_2d<f32>;
@group(0) @binding(1) var linear_sampler: sampler;

// --- Vertex shader -----------------------------------------------------------

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) texcoord: vec2<f32>,
};

fn vertex_position(vertex_index: u32) -> vec4<f32> {
    return vec4<f32>(
        mix(-1.0, 3.0, f32(vertex_index == 1u)),
        mix(-1.0, 3.0, f32(vertex_index == 2u)),
        0.0,
        1.0
    );
}

@vertex
fn bloom_vertex(
    @builtin(vertex_index) in_vertex_index: u32,
) -> VertexOutput {
    let position = vertex_position(in_vertex_index);

    // scale clip coordinates to 0-1 coordinates and flip Y
    let texcoord: vec2<f32> = position.xy * vec2<f32>(0.5, -0.5) + 0.5;

    return VertexOutput(position, texcoord);
}

// --- Fragment shader; doing the actual bloom scaling work -------------------
//
// This bloom strategy is the “Dual filter” described in
// _Bandwidth-Efficient Rendering_ by Marius Bjørge at SIGGRAPH 2015
// https://community.arm.com/cfs-file/__key/communityserver-blogs-components-weblogfiles/00-00-00-20-66/siggraph2015_2D00_mmg_2D00_marius_2D00_notes.pdf
// (The choice of this algorithm was simply because it looked easy to implement for good
// results, not for any specific benefit.)

// Fetch from input image (scene texture or previous downsampling stage).
fn input_pixel(in: VertexOutput, offset: vec2<f32>) -> vec4<f32> {
    let derivatives = vec2<f32>(dpdx(in.texcoord.x), dpdy(in.texcoord.y));

    return textureSampleLevel(
        input,
        linear_sampler,
        in.texcoord + offset * derivatives,
        // The mip level here is always 0 because selecting the actual mip level will be
        // handled by the texture view selection on the CPU side.
        0.0
    );
}

@fragment
fn bloom_downsample_fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    return 
        0.50 * input_pixel(in, vec2<f32>(0.0, 0.0))
        + 0.125 * input_pixel(in, vec2<f32>(0.5, 0.5))
        + 0.125 * input_pixel(in, vec2<f32>(0.5, -0.5))
        + 0.125 * input_pixel(in, vec2<f32>(-0.5, 0.5))
        + 0.125 * input_pixel(in, vec2<f32>(-0.5, -0.5));
}

@fragment
fn bloom_upsample_fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    return (
        2.0 * input_pixel(in, vec2<f32>(0.5, 0.5))
        + 2.0 * input_pixel(in, vec2<f32>(0.5, -0.5))
        + 2.0 * input_pixel(in, vec2<f32>(-0.5, 0.5))
        + 2.0 * input_pixel(in, vec2<f32>(-0.5, -0.5))
        + input_pixel(in, vec2<f32>(0.0, 1.0))
        + input_pixel(in, vec2<f32>(0.0, -1.0))
        + input_pixel(in, vec2<f32>(-1.0, 0.0))
        + input_pixel(in, vec2<f32>(1.0, 0.0))
    ) / 12.0;
}

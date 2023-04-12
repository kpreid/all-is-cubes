// Shader used to copy and upscale the raytracing frame buffer before postprocessing steps.

// --- Interface declarations --------------------------------------------------

// This group is named frame_copy_bind_group_layout in the code.
@group(0) @binding(0) var input_texture: texture_2d<f32>;
@group(0) @binding(1) var input_sampler: sampler;

// --- Vertex shader -----------------------------------------------------------

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tc: vec2<f32>,
};

@vertex
fn frame_copy_vertex(
    @builtin(vertex_index) in_vertex_index: u32,
) -> VertexOutput {
    /// Full-screen triangle
    let position = vec4<f32>(
        mix(-1.0, 3.0, f32(in_vertex_index == 1u)),
        mix(-1.0, 3.0, f32(in_vertex_index == 2u)),
        0.0,
        1.0
    );
    return VertexOutput(position, position.xy * vec2<f32>(0.5, -0.5) + 0.5);
}

// --- Fragment shader ---------------------------------------------------------

@fragment
fn frame_copy_fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let texcoord: vec2<f32> = in.tc.xy;
    return textureSampleLevel(input_texture, input_sampler, texcoord, 0.0);
}

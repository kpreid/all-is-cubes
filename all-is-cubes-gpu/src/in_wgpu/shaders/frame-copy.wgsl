// Shader used to copy and upscale the raytracing frame buffer before postprocessing steps.

// --- Interface declarations --------------------------------------------------

// This group is named rt_frame_copy_layout in the code.
@group(0) @binding(0) var input_color_texture: texture_2d<f32>;
// Note: The "depth" texture is not in a depth texture format because
// float depth textures cannot be copied to.
@group(0) @binding(1) var input_depth_texture: texture_2d<f32>;
@group(0) @binding(2) var input_sampler: sampler;

// --- Vertex shader -----------------------------------------------------------

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tc: vec2<f32>,
};

@vertex
fn rt_frame_copy_vertex(
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

struct FragmentOutput {
    @location(0) color: vec4f,
    @builtin(frag_depth) depth: f32,
}

@fragment
fn rt_frame_copy_fragment(in: VertexOutput) -> FragmentOutput {
    let texcoord: vec2f = in.tc.xy;
    let integer_texcoord: vec2<u32> =
        vec2u(texcoord * vec2f(textureDimensions(input_depth_texture)));
    
    let color_sample = textureSampleLevel(input_color_texture, input_sampler, texcoord, 0.0);
    // Depth texture should not be interpolated.
    let depth_sample = textureLoad(input_depth_texture, integer_texcoord, 0).r;
    
    return FragmentOutput(
        // Discard alpha channel so that we're not writing transparent output that would be
        // processed nonsensically. (Also, the alpha is non-premultiplied so doesn't make sense.)
        vec4f(color_sample.rgb, 1.0),
        depth_sample,
    );
}

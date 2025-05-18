// Shader used to resample and reformat color and depth buffers for Rerun.

// --- Interface declarations --------------------------------------------------

// Mirrors `struct RerunCopyCamera` on the Rust side.
struct RerunCopyCamera {
    inverse_projection: mat4x4<f32>,
}

// This group is named rerun_copy_layout in the code.
@group(0) @binding(0) var color_texture: texture_2d<f32>;
@group(0) @binding(1) var linear_sampler: sampler;
@group(0) @binding(2) var<uniform> camera: RerunCopyCamera;
// Only one of these is actually bound and used
@group(0) @binding(10) var depth_texture_multisampled: texture_depth_multisampled_2d;
@group(0) @binding(11) var depth_texture_single_sample: texture_depth_2d;

// --- Vertex shader -----------------------------------------------------------

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) tc: vec2<f32>,
}

@vertex
fn rerun_frame_copy_vertex(
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
    @location(0) color: vec4<f32>,
    @location(1) depth: f32,
}

fn linearize_depth_value(depth_texel: f32) -> f32 {
    if depth_texel == 1.0 {
        // didn't hit anything; set the distance to zero so the point doesn't appear anywhere in space
        return 0.0;
    } else {
        // Unproject the projected depth value to get world-space depth.
        //
        // This is a matrix multiplication followed by a homogenous-to-Cartesian conversion,
        // with the extra knowledge that the depth coordinate of the point is not affected by
        // the XY coordinates, so we can ignore everything but the Z and W components. (If the
        // image plane were not perpendicular to the view direction, that would not be true.)
        //
        // This particular technique for depth unprojection was recommended by
        // @jasperrlz:matrix.org in the WebGPU Matrix chat room.
        let ipz = camera.inverse_projection[2];
        let ipw = camera.inverse_projection[3];
        return -(depth_texel * ipz.z + ipw.z) / (depth_texel * ipz.w + ipw.w);
    }
}

fn get_color(in: VertexOutput) -> vec4<f32> {
    return textureSampleLevel(color_texture, linear_sampler, in.tc.xy, 0.0);
}

@fragment
fn rerun_frame_copy_multisampled_fragment(in: VertexOutput) -> FragmentOutput {
    let integer_texcoord: vec2<u32> = vec2u(in.tc.xy * vec2f(textureDimensions(depth_texture_multisampled)));
    return FragmentOutput(
        get_color(in),
        // arbitrary choice of sample index 0.
        // TODO: Take the median of samples instead.
        linearize_depth_value(textureLoad(depth_texture_multisampled, integer_texcoord, 0)),
    );
}

@fragment
fn rerun_frame_copy_single_sample_fragment(in: VertexOutput) -> FragmentOutput {
    let integer_texcoord: vec2<u32> = vec2u(in.tc.xy * vec2f(textureDimensions(depth_texture_single_sample)));
    return FragmentOutput(
        get_color(in),
        linearize_depth_value(textureLoad(depth_texture_single_sample, integer_texcoord, 0)),
    );
}

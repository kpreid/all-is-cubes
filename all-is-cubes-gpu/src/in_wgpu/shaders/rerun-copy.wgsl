// Shader used to resample and reformat color and depth buffers for Rerun.

// --- Interface declarations --------------------------------------------------

// Mirrors `struct RerunCopyCamera` on the Rust side.
struct RerunCopyCamera {
    inverse_projection: mat4x4<f32>,
}

// This group is named rerun_copy_layout in the code.
@group(0) @binding(0) var color_texture: texture_2d<f32>;
@group(0) @binding(1) var depth_texture: texture_depth_multisampled_2d;
@group(0) @binding(2) var linear_sampler: sampler;
@group(0) @binding(3) var<uniform> camera: RerunCopyCamera;

// --- Vertex shader -----------------------------------------------------------

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) tc: vec2<f32>,
};

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

fn depth(texcoord: vec2<f32>) -> f32 {
    // Compute integer (whole texel) coordinates for the depth texture,
    // because we are not using a sampler.
    let integer_texcoord: vec2<u32> = vec2<u32>(texcoord * vec2<f32>(textureDimensions(depth_texture)));

    // arbitrary choice of sample index 0.
    // TODO: Take the median of samples instead.
    let raw_value = textureLoad(depth_texture, integer_texcoord, 0);

    if raw_value == 1.0 {
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
        return -(raw_value * ipz.z + ipw.z) / (raw_value * ipz.w + ipw.w);
    }
}

@fragment
fn rerun_frame_copy_fragment(in: VertexOutput) -> FragmentOutput {
    let texcoord: vec2<f32> = in.tc.xy;

    return FragmentOutput(
        textureSampleLevel(color_texture, linear_sampler, texcoord, 0.0),
        depth(texcoord),
    );
}

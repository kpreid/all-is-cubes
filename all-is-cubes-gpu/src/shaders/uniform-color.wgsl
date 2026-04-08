// Note that the interpretation of this color (premultiplied or not, etc) is entirely determined by
// the render pass state and not by this shader.
@group(0) @binding(0) var<uniform> uniform_color: vec4f;

@vertex
fn full_screen_vertex(
    @builtin(vertex_index) in_vertex_index: u32,
) -> @builtin(position) vec4<f32> {
    // Full-screen triangle
    return vec4<f32>(
        mix(-1.0, 3.0, f32(in_vertex_index == 1u)),
        mix(-1.0, 3.0, f32(in_vertex_index == 2u)),
        0.0,
        1.0
    );
}

@fragment
fn uniform_color_fragment() -> @location(0) vec4<f32> {
    return uniform_color;
}

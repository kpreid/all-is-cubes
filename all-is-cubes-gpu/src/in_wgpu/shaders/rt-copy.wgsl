// Shader used to copy and upscale the raytracing frame buffer before postprocessing steps.

// --- Interface declarations --------------------------------------------------

struct ReprojectionUniforms {
    reprojection_matrix: mat4x4<f32>,
    inverse_projection: mat4x4<f32>,
    // TODO: was going to be used but is not, currently
    output_pixel_scale: vec2<f32>,
    _padding: vec2<f32>,
}

// This group is named rt_copy_layout in the code.
@group(0) @binding(0) var input_color_texture: texture_2d<f32>;
// Note: The "depth" texture is not in a depth texture format because
// float depth textures cannot be copied to and because we are using negative values specially.
@group(0) @binding(1) var input_depth_texture: texture_2d<f32>;
@group(0) @binding(2) var input_sampler: sampler;

// Used for reprojection but not straight copying
@group(0) @binding(3) var<uniform> reprojection: ReprojectionUniforms;

struct FragmentOutput {
    @location(0) color: vec4f,
    @builtin(frag_depth) depth: f32,
}

// --- Data copied without reprojection ----------------------------------------

struct CopyVertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tc: vec2<f32>,
}

@vertex
fn rt_frame_copy_vertex(
    @builtin(vertex_index) in_vertex_index: u32,
) -> CopyVertexOutput {
    /// Full-screen triangle
    let position = vec4<f32>(
        mix(-1.0, 3.0, f32(in_vertex_index == 1u)),
        mix(-1.0, 3.0, f32(in_vertex_index == 2u)),
        0.0,
        1.0
    );
    return CopyVertexOutput(position, position.xy * vec2<f32>(0.5, -0.5) + 0.5);
}

@fragment
fn rt_frame_copy_fragment(in: CopyVertexOutput) -> FragmentOutput {
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

// --- Reprojection ------------------------------------------------------------

const equilateral_triangle_vertices: array<vec2f, 3> = array(
    vec2f(0.,              1.),
    vec2f(-sqrt(3.0)/2.0, -0.5),
    vec2f( sqrt(3.0)/2.0, -0.5),
);

struct ReprojectVertexOutput {
    // Vertex position of the triangle
    @builtin(position) clip_position: vec4<f32>,
    
    // Texture coordinate for lookup in the color texture.
    // TODO: Better to do the sample in the vertex shader?
    @location(0) sample_tc: vec2<f32>,

    // // Pixel-center position of the reprojected point, without the triangle vertex part.
    // // Used to produce depth cone effect.
    // @location(1) sample_position: vec2<f32>,

    // Model-space coordinates of the triangle vertices, used as "texture" coords for drawing
    // the point/circle.
    @location(1) triangle_vertex_position: vec2<f32>,
}

// This vertex shader is called with one triangle per pixel in the raytracer output buffer,
// and reprojects those pixels from the camera they were traced in to the current camera.
@vertex
fn rt_reproject_vertex(
    @builtin(vertex_index) vertex_index: u32,
) -> ReprojectVertexOutput {
    // Compute which triangle this is.
    let triangle_index = vertex_index / 3;
    let vertex_of_triangle = vertex_index % 3;

    // From the vertex index and the input texture size, compute which input pixel this is.
    let image_dimensions = textureDimensions(input_depth_texture);
    let image_dimensions_recip = 1.0 / vec2f(image_dimensions);
    let width = image_dimensions.x;
    let pixel_position = vec2u(triangle_index % width, triangle_index / width);

    let sample_texcoord = (vec2f(pixel_position) + vec2f(0.5)) * image_dimensions_recip;
    // Convert from Y-down texture coordinates to Y-up NDC
    let sample_ndc_position_2d = vec2f(
        sample_texcoord.x * 2.0 - 1.0,
        -(sample_texcoord.y * 2.0 - 1.0),
    );
    
    // Obtain the projected depth value.
    let encoded_input_depth_sample = textureLoad(input_depth_texture, pixel_position, 0).r;
    let input_depth_sample = abs(encoded_input_depth_sample);
    let should_reproject = encoded_input_depth_sample >= 0.0;

    // Determine output pixel position and scale.
    var output_ndc_position: vec3f;
    var inverse_depth_ratio: f32;
    if should_reproject {
        // Perform reprojection.
        let sample_ndc_position_homogeneous = vec4f(
            sample_ndc_position_2d,
            input_depth_sample,
            1.0,
        );
        let output_ndc_position_homogeneous =
            reprojection.reprojection_matrix * sample_ndc_position_homogeneous;


        if output_ndc_position_homogeneous.w < 0.0 {
            // This happens when the output point would be behind the camera.
            // If we proceeded, we would end up drawing the point in a position reflected through
            // the camera position (as in <https://en.wikipedia.org/wiki/Point_reflection>).
            // Instead, return a degenerate triangle which will not be drawn (the closest
            // approximation to discarding that a vertex shader can do).
            return ReprojectVertexOutput(
                vec4f(0.0, 0.0, 0.0, 0.0),
                vec2f(0.0, 0.0),
                vec2f(0.0, 0.0),
            );
        }

        output_ndc_position =
            output_ndc_position_homogeneous.xyz / output_ndc_position_homogeneous.w;
        inverse_depth_ratio =
            linearize_depth_value(input_depth_sample) / linearize_depth_value(output_ndc_position.z);
    } else {
        // We don't reproject pixels that belong to the UI, because we assume the UI doesn't move,
        // and the only reprojection matrix we have is for the world (this could be changed).
        output_ndc_position = vec3f(
            sample_ndc_position_2d,
            // Force depth to 0 so reprojected world pixels don't occlude it.
            0.0,
        );
        // No point scaling.
        inverse_depth_ratio = 1.0;
    }

    // Clamp Z to renderable range so sky doesn't vanish, etc.
    output_ndc_position.z = clamp(output_ndc_position.z, 0.0, 1.0);

    // Determine how large our triangle/point should be.
    // 4.0 is an empirically-determined number for the basic scale.
    let triangle_scale = 4.0 * image_dimensions_recip * inverse_depth_ratio;

    // Produce triangle vertex.
    let triangle_vertex_position = equilateral_triangle_vertices[vertex_of_triangle];
    return ReprojectVertexOutput(
        vec4f(
            output_ndc_position + vec3f(triangle_vertex_position * triangle_scale, 0.0),
            1.0),
        sample_texcoord,
        triangle_vertex_position.xy,
    );
}

@fragment
fn rt_reproject_fragment(
    in: ReprojectVertexOutput,
) -> FragmentOutput {
    // DEBUG: show constant color
    //return vec4f(1.0, 0.0, 0.0, 1.0);

    let distance_squared = dot(in.triangle_vertex_position, in.triangle_vertex_position);
    if distance_squared > 0.33 { 
        // Roughly crop the triangle to be a circle
        discard;
    }

    return FragmentOutput(
        //vec4(in.clip_position.xy / 2000.0, 0.0, 1.0),
        //vec4(in.sample_position * reprojection.output_pixel_scale / 2000.0, 0.0, 1.0),
        textureSampleLevel(input_color_texture, input_sampler, in.sample_tc, 0.0),
        // TODO: Decide whether to form the depth into a cone.
        in.clip_position.z + distance_squared * 0.0125,
    );
}

// Note: This algorithm is semi-duplicated between rerun-copy.wgsl and rt-copy.wgsl
fn linearize_depth_value(depth_texel: f32) -> f32 {
    // Unproject the projected depth value to get world-space depth.
    //
    // This is a matrix multiplication followed by a homogenous-to-Cartesian conversion,
    // with the extra knowledge that the depth coordinate of the point is not affected by
    // the XY coordinates, so we can ignore everything but the Z and W components. (If the
    // image plane were not perpendicular to the view direction, that would not be true.)
    //
    // This particular technique for depth unprojection was recommended by
    // @jasperrlz:matrix.org in the WebGPU Matrix chat room.
    let ipz = reprojection.inverse_projection[2];
    let ipw = reprojection.inverse_projection[3];
    return -(depth_texel * ipz.z + ipw.z) / (depth_texel * ipz.w + ipw.w);
}

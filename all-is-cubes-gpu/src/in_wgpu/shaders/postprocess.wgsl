// --- Interface declarations --------------------------------------------------

// Mirrors `struct PostprocessUniforms` on the Rust side.
struct PostprocessUniforms {
    tone_mapping_id: i32,
    maximum_intensity: f32,
    scene_texture_valid: i32,
    bloom_intensity: f32,
};


// This group is named postprocess_bind_group_layout in the code.
@group(0) @binding(0) var text_texture: texture_2d<f32>; // red-only
@group(0) @binding(1) var text_sampler: sampler;
@group(0) @binding(2) var linear_scene_texture: texture_2d<f32>;
@group(0) @binding(3) var<uniform> camera: PostprocessUniforms;
@group(0) @binding(4) var bloom_texture: texture_2d<f32>;

// --- Vertex shader -----------------------------------------------------------

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tc: vec4<f32>,
};

@vertex
fn postprocess_vertex(
    @builtin(vertex_index) in_vertex_index: u32,
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
    switch camera.tone_mapping_id {
        default: { // or case 0
            // Clamp
            return clamp(linear_rgb, vec3(0.0), vec3(camera.maximum_intensity));
        }
        case 1 {
            // Reinhard
            // TODO: Explain exactly which Reinhard, citation, etc
            return linear_rgb / (1.0 + luminance(linear_rgb) / camera.maximum_intensity);
        }
    }
}

// Fetch scene pixel, if scene texture is present.
// (It may be absent because there might have been nothing to draw in previous stages.)
fn scene_pixel(texcoord: vec2<f32>) -> vec4<f32> {
    if camera.scene_texture_valid == 0 {
        // There is no valid scene texture (e.g. because no layers drew anything).
        // In this case we display a placeholder and do no further processing.

        // TODO: make this a checkerboard or something to distinguish from “oops, all gray”.
        // (And when we do that, also use it for UI-on-top-of-nothing, by reading the alpha.)
        // Note: this color is equal to all_is_cubes::palette::NO_WORLD_TO_SHOW.

        return vec4<f32>(0.5, 0.5, 0.5, 1.0);
    }

    let scene_color = textureSampleLevel(
        linear_scene_texture,
        text_sampler, // TODO: wrong sampler
        texcoord,
        0.0
    );

    // Add bloom.
    let bloom_color = textureSampleLevel(
        bloom_texture,
        text_sampler, // TODO: probably provide a sampler specifically for bloom?
        texcoord,
        0.0
    );

    // TODO: think about what alpha handling we want
    return vec4<f32>(mix(scene_color.rgb, bloom_color.rgb, camera.bloom_intensity), scene_color.a);
}

// Given a point in the viewport using 0-1 coordinates, find the opacity the info text's
// shadow should have.
fn text_shadow_alpha(texcoord: vec2<f32>) -> f32 {
    let derivatives = vec2<f32>(dpdx(texcoord.x), dpdy(texcoord.y));

    var accumulator: f32 = 0.0;
    let radius: i32 = 2;
    let diameter = radius * 2 + 1;
    // Note: This roundabout method of computing the neighborhood coordinates using
    // a linear index is because the obvious nested for loop would stop early somehow.
    // (Mac, Metal, AMD Radeon Pro 5500M.) Obviously that is some kind of bug in some
    // part of the system, but I want to stop dealing with the unreadability and
    // inconsistency while I write rendering tests, so it's this workaround for now.
    for (var index = 0; index < diameter * diameter; index = index + 1) {
        let dx: i32 = index % diameter - radius;
        let dy: i32 = index / diameter - radius;

        let offset: vec2<f32> = vec2<f32>(vec2<i32>(dx, dy));
        let offset_sample: f32 = textureSampleLevel(
            text_texture,
            text_sampler,
            texcoord + offset * derivatives,
            0.0
        ).r;
        let weight: f32 = 0.35 / max(0.001, length(offset));
        accumulator = accumulator + offset_sample * weight;
    }
    return pow(clamp(accumulator, 0.0, 1.0), 0.7);
}

@fragment
fn postprocess_fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    // scale clip coordinates to 0-1 coordinates and flip Y
    let texcoord: vec2<f32> = in.tc.xy * vec2<f32>(0.5, -0.5) + 0.5;

    let scene_color = scene_pixel(texcoord);

    // apply tone mapping, respecting premultiplied alpha
    let tone_mapped_scene = vec4<f32>(tone_map(scene_color.rgb * scene_color.a) / scene_color.a, scene_color.a);

    let shadowing = text_shadow_alpha(texcoord);
    let foreground_texel = textureSampleLevel(text_texture, text_sampler, texcoord, 0.0).r;

    // Final compositing:
    // 1. Shadow layer on scene
    var color: vec4<f32> = mix(tone_mapped_scene, vec4<f32>(vec3<f32>(0.0), 1.0), shadowing);
    // 2. Blend foreground layer
    color = mix(color, vec4<f32>(vec3<f32>(1.0), 1.0), foreground_texel);
    return color;
}

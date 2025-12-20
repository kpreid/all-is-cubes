// --- Interface declarations --------------------------------------------------

// Mirrors `struct PostprocessUniforms` on the Rust side.
struct PostprocessUniforms {
    info_text_coordinate_scale: vec2f,
    info_text_origin: vec2f,
    font_cell_size: vec2u,
    font_cell_margin: u32,
    tone_mapping_id: i32,
    maximum_intensity: f32,
    bloom_intensity: f32,
    _padding: vec2u,
}

// Group 0 is named postprocess_bind_group_layout on the Rust side.
@group(0) @binding(0) var<uniform> camera: PostprocessUniforms;
// Not-yet-postprocessed scene rendering.
@group(0) @binding(1) var linear_scene_texture: texture_2d<f32>;
@group(0) @binding(2) var scene_sampler: sampler;
// Text overlay, as 8-bit ISO-8859-1 character codes in each texel
@group(0) @binding(3) var text_texture: texture_2d<u32>;
@group(0) @binding(4) var text_sampler: sampler;
// Output of bloom calculation, to be blended with the scene.
@group(0) @binding(5) var bloom_texture: texture_2d<f32>;
// Sampler to be used for reading the bloom texture.
@group(0) @binding(6) var bloom_sampler: sampler;
// Font atlas used for rendering the info text.
@group(0) @binding(7) var font_texture: texture_2d<f32>;

// --- Vertex shader -----------------------------------------------------------

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) scene_tc: vec2<f32>,
    @location(1) info_text_tc: vec2<f32>,
}

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

    // Y-downward texcoords
    let scene_tc: vec2f = position.xy * vec2f(0.5, -0.5) + 0.5;

    let info_text_tc: vec2f = (scene_tc - camera.info_text_origin) * camera.info_text_coordinate_scale;
    return VertexOutput(position, scene_tc, info_text_tc);
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
    let scene_color = textureSampleLevel(
        linear_scene_texture,
        scene_sampler,
        texcoord,
        0.0
    );

    // Add bloom.
    let bloom_color = textureSampleLevel(
        bloom_texture,
        bloom_sampler,
        texcoord,
        0.0
    );

    // TODO: think about what alpha handling we want
    return vec4<f32>(mix(scene_color.rgb, bloom_color.rgb, camera.bloom_intensity), scene_color.a);
}

/// Returns the premultiplied RGBA color of the info text layer at this position.
fn render_text(in: VertexOutput) -> vec4f {
    // Position of this fragment as measured in units of whole character cells.
    let tc_in_whole_cells = in.info_text_tc * vec2f(textureDimensions(text_texture));

    let cell = vec2i(floor(tc_in_whole_cells));
    let position_in_cell = tc_in_whole_cells - vec2f(cell);

    // Find the top-left cell of a neighborhood of the 4 nearest cells that contains this fragment.
    // This allows cells to overlap their outlines.
    var tl_cell = cell;
    var tl_pos = position_in_cell;
    if position_in_cell.x < 0.5 {
        tl_cell.x -= 1;
        tl_pos.x += 1.0;
    }
    if position_in_cell.y < 0.5 {
        tl_cell.y -= 1;
        tl_pos.y += 1.0;
    }
    
    // Draw neighborhood of 4 cells.
    // max() as a blending function serves to make white (foreground) override black (outline) and
    // black override transparent. This will need to be changed if we want text in other colors.
    return max(
        max(
            render_character_cell(tl_cell, tl_pos),
            render_character_cell(tl_cell + vec2i(1, 0), tl_pos - vec2f(1, 0)),
        ),
        max(
            render_character_cell(tl_cell + vec2i(0, 1), tl_pos - vec2f(0, 1)),
            render_character_cell(tl_cell + vec2i(1, 1), tl_pos - vec2f(1, 1)),
        )
    );
}

// Given an already picked character cell, returns the part of it in this fragment.
//
// `cell` is the coordinates of a character cell, or equivalently, a texel in `text_texture`.
// `position_in_cell` is coordinates for the position of this fragment relative to that cell,
// which are in the 0-1 range if the fragment is actually inside that cell, or outside it if
// we are rendering neighboring outlines.
fn render_character_cell(cell: vec2i, position_in_cell: vec2f) -> vec4f {
    if cell.x < 0 || cell.y < 0 {
        // avoid out of bounds reads of text_texture, which are undefined per WGSL
        return vec4f(0.0);
    }

    // Read the text_texture to determine which character to draw.
    var character_code: u32 = textureLoad(
        text_texture,
        vec2u(cell),
        0
    ).r;
    // This special case allows us to clear the texture to zero instead of space.
    // TODO: This case can be removed when we switch to arbitrary glyph indices instead of
    // using ISO-8859-1/beginning-of-Unicode code points, which will be desired for rendering
    // more characters anyway.
    if character_code == 0x00 {
        character_code = 0x20;
    }

    // 2D position in the font atlas texture of the glyph to render.
    let font_atlas_cell = vec2u(character_code % 16, character_code / 16);

    // Position in cell scaled to be whole-texel coordinates for the box within the atlas cell
    // *not* including the outline.
    let float_texel_in_cell =
        position_in_cell * vec2f(camera.font_cell_size - camera.font_cell_margin * 2)
        // offset so 0,0 is the top-left of the nominal character cell *inside* the margin
        + vec2f(f32(camera.font_cell_margin));

    if float_texel_in_cell.x < 0 
        || float_texel_in_cell.y < 0 
        || float_texel_in_cell.x >= f32(camera.font_cell_size.x)
        || float_texel_in_cell.y >= f32(camera.font_cell_size.y) {
        // Don't read from outside the atlas cell
        return vec4f(0.0);
    }

    // Texel coordinates of this fragment's part of the glyph.
    let font_atlas_tc =
        font_atlas_cell * camera.font_cell_size 
        + vec2u(floor(float_texel_in_cell));

    return textureLoad(font_texture, font_atlas_tc, 0);
}

@fragment
fn postprocess_fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let scene_color = scene_pixel(in.scene_tc);

    // apply tone mapping, respecting premultiplied alpha
    let tone_mapped_scene = vec4<f32>(
        tone_map(scene_color.rgb * scene_color.a) / scene_color.a,
        scene_color.a,
    );

    let text: vec4f = render_text(in);

    // Premultiplied alpha blend.
    return tone_mapped_scene * (1.0 - text.a) + text;
}

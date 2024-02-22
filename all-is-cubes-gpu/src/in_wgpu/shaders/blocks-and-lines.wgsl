// --- Interface declarations --------------------------------------------------

// Mirrors `struct ShaderSpaceCamera` on the Rust side.
struct ShaderSpaceCamera {
    projection: mat4x4<f32>,
    inverse_projection: mat4x4<f32>,
    view_matrix: mat4x4<f32>,
    // --- 16-byte aligned point ---
    view_position: vec3<f32>,
    exposure: f32,
    // --- 16-byte aligned point ---
    light_lookup_offset: vec3<i32>,
    light_option: i32,
    // --- 16-byte aligned point ---
    fog_mode_blend: f32,
    fog_distance: f32,
    // pad out to multiple of 16 bytes
    padding1: f32,
    padding2: f32,
};

// Mirrors `struct WgpuBlockVertex` on the Rust side.
struct WgpuBlockVertex {
    @location(0) cube_packed: u32,
    @location(1) position_in_cube_and_normal_packed: vec2<u32>,
    @location(2) color_or_texture: vec4<f32>,
    @location(3) clamp_min_max: vec3<u32>,
};

// Mirrors `struct WgpuInstanceData` on the Rust side.
struct WgpuInstanceData {
    @location(6) translation: vec3<f32>,
};

// Mirrors `struct WgpuLinesVertex` on the Rust side.
struct WgpuLinesVertex {
    @location(0) position: vec3<f32>,
    @location(1) color: vec4<f32>,
};

// This group is named camera_bind_group_layout in the code.
@group(0) @binding(0) var<uniform> camera: ShaderSpaceCamera;

// This group is named space_texture_bind_group_layout in the code.
@group(1) @binding(0) var light_texture: texture_3d<u32>;
@group(1) @binding(1) var block_g0_reflectance: texture_3d<f32>;
@group(1) @binding(2) var block_g1_reflectance: texture_3d<f32>;
@group(1) @binding(3) var block_g1_emission: texture_3d<f32>;
@group(1) @binding(4) var skybox_texture: texture_cube<f32>;
@group(1) @binding(5) var skybox_sampler: sampler;

// --- Fog computation --------------------------------------------------------

// Physically realistic fog, but doesn't ever reach 1 (fully opaque).
fn fog_exponential(distance: f32) -> f32 {
    let fog_density = 1.6;
    return 1.0 - exp(-fog_density * distance);
}

// Fog that goes all the way from fully transparent to fully opaque.
// The correction is smaller the denser the fog.
fn fog_exp_fudged(distance: f32) -> f32 {
    return fog_exponential(distance) / fog_exponential(1.0);
}

fn fog_combo(distance: f32) -> f32 {
    // Combination of realistic exponential (constant density) fog,
    // and slower-starting fog so nearby stuff is clearer.
    return mix(fog_exp_fudged(distance), pow(distance, 4.0), camera.fog_mode_blend);
}

// Returns the opacity (0 to 1) of the fog.
//
// Note: This function is run in the vertex shader, to reduce the cost of the
// computation. This is an approximation that works on the assumption that fog
// is locally close-to-linear on the scale of the largest triangles we draw.
// This assumption will need to be revisited if we start using triangles larger
// than a block.
fn compute_fog(world_position: vec3<f32>) -> f32 {
    // Camera-relative position not transformed by projection.
    let eye_vertex_position = camera.view_matrix * vec4<f32>(world_position, 1.0);
    let distance_from_eye: f32 = length(eye_vertex_position.xyz);

    // TODO: When we implement volumetric transparency, that's another use
    // for the distance_from_eye value, which we will want to pass out (in a struct)

    // Distance in range 0 (camera position) to 1 (opaque fog position/far clip position).
    let normalized_distance: f32 = distance_from_eye / camera.fog_distance;
    let fog_mix = clamp(fog_combo(normalized_distance), 0.0, 1.0);

    return fog_mix;
}

// --- Block vertex shader ----------------------------------------------------

// Vertex-to-fragment data for blocks
struct BlockFragmentInput {
    @builtin(position) clip_position: vec4<f32>,
    
    @location(0) world_position: vec3<f32>,
    
    // Position of the fragment relative to the cube it belongs to.
    // Range of 0.0 to 1.0 inclusive.
    @location(1) position_in_cube: vec3<f32>,

    // Cube position in world coordinates, used for space data lookups
    // (currently only LightingOption::Flat).
    @location(2) world_cube: vec3<f32>,

    // Vectors making up a transformation matrix from tangent space to world space,
    // also known as a TBN (tangent, bitangent, normal) matrix.
    // One of them is the normal vector.
    // Both tangents are always positive.
    //
    // Used to look up world light data when computing interpolated light.
    @location(3) tangent: vec3<f32>,
    @location(4) bitangent: vec3<f32>,
    @location(5) normal: vec3<f32>,
    
    @location(6) color_or_texture: vec4<f32>,
    @location(7) clamp_min: vec3<f32>,
    @location(8) clamp_max: vec3<f32>,
    
    @location(9) fog_mix: f32,

    // Direction vector, in the world coordinate system, which points
    // from the camera position to this fragment.
    @location(10) camera_ray_direction: vec3<f32>,
};

@vertex
fn block_vertex_main(
    input: WgpuBlockVertex,
    instance_input: WgpuInstanceData,
) -> BlockFragmentInput {
    // Unpack cube (three u8s represented as one u32).
    let cube = vec3<u32>(
        input.cube_packed & 0xFFu,
        (input.cube_packed >> 8u) & 0xFFu,
        (input.cube_packed >> 16u) & 0xFFu
    );

    // Unpack position_in_cube (three u16s represented as two u32s).
    let position_in_cube_fixed = vec3<u32>(
        input.position_in_cube_and_normal_packed[0] & 0xFFFFu,
        (input.position_in_cube_and_normal_packed[0] >> 16u) & 0xFFFFu,
        input.position_in_cube_and_normal_packed[1] & 0xFFFFu
    );
    // Undo fixed-point scale by 256.
    let position_in_cube = vec3<f32>(position_in_cube_fixed) / 256.0;

    // Unpack normal.
    var normal = vec3<f32>(1.0);
    switch ((input.position_in_cube_and_normal_packed[1] >> 16u) & 0xFFFFu) {
        case 1u { normal = vec3<f32>(-1.0, 0.0, 0.0); }
        case 2u { normal = vec3<f32>(0.0, -1.0, 0.0); }
        case 3u { normal = vec3<f32>(0.0, 0.0, -1.0); }
        case 4u { normal = vec3<f32>(1.0, 0.0, 0.0); }
        case 5u { normal = vec3<f32>(0.0, 1.0, 0.0); }
        case 6u { normal = vec3<f32>(0.0, 0.0, 1.0); }
        default: {}
    }

    // Unpack clamp rectangle coordinates.
    let clamp_min_fixpoint = input.clamp_min_max & vec3<u32>(0x0000FFFFu);
    let clamp_max_fixpoint = (input.clamp_min_max & vec3<u32>(0xFFFF0000u)) >> vec3<u32>(16u);
    let clamp_min = vec3<f32>(clamp_min_fixpoint) / 2.0;
    let clamp_max = vec3<f32>(clamp_max_fixpoint) / 2.0;

    // Generate tangents (with always positive components).
    // TODO: Would it be better to just put this in the above switch statement?
    // TODO: Is the always-positive rule actually needed?
    var tangent = vec3<f32>(0.0, 1.0, 0.0);
    var bitangent = vec3<f32>(0.0, 0.0, 1.0);
    if normal.x == 0.0 {
        tangent = vec3<f32>(1.0, 0.0, 0.0);
        bitangent = abs(cross(tangent, normal));
    }

    let combined_matrix = camera.projection * camera.view_matrix;
    // TODO: eventually this should become a camera-relative position, not a world position.
    // That will require further work in light-lookup cooordinates.
    let world_position = vec3<f32>(cube) + position_in_cube + instance_input.translation;

    return BlockFragmentInput(
        combined_matrix * vec4<f32>(world_position, 1.0), // clip_position
        world_position,
        position_in_cube,
        instance_input.translation + vec3<f32>(cube),  // world_cube
        tangent,
        bitangent,
        normal,
        input.color_or_texture,
        clamp_min,
        clamp_max,
        compute_fog(world_position),
        // Note that we do not normalize this vector: by keeping things linear, we
        // allow linear interpolation between vertices to get the right answer.
        world_position - camera.view_position, // camera_ray_direction
    );
}

// --- Block fragment shader ---------------------------------------------------

// Modulo, not remainder (matches GLSL builtin mod())
fn modulo(a: f32, b: f32) -> f32 {
    return ((a % b) + b) % b;
}

// Find the smallest positive `t` such that `s + t * ds` is an integer.
//
// In the current implementation, `s` must be in the range 0 to 1.
// (This is why it is called "partial".)
//
// If `ds` is zero, returns positive infinity; this is a useful answer because
// it means that the less-than comparisons in the raycast algorithm will never
// pick the corresponding axis. If any input is NaN, returns NaN.
//
// The canonical version of this algorithm is
// `all_is_cubes::raycast::scale_to_integer_step`.
fn partial_scale_to_integer_step(s_in: f32, ds_in: f32) -> f32 {
    var s = s_in;
    var ds = ds_in;
    s = clamp(s, 0.0, 1.0);  // Out of bounds may appear on triangle edges
    if sign(ds) < 0.0 {
        s = 1.0 - s;
        ds = -ds;
        // Note: This will not act on a negative zero.
        // That must be handled separately.
    }
    // problem is now s + t * ds = 1
    var result = (1.0 - s) / ds;

    // Fix sign error in case of negative zero.
    if s < 0.0 {
        result *= -1.0;
    }

    return result;
}

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
    let size: vec3<i32> = vec3<i32>(textureDimensions(light_texture, 0));
    lookup_position = (lookup_position % size + size) % size;

    let texel: vec4<u32> = textureLoad(light_texture, lookup_position, 0);
    let packed_light = vec3<i32>(texel.rgb);

    // Decode logarithmic representation.
    // Exception: A texel value of exactly 0 is taken as 0, not the lowest power of 2.
    let not_zero: vec3<bool> = packed_light > vec3<i32>(0);
    let unpacked_light: vec3<f32> = pow(vec3<f32>(2.0), vec3<f32>(packed_light - 128) / 16.0) * vec3<f32>(not_zero);

    // See all_is_cubes::space::LightStatus for the value this is interpreting.
    // The enum values are grouped into approximately {0, 128, 255}, so multiplying by 2 and
    // rounding produces -1, 0, and 1 without any conditionals.
    // TODO: Now that we're reading integer values, this is unnecessarily circuitous
    let status: f32 = round((f32(texel.a) / 255.0) * 2.0 - 1.0);

    // TODO: Return a struct instead of an unexplained vec4

    // If the light value is non-zero, it is probably meaningful regardless of what the status
    // shows.
    if any(not_zero) {
        return vec4<f32>(unpacked_light, 1.0);
    } else {
        return vec4<f32>(unpacked_light, status);
    }
}

// Simple directional lighting used to give corners extra definition.
// Note that this algorithm is also implemented in the Rust code.
fn fixed_directional_lighting(normal: vec3<f32>) -> f32 {
    let light_1_direction = vec3<f32>(0.4, -0.1, 0.0);
    let light_2_direction = vec3<f32>(-0.4, 0.35, 0.25);
    return (1.0 - 1.0 / 16.0) + 0.25 * (max(0.0, dot(light_1_direction, normal)) + max(0.0, dot(light_2_direction, normal)));
}

fn valid_light(light: vec4<f32>) -> bool {
    return light.a > 0.5;
}

// Tweak a light value for ambient occlusion -- and convert the light status 
// value returned from light_texture_fetch to an interpolation coefficient.
fn ao_fudge(light_value: vec4<f32>) -> vec4<f32> {
    // TODO: Make this a (uniform) graphics option
    let fudge = 0.25;
    let status = light_value.a;
    // Fudge applies only to opaque cubes, not to no-rays cubes.
    // This multiplication provides a branchless calculation:
    // If status is -1 (no-rays or uninitialized), return 0.
    // If status is 0 (opaque), return fudge value.
    // If status is 1 (normal light value), return that.
    return vec4<f32>(light_value.rgb, f32(status > -0.5) * max(status, fudge));
}

// Compute the interpolated ('smooth') light for the surface from light_texture.
// This implementation is duplicated in Rust at all-is-cubes/src/raytracer.rs
fn interpolated_space_light(in: BlockFragmentInput) -> vec3<f32> {
    // About half the size of the smallest permissible voxel.
    let above_surface_epsilon = 0.5 / 256.0;

    // The position we should start with for texture lookup and interpolation.
    let origin = in.world_position + in.normal * above_surface_epsilon;

    // Find linear interpolation coefficients based on where we are relative to
    // a half-cube-offset grid.
    var mix_1: f32 = modulo(dot(origin, in.tangent) - 0.5, 1.0);
    var mix_2: f32 = modulo(dot(origin, in.bitangent) - 0.5, 1.0);

    // Ensure that mix <= 0.5, i.e. the 'near' side below is the side we are on.
    var dir_1: vec3<f32> = in.tangent;
    var dir_2: vec3<f32> = in.bitangent;
    if mix_1 > 0.5 {
        dir_1 = dir_1 * -1.0;
        mix_1 = 1.0 - mix_1;
    }
    if mix_2 > 0.5 {
        dir_2 = dir_2 * -1.0;
        mix_2 = 1.0 - mix_2;
    }

    // Modify interpolation by smoothstep to change the visual impression towards
    // "blurred blocks" and away from the diamond-shaped gradients of linear interpolation
    // which, being so familiar, can give an unfortunate impression of "here is 
    // a closeup of a really low-resolution texture".
    // TODO: disabled because current wgpu doesn't implement smoothstep
    // mix_1 = smoothstep(0.0, 1.0, mix_1);
    // mix_2 = smoothstep(0.0, 1.0, mix_2);

    // Retrieve texels, again using the half-cube-offset grid (this way we won't have edge artifacts).
    let lin_lo = -0.5;
    let lin_hi = 0.5;
    var near12 = light_texture_fetch(origin + lin_lo * dir_1 + lin_lo * dir_2);
    var near1far2 = light_texture_fetch(origin + lin_lo * dir_1 + lin_hi * dir_2);
    var near2far1 = light_texture_fetch(origin + lin_hi * dir_1 + lin_lo * dir_2);
    var far12 = light_texture_fetch(origin + lin_hi * dir_1 + lin_hi * dir_2);

    if !valid_light(near1far2) && !valid_light(near2far1) {
        // The far corner is on the other side of a diagonal wall, so should be
        // ignored to prevent light leaks.
        far12 = near12;
    }

    // Apply ambient occlusion.
    near12 = ao_fudge(near12);
    near1far2 = ao_fudge(near1far2);
    near2far1 = ao_fudge(near2far1);
    far12 = ao_fudge(far12);

    // Perform bilinear interpolation.
    let v = mix(
        mix(near12, near1far2, mix_2),
        mix(near2far1, far12, mix_2),
        mix_1
    );
    // Scale result by sum of valid texels.
    // Because v.a went through the mix, it scales with the proportion of valid texels
    // that were used, so it is always a smooth blend without block edge effects.
    // However, we don't want divide-by-a-small-number effects so we cap the divisor.
    return v.rgb / max(0.1, v.a);
}

// Compute light intensity applying to the fragment.
fn lighting(in: BlockFragmentInput) -> vec3<f32> {
    switch camera.light_option {
        // LightingOption::None or fallback: no lighting
        default: {
            return vec3<f32>(1.0);
        }
        
        // LightingOption::Flat
        case 1 {
            let origin = in.world_cube + in.normal + vec3<f32>(0.5);
            let local_light = light_texture_fetch(origin).rgb;
            return fixed_directional_lighting(in.normal) * local_light;
        }

        // LightingOption::Smooth
        case 2 {
            return fixed_directional_lighting(in.normal) * interpolated_space_light(in);
        }
    }
}

struct Material {
    reflectance: vec4<f32>,
    emission: vec3<f32>,
}
// Get the material details from vertex color or texture lookup.
fn get_material(in: BlockFragmentInput) -> Material {
    if in.color_or_texture[3] < -0.5 {
        // Texture coordinates.
        let atlas_id = i32(round(-in.color_or_texture[3] - 1.0));
        let texcoord: vec3<i32> =
            vec3<i32>(clamp(in.color_or_texture.xyz, in.clamp_min, in.clamp_max));
        
        // If activated, this code will produce an “x-ray” view of all textured surfaces
        // by cutting out all but the clamped border. Other similar changes could be used to
        // visualize the clamping itself without adding any transparency.
        //
        // if (all(texcoord == in.color_or_texture.xyz)) {
        //     discard;
        // }
        
        // Read texture; using textureLoad because our coordinates are in units of texels
        // and we don't want any filtering or wrapping, so using a sampler gives no benefit.
        // Note that coordinate rounding towards zero is effectively floor() since the
        // input is nonnegative.
        if atlas_id == 1 {
            return Material(
                textureLoad(block_g1_reflectance, texcoord, 0),
                textureLoad(block_g1_emission, texcoord, 0).rgb,
            );
        } else if atlas_id == 0 {
            return Material(
                textureLoad(block_g0_reflectance, texcoord, 0),
                vec3<f32>(0.0),
            );
        } else {
            // Error — incorrect atlas ID.
            return Material(
                vec4<f32>(1.0, (-in.color_or_texture[3] - 1.0), 0.0, 1.0),
                vec3<f32>(0.0),
            );
        }
    } else {
        // Solid color.
        return Material(in.color_or_texture, vec3<f32>(0.0));
    }
}

// Apply the effects of distance fog and camera exposure.
// These effects are independent of alpha and therefore the input and output is RGB.
fn apply_fog_and_exposure(
    lit_color: vec3<f32>,
    fog_mix: f32,
    view_direction: vec3<f32>,
) -> vec3<f32> {

    // Fog
    let fog_color = textureSample(skybox_texture, skybox_sampler, view_direction).rgb;
    let fogged_color = mix(lit_color, fog_color, fog_mix);

    // Exposure/eye adaptation
    let exposed_color = fogged_color.rgb * camera.exposure;

    return exposed_color;
}

fn volumetric_transparency(in: BlockFragmentInput, starting_alpha: f32) -> f32 {
    if starting_alpha < 1.0 {
        // Apply volumetric opacity.
        //
        // This is a very crude approximation of future support for more general
        // volumetric/raytraced blocks.
        
        // Run a minimal version of the same raycasting algorithm we use on the CPU side.
        let t_delta = vec3<f32>(
            partial_scale_to_integer_step(in.position_in_cube.x, in.camera_ray_direction.x),
            partial_scale_to_integer_step(in.position_in_cube.y, in.camera_ray_direction.y),
            partial_scale_to_integer_step(in.position_in_cube.z, in.camera_ray_direction.z)
        );
        // t_delta now represents the distance, in units of
        // length(in.camera_ray_direction), to the next cube face. Normalize this
        // to obtain a length through the volume.
        let exit_t = min(t_delta.x, min(t_delta.y, t_delta.z));
        let thickness = exit_t * length(in.camera_ray_direction);

        // Convert alpha to transmittance (light transmitted / light received).
        let transmittance = 1.0 - starting_alpha;
        // Adjust transmittance for the thickness relative to an assumed 1.0 thickness.
        let adj_transmittance = pow(transmittance, thickness);
        // Convert back to alpha.
        return 1.0 - adj_transmittance;
    } else {
        return 1.0;
    }
}

// Entry point for opaque geometry.
@fragment
fn block_fragment_opaque(in: BlockFragmentInput) -> @location(0) vec4<f32> {
    let material = get_material(in);
    let lit_color: vec3<f32> = material.reflectance.rgb * lighting(in) + material.emission;
    return vec4<f32>(
        apply_fog_and_exposure(lit_color, in.fog_mix, in.camera_ray_direction),
        1.0,
    );
}

// Entry point for transparency under TransparencyOption::Surface.
@fragment
fn block_fragment_transparent_surface(in: BlockFragmentInput) -> @location(0) vec4<f32> {
    let material = get_material(in);
    let lit_color = material.reflectance * vec4(lighting(in), 1.0) + vec4(material.emission, 0.0);
    let exposed_color = vec4<f32>(
        apply_fog_and_exposure(lit_color.rgb, in.fog_mix, in.camera_ray_direction),
        lit_color.a,
    );
    return vec4<f32>(exposed_color.rgb * exposed_color.a, exposed_color.a);
}

// Entry point for transparency under TransparencyOption::Volumetric.
@fragment
fn block_fragment_transparent_volumetric(in: BlockFragmentInput) -> @location(0) vec4<f32> {
    var material = get_material(in);

    // Apply volumetric adjustment
    material.reflectance.a = volumetric_transparency(in, material.reflectance.a);

    let lit_color = material.reflectance * vec4(lighting(in), 1.0) + vec4(material.emission, 0.0);
    let exposed_color = vec4<f32>(
        apply_fog_and_exposure(lit_color.rgb, in.fog_mix, in.camera_ray_direction),
        lit_color.a,
    );
    return vec4<f32>(exposed_color.rgb * exposed_color.a, exposed_color.a);
}

// --- Lines shader ------------------------------------------------------------
//
// This is in the same shader source file as the block shader so that it can share
// the camera struct and uniform.

// Vertex-to-fragment data for lines
struct LinesFragmentInput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec4<f32>,
    @location(1) fog_mix: f32,
};

@vertex
fn lines_vertex(
    input: WgpuLinesVertex,
) -> LinesFragmentInput {
    return LinesFragmentInput(
        camera.projection * camera.view_matrix * vec4<f32>(input.position, 1.0),
        input.color,
        compute_fog(input.position),
    );
}

@fragment
fn lines_fragment(input: LinesFragmentInput) -> @location(0) vec4<f32> {
    // TODO: refactor so that the skybox data is available when drawing lines,
    // then implement fogging. (Or maybe the skybox should be moved entirely to the
    // postprocessing pass?)
    return input.color;
}

// --- Skybox shader -----------------------------------------------------------

// Vertex-to-fragment data for skybox
struct SkyboxFragmentInput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) camera_ray_direction: vec3<f32>,
};

fn fullscreen_vertex_position(vertex_index: u32) -> vec4<f32> {
    return vec4<f32>(
        mix(-1.0, 3.0, f32(vertex_index == 1u)),
        mix(-1.0, 3.0, f32(vertex_index == 2u)),
        1.0, // make this a cube corner point
        1.0
    );
}

@vertex
fn skybox_vertex(
    @builtin(vertex_index) vertex_index: u32,
) -> SkyboxFragmentInput {
    // Construct the transpose (inverse) view rotation
    // to find the ray direction starting from our NDC-space triangle points.
    let inverse_view = transpose(mat3x3<f32>(
        camera.view_matrix[0].xyz,
        camera.view_matrix[1].xyz,
        camera.view_matrix[2].xyz));

    let position = fullscreen_vertex_position(vertex_index);

    return SkyboxFragmentInput(
        position,
        inverse_view * (camera.inverse_projection * position).xyz,
    );
}


@fragment
fn skybox_fragment(in: SkyboxFragmentInput) -> @location(0) vec4<f32> {
    return vec4<f32>(apply_fog_and_exposure(vec3(0.0), 1.0, in.camera_ray_direction), 1.0);
}

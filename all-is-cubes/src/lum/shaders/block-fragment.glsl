// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

/// Scale final brightness outputs by this value
uniform mediump float exposure;

in highp vec3 v_position;
in highp vec3 v_position_in_cube;
in mediump vec4 v_color_or_texture;
in mediump vec3 v_normal;
in mediump vec3 v_clamp_min;
in mediump vec3 v_clamp_max;

#ifdef SMOOTH_LIGHTING
  // Two positive unit vectors perpendicular to the normal vector.
  in lowp vec3 v_perpendicular_1, v_perpendicular_2;
#else
  in lowp vec3 v_lighting;
#endif

// What fraction of the fragment color should be fog?
in lowp float fog_mix;

// Direction vector in world coordinate axes (same as v_position) which points
// from the camera position to this fragment.
in highp vec3 camera_ray_direction;

out mediump vec4 fragment_color_srgb;

mediump float luminance(mediump vec3 linear_rgb) {
  return dot(linear_rgb, vec3(0.2126, 0.7152, 0.0722));
}

mediump vec3 tone_map(mediump vec3 linear_rgb) {
  #if TONE_MAPPING_ID == 0
    // Clamp (implicitly)
    return linear_rgb;
  #elif TONE_MAPPING_ID == 1
    // Reinhard
    // TODO: Explain exactly which Reinhard, citation, etc
    return linear_rgb / (1.0 + luminance(linear_rgb));
  #endif
}

// Find the smallest positive `t` such that `s + t * ds` is an integer,
// given that `s` is in the range 0 to 1.
//
// If `ds` is zero, returns positive infinity; this is a useful answer because
// it means that the less-than comparisons in the raycast algorithm will never
// pick the corresponding axis. If any input is NaN, returns NaN.
//
// The canonical version of this algorithm is
// `all_is_cubes::raycast::scale_to_integer_step`.
// TODO: Add crosscheck test cases once we have the ability to run shader unit tests.
mediump float partial_scale_to_integer_step(mediump float s, mediump float ds) {
  s = clamp(s, 0.0, 1.0);  // Out of bounds may appear on triangle edges
  if (sign(ds) < 0.0) {
        s = 1.0 - s;
        ds = -ds;
        // Note: This will not act on a negative zero.
        // That must be handled separately.
    }
    // problem is now s + t * ds = 1
    mediump float result = (1.0 - s) / ds;

    // Fix sign error in case of negative zero.
    result *= (s < 0.0) ? -1.0 : 1.0;

    return result;
}


// Simple directional lighting used to give corners extra definition.
// Note that this algorithm is also implemented in the Rust code.
lowp float fixed_directional_lighting() {
  lowp vec3 normal = vec3(v_normal);
  const lowp vec3 light_1_direction = vec3(0.4, -0.1, 0);
  const lowp vec3 light_2_direction = vec3(-0.4, 0.35, 0.25);
  return (1.0 - 1.0 / 16.0) + 0.25 * (max(0.0, dot(light_1_direction, normal)) + max(0.0, dot(light_2_direction, normal)));
}

bool valid_light(vec4 light) {
  return light.a > 0.5;
}

// Tweak a light value for ambient occlusion -- and convert the light status 
// value returned from light_texture_fetch to an interpolation coefficient.
lowp vec4 ao_fudge(lowp vec4 light_value) {
  // TODO: Make this a (uniform) graphics option
  const lowp float fudge = 0.25;
  lowp float status = light_value.a;
  // Fudge applies only to opaque cubes, not to no-rays cubes.
  // This multiplication provides a branchless calculation:
  // If status is -1 (no-rays or uninitialized), return 0.
  // If status is 0 (opaque), return fudge value.
  // If status is 1 (normal light value), return that.
  return vec4(light_value.rgb, float(status > -0.5) * max(status, fudge));
}

#ifdef SMOOTH_LIGHTING
// Compute the interpolated ('smooth') light for the surface from light_texture.
// This implementation is duplicated in Rust at src/raytracer.rs
lowp vec3 interpolated_space_light() {
  // About half the size of the smallest permissible voxel.
  const highp float above_surface_epsilon = 0.5 / 256.0;

  // The position we should start with for texture lookup and interpolation.
  highp vec3 origin = v_position + v_normal * above_surface_epsilon;

  // Find linear interpolation coefficients based on where we are relative to
  // a half-cube-offset grid.
  mediump float mix_1 = mod(dot(origin, v_perpendicular_1) - 0.5, 1.0);
  mediump float mix_2 = mod(dot(origin, v_perpendicular_2) - 0.5, 1.0);

  // Ensure that mix <= 0.5, i.e. the 'near' side below is the side we are on
  lowp vec3 dir_1 = v_perpendicular_1;
  lowp vec3 dir_2 = v_perpendicular_2;
  if (mix_1 > 0.5) {
    dir_1 *= -1.0;
    mix_1 = 1.0 - mix_1;
  }
  if (mix_2 > 0.5) {
    dir_2 *= -1.0;
    mix_2 = 1.0 - mix_2;
  }

  // Modify interpolation by smoothstep to change the visual impression towards
  // "blurred blocks" and away from the diamond-shaped gradients of linear interpolation
  // which, being so familiar, can give an unfortunate impression of "here is 
  // a closeup of a really low-resolution texture".
  mix_1 = smoothstep(0.0, 1.0, mix_1);
  mix_2 = smoothstep(0.0, 1.0, mix_2);

  // Retrieve texels, again using the half-cube-offset grid (this way we won't have edge artifacts).
  const mediump float lin_lo = -0.5;
  const mediump float lin_hi = +0.5;
  lowp vec4 near12    = light_texture_fetch(origin + lin_lo * dir_1 + lin_lo * dir_2);
  lowp vec4 near1far2 = light_texture_fetch(origin + lin_lo * dir_1 + lin_hi * dir_2);
  lowp vec4 near2far1 = light_texture_fetch(origin + lin_hi * dir_1 + lin_lo * dir_2);
  lowp vec4 far12     = light_texture_fetch(origin + lin_hi * dir_1 + lin_hi * dir_2);
  
  if (!valid_light(near1far2) && !valid_light(near2far1)) {
    // The far corner is on the other side of a diagonal wall, so should be
    // ignored to prevent light leaks.
    far12 = near12;
  }

  // Apply ambient occlusion.
  near12    = ao_fudge(near12);
  near1far2 = ao_fudge(near1far2);
  near2far1 = ao_fudge(near2far1);
  far12     = ao_fudge(far12);

  // Perform bilinear interpolation.
  lowp vec4 v = mix(
    mix(near12,    near1far2, mix_2),
    mix(near2far1, far12,     mix_2),
    mix_1
  );
  // Scale result by sum of valid texels.
  // Because v.a went through the mix, it scales with the proportion of valid texels
  // that were used, so it is always a smooth blend without block edge effects.
  // However, we don't want divide-by-a-small-number effects so we cap the divisor.
  return v.rgb / max(0.1, v.a);
}
#endif

lowp vec3 lighting() {
  #ifdef LIGHTING
    lowp vec3 local_light;
    #ifdef SMOOTH_LIGHTING
      local_light = interpolated_space_light();
    #else
      local_light = v_lighting;
    #endif
    return fixed_directional_lighting() * local_light;
  #else
    return vec3(1.0);
  #endif
}

void main(void) {
  // Parse multipurpose v_color_or_texture.
  // In either case, the colors given are non-premultiplied-alpha.
  // TODO: Consider changing that.
  mediump vec4 diffuse_color;
  if (v_color_or_texture[3] < -0.5) {
    // Texture coordinates.
    mediump vec3 unclamped = v_color_or_texture.stp;
    mediump vec3 texcoord = clamp(unclamped, v_clamp_min, v_clamp_max);
    diffuse_color = texture(block_texture, texcoord);

    #ifdef DEBUG_TEXTURE_EDGE
      // Visualize the texture coordinate clamp boundaries, which happens to
      // double as visualizing the edges of textured quads.
      if (texcoord != v_color_or_texture.stp) {
        bool dither = mod(dot(floor(gl_FragCoord.xy / 2.0), vec2(1.0, 1.0)), 2.0) > 0.75;
        diffuse_color = dither ? vec4(0.7, 0.2, 0.2, 1.0) : vec4(vec3(any(lessThan(unclamped, texcoord)) ? 0.0 : 1.0), 1.0);
      }
    #endif
  } else {
    // Solid color.
    diffuse_color = v_color_or_texture;
  }

  #ifdef ALLOW_TRANSPARENCY
    #ifdef VOLUMETRIC
      if (diffuse_color.a < 1.0) {
        // Apply volumetric opacity.
        //
        // This is a very crude approximation of future support for more general
        // volumetric/raytraced blocks.
        
        // Run a minimal version of the same raycasting algorithm we use on the CPU side.
        mediump vec3 t_delta = vec3(
          partial_scale_to_integer_step(v_position_in_cube.x, camera_ray_direction.x),
          partial_scale_to_integer_step(v_position_in_cube.y, camera_ray_direction.y),
          partial_scale_to_integer_step(v_position_in_cube.z, camera_ray_direction.z)
        );
        // t_delta now represents the distance, in units of
        // length(camera_ray_direction), to the next cube face. Normalize this
        // to obtain a length through the volume.
        mediump float exit_t = min(t_delta.x, min(t_delta.y, t_delta.z));
        mediump float thickness = exit_t * length(camera_ray_direction);

        // Convert alpha to transmittance (light transmitted / light received).
        mediump float transmittance = 1.0 - diffuse_color.a;
        // Adjust transmittance for the thickness relative to an assumed 1.0 thickness.
        transmittance = pow(transmittance, thickness);
        // Convert back to alpha.
        diffuse_color.a = 1.0 - transmittance;
      }
    #endif
  #else
    // Ban alpha in our opaque mode, to make mistakes show up faster
    // and to prevent transparent holes in the mesh if the opaque mesh's texture
    // isn't up to date.
    diffuse_color.a = 1.0;
  #endif

  // Lighting
  // TODO: What's a better name for this variable?
  mediump vec4 color = diffuse_color * vec4(lighting(), 1.0);

  // Fog
  color.rgb = mix(color.rgb, fog_color, fog_mix);

  // Exposure/eye adaptation
  color.rgb *= exposure;

  // Tone mapping
  color.rgb = tone_map(color.rgb);

  // Convert from linear to sRGB color.
  // Source: <https://en.wikipedia.org/w/index.php?title=SRGB&oldid=1002296118#The_forward_transformation_(CIE_XYZ_to_sRGB)> (version as of Feb 3, 2020)
  if (true) {
    color.rgb = mix(
      (211. * pow(color.rgb, vec3(5.0 / 12.0)) - vec3(11.0)) / 200.0,
      color.rgb * (323.0 / 25.0),
      vec3(lessThan(color.rgb, vec3(0.0031308)))
    );
  }

  // Multiply alpha because our blend function choice is premultiplied alpha.
  fragment_color_srgb = vec4(color.rgb * color.a, color.a);
}

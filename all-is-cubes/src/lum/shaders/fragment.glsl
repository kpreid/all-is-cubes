// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

in highp vec3 v_position;
in mediump vec4 v_color_or_texture;
in mediump vec3 v_normal;
in mediump vec3 v_clamp_min;
in mediump vec3 v_clamp_max;

// What fraction of the fragment color should be fog?
in lowp float fog_mix;

out mediump vec4 fragment_color;

// Simple directional lighting used to give corners extra definition.
// Note that this algorithm is also implemented in the Rust code.
lowp float fixed_directional_lighting() {
  lowp vec3 normal = vec3(v_normal);
  const lowp vec3 light_1_direction = vec3(0.4, -0.1, 0);
  const lowp vec3 light_2_direction = vec3(-0.4, 0.35, 0.25);
  return (1.0 - 1.0 / 16.0) + 0.25 * (max(0.0, dot(light_1_direction, normal)) + max(0.0, dot(light_2_direction, normal)));
}

// Given integer cube coordinates, fetch and unpack a light_texture RGB value.
// The alpha component is either 0 or 1 indicating whether this is a "valid" 
// light value; one which is neither inside an opaque block nor in empty unlit
// air.
lowp vec4 light_texture_fetch(mediump vec3 p) {
  ivec3 lookup_position = ivec3(floor(p));
  lookup_position += light_offset;
  // Implement wrapping (not automatic since we're using texelFetch).
  // Wrapping is used to handle sky light and in the future will be used for
  // circular buffering of the local light in an unbounded world.
  ivec3 size = textureSize(light_texture, 0);
  lookup_position = (lookup_position % size + size) % size;

  lowp vec4 texel = texelFetch(light_texture, lookup_position, 0);
  lowp vec3 packed_light = texel.rgb;
  lowp vec3 unpacked_light = pow(vec3(2.0), (packed_light - 128.0 / 255.0) * (255.0 / 16.0));

  // See all_is_cubes::space::LightStatus for the value this is interpreting.
  bool valid = texel.a >= 0.5;

  return vec4(unpacked_light, float(valid));
}

lowp vec3 flat_space_light() {
  // About half the size of the smallest permissible voxel.
  const mediump float above_surface_epsilon = 0.5 / 256.0;

  // The position we should start with for texture lookup and interpolation.
  mediump vec3 origin = v_position + v_normal * above_surface_epsilon;

  return light_texture_fetch(origin).rgb;
}

lowp vec3 lighting() {
  lowp vec3 local_light;
  if (true) {  // TODO: uniform or compile-time parameter
    local_light = flat_space_light();
  } else {
    local_light = vec3(1.0);
  }
  return fixed_directional_lighting() * local_light;
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

  if (diffuse_color.a <= 0.0001) {
    // Prevent (effectively) fully transparent areas from writing to the depth buffer.
    // This allows using binary opacity in color or texture without depth sorting.
    discard;
  }

  // Lighting
  // TODO: What's a better name for this variable?
  mediump vec4 color = diffuse_color * vec4(lighting(), 1.0);

  // Fog
  color.rgb = mix(color.rgb, fog_color, fog_mix);

  // Multiply alpha because our blend function choice is premultiplied alpha.
  fragment_color = vec4(color.rgb * color.a, color.a);
}

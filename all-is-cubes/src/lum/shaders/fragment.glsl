// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

in mediump vec4 v_color_or_texture;
in mediump vec3 v_normal;
in mediump vec3 v_clamp_min;
in mediump vec3 v_clamp_max;
in lowp vec3 v_lighting;
// What fraction of the fragment color should be fog?
in lowp float fog_mix;

out mediump vec4 fragment_color;

// Simple directional lighting used to give corners extra definition.
// Note that this algorithm is also implemented in the Rust code.
lowp float fixed_directional_lighting() {
  lowp vec3 normal = vec3(v_normal);
  const lowp vec3 light_1_direction = vec3(0.4,-0.1,0);
  const lowp vec3 light_2_direction = vec3(-0.4,0.35,0.25);
  return (1.0 - 1.0/16.0) + 0.25 * (
    max(0.0, dot(light_1_direction, normal))
    + max(0.0, dot(light_2_direction, normal))
  );
}

lowp vec3 lighting() {
  return fixed_directional_lighting() * v_lighting;
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
        diffuse_color = dither
          ? vec4(0.7, 0.2, 0.2, 1.0)
          : vec4(vec3(
              any(lessThan(unclamped, texcoord)) ? 0.0 : 1.0
            ), 1.0);
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

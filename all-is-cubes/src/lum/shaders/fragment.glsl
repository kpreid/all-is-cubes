// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

in mediump vec4 v_color_or_texture;
in mediump vec3 v_normal;
in lowp vec3 v_lighting;

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
    diffuse_color = texture(block_texture, v_color_or_texture.stp);
  } else {
    // Solid color.
    diffuse_color = v_color_or_texture;
  }

  if (diffuse_color.a <= 0.0001) {
    // Prevent (effectively) fully transparent areas from writing to the depth buffer.
    // This allows using binary opacity in color or texture without depth sorting.
    discard;
  }
  // Multiply alpha because our blend function choice is premultiplied alpha.
  diffuse_color.rgb *= diffuse_color.a;

  // Output fragment color is in premultiplied alpha.
  fragment_color = diffuse_color * vec4(lighting(), 1.0);
}

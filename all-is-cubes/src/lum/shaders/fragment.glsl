// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

in mediump vec4 v_color_or_texture;
in mediump vec3 v_normal;
in lowp vec3 v_lighting;

out mediump vec4 fragment_color;

// Simple directional lighting used to give corners definition.
lowp float fixed_lighting_environment() {
  lowp vec3 normal = vec3(v_normal);
  const lowp vec3 light_1_direction = vec3(0.4,-0.1,0);
  const lowp vec3 light_2_direction = vec3(-0.4,0.35,0.25);
  return 0.875 + 0.5 * (
    max(0.0, dot(light_1_direction, normal))
    + max(0.0, dot(light_2_direction, normal))
  );
}

lowp vec3 lighting() {
  return fixed_lighting_environment() * v_lighting;
}

void main(void) {
  // Parse multipurpose v_color_or_texture.
  mediump vec4 diffuse_color;
  if (v_color_or_texture[3] < -0.5) {
    // Texture coordinates.
    diffuse_color = texture(block_texture, v_color_or_texture.stp);
  } else {
    // Solid color.
    diffuse_color = v_color_or_texture;
  }

  if (diffuse_color.a <= 0.0001) {
    // Fully transparent.
    discard;
  } else {
    // We don't support partial transparency, so in case it arises, force to opaque.
    diffuse_color.a = 1.0;
  }
  

  fragment_color = diffuse_color * vec4(lighting(), 1.0);
}

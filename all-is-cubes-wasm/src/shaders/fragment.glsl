in mediump vec4 v_color;
in mediump vec3 v_normal;
in mediump vec3 v_tex;
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
  fragment_color = v_color * vec4(lighting(), 1.0) * texture(block_texture, v_tex);
}

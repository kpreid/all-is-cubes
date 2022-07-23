in highp vec3 a_position;
in mediump vec4 a_color_or_texture;

out mediump vec4 v_color;

void main(void) {
  v_color = a_color_or_texture;
  basic_vertex(a_position);
}


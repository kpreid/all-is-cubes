// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

in highp vec3 a_position;
in lowp vec3 a_normal;
in mediump vec4 a_color_or_texture;
in mediump vec3 a_clamp_min;
in mediump vec3 a_clamp_max;

out highp vec3 v_position;
out mediump vec4 v_color_or_texture;
out mediump vec3 v_clamp_min;
out mediump vec3 v_clamp_max;
out lowp vec3 v_normal;

void main(void) {
  basic_vertex(a_position);
  v_position = a_position;
  v_normal = a_normal;
  v_color_or_texture = a_color_or_texture;
  v_clamp_min = a_clamp_min;
  v_clamp_max = a_clamp_max;
}

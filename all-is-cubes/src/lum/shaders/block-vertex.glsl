// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

in highp vec3 a_position;
in highp vec3 a_cube;
in lowp vec3 a_normal;
in mediump vec4 a_color_or_texture;
in mediump vec3 a_clamp_min;
in mediump vec3 a_clamp_max;

out highp vec3 v_position;
out highp vec3 v_position_in_cube;
out mediump vec4 v_color_or_texture;
out mediump vec3 v_clamp_min;
out mediump vec3 v_clamp_max;
out lowp vec3 v_normal;

#ifdef LIGHTING
  #ifdef SMOOTH_LIGHTING
    // Two positive unit vectors perpendicular to the normal vector.
    out lowp vec3 v_perpendicular_1, v_perpendicular_2;
  #else
    out lowp vec3 v_lighting;
  #endif
#endif

lowp vec3 flat_space_light() {
  mediump vec3 origin = a_cube + a_normal + vec3(0.5);
  return light_texture_fetch(origin).rgb;
}

void main(void) {
  basic_vertex(a_position);
  v_position = a_position;
  v_normal = a_normal;
  v_color_or_texture = a_color_or_texture;
  v_clamp_min = a_clamp_min;
  v_clamp_max = a_clamp_max;

  v_position_in_cube = a_position - a_cube;

  #ifdef LIGHTING
    #ifdef SMOOTH_LIGHTING
      // Choose two vectors that are perpendicular to each other and the normal,
      // and in the positive direction on that axis.
      // Assumes v_normal is an axis-aligned unit vector.
      if (v_normal.x != 0.0) {
        v_perpendicular_1 = vec3(0.0, 1.0, 0.0);
        v_perpendicular_2 = vec3(0.0, 0.0, 1.0);
      } else {
        v_perpendicular_1 = vec3(1.0, 0.0, 0.0);
        v_perpendicular_2 = abs(cross(v_perpendicular_1, v_normal));
      }
    #else
      v_lighting = flat_space_light();
    #endif
  #endif
}


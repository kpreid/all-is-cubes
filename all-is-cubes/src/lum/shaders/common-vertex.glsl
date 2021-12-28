// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

// What fraction of the fragment color should be fog?
out lowp float fog_mix;

// Direction vector in world coordinate axes (same as vertex_position)
// from the camera position to this vertex (and thus, when interpolated,
// to the fragment as well).
out highp vec3 camera_ray_direction;

// Physically realistic fog, but doesn't ever reach 1 (fully opaque).
lowp float fog_exponential(highp float d) {
  const lowp float fog_density = 1.6;
  return 1.0 - exp(-fog_density * d);
}

// Fog that goes all the way from fully transparent to fully opaque.
// The correction is smaller the denser the fog.
lowp float fog_exp_fudged(highp float d) {
  return fog_exponential(d) / fog_exponential(1.0);
}

lowp float fog_combo(highp float d) {
  // Combination of realistic exponential (constant density) fog,
  // and slower-starting fog so nearby stuff is clearer.
  return mix(fog_exp_fudged(d), pow(d, 4.0), fog_mode_blend);
}

void basic_vertex(highp vec3 vertex_position) {
  // Camera-relative position not transformed by projection.
  highp vec4 eye_vertex_position = view_matrix * vec4(vertex_position, 1.0);
  highp float distance_from_eye = length(eye_vertex_position.xyz);

  // Send direction vector to fragment shader.
  // Note that we do not normalize this vector: by keeping things linear, we
  // allow linear interpolation between vertices to get the right answer.
  camera_ray_direction = vertex_position - view_position;

  // Distance in range 0 (camera position) to 1 (opaque fog position/far clip position).
  highp float normalized_distance = distance_from_eye / fog_distance;
  fog_mix = clamp(fog_combo(normalized_distance), 0.0, 1.0);

  gl_Position = projection_matrix * eye_vertex_position;
}

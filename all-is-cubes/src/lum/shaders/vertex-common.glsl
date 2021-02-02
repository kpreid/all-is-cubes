// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

void basicVertex(highp vec3 vertex_position) {
  gl_Position = projection_matrix * view_matrix * vec4(vertex_position, 1.0);
}

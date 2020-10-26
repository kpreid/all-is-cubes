
void basicVertex(highp vec3 vertex_position) {
  gl_Position = projection_matrix * view_matrix * vec4(vertex_position, 1.0);
}

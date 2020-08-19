
void basicVertex(highp vec3 vertex_position) {
  highp mat4 projection_matrix = mat4(projection_matrix0, projection_matrix1, projection_matrix2, projection_matrix3);
  highp mat4 view_matrix = mat4(view_matrix0, view_matrix1, view_matrix2, view_matrix3);
  gl_Position = projection_matrix * view_matrix * vec4(vertex_position, 1.0);
}

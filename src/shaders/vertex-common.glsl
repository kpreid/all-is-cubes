void basicVertex(highp vec3 vertex_position) {
  // Temporary placeholder, to be replaced with actual matrices
  vertex_position /= 30.0;

  gl_Position = vec4(vertex_position, 1.0);
}

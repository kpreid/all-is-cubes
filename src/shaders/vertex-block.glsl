in highp vec3 vertex_position;
in mediump vec4 vertex_color;

out mediump vec4 varying_color;

void main(void) {
  basicVertex(vertex_position);
  varying_color = vertex_color;
}

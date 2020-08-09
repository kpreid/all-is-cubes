in highp vec3 position;
in mediump vec4 color;

out mediump vec4 varying_color;

void main(void) {
  basicVertex(position);
  varying_color = color;
}

in highp vec3 a_position;
in lowp vec3 a_normal;
in mediump vec4 a_color;

out mediump vec4 v_color;
out lowp vec3 v_normal;

void main(void) {
  basicVertex(a_position);
  v_normal = a_normal;
  v_color = a_color;
}

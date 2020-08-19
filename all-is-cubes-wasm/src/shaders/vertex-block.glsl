in highp vec3 a_position;
in lowp vec3 a_normal;
in mediump vec4 a_color;
in lowp vec3 a_lighting;

out mediump vec4 v_color;
out lowp vec3 v_normal;
out lowp vec3 v_lighting;

void main(void) {
  basicVertex(a_position);
  v_normal = a_normal;
  v_color = a_color;
  v_lighting = a_lighting;
}

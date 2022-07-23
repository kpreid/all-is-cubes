in mediump vec4 v_color;

out mediump vec4 fragment_color_srgb;

void main(void) {
  mediump vec4 color = v_color;

  // Convert from linear to sRGB color.
  // Source: <https://en.wikipedia.org/w/index.php?title=SRGB&oldid=1002296118#The_forward_transformation_(CIE_XYZ_to_sRGB)> (version as of Feb 3, 2020)
  // TODO: deduplicate shared code with fragment.glsl
  color.rgb = mix(
    (211. * pow(color.rgb, vec3(5.0 / 12.0)) - vec3(11.0)) / 200.0,
    color.rgb * (323.0 / 25.0),
    vec3(lessThan(color.rgb, vec3(0.0031308)))
  );

  // Multiply alpha because our blend function choice is premultiplied alpha.
  fragment_color_srgb = vec4(color.rgb * color.a, color.a);
}

// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

out highp vec2 texcoord;

/// A single triangle that covers all of the screen area
const highp vec2[3] VERTICES = vec2[](
    vec2(-1., -1.),
    vec2(3., -1.),
    vec2(-1., 3.)
);

void main() {
    highp vec2 ndc_vertex = VERTICES[gl_VertexID];
    texcoord = (ndc_vertex + vec2(1.0)) * 0.5;
    gl_Position = vec4(ndc_vertex, 0.5, 1.);
}

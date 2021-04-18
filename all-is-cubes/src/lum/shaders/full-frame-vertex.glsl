// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

/// A single triangle that covers all of the screenarea
const highp vec2[3] VERTICES = vec2[](
    vec2(-1., -1.),
    vec2(3., -1.),
    vec2(-1., 3.)
);
void main() {
    gl_Position = vec4(VERTICES[gl_VertexID], 0.5, 1.);
}

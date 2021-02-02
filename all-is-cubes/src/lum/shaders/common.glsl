// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

uniform highp mat4 projection_matrix;
uniform highp mat4 view_matrix;

uniform lowp sampler3D block_texture;

// How far out should be fully fogged?
uniform highp float fog_distance;

// What color should fog fade into?
uniform mediump vec3 fog_color;

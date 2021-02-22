// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

// TODO: Move these debug options into being set from host code
// #define DEBUG_TEXTURE_EDGE

uniform highp mat4 projection_matrix;
uniform highp mat4 view_matrix;

uniform lowp sampler3D block_texture;

// Fog equation blending: 0 is realistic fog and 1 is distant more abrupt fog.
// TODO: Replace this uniform with a compiled-in flag since it doesn't need to be continuously changing.
uniform lowp float fog_mode_blend;

// How far out should be fully fogged?
uniform highp float fog_distance;

// What color should fog fade into?
uniform mediump vec3 fog_color;

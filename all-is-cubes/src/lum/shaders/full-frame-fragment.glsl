// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

in highp vec2 texcoord;
out mediump vec4 color;
uniform sampler2D frame_texture;
void main() {
    color = texture(frame_texture, texcoord);
}

// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

out mediump vec4 color;
uniform sampler2D texture;
void main() {
    color = texelFetch(texture, ivec2(gl_FragCoord.xy), 0);
}

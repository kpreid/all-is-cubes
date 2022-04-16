// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

uniform sampler2D frame_texture;
in highp vec2 texcoord;
out mediump vec4 color;

void main() {
    // Y flip to draw texture with Y-down coordinates
    highp vec2 texcoord_flip = vec2(texcoord.x, 1.0 - texcoord.y);

    // This program copies values to the framebuffer with no conversion, and as such,
    // expects the texture to produce sRGB values. That is, the pixel format should
    // *not* be one which implicitly converts sRGB to linear.

    vec2 derivatives = vec2(dFdx(texcoord_flip.x), dFdy(texcoord_flip.y));

    lowp float shadowing = 0.0;
    const int radius = 2;
    for (int dx = -radius; dx <= radius; dx++)
    for (int dy = -radius; dy <= radius; dy++) {
        highp vec2 offset = vec2(ivec2(dx, dy));
        lowp float offset_alpha = texture(frame_texture, texcoord_flip + offset * derivatives).a;
        lowp float weight = 0.2 / max(1.0, length(offset));
        shadowing += offset_alpha * weight;
    }
    shadowing = clamp(shadowing, 0.0, 0.5);

    lowp vec4 foreground_texel = texture(frame_texture, texcoord_flip);

    // Shadow layer
    color = mix(vec4(0.0), vec4(vec3(0.0), 1.0), shadowing);
    // Blend foreground layer (ignoring texture color)
    color = mix(color, vec4(vec3(1.0), 1.0), foreground_texel.a);
}

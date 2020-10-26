// TODO: Passing the projection matrix as four vectors due to bug
//     https://github.com/phaazon/luminance-rs/issues/434
uniform highp mat4 projection_matrix;
uniform highp mat4 view_matrix;

uniform lowp sampler2DArray block_texture;

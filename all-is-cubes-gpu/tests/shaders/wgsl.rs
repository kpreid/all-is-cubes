//! Functions for generating test case WGSL code.

use std::fmt;

use all_is_cubes::cgmath;
use all_is_cubes::util::CustomFormat;

/// Generate a fragment shader entry point from an expression.
pub fn frag_expr(expr: &str) -> String {
    format!(
        "@fragment
        fn test_entry_point(in: BlockFragmentInput) -> @location(0) vec4<f32> {{
            return {expr};
        }}"
    )
}

trait WgslTypeName {
    fn wgsl_type_name() -> &'static str;
}

/// Serialize data structures to WGSL text.
#[derive(Clone, Copy, Debug)]
pub struct ToWgsl;

pub fn to_wgsl<T: CustomFormat<ToWgsl>>(value: T) -> String {
    value.custom_format(ToWgsl).to_string()
}

impl CustomFormat<ToWgsl> for f32 {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ToWgsl) -> fmt::Result {
        write!(fmt, "{:?}", self)
    }
}

impl<T> CustomFormat<ToWgsl> for cgmath::Vector4<T>
where
    T: CustomFormat<ToWgsl> + WgslTypeName,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, f: ToWgsl) -> fmt::Result {
        let cgmath::Vector4 { x, y, z, w } = self;
        let t = T::wgsl_type_name();
        write!(
            fmt,
            "vec4<{t}>({x}, {y}, {z}, {w})",
            x = x.custom_format(f),
            y = y.custom_format(f),
            z = z.custom_format(f),
            w = w.custom_format(f),
        )
    }
}

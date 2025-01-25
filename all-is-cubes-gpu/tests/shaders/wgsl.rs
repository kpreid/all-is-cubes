//! Functions for generating test case WGSL code.

use core::fmt;

use all_is_cubes::util::{Fmt, Refmt as _};

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
pub(crate) struct ToWgsl;

pub(crate) fn to_wgsl<T: Fmt<ToWgsl>>(value: T) -> String {
    value.refmt(&ToWgsl).to_string()
}

impl Fmt<ToWgsl> for f32 {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &ToWgsl) -> fmt::Result {
        write!(fmt, "{self:?}")
    }
}

impl<T> Fmt<ToWgsl> for [T; 4]
where
    T: Fmt<ToWgsl> + WgslTypeName,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, f: &ToWgsl) -> fmt::Result {
        let [x, y, z, w] = self;
        let t = T::wgsl_type_name();
        write!(
            fmt,
            "vec4<{t}>({x}, {y}, {z}, {w})",
            x = x.refmt(f),
            y = y.refmt(f),
            z = z.refmt(f),
            w = w.refmt(f),
        )
    }
}

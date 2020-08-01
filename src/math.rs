// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Mathematical utilities and decisions.

use std::ops::{Rem, Add};
use cgmath::Point3;

pub type GridCoordinate = isize;
pub type GridPoint = Point3<GridCoordinate>;
pub type FreeCoordinate = f64;

/// Implement modulo in terms of remainder.
pub fn modulo<T : Copy + Rem<Output = T> + Add<Output = T>>(value :T, modulus :T) -> T
{
    // Remainder, which lies in the range (-modulus, +modulus).
    let remainder = value % modulus;
    // Shift the range to (0, 2*modulus).
    let guaranteed_positive = remainder + modulus;
    // Collapse the two cases (0, modulus) and [modulus, 2*modulus) to [0, modulus).
    return guaranteed_positive % modulus;
}


#[cfg(test)]
mod tests {
    use super::*;

    // Tests for modulo, which is not currently a public function so can't have doc tests.

    #[test]
    fn modulo_positive() { 
        assert_eq!(modulo(0.0, 1.0), 0.0);
        assert_eq!(modulo(0.25, 1.0), 0.25);
        assert_eq!(modulo(1.0, 1.0), 0.0);
        assert_eq!(modulo(1.25, 1.0), 0.25);
        assert_eq!(modulo(6.25, 1.0), 0.25);
        
        assert_eq!(modulo(0.0, 1.5), 0.0);
        assert_eq!(modulo(1.0, 1.5), 1.0);
        assert_eq!(modulo(1.5, 1.5), 0.0);
        assert_eq!(modulo(1.625, 1.5), 0.125);
    }

    #[test]
    fn modulo_negative_value() { 
        assert_eq!(modulo(-0.0, 1.0), 0.0);
        assert_eq!(modulo(-0.25, 1.0), 0.75);
        assert_eq!(modulo(-1.0, 1.0), 0.0);
        assert_eq!(modulo(-1.25, 1.0), 0.75);
        assert_eq!(modulo(-6.25, 1.0), 0.75);
    }

    #[test]
    fn modulo_negative_modulus() { 
        assert_eq!(modulo(0.0, -1.0), -0.0);
        assert_eq!(modulo(0.25, -1.0), -0.75);
        assert_eq!(modulo(1.0, -1.0), -0.0);
        assert_eq!(modulo(1.25, -1.0), -0.75);
        assert_eq!(modulo(6.25, -1.0), -0.75);
    }

    #[test]
    #[should_panic]
    fn modulo_zero_int() { modulo(3, 0); }

    #[test]
    // Note: Not specifically desiring this behavior, just documenting it.
    fn modulo_zero_float() { assert!(modulo(3.0 as f64, 0.0).is_nan()); }
}

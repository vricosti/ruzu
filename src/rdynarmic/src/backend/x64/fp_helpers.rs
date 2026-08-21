// Pure Rust implementations of ARM floating-point semantics.
//
// These are used as host_call fallback functions when there is no native
// x86-64 instruction that implements the exact ARM semantics.

use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::op::fp_convert::fp_convert;
use crate::common::fp::op::fp_mul_add::{fp_mul_add, fp_mul_sub};
use crate::common::fp::op::fp_recip_estimate::fp_recip_estimate;
use crate::common::fp::op::fp_recip_exponent::fp_recip_exponent;
use crate::common::fp::op::fp_recip_step_fused::fp_recip_step_fused;
use crate::common::fp::op::fp_rsqrt_estimate::fp_rsqrt_estimate;
use crate::common::fp::op::fp_rsqrt_step_fused::fp_rsqrt_step_fused;
use crate::common::fp::op::fp_to_fixed::fp_to_fixed;
use crate::common::fp::rounding_mode::RoundingMode;

// ---------------------------------------------------------------------------
// Half-precision (f16) conversion helpers
// ---------------------------------------------------------------------------

/// Convert half-precision float (u16 bits) to f32.
pub fn f16_to_f32(half: u16) -> f32 {
    let sign = ((half >> 15) & 1) as u32;
    let exp = ((half >> 10) & 0x1F) as u32;
    let frac = (half & 0x3FF) as u32;

    if exp == 0 {
        if frac == 0 {
            // ±0
            f32::from_bits(sign << 31)
        } else {
            // Denormalized: renormalize
            let mut f = frac;
            let mut e = 0i32;
            while (f & 0x400) == 0 {
                f <<= 1;
                e -= 1;
            }
            f &= !0x400; // remove implicit bit
            let exp32 = (127 - 15 + 1 + e) as u32;
            f32::from_bits((sign << 31) | (exp32 << 23) | (f << 13))
        }
    } else if exp == 0x1F {
        if frac == 0 {
            // ±Infinity
            f32::from_bits((sign << 31) | 0x7F80_0000)
        } else {
            // NaN — preserve payload
            f32::from_bits((sign << 31) | 0x7FC0_0000 | (frac << 13))
        }
    } else {
        // Normalized
        let exp32 = exp + 127 - 15;
        f32::from_bits((sign << 31) | (exp32 << 23) | (frac << 13))
    }
}

/// Convert f32 to half-precision float (u16 bits), rounding to nearest even.
pub fn f32_to_f16(value: f32) -> u16 {
    let bits = value.to_bits();
    let sign = (bits >> 31) & 1;
    let exp = ((bits >> 23) & 0xFF) as i32;
    let frac = bits & 0x7F_FFFF;

    if exp == 0xFF {
        // Inf or NaN
        if frac == 0 {
            // Infinity
            ((sign << 15) | 0x7C00) as u16
        } else {
            // NaN — preserve some payload, set quiet bit
            ((sign << 15) | 0x7E00 | (frac >> 13)) as u16
        }
    } else if exp > 142 {
        // Overflow → infinity
        ((sign << 15) | 0x7C00) as u16
    } else if exp < 113 {
        // Underflow → zero or denorm
        if exp < 103 {
            (sign << 15) as u16
        } else {
            let mant = frac | 0x80_0000;
            let shift = 126 - exp;
            let half_frac = mant >> (shift as u32);
            // Round to nearest even
            let round_bit = (mant >> ((shift - 1) as u32)) & 1;
            let sticky = if (shift - 1) > 0 {
                mant & ((1 << ((shift - 1) as u32)) - 1)
            } else {
                0
            };
            let result = (half_frac >> 13) as u16;
            let result = if round_bit != 0 && (sticky != 0 || (result & 1) != 0) {
                result + 1
            } else {
                result
            };
            ((sign << 15) as u16) | result
        }
    } else {
        // Normalized
        let half_exp = ((exp - 112) as u32) << 10;
        let half_frac = frac >> 13;
        // Round to nearest even
        let round_bit = (frac >> 12) & 1;
        let sticky = frac & 0xFFF;
        let result = ((sign << 15) | half_exp | half_frac) as u16;
        if round_bit != 0 && (sticky != 0 || (result & 1) != 0) {
            result + 1
        } else {
            result
        }
    }
}

/// Convert half-precision float (u16 bits) to f64.
pub fn f16_to_f64(half: u16) -> f64 {
    f16_to_f32(half) as f64
}

/// Convert f64 to half-precision float (u16 bits).
pub fn f64_to_f16(value: f64) -> u16 {
    f32_to_f16(value as f32)
}

// ---------------------------------------------------------------------------
// ARM FP multiply extended (FPMulX)
// ---------------------------------------------------------------------------

/// ARM FPMulX32: like multiply but ±0 × ±∞ = ±2.0 instead of NaN.
pub extern "C" fn fp_mul_x32(a: u32, b: u32) -> u32 {
    let fa = f32::from_bits(a);
    let fb = f32::from_bits(b);

    let a_is_zero = fa == 0.0 || fa == -0.0;
    let b_is_inf = fb.is_infinite();
    let a_is_inf = fa.is_infinite();
    let b_is_zero = fb == 0.0 || fb == -0.0;

    if (a_is_zero && b_is_inf) || (a_is_inf && b_is_zero) {
        // Result sign = XOR of input signs
        let sign = (a ^ b) & 0x8000_0000;
        // ±2.0
        (sign | 0x4000_0000).to_le()
    } else {
        (fa * fb).to_bits()
    }
}

/// ARM FPMulX64: like multiply but ±0 × ±∞ = ±2.0 instead of NaN.
pub extern "C" fn fp_mul_x64(a: u64, b: u64) -> u64 {
    let fa = f64::from_bits(a);
    let fb = f64::from_bits(b);

    let a_is_zero = fa == 0.0 || fa == -0.0;
    let b_is_inf = fb.is_infinite();
    let a_is_inf = fa.is_infinite();
    let b_is_zero = fb == 0.0 || fb == -0.0;

    if (a_is_zero && b_is_inf) || (a_is_inf && b_is_zero) {
        let sign = (a ^ b) & 0x8000_0000_0000_0000;
        // ±2.0 in f64
        sign | 0x4000_0000_0000_0000
    } else {
        (fa * fb).to_bits()
    }
}

// ---------------------------------------------------------------------------
// ARM FP reciprocal/sqrt estimates
// ---------------------------------------------------------------------------

unsafe fn with_fpsr<T>(fpsr_exc: *mut u32, operation: impl FnOnce(&mut Fpsr) -> T) -> T {
    assert!(!fpsr_exc.is_null());
    let mut fpsr = Fpsr::new(unsafe { fpsr_exc.read() });
    let result = operation(&mut fpsr);
    unsafe { fpsr_exc.write(fpsr.value()) };
    result
}

pub extern "C" fn fp_recip_estimate32(bits: u32, fpcr: u32, fpsr_exc: *mut u32) -> u32 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_recip_estimate(bits, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_recip_estimate64(bits: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_recip_estimate(bits, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_recip_exponent32(bits: u32, fpcr: u32, fpsr_exc: *mut u32) -> u32 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_recip_exponent(bits, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_recip_exponent64(bits: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_recip_exponent(bits, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_recip_step_fused32(a: u32, b: u32, fpcr: u32, fpsr_exc: *mut u32) -> u32 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_recip_step_fused(a, b, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_recip_step_fused64(a: u64, b: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_recip_step_fused(a, b, Fpcr::new(fpcr), fpsr)
        })
    }
}

/// ARM FPRSqrtEstimate32.
pub extern "C" fn fp_rsqrt_estimate32(bits: u32, fpcr: u32, fpsr_exc: *mut u32) -> u32 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_rsqrt_estimate(bits, Fpcr::new(fpcr), fpsr)
        })
    }
}

/// ARM FPRSqrtEstimate64.
pub extern "C" fn fp_rsqrt_estimate64(bits: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_rsqrt_estimate(bits, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_rsqrt_step_fused32(a: u32, b: u32, fpcr: u32, fpsr_exc: *mut u32) -> u32 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_rsqrt_step_fused(a, b, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_rsqrt_step_fused64(a: u64, b: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_rsqrt_step_fused(a, b, Fpcr::new(fpcr), fpsr)
        })
    }
}

// ---------------------------------------------------------------------------
// Half-precision FP helpers (extern "C" for host_call)
// ---------------------------------------------------------------------------

/// FPAbs16: clear sign bit of half-precision float.
pub extern "C" fn fp_abs16(bits: u64) -> u64 {
    (bits as u16 & 0x7FFF) as u64
}

/// FPNeg16: flip sign bit of half-precision float.
pub extern "C" fn fp_neg16(bits: u64) -> u64 {
    (bits as u16 ^ 0x8000) as u64
}

fn fp_convert_rounding_mode(rounding: u32) -> RoundingMode {
    match rounding {
        0 => RoundingMode::ToNearestTieEven,
        1 => RoundingMode::TowardsPlusInfinity,
        2 => RoundingMode::TowardsMinusInfinity,
        3 => RoundingMode::TowardsZero,
        4 => RoundingMode::ToNearestTieAwayFromZero,
        5 => RoundingMode::ToOdd,
        _ => unreachable!("invalid FP conversion rounding mode {rounding}"),
    }
}

macro_rules! define_fp_convert_helper {
    ($name:ident, $to:ty, $from:ty) => {
        pub extern "C" fn $name(bits: u64, fpcr: u32, rounding: u32, fpsr_exc: *mut u32) -> u64 {
            unsafe {
                with_fpsr(fpsr_exc, |fpsr| {
                    fp_convert::<$to, $from>(
                        bits as $from,
                        Fpcr::new(fpcr),
                        fp_convert_rounding_mode(rounding),
                        fpsr,
                    ) as u64
                })
            }
        }
    };
}

define_fp_convert_helper!(fp_half_to_single, u32, u16);
define_fp_convert_helper!(fp_half_to_double, u64, u16);
define_fp_convert_helper!(fp_single_to_half, u16, u32);
define_fp_convert_helper!(fp_single_to_double, u64, u32);
define_fp_convert_helper!(fp_double_to_half, u16, u64);
define_fp_convert_helper!(fp_double_to_single, u32, u64);

/// FPRecipEstimate16.
pub extern "C" fn fp_recip_estimate16(bits: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_recip_estimate(bits as u16, Fpcr::new(fpcr), fpsr) as u64
        })
    }
}

/// FPRecipExponent16.
pub extern "C" fn fp_recip_exponent16(bits: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_recip_exponent(bits as u16, Fpcr::new(fpcr), fpsr) as u64
        })
    }
}

pub extern "C" fn fp_recip_step_fused16(a: u64, b: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_recip_step_fused(a as u16, b as u16, Fpcr::new(fpcr), fpsr) as u64
        })
    }
}

/// FPRSqrtEstimate16.
pub extern "C" fn fp_rsqrt_estimate16(bits: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_rsqrt_estimate(bits as u16, Fpcr::new(fpcr), fpsr) as u64
        })
    }
}

pub extern "C" fn fp_rsqrt_step_fused16(a: u64, b: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_rsqrt_step_fused(a as u16, b as u16, Fpcr::new(fpcr), fpsr) as u64
        })
    }
}

/// FPMulAdd16: fused multiply-add for f16.
pub extern "C" fn fp_mul_add16(addend: u64, a: u64, b: u64) -> u64 {
    let fa = f16_to_f32(a as u16) as f64;
    let fb = f16_to_f32(b as u16) as f64;
    let fc = f16_to_f32(addend as u16) as f64;
    let result = fc + fa * fb;
    f32_to_f16(result as f32) as u64
}

/// FPMulSub16: fused multiply-subtract for f16 (addend - a*b).
/// Dynarmic implements this as addend + (-a * b).
pub extern "C" fn fp_mul_sub16(addend: u64, a: u64, b: u64) -> u64 {
    let fa = f16_to_f32(a as u16) as f64;
    let fb = f16_to_f32(b as u16) as f64;
    let fc = f16_to_f32(addend as u16) as f64;
    let result = fc + (-fa) * fb;
    f32_to_f16(result as f32) as u64
}

pub extern "C" fn fp_mul_add32(addend: u32, a: u32, b: u32, fpcr: u32, fpsr_exc: *mut u32) -> u32 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_mul_add(addend, a, b, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_mul_add64(addend: u64, a: u64, b: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_mul_add(addend, a, b, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_mul_sub32(addend: u32, a: u32, b: u32, fpcr: u32, fpsr_exc: *mut u32) -> u32 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_mul_sub(addend, a, b, Fpcr::new(fpcr), fpsr)
        })
    }
}

pub extern "C" fn fp_mul_sub64(addend: u64, a: u64, b: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_mul_sub(addend, a, b, Fpcr::new(fpcr), fpsr)
        })
    }
}

// ---------------------------------------------------------------------------
// Fixed-point conversion helpers (host_call for uncommon sizes)
// ---------------------------------------------------------------------------

/// FPHalfToFixedS16/S32/S64.
pub extern "C" fn fp_half_to_fixed_s(bits: u64, fbits: u64, dest_size: u64) -> u64 {
    let f = f16_to_f32(bits as u16) as f64;
    let scaled = f * (1u64.wrapping_shl(fbits as u32)) as f64;
    let clamped = scaled.trunc();
    match dest_size {
        16 => (clamped.clamp(i16::MIN as f64, i16::MAX as f64) as i16 as u16) as u64,
        32 => (clamped.clamp(i32::MIN as f64, i32::MAX as f64) as i32 as u32) as u64,
        64 => clamped.clamp(i64::MIN as f64, i64::MAX as f64) as i64 as u64,
        _ => 0,
    }
}

/// FPHalfToFixedU16/U32/U64.
pub extern "C" fn fp_half_to_fixed_u(bits: u64, fbits: u64, dest_size: u64) -> u64 {
    let f = f16_to_f32(bits as u16) as f64;
    let scaled = f * (1u64.wrapping_shl(fbits as u32)) as f64;
    let clamped = scaled.trunc();
    match dest_size {
        16 => (clamped.clamp(0.0, u16::MAX as f64) as u16) as u64,
        32 => (clamped.clamp(0.0, u32::MAX as f64) as u32) as u64,
        64 => {
            if clamped < 0.0 {
                0
            } else if clamped >= u64::MAX as f64 {
                u64::MAX
            } else {
                clamped as u64
            }
        }
        _ => 0,
    }
}

/// FPDoubleToFixedU16 / FPSingleToFixedU16.
pub extern "C" fn fp_to_fixed_u16(bits: u64, fbits: u64, is_double: u64) -> u64 {
    let f = if is_double != 0 {
        f64::from_bits(bits)
    } else {
        f32::from_bits(bits as u32) as f64
    };
    let scaled = f * (1u64.wrapping_shl(fbits as u32)) as f64;
    (scaled.trunc().clamp(0.0, u16::MAX as f64) as u16) as u64
}

pub extern "C" fn fp_single_to_fixed_s32(
    bits: u64,
    parameters: u64,
    fpcr: u64,
    fpsr_exc: *mut u32,
) -> u64 {
    let fbits = parameters as u8 as usize;
    let rounding = rounding_mode_from_u8((parameters >> 8) as u8);
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_to_fixed(
                32,
                bits as u32,
                fbits,
                false,
                Fpcr::new(fpcr as u32),
                rounding,
                fpsr,
            )
        })
    }
}

pub extern "C" fn fp_double_to_fixed_s32(
    bits: u64,
    parameters: u64,
    fpcr: u64,
    fpsr_exc: *mut u32,
) -> u64 {
    let fbits = parameters as u8 as usize;
    let rounding = rounding_mode_from_u8((parameters >> 8) as u8);
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_to_fixed(
                32,
                bits,
                fbits,
                false,
                Fpcr::new(fpcr as u32),
                rounding,
                fpsr,
            )
        })
    }
}

pub extern "C" fn fp_single_to_fixed_s64(
    bits: u64,
    parameters: u64,
    fpcr: u64,
    fpsr_exc: *mut u32,
) -> u64 {
    let fbits = parameters as u8 as usize;
    let rounding = rounding_mode_from_u8((parameters >> 8) as u8);
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_to_fixed(
                64,
                bits as u32,
                fbits,
                false,
                Fpcr::new(fpcr as u32),
                rounding,
                fpsr,
            )
        })
    }
}

pub extern "C" fn fp_double_to_fixed_s64(
    bits: u64,
    parameters: u64,
    fpcr: u64,
    fpsr_exc: *mut u32,
) -> u64 {
    let fbits = parameters as u8 as usize;
    let rounding = rounding_mode_from_u8((parameters >> 8) as u8);
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_to_fixed(
                64,
                bits,
                fbits,
                false,
                Fpcr::new(fpcr as u32),
                rounding,
                fpsr,
            )
        })
    }
}

fn rounding_mode_from_u8(value: u8) -> RoundingMode {
    match value {
        0 => RoundingMode::ToNearestTieEven,
        1 => RoundingMode::TowardsPlusInfinity,
        2 => RoundingMode::TowardsMinusInfinity,
        3 => RoundingMode::TowardsZero,
        4 => RoundingMode::ToNearestTieAwayFromZero,
        5 => RoundingMode::ToOdd,
        _ => panic!("invalid floating-point rounding mode {value}"),
    }
}

macro_rules! define_fp_to_fixed_unsigned_helper {
    ($name:ident, $type:ty, $ibits:expr) => {
        pub extern "C" fn $name(bits: u64, parameters: u64, fpcr: u64, fpsr_exc: *mut u32) -> u64 {
            let fbits = parameters as u8 as usize;
            let rounding = rounding_mode_from_u8((parameters >> 8) as u8);
            unsafe {
                with_fpsr(fpsr_exc, |fpsr| {
                    fp_to_fixed(
                        $ibits,
                        bits as $type,
                        fbits,
                        true,
                        Fpcr::new(fpcr as u32),
                        rounding,
                        fpsr,
                    )
                })
            }
        }
    };
}

define_fp_to_fixed_unsigned_helper!(fp_single_to_fixed_u32, u32, 32);
define_fp_to_fixed_unsigned_helper!(fp_single_to_fixed_u64, u32, 64);
define_fp_to_fixed_unsigned_helper!(fp_double_to_fixed_u32, u64, 32);
define_fp_to_fixed_unsigned_helper!(fp_double_to_fixed_u64, u64, 64);

/// FPFixedU16ToSingle.
pub extern "C" fn fp_fixed_u16_to_single(bits: u64, fbits: u64) -> u64 {
    let value = bits as u16;
    let result = (value as f32) / (1u64.wrapping_shl(fbits as u32)) as f32;
    result.to_bits() as u64
}

/// FPFixedS16ToSingle.
pub extern "C" fn fp_fixed_s16_to_single(bits: u64, fbits: u64) -> u64 {
    let value = bits as i16;
    let result = (value as f32) / (1u64.wrapping_shl(fbits as u32)) as f32;
    result.to_bits() as u64
}

/// FPFixedU16ToDouble.
pub extern "C" fn fp_fixed_u16_to_double(bits: u64, fbits: u64) -> u64 {
    let value = bits as u16;
    let result = (value as f64) / (1u64.wrapping_shl(fbits as u32)) as f64;
    result.to_bits()
}

/// FPFixedS16ToDouble.
pub extern "C" fn fp_fixed_s16_to_double(bits: u64, fbits: u64) -> u64 {
    let value = bits as i16;
    let result = (value as f64) / (1u64.wrapping_shl(fbits as u32)) as f64;
    result.to_bits()
}

/// FPFixedU64ToSingle.
pub extern "C" fn fp_fixed_u64_to_single(bits: u64, fbits: u64) -> u64 {
    let result = (bits as f64) / (1u64.wrapping_shl(fbits as u32)) as f64;
    (result as f32).to_bits() as u64
}

/// FPFixedU64ToDouble.
pub extern "C" fn fp_fixed_u64_to_double(bits: u64, fbits: u64) -> u64 {
    let result = (bits as f64) / (1u64.wrapping_shl(fbits as u32)) as f64;
    result.to_bits()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_f16_to_f32_zero() {
        assert_eq!(f16_to_f32(0x0000), 0.0f32);
        assert_eq!(f16_to_f32(0x8000), -0.0f32);
    }

    #[test]
    fn test_f16_to_f32_one() {
        let one = f16_to_f32(0x3C00);
        assert!((one - 1.0f32).abs() < 1e-6);
    }

    #[test]
    fn test_f16_to_f32_infinity() {
        assert!(f16_to_f32(0x7C00).is_infinite());
        assert!(f16_to_f32(0x7C00).is_sign_positive());
        assert!(f16_to_f32(0xFC00).is_infinite());
        assert!(f16_to_f32(0xFC00).is_sign_negative());
    }

    #[test]
    fn test_f16_to_f32_nan() {
        assert!(f16_to_f32(0x7E00).is_nan());
    }

    #[test]
    fn test_f32_to_f16_round_trip() {
        let values = [0.0f32, 1.0, -1.0, 0.5, 65504.0];
        for &v in &values {
            let half = f32_to_f16(v);
            let back = f16_to_f32(half);
            assert!((back - v).abs() < 1.0, "Round-trip failed for {}", v);
        }
    }

    #[test]
    fn test_fp_mul_x32_zero_times_inf() {
        let zero = 0.0f32.to_bits();
        let inf = f32::INFINITY.to_bits();
        let result = f32::from_bits(fp_mul_x32(zero, inf));
        assert_eq!(result, 2.0);
    }

    #[test]
    fn test_fp_mul_x64_zero_times_inf() {
        let zero = 0.0f64.to_bits();
        let inf = f64::INFINITY.to_bits();
        let result = f64::from_bits(fp_mul_x64(zero, inf));
        assert_eq!(result, 2.0);
    }

    #[test]
    fn test_fp_recip_step_fused32_zero_inf() {
        let zero = 0.0f32.to_bits();
        let inf = f32::INFINITY.to_bits();
        let mut fpsr = 0;
        let result = f32::from_bits(fp_recip_step_fused32(zero, inf, 0, &mut fpsr));
        assert_eq!(result, 2.0);
    }

    #[test]
    fn test_fp_rsqrt_step_fused32_zero_inf() {
        let zero = 0.0f32.to_bits();
        let inf = f32::INFINITY.to_bits();
        let mut fpsr = 0;
        let result = f32::from_bits(fp_rsqrt_step_fused32(zero, inf, 0, &mut fpsr));
        assert_eq!(result, 1.5);
    }

    #[test]
    fn reciprocal_step_wrappers_apply_fpcr_and_preserve_sticky_fpsr() {
        let mut fpsr = 1 << 1;
        let snan = 0x7f80_0001u32;
        assert_eq!(
            fp_recip_step_fused32(snan, 1.0f32.to_bits(), 0, &mut fpsr),
            0xffc0_0001
        );
        assert_eq!(fpsr, (1 << 1) | 1);

        let mut fpsr = 0;
        let result = fp_rsqrt_step_fused32(1, 1.0f32.to_bits(), 1 << 24, &mut fpsr);
        assert_eq!(result, 1.5f32.to_bits());
        assert_eq!(fpsr & (1 << 7), 1 << 7);
    }

    #[test]
    fn reciprocal_exponent_wrappers_process_nan_and_denormal() {
        let mut fpsr = 0;
        assert_eq!(fp_recip_exponent32(0x7f80_0001, 0, &mut fpsr), 0x7fc0_0001);
        assert_eq!(fpsr & 1, 1);

        let mut fpsr = 0;
        assert_eq!(fp_recip_exponent32(1, 1 << 24, &mut fpsr), 0x7f00_0000);
        assert_eq!(fpsr & (1 << 7), 1 << 7);
    }

    #[test]
    fn fp_estimate_wrappers_preserve_sticky_fpsr_exceptions() {
        let mut fpsr_exc = 0;

        assert_eq!(
            fp_recip_estimate32(1.0f32.to_bits(), 0, &mut fpsr_exc),
            0x3f7f_8000
        );
        assert_eq!(fpsr_exc, 0);

        assert_eq!(
            fp_recip_estimate32(0, 0, &mut fpsr_exc),
            f32::INFINITY.to_bits()
        );
        assert_eq!(fpsr_exc, 1 << 1);

        assert_eq!(
            fp_rsqrt_estimate32((-1.0f32).to_bits(), 0, &mut fpsr_exc),
            0x7fc0_0000
        );
        assert_eq!(fpsr_exc, (1 << 1) | 1);
    }

    #[test]
    fn fp_mul_add_wrappers_preserve_fused_arm_semantics() {
        let mut fpsr_exc = 0;
        assert_eq!(
            fp_mul_add32(
                1.0f32.to_bits(),
                2.0f32.to_bits(),
                3.0f32.to_bits(),
                0,
                &mut fpsr_exc,
            ),
            7.0f32.to_bits()
        );
        assert_eq!(
            fp_mul_sub64(
                7.0f64.to_bits(),
                2.0f64.to_bits(),
                3.0f64.to_bits(),
                0,
                &mut fpsr_exc,
            ),
            1.0f64.to_bits()
        );
        assert_eq!(fpsr_exc, 0);
    }

    #[test]
    fn fp_mul_add_wrapper_updates_the_jit_fpsr() {
        let mut fpsr_exc = 1 << 1;
        let result = fp_mul_add32(
            0xffc1_2345,
            f32::INFINITY.to_bits(),
            0.0f32.to_bits(),
            0,
            &mut fpsr_exc,
        );
        assert_eq!(result, 0x7fc0_0000);
        assert_eq!(fpsr_exc, (1 << 1) | 1);
    }

    #[test]
    fn test_fp_fixed_u64_to_double_keeps_full_width() {
        let bits = fp_fixed_u64_to_double(1_000_000, 0);
        assert_eq!(f64::from_bits(bits), 1_000_000.0);
    }

    #[test]
    fn test_fp_fixed_u64_to_single_keeps_full_width() {
        let bits = fp_fixed_u64_to_single(1_000_000, 0);
        assert_eq!(f32::from_bits(bits as u32), 1_000_000.0);
    }
}

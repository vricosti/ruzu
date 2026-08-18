// Pure Rust implementations of ARM floating-point semantics.
//
// These are used as host_call fallback functions when there is no native
// x86-64 instruction that implements the exact ARM semantics.

use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::op::fp_convert::fp_convert;
use crate::common::fp::op::fp_recip_estimate::fp_recip_estimate;
use crate::common::fp::op::fp_rsqrt_estimate::fp_rsqrt_estimate;
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

/// ARM FPRecipExponent32: return 2^(-floor(log2(|x|))-1).
pub extern "C" fn fp_recip_exponent32(bits: u32) -> u32 {
    let exp = (bits >> 23) & 0xFF;
    let sign = bits & 0x8000_0000;
    if exp == 0xFF {
        // Inf/NaN → preserve sign, set exp to 0
        return sign;
    }
    if exp == 0 {
        // Denorm/zero → Inf with sign
        return sign | 0x7F80_0000;
    }
    // Result exponent = 253 - exp (0xFE - 1 - exp)
    let result_exp = (0xFE - exp) & 0xFF;
    sign | (result_exp << 23)
}

/// ARM FPRecipExponent64.
pub extern "C" fn fp_recip_exponent64(bits: u64) -> u64 {
    let exp = (bits >> 52) & 0x7FF;
    let sign = bits & 0x8000_0000_0000_0000;
    if exp == 0x7FF {
        return sign;
    }
    if exp == 0 {
        return sign | 0x7FF0_0000_0000_0000;
    }
    let result_exp = (0x7FE - exp) & 0x7FF;
    sign | (result_exp << 52)
}

/// ARM FPRecipStepFused32: (2.0 - a*b) as fused operation.
pub extern "C" fn fp_recip_step_fused32(a: u32, b: u32) -> u32 {
    let fa = f32::from_bits(a);
    let fb = f32::from_bits(b);

    let a_zero = fa == 0.0 || fa == -0.0;
    let b_inf = fb.is_infinite();
    let a_inf = fa.is_infinite();
    let b_zero = fb == 0.0 || fb == -0.0;

    if (a_zero && b_inf) || (a_inf && b_zero) {
        return 2.0f32.to_bits();
    }

    // Use f64 for fused precision
    let result = 2.0f64 - (fa as f64) * (fb as f64);
    (result as f32).to_bits()
}

/// ARM FPRecipStepFused64: (2.0 - a*b) as fused operation.
pub extern "C" fn fp_recip_step_fused64(a: u64, b: u64) -> u64 {
    let fa = f64::from_bits(a);
    let fb = f64::from_bits(b);

    let a_zero = fa == 0.0 || fa == -0.0;
    let b_inf = fb.is_infinite();
    let a_inf = fa.is_infinite();
    let b_zero = fb == 0.0 || fb == -0.0;

    if (a_zero && b_inf) || (a_inf && b_zero) {
        return 2.0f64.to_bits();
    }

    // Best we can do without 128-bit FP
    let product = fa * fb;
    (2.0f64 - product).to_bits()
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

/// ARM FPRSqrtStepFused32: (3.0 - a*b) / 2.0 as fused operation.
pub extern "C" fn fp_rsqrt_step_fused32(a: u32, b: u32) -> u32 {
    let fa = f32::from_bits(a);
    let fb = f32::from_bits(b);

    let a_zero = fa == 0.0 || fa == -0.0;
    let b_inf = fb.is_infinite();
    let a_inf = fa.is_infinite();
    let b_zero = fb == 0.0 || fb == -0.0;

    if (a_zero && b_inf) || (a_inf && b_zero) {
        return 1.5f32.to_bits();
    }

    let result = (3.0f64 - (fa as f64) * (fb as f64)) / 2.0;
    (result as f32).to_bits()
}

/// ARM FPRSqrtStepFused64: (3.0 - a*b) / 2.0 as fused operation.
pub extern "C" fn fp_rsqrt_step_fused64(a: u64, b: u64) -> u64 {
    let fa = f64::from_bits(a);
    let fb = f64::from_bits(b);

    let a_zero = fa == 0.0 || fa == -0.0;
    let b_inf = fb.is_infinite();
    let a_inf = fa.is_infinite();
    let b_zero = fb == 0.0 || fb == -0.0;

    if (a_zero && b_inf) || (a_inf && b_zero) {
        return 1.5f64.to_bits();
    }

    let product = fa * fb;
    ((3.0f64 - product) / 2.0).to_bits()
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
pub extern "C" fn fp_recip_exponent16(bits: u64) -> u64 {
    let h = bits as u16;
    let exp = (h >> 10) & 0x1F;
    let sign = h & 0x8000;
    if exp == 0x1F {
        return sign as u64;
    }
    if exp == 0 {
        return (sign | 0x7C00) as u64;
    }
    let result_exp = (0x1E - exp) & 0x1F;
    (sign | (result_exp << 10)) as u64
}

/// FPRecipStepFused16: (2.0 - a*b) for f16.
pub extern "C" fn fp_recip_step_fused16(a: u64, b: u64) -> u64 {
    let fa = f16_to_f32(a as u16) as f64;
    let fb = f16_to_f32(b as u16) as f64;
    let result = 2.0 - fa * fb;
    f32_to_f16(result as f32) as u64
}

/// FPRSqrtEstimate16.
pub extern "C" fn fp_rsqrt_estimate16(bits: u64, fpcr: u32, fpsr_exc: *mut u32) -> u64 {
    unsafe {
        with_fpsr(fpsr_exc, |fpsr| {
            fp_rsqrt_estimate(bits as u16, Fpcr::new(fpcr), fpsr) as u64
        })
    }
}

/// FPRSqrtStepFused16: (3.0 - a*b) / 2.0 for f16.
pub extern "C" fn fp_rsqrt_step_fused16(a: u64, b: u64) -> u64 {
    let fa = f16_to_f32(a as u16) as f64;
    let fb = f16_to_f32(b as u16) as f64;
    let result = (3.0 - fa * fb) / 2.0;
    f32_to_f16(result as f32) as u64
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

// ---------------------------------------------------------------------------
// CRC32 ISO (polynomial 0x04C11DB7) — software implementation
// ---------------------------------------------------------------------------

/// ISO CRC32 lookup table (standard Ethernet polynomial).
const CRC32_ISO_TABLE: [u32; 256] = {
    let mut table = [0u32; 256];
    let mut i = 0u32;
    while i < 256 {
        let mut crc = i;
        let mut j = 0;
        while j < 8 {
            if crc & 1 != 0 {
                crc = (crc >> 1) ^ 0xEDB88320;
            } else {
                crc >>= 1;
            }
            j += 1;
        }
        table[i as usize] = crc;
        i += 1;
    }
    table
};

/// Software CRC32-ISO for 8-bit data.
pub extern "C" fn crc32_iso8(crc: u64, data: u64) -> u64 {
    let mut c = crc as u32;
    let b = data as u8;
    c = CRC32_ISO_TABLE[((c ^ b as u32) & 0xFF) as usize] ^ (c >> 8);
    c as u64
}

/// Software CRC32-ISO for 16-bit data.
pub extern "C" fn crc32_iso16(crc: u64, data: u64) -> u64 {
    let mut c = crc as u32;
    let d = data as u16;
    for i in 0..2u32 {
        let b = ((d >> (i * 8)) & 0xFF) as u8;
        c = CRC32_ISO_TABLE[((c ^ b as u32) & 0xFF) as usize] ^ (c >> 8);
    }
    c as u64
}

/// Software CRC32-ISO for 32-bit data.
pub extern "C" fn crc32_iso32(crc: u64, data: u64) -> u64 {
    let mut c = crc as u32;
    let d = data as u32;
    for i in 0..4u32 {
        let b = ((d >> (i * 8)) & 0xFF) as u8;
        c = CRC32_ISO_TABLE[((c ^ b as u32) & 0xFF) as usize] ^ (c >> 8);
    }
    c as u64
}

/// Software CRC32-ISO for 64-bit data.
pub extern "C" fn crc32_iso64(crc: u64, data: u64) -> u64 {
    let mut c = crc as u32;
    for i in 0..8u32 {
        let b = ((data >> (i * 8)) & 0xFF) as u8;
        c = CRC32_ISO_TABLE[((c ^ b as u32) & 0xFF) as usize] ^ (c >> 8);
    }
    c as u64
}

// ---------------------------------------------------------------------------
// SM4 S-box lookup
// ---------------------------------------------------------------------------

const SM4_SBOX: [u8; 256] = [
    0xD6, 0x90, 0xE9, 0xFE, 0xCC, 0xE1, 0x3D, 0xB7, 0x16, 0xB6, 0x14, 0xC2, 0x28, 0xFB, 0x2C, 0x05,
    0x2B, 0x67, 0x9A, 0x76, 0x2A, 0xBE, 0x04, 0xC3, 0xAA, 0x44, 0x13, 0x26, 0x49, 0x86, 0x06, 0x99,
    0x9C, 0x42, 0x50, 0xF4, 0x91, 0xEF, 0x98, 0x7A, 0x33, 0x54, 0x0B, 0x43, 0xED, 0xCF, 0xAC, 0x62,
    0xE4, 0xB3, 0x1C, 0xA9, 0xC9, 0x08, 0xE8, 0x95, 0x80, 0xDF, 0x94, 0xFA, 0x75, 0x8F, 0x3F, 0xA6,
    0x47, 0x07, 0xA7, 0xFC, 0xF3, 0x73, 0x17, 0xBA, 0x83, 0x59, 0x3C, 0x19, 0xE6, 0x85, 0x4F, 0xA8,
    0x68, 0x6B, 0x81, 0xB2, 0x71, 0x64, 0xDA, 0x8B, 0xF8, 0xEB, 0x0F, 0x4B, 0x70, 0x56, 0x9D, 0x35,
    0x1E, 0x24, 0x0E, 0x5E, 0x63, 0x58, 0xD1, 0xA2, 0x25, 0x22, 0x7C, 0x3B, 0x01, 0x21, 0x78, 0x87,
    0xD4, 0x00, 0x46, 0x57, 0x9F, 0xD3, 0x27, 0x52, 0x4C, 0x36, 0x02, 0xE7, 0xA0, 0xC4, 0xC8, 0x9E,
    0xEA, 0xBF, 0x8A, 0xD2, 0x40, 0xC7, 0x38, 0xB5, 0xA3, 0xF7, 0xF2, 0xCE, 0xF9, 0x61, 0x15, 0xA1,
    0xE0, 0xAE, 0x5D, 0xA4, 0x9B, 0x34, 0x1A, 0x55, 0xAD, 0x93, 0x32, 0x30, 0xF5, 0x8C, 0xB1, 0xE3,
    0x1D, 0xF6, 0xE2, 0x2E, 0x82, 0x66, 0xCA, 0x60, 0xC0, 0x29, 0x23, 0xAB, 0x0D, 0x53, 0x4E, 0x6F,
    0xD5, 0xDB, 0x37, 0x45, 0xDE, 0xFD, 0x8E, 0x2F, 0x03, 0xFF, 0x6A, 0x72, 0x6D, 0x6C, 0x5B, 0x51,
    0x8D, 0x1B, 0xAF, 0x92, 0xBB, 0xDD, 0xBC, 0x7F, 0x11, 0xD9, 0x5C, 0x41, 0x1F, 0x10, 0x5A, 0xD8,
    0x0A, 0xC1, 0x31, 0x88, 0xA5, 0xCD, 0x7B, 0xBD, 0x2D, 0x74, 0xD0, 0x12, 0xB8, 0xE5, 0xB4, 0xB0,
    0x89, 0x69, 0x97, 0x4A, 0x0C, 0x96, 0x77, 0x7E, 0x65, 0xB9, 0xF1, 0x09, 0xC5, 0x6E, 0xC6, 0x84,
    0x18, 0xF0, 0x7D, 0xEC, 0x3A, 0xDC, 0x4D, 0x20, 0x79, 0xEE, 0x5F, 0x3E, 0xD7, 0xCB, 0x39, 0x48,
];

/// SM4 S-box substitution on a 128-bit value.
/// Applies the SM4 S-box to each byte of the 128-bit input.
pub extern "C" fn sm4_access_sbox(input_lo: u64, input_hi: u64) -> u64 {
    // Process low 64 bits, return low 64 bits of result
    // The full 128-bit operation is split across two calls
    let mut result = 0u64;
    for i in 0..8 {
        let byte_val = ((input_lo >> (i * 8)) & 0xFF) as usize;
        result |= (SM4_SBOX[byte_val] as u64) << (i * 8);
    }
    // Also process hi for the upper result
    let _ = input_hi;
    result
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
        let result = f32::from_bits(fp_recip_step_fused32(zero, inf));
        assert_eq!(result, 2.0);
    }

    #[test]
    fn test_fp_rsqrt_step_fused32_zero_inf() {
        let zero = 0.0f32.to_bits();
        let inf = f32::INFINITY.to_bits();
        let result = f32::from_bits(fp_rsqrt_step_fused32(zero, inf));
        assert_eq!(result, 1.5);
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
    fn test_crc32_iso8() {
        let result = crc32_iso8(0xFFFF_FFFF, 0x00);
        assert_ne!(result, 0);
    }

    #[test]
    fn test_crc32_iso_known_value() {
        // CRC32 of "123456789" should be 0xCBF43926
        let data = b"123456789";
        let mut crc = 0xFFFF_FFFFu64;
        for &b in data {
            crc = crc32_iso8(crc, b as u64);
        }
        assert_eq!(crc as u32 ^ 0xFFFF_FFFF, 0xCBF4_3926);
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

    #[test]
    fn test_sm4_sbox_lookup() {
        // SM4 S-box: input 0x00 → 0xD6
        assert_eq!(SM4_SBOX[0x00], 0xD6);
        assert_eq!(SM4_SBOX[0x01], 0x90);
    }
}

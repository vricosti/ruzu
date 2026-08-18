#![allow(
    clippy::missing_transmute_annotations,
    clippy::useless_transmute,
    unnecessary_transmutes
)]

use rxbyak::{dword_ptr, xmmword_ptr, RegExp, R15, XMM0};

use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::emit_vector_helpers::*;
use crate::backend::x64::fp_helpers;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::op::fp_mul_add::fp_mul_add;
use crate::common::fp::op::fp_recip_step_fused::fp_recip_step_fused;
use crate::common::fp::op::fp_round_int::fp_round_int;
use crate::common::fp::op::fp_rsqrt_step_fused::fp_rsqrt_step_fused;
use crate::common::fp::op::fp_to_fixed::fp_to_fixed;
use crate::common::fp::rounding_mode::RoundingMode;
use crate::ir::inst::Inst;
use crate::ir::value::InstRef;

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
fn host_supports_sse41() -> bool {
    std::is_x86_feature_detected!("sse4.1")
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
fn host_supports_fma_avx() -> bool {
    std::is_x86_feature_detected!("fma") && std::is_x86_feature_detected!("avx")
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
fn host_supports_avx() -> bool {
    std::is_x86_feature_detected!("avx")
}

#[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
fn host_supports_fma_avx() -> bool {
    false
}

#[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
fn host_supports_avx() -> bool {
    false
}

fn step_uses_native_result(esize: usize, result: u64, rsqrt: bool) -> bool {
    if esize == 16 || !host_supports_fma_avx() {
        return false;
    }
    let (exponent, mantissa, max_exponent) = match esize {
        32 => ((result >> 23) & 0xff, result & 0x7f_ffff, 0xff),
        64 => ((result >> 52) & 0x7ff, result & 0xf_ffff_ffff_ffff, 0x7ff),
        _ => unreachable!("invalid FP element size {esize}"),
    };
    if rsqrt {
        // Upstream tests the fused intermediate before the exact division by
        // two, so an output exponent one below the dangerous range also used
        // the reference fallback.
        exponent < max_exponent - 2
    } else {
        !(exponent == max_exponent && mantissa != 0)
    }
}

fn rsqrt_native_attempt_overflowed(esize: usize, result: u64) -> bool {
    match esize {
        32 => (result >> 23) & 0xff == 0xfe,
        64 => (result >> 52) & 0x7ff == 0x7fe,
        16 => false,
        _ => unreachable!("invalid FP element size {esize}"),
    }
}

#[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
fn host_supports_sse41() -> bool {
    false
}

// ---------------------------------------------------------------------------
// fp16 helpers (avoid external dependency)
// ---------------------------------------------------------------------------

fn f16_to_f32(bits: u16) -> f32 {
    let sign = ((bits >> 15) & 1) as u32;
    let exp = ((bits >> 10) & 0x1F) as u32;
    let frac = (bits & 0x3FF) as u32;

    if exp == 0x1F {
        // Inf or NaN
        let f_bits = (sign << 31) | (0xFF << 23) | (frac << 13);
        f32::from_bits(f_bits)
    } else if exp == 0 {
        if frac == 0 {
            // Zero
            f32::from_bits(sign << 31)
        } else {
            // Subnormal: convert to normalized f32
            let mut f = frac as f32 / 1024.0;
            f *= 1.0 / 16384.0; // 2^-14
            if sign != 0 {
                -f
            } else {
                f
            }
        }
    } else {
        let f_bits = (sign << 31) | ((exp + 112) << 23) | (frac << 13);
        f32::from_bits(f_bits)
    }
}

fn f32_to_f16(f: f32) -> u16 {
    let bits = f.to_bits();
    let sign = (bits >> 31) & 1;
    let exp = ((bits >> 23) & 0xFF) as i32;
    let frac = bits & 0x7FFFFF;

    if exp == 0xFF {
        // Inf or NaN
        let h_frac = if frac != 0 { (frac >> 13) | 1 } else { 0 };
        return ((sign << 15) | (0x1F << 10) | h_frac) as u16;
    }

    let unbiased = exp - 127;
    if unbiased > 15 {
        // Overflow -> Inf
        return ((sign << 15) | (0x1F << 10)) as u16;
    }
    if unbiased < -24 {
        // Underflow -> zero
        return (sign << 15) as u16;
    }
    if unbiased < -14 {
        // Subnormal
        let shift = -14 - unbiased;
        let mantissa = (frac | 0x800000) >> (13 + shift);
        return ((sign << 15) | mantissa) as u16;
    }

    let h_exp = (unbiased + 15) as u32;
    let h_frac = frac >> 13;
    ((sign << 15) | (h_exp << 10) | h_frac) as u16
}

// ---------------------------------------------------------------------------
// FPVectorMulAdd — fallback (fused multiply-add: result = a + b*c or a*b+c)
// FPVectorMulAdd16/32/64
// ---------------------------------------------------------------------------

macro_rules! define_fp_muladd_fallback {
    ($name:ident, $type:ty, $count:expr, $exponent_mask:expr, $mantissa_mask:expr, $smallest_normal:expr) => {
        extern "C" fn $name(
            result: *mut [u8; 16],
            addend: *const [u8; 16],
            op1: *const [u8; 16],
            op2: *const [u8; 16],
            fpcr: u32,
            fpsr_exc: *mut u32,
        ) {
            unsafe {
                let addend: [$type; $count] = std::mem::transmute(*addend);
                let op1: [$type; $count] = std::mem::transmute(*op1);
                let op2: [$type; $count] = std::mem::transmute(*op2);
                let mut output = [0 as $type; $count];
                let fpcr = Fpcr::new(fpcr);
                let mut fpsr = Fpsr::new(fpsr_exc.read());
                let had_idc = fpsr.value() & (1 << 7) != 0;
                let mut correction_raises_idc = false;
                for index in 0..$count {
                    output[index] =
                        fp_mul_add(addend[index], op1[index], op2[index], fpcr, &mut fpsr);

                    // Upstream normally executes vector FMA through the host
                    // instruction. With FZ enabled, it invokes the reference
                    // helper only for lanes whose magnitude is exactly the
                    // smallest normal number. DAZ otherwise consumes input
                    // denormals without mapping MXCSR.DE to FPSR.IDC.
                    if fpcr.fz()
                        && (output[index] as u64 & ($exponent_mask | $mantissa_mask))
                            == $smallest_normal
                        && [addend[index], op1[index], op2[index]]
                            .into_iter()
                            .any(|value| {
                                let bits = value as u64;
                                bits & $exponent_mask == 0 && bits & $mantissa_mask != 0
                            })
                    {
                        correction_raises_idc = true;
                    }
                }
                if host_supports_fma_avx() && !had_idc && !correction_raises_idc {
                    fpsr.set_idc(false);
                }
                fpsr_exc.write(fpsr.value());
                *result = std::mem::transmute(output);
            }
        }
    };
}

define_fp_muladd_fallback!(fallback_fp_muladd16, u16, 8, 0x7c00, 0x03ff, 0x0400);
define_fp_muladd_fallback!(
    fallback_fp_muladd32,
    u32,
    4,
    0x7f80_0000,
    0x007f_ffff,
    0x0080_0000
);
define_fp_muladd_fallback!(
    fallback_fp_muladd64,
    u64,
    2,
    0x7ff0_0000_0000_0000,
    0x000f_ffff_ffff_ffff,
    0x0010_0000_0000_0000
);

pub fn emit_fp_vector_muladd16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_four_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_muladd16 as usize);
}
pub fn emit_fp_vector_muladd32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_four_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_muladd32 as usize);
}
pub fn emit_fp_vector_muladd64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_four_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_muladd64 as usize);
}

// ---------------------------------------------------------------------------
// FPVectorRecipEstimate — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_fp_recip_est16(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fpcr: u32,
    fpsr_exc: *mut u32,
) {
    unsafe {
        let va: [u16; 8] = std::mem::transmute(*a);
        let mut out = [0u16; 8];
        for i in 0..8 {
            out[i] = fp_helpers::fp_recip_estimate16(va[i] as u64, fpcr, fpsr_exc) as u16;
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_recip_est32(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fpcr: u32,
    fpsr_exc: *mut u32,
) {
    unsafe {
        let va: [f32; 4] = std::mem::transmute(*a);
        let out: [f32; 4] = [
            f32::from_bits(fp_helpers::fp_recip_estimate32(
                va[0].to_bits(),
                fpcr,
                fpsr_exc,
            )),
            f32::from_bits(fp_helpers::fp_recip_estimate32(
                va[1].to_bits(),
                fpcr,
                fpsr_exc,
            )),
            f32::from_bits(fp_helpers::fp_recip_estimate32(
                va[2].to_bits(),
                fpcr,
                fpsr_exc,
            )),
            f32::from_bits(fp_helpers::fp_recip_estimate32(
                va[3].to_bits(),
                fpcr,
                fpsr_exc,
            )),
        ];
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_recip_est64(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fpcr: u32,
    fpsr_exc: *mut u32,
) {
    unsafe {
        let va: [f64; 2] = std::mem::transmute(*a);
        let out: [f64; 2] = [
            f64::from_bits(fp_helpers::fp_recip_estimate64(
                va[0].to_bits(),
                fpcr,
                fpsr_exc,
            )),
            f64::from_bits(fp_helpers::fp_recip_estimate64(
                va[1].to_bits(),
                fpcr,
                fpsr_exc,
            )),
        ];
        *result = std::mem::transmute(out);
    }
}

pub fn emit_fp_vector_recip_estimate16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_recip_est16 as usize);
}
pub fn emit_fp_vector_recip_estimate32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_recip_est32 as usize);
}
pub fn emit_fp_vector_recip_estimate64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_recip_est64 as usize);
}

// ---------------------------------------------------------------------------
// FPVectorRecipStepFused / FPVectorRSqrtStepFused — fused Newton-Raphson step.
//
// Per the ARM pseudocode (FPRecipStepFused / FPRSqrtStepFused): when one operand
// is 0 and the other is infinity the result is the constant 2.0 (recip) / 1.5
// (rsqrt); otherwise it is `2 - a*b` / `(3 - a*b)/2` computed with a SINGLE
// rounding (FMA). The previous fallbacks computed the arithmetic non-fused
// (1-ULP error in the common finite case) AND produced a NaN for the 0*inf
// case — both wrong vs hardware.
// ---------------------------------------------------------------------------

macro_rules! define_fp_step_fallback {
    ($name:ident, $type:ty, $count:expr, $bits:expr, $rsqrt:expr, $operation:ident) => {
        extern "C" fn $name(
            result: *mut [u8; 16],
            a: *const [u8; 16],
            b: *const [u8; 16],
            fpcr: u32,
            fpsr_exc: *mut u32,
        ) {
            unsafe {
                let va: [$type; $count] = std::mem::transmute(*a);
                let vb: [$type; $count] = std::mem::transmute(*b);
                let mut out = [0 as $type; $count];
                let mut lane_fpsr = [0u32; $count];
                let fpcr = Fpcr::new(fpcr);
                for index in 0..$count {
                    let mut exceptions = Fpsr::default();
                    out[index] = $operation(va[index], vb[index], fpcr, &mut exceptions);
                    lane_fpsr[index] = exceptions.value();
                }
                let native_vector = out
                    .iter()
                    .all(|value| step_uses_native_result($bits, *value as u64, $rsqrt));
                let mut fpsr = Fpsr::new(fpsr_exc.read());
                for exceptions in lane_fpsr {
                    let exceptions = if native_vector {
                        exceptions & !(1 << 7)
                    } else {
                        exceptions
                    };
                    fpsr = Fpsr::new(fpsr.value() | exceptions);
                }
                if !native_vector
                    && $rsqrt
                    && out
                        .iter()
                        .any(|value| rsqrt_native_attempt_overflowed($bits, *value as u64))
                {
                    fpsr.set_ofc(true);
                    fpsr.set_ixc(true);
                }
                fpsr_exc.write(fpsr.value());
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_fp_step_fallback!(
    fallback_fp_recip_step16,
    u16,
    8,
    16,
    false,
    fp_recip_step_fused
);
define_fp_step_fallback!(
    fallback_fp_recip_step32,
    u32,
    4,
    32,
    false,
    fp_recip_step_fused
);
define_fp_step_fallback!(
    fallback_fp_recip_step64,
    u64,
    2,
    64,
    false,
    fp_recip_step_fused
);

pub fn emit_fp_vector_recip_step_fused16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_three_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_recip_step16 as usize);
}
pub fn emit_fp_vector_recip_step_fused32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_three_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_recip_step32 as usize);
}
pub fn emit_fp_vector_recip_step_fused64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_three_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_recip_step64 as usize);
}

// ---------------------------------------------------------------------------
// FPVectorRSqrtEstimate — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_fp_rsqrt_est16(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fpcr: u32,
    fpsr_exc: *mut u32,
) {
    unsafe {
        let va: [u16; 8] = std::mem::transmute(*a);
        let mut out = [0u16; 8];
        for i in 0..8 {
            out[i] = fp_helpers::fp_rsqrt_estimate16(va[i] as u64, fpcr, fpsr_exc) as u16;
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_rsqrt_est32(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fpcr: u32,
    fpsr_exc: *mut u32,
) {
    unsafe {
        let va: [f32; 4] = std::mem::transmute(*a);
        let out: [f32; 4] = [
            f32::from_bits(fp_helpers::fp_rsqrt_estimate32(
                va[0].to_bits(),
                fpcr,
                fpsr_exc,
            )),
            f32::from_bits(fp_helpers::fp_rsqrt_estimate32(
                va[1].to_bits(),
                fpcr,
                fpsr_exc,
            )),
            f32::from_bits(fp_helpers::fp_rsqrt_estimate32(
                va[2].to_bits(),
                fpcr,
                fpsr_exc,
            )),
            f32::from_bits(fp_helpers::fp_rsqrt_estimate32(
                va[3].to_bits(),
                fpcr,
                fpsr_exc,
            )),
        ];
        // Upstream's AVX fast path handles the entire vector only when every
        // lane is positive, normal and finite. Its sqrt/div sequence always
        // raises inexact because it first injects a mantissa bit. One special
        // lane branches the complete vector to the reference fallback.
        if host_supports_avx()
            && va
                .iter()
                .all(|value| value.is_normal() && value.is_sign_positive())
        {
            fpsr_exc.write(fpsr_exc.read() | (1 << 4));
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_rsqrt_est64(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fpcr: u32,
    fpsr_exc: *mut u32,
) {
    unsafe {
        let va: [f64; 2] = std::mem::transmute(*a);
        let out: [f64; 2] = [
            f64::from_bits(fp_helpers::fp_rsqrt_estimate64(
                va[0].to_bits(),
                fpcr,
                fpsr_exc,
            )),
            f64::from_bits(fp_helpers::fp_rsqrt_estimate64(
                va[1].to_bits(),
                fpcr,
                fpsr_exc,
            )),
        ];
        if host_supports_avx()
            && va
                .iter()
                .all(|value| value.is_normal() && value.is_sign_positive())
        {
            fpsr_exc.write(fpsr_exc.read() | (1 << 4));
        }
        *result = std::mem::transmute(out);
    }
}

pub fn emit_fp_vector_rsqrt_estimate16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_rsqrt_est16 as usize);
}
pub fn emit_fp_vector_rsqrt_estimate32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_rsqrt_est32 as usize);
}
pub fn emit_fp_vector_rsqrt_estimate64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_rsqrt_est64 as usize);
}

// ---------------------------------------------------------------------------
// FPVectorRSqrtStepFused — fallback: (3.0 - a*b) / 2.0
// ---------------------------------------------------------------------------

define_fp_step_fallback!(
    fallback_fp_rsqrt_step16,
    u16,
    8,
    16,
    true,
    fp_rsqrt_step_fused
);
define_fp_step_fallback!(
    fallback_fp_rsqrt_step32,
    u32,
    4,
    32,
    true,
    fp_rsqrt_step_fused
);
define_fp_step_fallback!(
    fallback_fp_rsqrt_step64,
    u64,
    2,
    64,
    true,
    fp_rsqrt_step_fused
);

pub fn emit_fp_vector_rsqrt_step_fused16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_three_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_rsqrt_step16 as usize);
}
pub fn emit_fp_vector_rsqrt_step_fused32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_three_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_rsqrt_step32 as usize);
}
pub fn emit_fp_vector_rsqrt_step_fused64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_three_op_fallback(ctx, ra, inst_ref, inst, fallback_fp_rsqrt_step64 as usize);
}

// ---------------------------------------------------------------------------
// FPVectorRoundInt
// ---------------------------------------------------------------------------

fn rounding_mode(rounding: u8) -> RoundingMode {
    match rounding {
        0 => RoundingMode::ToNearestTieEven,
        1 => RoundingMode::TowardsPlusInfinity,
        2 => RoundingMode::TowardsMinusInfinity,
        3 => RoundingMode::TowardsZero,
        4 => RoundingMode::ToNearestTieAwayFromZero,
        _ => unreachable!("invalid FP rounding mode {rounding}"),
    }
}

extern "C" fn fallback_fp_round_int16<const ROUNDING: u8, const EXACT: bool>(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fpcr: u32,
    fpsr_exc: *mut u32,
) {
    unsafe {
        let va: [u16; 8] = std::mem::transmute(*a);
        let mut out = [0u16; 8];
        let fpcr = Fpcr::new(fpcr);
        let mut fpsr = Fpsr::new(fpsr_exc.read());
        for i in 0..8 {
            out[i] = fp_round_int(va[i], fpcr, rounding_mode(ROUNDING), EXACT, &mut fpsr);
        }
        fpsr_exc.write(fpsr.value());
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_round_int32<const ROUNDING: u8, const EXACT: bool>(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fpcr: u32,
    fpsr_exc: *mut u32,
) {
    unsafe {
        let va: [u32; 4] = std::mem::transmute(*a);
        let mut out = [0u32; 4];
        let fpcr = Fpcr::new(fpcr);
        let mut fpsr = Fpsr::new(fpsr_exc.read());
        for i in 0..4 {
            out[i] = fp_round_int(va[i], fpcr, rounding_mode(ROUNDING), EXACT, &mut fpsr);
        }
        fpsr_exc.write(fpsr.value());
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_round_int64<const ROUNDING: u8, const EXACT: bool>(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fpcr: u32,
    fpsr_exc: *mut u32,
) {
    unsafe {
        let va: [u64; 2] = std::mem::transmute(*a);
        let mut out = [0u64; 2];
        let fpcr = Fpcr::new(fpcr);
        let mut fpsr = Fpsr::new(fpsr_exc.read());
        for i in 0..2 {
            out[i] = fp_round_int(va[i], fpcr, rounding_mode(ROUNDING), EXACT, &mut fpsr);
        }
        fpsr_exc.write(fpsr.value());
        *result = std::mem::transmute(out);
    }
}

macro_rules! round_fallback {
    ($function:ident, $rounding:expr, $exact:expr) => {
        $function::<$rounding, $exact> as usize
    };
}

fn round_fallback_for(esize: usize, rounding: u8, exact: bool) -> usize {
    macro_rules! select_exact {
        ($function:ident, $rounding:expr) => {
            if exact {
                round_fallback!($function, $rounding, true)
            } else {
                round_fallback!($function, $rounding, false)
            }
        };
    }
    macro_rules! select_rounding {
        ($function:ident) => {
            match rounding {
                0 => select_exact!($function, 0),
                1 => select_exact!($function, 1),
                2 => select_exact!($function, 2),
                3 => select_exact!($function, 3),
                4 => select_exact!($function, 4),
                _ => unreachable!("invalid FP rounding mode {rounding}"),
            }
        };
    }
    match esize {
        16 => select_rounding!(fallback_fp_round_int16),
        32 => select_rounding!(fallback_fp_round_int32),
        64 => select_rounding!(fallback_fp_round_int64),
        _ => unreachable!("invalid FP element size {esize}"),
    }
}

fn emit_fp_vector_round_int(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
    esize: usize,
) {
    let rounding = inst.args[1].get_u8();
    let exact = inst.args[2].get_u1();

    if esize != 16 && rounding != 4 && !exact {
        let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
        let result = ra.use_scratch_xmm(&mut args[0]);
        let round_imm = match rounding {
            0 => 0b00,
            1 => 0b10,
            2 => 0b01,
            3 => 0b11,
            _ => unreachable!(),
        };
        if esize == 32 {
            ra.asm.roundps(result, result, round_imm).unwrap();
        } else {
            ra.asm.roundpd(result, result, round_imm).unwrap();
        }
        ra.define_value(inst_ref, result);
        return;
    }

    let fallback = round_fallback_for(esize, rounding, exact);
    emit_two_op_fallback_with_fpcr_arg(ctx, ra, inst_ref, inst, 3, fallback);
}

pub fn emit_fp_vector_round_int16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_fp_vector_round_int(ctx, ra, inst_ref, inst, 16);
}
pub fn emit_fp_vector_round_int32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_fp_vector_round_int(ctx, ra, inst_ref, inst, 32);
}
pub fn emit_fp_vector_round_int64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_fp_vector_round_int(ctx, ra, inst_ref, inst, 64);
}

// ---------------------------------------------------------------------------
// FPVectorFromSignedFixed / FPVectorFromUnsignedFixed — fallback (with imm = frac bits)
// ---------------------------------------------------------------------------

extern "C" fn fallback_fp_from_signed_fixed32(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fbits: u8,
) {
    unsafe {
        let va: [i32; 4] = std::mem::transmute(*a);
        let scale = (1u64 << fbits) as f32;
        let out: [f32; 4] = [
            va[0] as f32 / scale,
            va[1] as f32 / scale,
            va[2] as f32 / scale,
            va[3] as f32 / scale,
        ];
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_from_signed_fixed64(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fbits: u8,
) {
    unsafe {
        let va: [i64; 2] = std::mem::transmute(*a);
        let scale = (1u64 << fbits) as f64;
        let out: [f64; 2] = [va[0] as f64 / scale, va[1] as f64 / scale];
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_from_unsigned_fixed32(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fbits: u8,
) {
    unsafe {
        let va: [u32; 4] = std::mem::transmute(*a);
        let scale = (1u64 << fbits) as f32;
        let out: [f32; 4] = [
            va[0] as f32 / scale,
            va[1] as f32 / scale,
            va[2] as f32 / scale,
            va[3] as f32 / scale,
        ];
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_from_unsigned_fixed64(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    fbits: u8,
) {
    unsafe {
        let va: [u64; 2] = std::mem::transmute(*a);
        let scale = (1u64 << fbits) as f64;
        let out: [f64; 2] = [va[0] as f64 / scale, va[1] as f64 / scale];
        *result = std::mem::transmute(out);
    }
}

pub fn emit_fp_vector_from_signed_fixed32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback_with_imm(ra, inst_ref, inst, fallback_fp_from_signed_fixed32 as usize);
}
pub fn emit_fp_vector_from_signed_fixed64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback_with_imm(ra, inst_ref, inst, fallback_fp_from_signed_fixed64 as usize);
}
pub fn emit_fp_vector_from_unsigned_fixed32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback_with_imm(
        ra,
        inst_ref,
        inst,
        fallback_fp_from_unsigned_fixed32 as usize,
    );
}
pub fn emit_fp_vector_from_unsigned_fixed64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback_with_imm(
        ra,
        inst_ref,
        inst,
        fallback_fp_from_unsigned_fixed64 as usize,
    );
}

// ---------------------------------------------------------------------------
// FPVectorToSignedFixed / FPVectorToUnsignedFixed — fallback (with imm = frac bits)
// ---------------------------------------------------------------------------

macro_rules! define_fp_vector_to_fixed_fallback {
    ($name:ident, $type:ty, $count:expr, $bits:expr, $unsigned:expr) => {
        extern "C" fn $name(
            result: *mut [u8; 16],
            a: *const [u8; 16],
            parameters: u64,
            fpcr: u32,
            fpsr_exc: *mut u32,
        ) {
            unsafe {
                let input: [$type; $count] = std::mem::transmute(*a);
                let mut output = [0 as $type; $count];
                let fbits = parameters as u8 as usize;
                let rounding = rounding_mode((parameters >> 8) as u8);
                let fpcr = Fpcr::new(fpcr);
                let mut fpsr = Fpsr::new(fpsr_exc.read());
                for (output, input) in output.iter_mut().zip(input) {
                    *output = fp_to_fixed($bits, input, fbits, $unsigned, fpcr, rounding, &mut fpsr)
                        as $type;
                }
                fpsr_exc.write(fpsr.value());
                *result = std::mem::transmute(output);
            }
        }
    };
}

define_fp_vector_to_fixed_fallback!(fallback_fp_to_signed_fixed16, u16, 8, 16, false);
define_fp_vector_to_fixed_fallback!(fallback_fp_to_signed_fixed32, u32, 4, 32, false);
define_fp_vector_to_fixed_fallback!(fallback_fp_to_signed_fixed64, u64, 2, 64, false);
define_fp_vector_to_fixed_fallback!(fallback_fp_to_unsigned_fixed16, u16, 8, 16, true);
define_fp_vector_to_fixed_fallback!(fallback_fp_to_unsigned_fixed32, u32, 4, 32, true);
define_fp_vector_to_fixed_fallback!(fallback_fp_to_unsigned_fixed64, u64, 2, 64, true);

fn vector_constant(ra: &mut RegAlloc, esize: usize, value: u64) -> RegExp {
    let (lo, hi) = match esize {
        32 => {
            let lanes = value as u32 as u64 * 0x0000_0001_0000_0001;
            (lanes, lanes)
        }
        64 => (value, value),
        _ => unreachable!("invalid FP element size {esize}"),
    };
    ra.constant_pool
        .as_mut()
        .expect("constant pool required for FP vector conversion")
        .get_constant(lo, hi)
}

fn convert_vector_to_signed_host(ra: &mut RegAlloc, src: rxbyak::Reg, esize: usize) {
    match esize {
        32 => ra.asm.cvttps2dq(src, src).unwrap(),
        64 => {
            let hi = ra.scratch_gpr();
            let lo = ra.scratch_gpr();
            ra.asm.cvttsd2si(lo, src).unwrap();
            ra.asm.punpckhqdq(src, src).unwrap();
            ra.asm.cvttsd2si(hi, src).unwrap();
            ra.asm.movq(src, lo).unwrap();
            ra.asm.pinsrq(src, hi, 1).unwrap();
            ra.release(hi);
            ra.release(lo);
        }
        _ => unreachable!("invalid FP element size {esize}"),
    }
}

fn emit_fp_vector_to_fixed_native(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
    esize: usize,
    unsigned: bool,
) -> bool {
    let fbits = inst.args[1].get_u8();
    let rounding = inst.args[2].get_u8();
    if esize == 16 || !host_supports_sse41() || rounding == 4 {
        return false;
    }

    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let fpcr_controlled = args[3].get_immediate_u1();
    let src = ra.use_scratch_xmm(&mut args[0]);
    let switch_mxcsr = ctx.fpcr(fpcr_controlled) != ctx.fpcr(true);
    if switch_mxcsr {
        ra.asm
            .stmxcsr(dword_ptr(
                RegExp::from(R15) + ctx.arch.guest_mxcsr_offset() as i32,
            ))
            .unwrap();
        ra.asm
            .ldmxcsr(dword_ptr(
                RegExp::from(R15) + ctx.arch.asimd_mxcsr_offset() as i32,
            ))
            .unwrap();
    }

    if fbits != 0 {
        let exponent = match esize {
            32 => (u64::from(fbits) + 127) << 23,
            64 => (u64::from(fbits) + 1023) << 52,
            _ => unreachable!(),
        };
        let scale = vector_constant(ra, esize, exponent);
        if esize == 32 {
            ra.asm.mulps(src, xmmword_ptr(scale)).unwrap();
        } else {
            ra.asm.mulpd(src, xmmword_ptr(scale)).unwrap();
        }
    }

    let round_imm = match rounding {
        0 => 0b00,
        1 => 0b10,
        2 => 0b01,
        3 => 0b11,
        _ => unreachable!(),
    };
    if esize == 32 {
        ra.asm.roundps(src, src, round_imm).unwrap();
        ra.asm.movaps(XMM0, src).unwrap();
        ra.asm.cmpps(XMM0, XMM0, 7).unwrap();
    } else {
        ra.asm.roundpd(src, src, round_imm).unwrap();
        ra.asm.movaps(XMM0, src).unwrap();
        ra.asm.cmppd(XMM0, XMM0, 7).unwrap();
    }
    ra.asm.andps(src, XMM0).unwrap();

    let signed_upper = match esize {
        32 => 0x4f00_0000,
        64 => 0x43e0_0000_0000_0000,
        _ => unreachable!(),
    };

    if unsigned {
        let unsigned_upper = match esize {
            32 => 0x4f80_0000,
            64 => 0x43f0_0000_0000_0000,
            _ => unreachable!(),
        };

        ra.asm.xorps(XMM0, XMM0).unwrap();
        if esize == 32 {
            ra.asm.cmpps(XMM0, src, 2).unwrap();
        } else {
            ra.asm.cmppd(XMM0, src, 2).unwrap();
        }
        ra.asm.andps(src, XMM0).unwrap();

        let exceed_unsigned = ra.scratch_xmm();
        let unsigned_limit = vector_constant(ra, esize, unsigned_upper);
        ra.asm
            .movaps(exceed_unsigned, xmmword_ptr(unsigned_limit))
            .unwrap();
        if esize == 32 {
            ra.asm.cmpps(exceed_unsigned, src, 2).unwrap();
        } else {
            ra.asm.cmppd(exceed_unsigned, src, 2).unwrap();
        }

        let tmp = ra.scratch_xmm();
        let signed_limit = vector_constant(ra, esize, signed_upper);
        ra.asm.movaps(tmp, xmmword_ptr(signed_limit)).unwrap();
        ra.asm.movaps(XMM0, tmp).unwrap();
        if esize == 32 {
            ra.asm.cmpps(XMM0, src, 2).unwrap();
            ra.asm.andps(tmp, XMM0).unwrap();
            ra.asm.subps(src, tmp).unwrap();
        } else {
            ra.asm.cmppd(XMM0, src, 2).unwrap();
            ra.asm.andpd(tmp, XMM0).unwrap();
            ra.asm.subpd(src, tmp).unwrap();
        }
        convert_vector_to_signed_host(ra, src, esize);
        if esize == 32 {
            ra.asm.pslld_imm(XMM0, 31).unwrap();
            ra.asm.orps(src, XMM0).unwrap();
            ra.asm.orps(src, exceed_unsigned).unwrap();
        } else {
            ra.asm.psllq_imm(XMM0, 63).unwrap();
            ra.asm.orpd(src, XMM0).unwrap();
            ra.asm.orpd(src, exceed_unsigned).unwrap();
        }
        ra.release(tmp);
        ra.release(exceed_unsigned);
    } else {
        let signed_limit = vector_constant(ra, esize, signed_upper);
        ra.asm.movaps(XMM0, xmmword_ptr(signed_limit)).unwrap();
        if esize == 32 {
            ra.asm.cmpps(XMM0, src, 2).unwrap();
        } else {
            ra.asm.cmppd(XMM0, src, 2).unwrap();
        }
        convert_vector_to_signed_host(ra, src, esize);

        let integer_max = match esize {
            32 => i32::MAX as u64,
            64 => i64::MAX as u64,
            _ => unreachable!(),
        };
        let maximum = vector_constant(ra, esize, integer_max);
        if esize == 32 {
            ra.asm.blendvps(src, xmmword_ptr(maximum)).unwrap();
        } else {
            ra.asm.blendvpd(src, xmmword_ptr(maximum)).unwrap();
        }
    }

    if switch_mxcsr {
        ra.asm
            .stmxcsr(dword_ptr(
                RegExp::from(R15) + ctx.arch.asimd_mxcsr_offset() as i32,
            ))
            .unwrap();
        ra.asm
            .ldmxcsr(dword_ptr(
                RegExp::from(R15) + ctx.arch.guest_mxcsr_offset() as i32,
            ))
            .unwrap();
    }
    ra.define_value(inst_ref, src);
    true
}

pub fn emit_fp_vector_to_signed_fixed16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    if emit_fp_vector_to_fixed_native(ctx, ra, inst_ref, inst, 16, false) {
        return;
    }
    emit_fp_one_arg_fallback_with_params(
        ctx,
        ra,
        inst_ref,
        inst,
        fallback_fp_to_signed_fixed16 as usize,
    );
}
pub fn emit_fp_vector_to_signed_fixed32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    if emit_fp_vector_to_fixed_native(ctx, ra, inst_ref, inst, 32, false) {
        return;
    }
    emit_fp_one_arg_fallback_with_params(
        ctx,
        ra,
        inst_ref,
        inst,
        fallback_fp_to_signed_fixed32 as usize,
    );
}
pub fn emit_fp_vector_to_signed_fixed64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    if emit_fp_vector_to_fixed_native(ctx, ra, inst_ref, inst, 64, false) {
        return;
    }
    emit_fp_one_arg_fallback_with_params(
        ctx,
        ra,
        inst_ref,
        inst,
        fallback_fp_to_signed_fixed64 as usize,
    );
}
pub fn emit_fp_vector_to_unsigned_fixed16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    if emit_fp_vector_to_fixed_native(ctx, ra, inst_ref, inst, 16, true) {
        return;
    }
    emit_fp_one_arg_fallback_with_params(
        ctx,
        ra,
        inst_ref,
        inst,
        fallback_fp_to_unsigned_fixed16 as usize,
    );
}
pub fn emit_fp_vector_to_unsigned_fixed32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    if emit_fp_vector_to_fixed_native(ctx, ra, inst_ref, inst, 32, true) {
        return;
    }
    emit_fp_one_arg_fallback_with_params(
        ctx,
        ra,
        inst_ref,
        inst,
        fallback_fp_to_unsigned_fixed32 as usize,
    );
}
pub fn emit_fp_vector_to_unsigned_fixed64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    if emit_fp_vector_to_fixed_native(ctx, ra, inst_ref, inst, 64, true) {
        return;
    }
    emit_fp_one_arg_fallback_with_params(
        ctx,
        ra,
        inst_ref,
        inst,
        fallback_fp_to_unsigned_fixed64 as usize,
    );
}

// ---------------------------------------------------------------------------
// FPVectorFromHalf32 / FPVectorToHalf32 — fallback (half <-> single conversion)
// ---------------------------------------------------------------------------

extern "C" fn fallback_fp_from_half32(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u16; 8] = std::mem::transmute(*a);
        // Convert lower 4 half-floats to 4 singles
        let out: [f32; 4] = [
            f16_to_f32(va[0]),
            f16_to_f32(va[1]),
            f16_to_f32(va[2]),
            f16_to_f32(va[3]),
        ];
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_fp_to_half32(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [f32; 4] = std::mem::transmute(*a);
        // Convert 4 singles to 4 half-floats in lower 64 bits, upper zeroed
        let out: [u16; 8] = [
            f32_to_f16(va[0]),
            f32_to_f16(va[1]),
            f32_to_f16(va[2]),
            f32_to_f16(va[3]),
            0,
            0,
            0,
            0,
        ];
        *result = std::mem::transmute(out);
    }
}

pub fn emit_fp_vector_from_half32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback(ra, inst_ref, inst, fallback_fp_from_half32 as usize);
}
pub fn emit_fp_vector_to_half32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback(ra, inst_ref, inst, fallback_fp_to_half32 as usize);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn_signatures() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_muladd16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_muladd32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_muladd64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_recip_estimate16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_recip_estimate32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_recip_estimate64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_recip_step_fused16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_recip_step_fused32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_rsqrt_estimate16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_rsqrt_estimate32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_rsqrt_step_fused16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_rsqrt_step_fused64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_round_int16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_round_int32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_round_int64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_from_signed_fixed32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_fp_vector_from_unsigned_fixed64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_to_signed_fixed16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_to_unsigned_fixed64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_from_half32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_fp_vector_to_half32;
    }

    #[test]
    fn test_fallback_fp_muladd32() {
        let addend: [u8; 16] = unsafe { std::mem::transmute([1.0f32, 2.0f32, 3.0f32, 4.0f32]) };
        let op1: [u8; 16] = unsafe { std::mem::transmute([2.0f32, 3.0f32, 4.0f32, 5.0f32]) };
        let op2: [u8; 16] = unsafe { std::mem::transmute([3.0f32, 4.0f32, 5.0f32, 6.0f32]) };
        let mut result = [0u8; 16];
        let mut fpsr = 0;
        fallback_fp_muladd32(&mut result, &addend, &op1, &op2, 0, &mut fpsr);
        let out: [f32; 4] = unsafe { std::mem::transmute(result) };
        assert_eq!(out[0], 7.0); // 1 + 2*3
        assert_eq!(out[1], 14.0); // 2 + 3*4
        assert_eq!(out[2], 23.0); // 3 + 4*5
        assert_eq!(out[3], 34.0); // 4 + 5*6
        assert_eq!(fpsr, 0);
    }

    #[test]
    fn muladd_fallback_applies_default_nan_mode() {
        let addend = [0u8; 16];
        let op1: [u8; 16] = unsafe { std::mem::transmute([f32::INFINITY.to_bits(), 0, 0, 0]) };
        let op2 = [0u8; 16];
        let mut result = [0u8; 16];
        let mut fpsr = 0;
        fallback_fp_muladd32(&mut result, &addend, &op1, &op2, 1 << 25, &mut fpsr);
        let out: [u32; 4] = unsafe { std::mem::transmute(result) };
        assert_eq!(out[0], 0x7fc0_0000);
        assert_ne!(fpsr & 1, 0);
    }

    #[test]
    fn muladd_fallback_matches_native_input_denormal_exception_behavior() {
        if !host_supports_fma_avx() {
            return;
        }

        let addend: [u8; 16] = unsafe { std::mem::transmute([1.0f32.to_bits(); 4]) };
        let op1: [u8; 16] = unsafe { std::mem::transmute([1u32; 4]) };
        let op2: [u8; 16] = unsafe { std::mem::transmute([1.0f32.to_bits(); 4]) };
        let mut result = [0u8; 16];
        let mut fpsr = 0;
        fallback_fp_muladd32(&mut result, &addend, &op1, &op2, 1 << 24, &mut fpsr);
        assert_eq!(fpsr & (1 << 7), 0);

        fpsr = 1 << 7;
        fallback_fp_muladd32(&mut result, &addend, &op1, &op2, 1 << 24, &mut fpsr);
        assert_ne!(fpsr & (1 << 7), 0);
    }

    #[test]
    fn fp_estimate_fallbacks_match_arm_values_and_accumulate_fpsr() {
        let input: [u8; 16] = unsafe {
            std::mem::transmute([
                1.0f32.to_bits(),
                0.0f32.to_bits(),
                f32::INFINITY.to_bits(),
                0x7f80_0001u32,
            ])
        };
        let mut result = [0u8; 16];
        let mut fpsr_exc = 0;

        fallback_fp_recip_est32(&mut result, &input, 0, &mut fpsr_exc);

        let output: [u32; 4] = unsafe { std::mem::transmute(result) };
        assert_eq!(
            output,
            [0x3f7f_8000, f32::INFINITY.to_bits(), 0, 0x7fc0_0001]
        );
        assert_eq!(fpsr_exc, (1 << 1) | 1);
    }

    #[test]
    fn rsqrt_estimate_matches_native_vector_inexact_behavior() {
        if !host_supports_avx() {
            return;
        }

        let normal: [u8; 16] = unsafe { std::mem::transmute([1.0f32, 2.0, 3.0, 4.0]) };
        let mut result = [0u8; 16];
        let mut fpsr = 0;
        fallback_fp_rsqrt_est32(&mut result, &normal, 0, &mut fpsr);
        assert_ne!(fpsr & (1 << 4), 0);

        let with_zero: [u8; 16] = unsafe { std::mem::transmute([1.0f32, 2.0, 3.0, 0.0]) };
        fpsr = 0;
        fallback_fp_rsqrt_est32(&mut result, &with_zero, 0, &mut fpsr);
        assert_eq!(fpsr & (1 << 4), 0);
        assert_ne!(fpsr & (1 << 1), 0);
    }

    #[test]
    fn test_fallback_fp_to_signed_fixed32() {
        let a: [u8; 16] = unsafe { std::mem::transmute([1.5f32, -2.5f32, 0.0f32, 100.0f32]) };
        let mut result = [0u8; 16];
        let mut fpsr = 0;
        fallback_fp_to_signed_fixed32(&mut result, &a, 4 << 8, 0, &mut fpsr);
        let out: [i32; 4] = unsafe { std::mem::transmute(result) };
        assert_eq!(out[0], 2);
        assert_eq!(out[1], -3);
        assert_eq!(out[2], 0);
        assert_eq!(out[3], 100);
        assert_ne!(fpsr & (1 << 4), 0);
    }

    #[test]
    fn vector_to_fixed_fallback_accumulates_fpsr_exceptions() {
        let a: [u8; 16] = unsafe {
            std::mem::transmute([
                f32::NAN.to_bits(),
                1.5f32.to_bits(),
                f32::INFINITY.to_bits(),
                (-1.5f32).to_bits(),
            ])
        };
        let mut result = [0u8; 16];
        let mut fpsr = 0x0e;
        fallback_fp_to_signed_fixed32(&mut result, &a, 3 << 8, 0, &mut fpsr);

        let out: [u32; 4] = unsafe { std::mem::transmute(result) };
        assert_eq!(out, [0, 1, i32::MAX as u32, (-1i32) as u32]);
        assert_eq!(fpsr, 0x1f);
    }
}

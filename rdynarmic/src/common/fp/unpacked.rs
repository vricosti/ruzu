use super::fpcr::Fpcr;
use super::fpsr::Fpsr;
use super::info::FloatFormat;
use super::mantissa_util::{residual_error_on_right_shift, ResidualError};
use super::process_exception::{process_exception, FpException};
use super::rounding_mode::RoundingMode;
use crate::common::safe_ops::logical_shift_right_u64;

pub const NORMALIZED_POINT_POSITION: usize = 62;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FpType {
    Nonzero,
    Zero,
    Infinity,
    QNaN,
    SNaN,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FpUnpacked {
    pub sign: bool,
    pub exponent: i32,
    pub mantissa: u64,
}

pub fn to_normalized(sign: bool, mut exponent: i32, mut value: u64) -> FpUnpacked {
    if value == 0 {
        return FpUnpacked {
            sign,
            exponent: 0,
            mantissa: 0,
        };
    }

    let highest_bit = 63 - value.leading_zeros() as i32;
    let offset = NORMALIZED_POINT_POSITION as i32 - highest_bit;
    value <<= offset;
    exponent -= offset - NORMALIZED_POINT_POSITION as i32;
    FpUnpacked {
        sign,
        exponent,
        mantissa: value,
    }
}

pub fn fp_unpack_base<F: FloatFormat>(
    op: F,
    fpcr: Fpcr,
    fpsr: &mut Fpsr,
) -> (FpType, bool, FpUnpacked) {
    let bits = op.to_bits();
    let sign = bits & F::SIGN_MASK != 0;
    let exp_raw = (bits & F::EXPONENT_MASK) >> F::EXPLICIT_MANTISSA_WIDTH;
    let frac_raw = bits & F::MANTISSA_MASK;
    let denormal_exponent = F::EXPONENT_MIN - F::EXPLICIT_MANTISSA_WIDTH as i32;
    let is_half = F::TOTAL_WIDTH == 16;

    if exp_raw == 0 {
        if is_half {
            if frac_raw == 0 || fpcr.fz16() {
                return (
                    FpType::Zero,
                    sign,
                    FpUnpacked {
                        sign,
                        exponent: 0,
                        mantissa: 0,
                    },
                );
            }
            return (
                FpType::Nonzero,
                sign,
                to_normalized(sign, denormal_exponent, frac_raw),
            );
        }

        if frac_raw == 0 || fpcr.fz() {
            if frac_raw != 0 {
                process_exception(FpException::InputDenorm, fpcr, fpsr);
            }
            return (
                FpType::Zero,
                sign,
                FpUnpacked {
                    sign,
                    exponent: 0,
                    mantissa: 0,
                },
            );
        }
        return (
            FpType::Nonzero,
            sign,
            to_normalized(sign, denormal_exponent, frac_raw),
        );
    }

    let exp_all_ones = exp_raw == (1u64 << F::EXPONENT_WIDTH) - 1;
    if exp_all_ones && (!is_half || !fpcr.ahp()) {
        if frac_raw == 0 {
            return (FpType::Infinity, sign, to_normalized(sign, 1_000_000, 1));
        }
        let fp_type = if frac_raw & F::MANTISSA_MSB != 0 {
            FpType::QNaN
        } else {
            FpType::SNaN
        };
        return (
            fp_type,
            sign,
            FpUnpacked {
                sign,
                exponent: 0,
                mantissa: 0,
            },
        );
    }

    let exponent = exp_raw as i32 - F::EXPONENT_BIAS;
    let mantissa = (frac_raw | F::IMPLICIT_LEADING_BIT)
        << (NORMALIZED_POINT_POSITION - F::EXPLICIT_MANTISSA_WIDTH);
    (
        FpType::Nonzero,
        sign,
        FpUnpacked {
            sign,
            exponent,
            mantissa,
        },
    )
}

pub fn fp_unpack<F: FloatFormat>(
    op: F,
    mut fpcr: Fpcr,
    fpsr: &mut Fpsr,
) -> (FpType, bool, FpUnpacked) {
    fpcr.set_ahp(false);
    fp_unpack_base(op, fpcr, fpsr)
}

pub fn fp_unpack_cv<F: FloatFormat>(
    op: F,
    mut fpcr: Fpcr,
    fpsr: &mut Fpsr,
) -> (FpType, bool, FpUnpacked) {
    fpcr.set_fz16(false);
    fp_unpack_base(op, fpcr, fpsr)
}

fn normalize<const F: usize>(
    op: FpUnpacked,
    extra_right_shift: i32,
) -> (bool, i32, u64, ResidualError) {
    let highest_set_bit = 63 - op.mantissa.leading_zeros() as i32;
    let shift_amount = highest_set_bit - F as i32 + extra_right_shift;
    let mantissa = logical_shift_right_u64(op.mantissa, shift_amount);
    let error = residual_error_on_right_shift(op.mantissa, shift_amount);
    let exponent = op.exponent + highest_set_bit - NORMALIZED_POINT_POSITION as i32;
    (op.sign, exponent, mantissa, error)
}

pub fn fp_round_base<F: FloatFormat>(
    op: FpUnpacked,
    fpcr: Fpcr,
    rounding: RoundingMode,
    fpsr: &mut Fpsr,
) -> F {
    assert!(op.mantissa != 0);
    assert!(rounding != RoundingMode::ToNearestTieAwayFromZero);

    let (mut sign, exponent, mut mantissa, mut error) = match F::EXPLICIT_MANTISSA_WIDTH {
        10 => normalize::<10>(op, 0),
        23 => normalize::<23>(op, 0),
        52 => normalize::<52>(op, 0),
        _ => unreachable!("unsupported floating-point format"),
    };
    let is_fp16 = F::TOTAL_WIDTH == 16;

    if ((!is_fp16 && fpcr.fz()) || (is_fp16 && fpcr.fz16())) && exponent < F::EXPONENT_MIN {
        fpsr.set_ufc(true);
        return F::zero(sign);
    }

    let mut biased_exp = (exponent - F::EXPONENT_MIN + 1).max(0);
    if biased_exp == 0 {
        (sign, _, mantissa, error) = match F::EXPLICIT_MANTISSA_WIDTH {
            10 => normalize::<10>(op, F::EXPONENT_MIN - exponent),
            23 => normalize::<23>(op, F::EXPONENT_MIN - exponent),
            52 => normalize::<52>(op, F::EXPONENT_MIN - exponent),
            _ => unreachable!("unsupported floating-point format"),
        };
    }

    if biased_exp == 0 && (error != ResidualError::Zero || fpcr.ufe()) {
        process_exception(FpException::Underflow, fpcr, fpsr);
    }

    let (round_up, overflow_to_inf) = match rounding {
        RoundingMode::ToNearestTieEven => (
            error > ResidualError::Half || (error == ResidualError::Half && mantissa & 1 != 0),
            true,
        ),
        RoundingMode::TowardsPlusInfinity => (error != ResidualError::Zero && !sign, !sign),
        RoundingMode::TowardsMinusInfinity => (error != ResidualError::Zero && sign, sign),
        _ => (false, false),
    };

    if round_up {
        if mantissa & F::MANTISSA_MASK == F::MANTISSA_MASK {
            if mantissa == F::MANTISSA_MASK {
                mantissa += 1;
                biased_exp += 1;
            } else {
                mantissa = (mantissa + 1) / 2;
                biased_exp += 1;
            }
        } else {
            mantissa += 1;
        }
    }

    if error != ResidualError::Zero && rounding == RoundingMode::ToOdd {
        mantissa |= 1;
    }

    if !is_fp16 || !fpcr.ahp() {
        let max_biased_exp = (1i32 << F::EXPONENT_WIDTH) - 1;
        if biased_exp >= max_biased_exp {
            let result = if overflow_to_inf {
                F::infinity(sign)
            } else {
                F::max_normal(sign)
            };
            process_exception(FpException::Overflow, fpcr, fpsr);
            process_exception(FpException::Inexact, fpcr, fpsr);
            result
        } else {
            let bits = (((sign as u64) << F::EXPONENT_WIDTH) | biased_exp as u64)
                << F::EXPLICIT_MANTISSA_WIDTH
                | (mantissa & F::MANTISSA_MASK);
            if error != ResidualError::Zero {
                process_exception(FpException::Inexact, fpcr, fpsr);
            }
            F::from_bits(bits)
        }
    } else {
        let max_biased_exp = 1i32 << F::EXPONENT_WIDTH;
        if biased_exp >= max_biased_exp {
            process_exception(FpException::InvalidOp, fpcr, fpsr);
            F::from_bits(if sign { 0xffff } else { 0x7fff })
        } else {
            let bits = (((sign as u64) << F::EXPONENT_WIDTH) | biased_exp as u64)
                << F::EXPLICIT_MANTISSA_WIDTH
                | (mantissa & F::MANTISSA_MASK);
            if error != ResidualError::Zero {
                process_exception(FpException::Inexact, fpcr, fpsr);
            }
            F::from_bits(bits)
        }
    }
}

pub fn fp_round<F: FloatFormat>(
    op: FpUnpacked,
    mut fpcr: Fpcr,
    rounding: RoundingMode,
    fpsr: &mut Fpsr,
) -> F {
    fpcr.set_ahp(false);
    fp_round_base(op, fpcr, rounding, fpsr)
}

pub fn fp_round_cv<F: FloatFormat>(
    op: FpUnpacked,
    mut fpcr: Fpcr,
    rounding: RoundingMode,
    fpsr: &mut Fpsr,
) -> F {
    fpcr.set_fz16(false);
    fp_round_base(op, fpcr, rounding, fpsr)
}

pub fn fp_round_from_fpcr<F: FloatFormat>(op: FpUnpacked, fpcr: Fpcr, fpsr: &mut Fpsr) -> F {
    fp_round(op, fpcr, fpcr.rmode(), fpsr)
}

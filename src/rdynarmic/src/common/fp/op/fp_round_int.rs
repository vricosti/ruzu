use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::info::FloatFormat;
use crate::common::fp::mantissa_util::{residual_error_on_right_shift, ResidualError};
use crate::common::fp::process_exception::{process_exception, FpException};
use crate::common::fp::process_nan::process_nan;
use crate::common::fp::rounding_mode::RoundingMode;
use crate::common::fp::unpacked::{fp_unpack, FpType, NORMALIZED_POINT_POSITION};

fn arithmetic_shift_left(value: u64, shift_amount: i32) -> u64 {
    if shift_amount >= 0 {
        return value.checked_shl(shift_amount as u32).unwrap_or(0);
    }

    let right_shift = shift_amount.unsigned_abs();
    if right_shift >= u64::BITS {
        if value >> 63 != 0 {
            u64::MAX
        } else {
            0
        }
    } else {
        ((value as i64) >> right_shift) as u64
    }
}

fn encode_integral<F: FloatFormat>(sign: bool, magnitude: u64) -> F {
    if magnitude == 0 {
        return F::zero(sign);
    }

    let top_bit = u64::BITS as usize - 1 - magnitude.leading_zeros() as usize;
    let significand = if top_bit > F::EXPLICIT_MANTISSA_WIDTH {
        magnitude >> (top_bit - F::EXPLICIT_MANTISSA_WIDTH)
    } else {
        magnitude << (F::EXPLICIT_MANTISSA_WIDTH - top_bit)
    };
    let biased_exponent = (top_bit as i32 + F::EXPONENT_BIAS) as u64;
    let sign_bits = if sign { F::SIGN_MASK } else { 0 };
    F::from_bits(
        sign_bits
            | (biased_exponent << F::EXPLICIT_MANTISSA_WIDTH)
            | (significand & F::MANTISSA_MASK),
    )
}

pub fn fp_round_int<F: FloatFormat>(
    op: F,
    fpcr: Fpcr,
    rounding: RoundingMode,
    exact: bool,
    fpsr: &mut Fpsr,
) -> F {
    assert_ne!(rounding, RoundingMode::ToOdd);

    let (fp_type, sign, value) = fp_unpack(op, fpcr, fpsr);
    match fp_type {
        FpType::SNaN | FpType::QNaN => return process_nan(fp_type, op, fpcr, fpsr),
        FpType::Infinity => return F::infinity(sign),
        FpType::Zero => return F::zero(sign),
        FpType::Nonzero => {}
    }

    let exponent = value.exponent - NORMALIZED_POINT_POSITION as i32;
    if exponent >= 0 {
        return op;
    }

    let mut int_result = if sign {
        value.mantissa.wrapping_neg()
    } else {
        value.mantissa
    };
    let error = residual_error_on_right_shift(int_result, -exponent);
    int_result = arithmetic_shift_left(int_result, exponent);

    let round_up = match rounding {
        RoundingMode::ToNearestTieEven => {
            error > ResidualError::Half || (error == ResidualError::Half && int_result & 1 != 0)
        }
        RoundingMode::TowardsPlusInfinity => error != ResidualError::Zero,
        RoundingMode::TowardsMinusInfinity => false,
        RoundingMode::TowardsZero => error != ResidualError::Zero && int_result >> 63 != 0,
        RoundingMode::ToNearestTieAwayFromZero => {
            error > ResidualError::Half || (error == ResidualError::Half && int_result >> 63 == 0)
        }
        RoundingMode::ToOdd => unreachable!(),
    };
    if round_up {
        int_result = int_result.wrapping_add(1);
    }

    let new_sign = int_result >> 63 != 0;
    let magnitude = if new_sign {
        int_result.wrapping_neg()
    } else {
        int_result
    };
    let result = encode_integral::<F>(if int_result == 0 { sign } else { new_sign }, magnitude);

    if error != ResidualError::Zero && exact {
        process_exception(FpException::Inexact, fpcr, fpsr);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rounds_ties_and_preserves_signed_zero() {
        let fpcr = Fpcr::default();
        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_round_int(
                2.5f32.to_bits(),
                fpcr,
                RoundingMode::ToNearestTieEven,
                false,
                &mut fpsr
            ),
            2.0f32.to_bits()
        );
        assert_eq!(
            fp_round_int(
                2.5f32.to_bits(),
                fpcr,
                RoundingMode::ToNearestTieAwayFromZero,
                false,
                &mut fpsr
            ),
            3.0f32.to_bits()
        );
        assert_eq!(
            fp_round_int(
                (-0.25f32).to_bits(),
                fpcr,
                RoundingMode::TowardsZero,
                false,
                &mut fpsr
            ),
            (-0.0f32).to_bits()
        );
    }

    #[test]
    fn exact_rounding_accumulates_inexact_exception() {
        let fpcr = Fpcr::default();
        let mut fpsr = Fpsr::default();
        let result = fp_round_int(
            1.25f64.to_bits(),
            fpcr,
            RoundingMode::ToNearestTieEven,
            true,
            &mut fpsr,
        );
        assert_eq!(result, 1.0f64.to_bits());
        assert_eq!(fpsr.value() & (1 << 4), 1 << 4);
    }
}

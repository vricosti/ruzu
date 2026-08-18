use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::info::FloatFormat;
use crate::common::fp::mantissa_util::{residual_error_on_right_shift, ResidualError};
use crate::common::fp::process_exception::{process_exception, FpException};
use crate::common::fp::rounding_mode::RoundingMode;
use crate::common::fp::unpacked::{fp_unpack, FpType, NORMALIZED_POINT_POSITION};
use crate::common::safe_ops::{arithmetic_shift_left_u64, logical_shift_right_u64, negate_u64};

pub fn fp_to_fixed<F: FloatFormat>(
    ibits: usize,
    op: F,
    fbits: usize,
    unsigned: bool,
    fpcr: Fpcr,
    rounding: RoundingMode,
    fpsr: &mut Fpsr,
) -> u64 {
    assert_ne!(rounding, RoundingMode::ToOdd);
    assert!(ibits <= u64::BITS as usize);
    assert!(fbits <= ibits);

    let (fp_type, sign, value) = fp_unpack(op, fpcr, fpsr);
    if matches!(fp_type, FpType::SNaN | FpType::QNaN) {
        process_exception(FpException::InvalidOp, fpcr, fpsr);
    }

    if value.mantissa == 0 {
        return 0;
    }
    if sign && unsigned {
        process_exception(FpException::InvalidOp, fpcr, fpsr);
        return 0;
    }

    let exponent = value.exponent + fbits as i32 - NORMALIZED_POINT_POSITION as i32;
    let mut int_result = if sign {
        negate_u64(value.mantissa)
    } else {
        value.mantissa
    };
    let error = residual_error_on_right_shift(int_result, -exponent);
    int_result = arithmetic_shift_left_u64(int_result, exponent);

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

    let adjusted_mantissa = value.mantissa.wrapping_add(if round_up {
        logical_shift_right_u64(1, exponent)
    } else {
        0
    });
    let highest_set_bit = 63 - adjusted_mantissa.leading_zeros() as i32;
    let min_exponent_for_overflow = ibits as i32 - highest_set_bit - if unsigned { 0 } else { 1 };
    if exponent >= min_exponent_for_overflow {
        if unsigned || !sign {
            process_exception(FpException::InvalidOp, fpcr, fpsr);
            return low_ones(ibits - if unsigned { 0 } else { 1 });
        }

        let min_value = negate_u64(1u64 << (ibits - 1));
        if !(exponent == min_exponent_for_overflow && int_result == min_value) {
            process_exception(FpException::InvalidOp, fpcr, fpsr);
            return 1u64 << (ibits - 1);
        }
    }

    if error != ResidualError::Zero {
        process_exception(FpException::Inexact, fpcr, fpsr);
    }
    int_result & low_ones(ibits)
}

fn low_ones(bits: usize) -> u64 {
    if bits == u64::BITS as usize {
        u64::MAX
    } else {
        (1u64 << bits) - 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signed_s32_matches_upstream_saturation_and_exception_rules() {
        let fpcr = Fpcr::default();

        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_to_fixed(
                32,
                1.5f32.to_bits(),
                0,
                false,
                fpcr,
                RoundingMode::ToNearestTieAwayFromZero,
                &mut fpsr,
            ),
            2
        );
        assert_ne!(fpsr.value() & (1 << 4), 0);

        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_to_fixed(
                32,
                (-1.5f32).to_bits(),
                0,
                false,
                fpcr,
                RoundingMode::ToNearestTieAwayFromZero,
                &mut fpsr,
            ),
            (-2i32) as u32 as u64
        );
        assert_ne!(fpsr.value() & (1 << 4), 0);

        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_to_fixed(
                32,
                f32::INFINITY.to_bits(),
                0,
                false,
                fpcr,
                RoundingMode::TowardsZero,
                &mut fpsr,
            ),
            i32::MAX as u64
        );
        assert_ne!(fpsr.value() & 1, 0);

        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_to_fixed(
                32,
                f32::NAN.to_bits(),
                0,
                false,
                fpcr,
                RoundingMode::TowardsZero,
                &mut fpsr,
            ),
            0
        );
        assert_ne!(fpsr.value() & 1, 0);
    }
}

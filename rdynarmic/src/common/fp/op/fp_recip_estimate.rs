use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::info::FloatFormat;
use crate::common::fp::process_exception::{process_exception, FpException};
use crate::common::fp::process_nan::process_nan;
use crate::common::fp::rounding_mode::RoundingMode;
use crate::common::fp::unpacked::{fp_unpack, FpType, NORMALIZED_POINT_POSITION};
use crate::common::math_util::recip_estimate;

pub fn fp_recip_estimate<F: FloatFormat>(op: F, fpcr: Fpcr, fpsr: &mut Fpsr) -> F {
    let (fp_type, sign, value) = fp_unpack(op, fpcr, fpsr);

    if matches!(fp_type, FpType::SNaN | FpType::QNaN) {
        return process_nan(fp_type, op, fpcr, fpsr);
    }
    if fp_type == FpType::Infinity {
        return F::zero(sign);
    }
    if fp_type == FpType::Zero {
        process_exception(FpException::DivideByZero, fpcr, fpsr);
        return F::infinity(sign);
    }

    if value.exponent < F::EXPONENT_MIN - 2 {
        let overflow_to_inf = match fpcr.rmode() {
            RoundingMode::ToNearestTieEven => true,
            RoundingMode::TowardsPlusInfinity => !sign,
            RoundingMode::TowardsMinusInfinity => sign,
            RoundingMode::TowardsZero => false,
            _ => unreachable!(),
        };
        process_exception(FpException::Overflow, fpcr, fpsr);
        process_exception(FpException::Inexact, fpcr, fpsr);
        return if overflow_to_inf {
            F::infinity(sign)
        } else {
            F::max_normal(sign)
        };
    }

    if ((fpcr.fz() && F::TOTAL_WIDTH != 16) || (fpcr.fz16() && F::TOTAL_WIDTH == 16))
        && value.exponent >= -F::EXPONENT_MIN
    {
        fpsr.set_ufc(true);
        return F::zero(sign);
    }

    let scaled = value.mantissa >> (NORMALIZED_POINT_POSITION - 8);
    let mut estimate = (recip_estimate(scaled) as u64) << (F::EXPLICIT_MANTISSA_WIDTH - 8);
    let mut result_exponent = -(value.exponent + 1);
    if result_exponent < F::EXPONENT_MIN {
        match result_exponent {
            exponent if exponent == F::EXPONENT_MIN - 1 => {
                estimate |= F::IMPLICIT_LEADING_BIT;
                estimate >>= 1;
            }
            exponent if exponent == F::EXPONENT_MIN - 2 => {
                estimate |= F::IMPLICIT_LEADING_BIT;
                estimate >>= 2;
                result_exponent += 1;
            }
            _ => unreachable!(),
        }
    }

    let bits_exponent = (result_exponent + F::EXPONENT_BIAS) as u64;
    F::from_bits(
        (bits_exponent << F::EXPLICIT_MANTISSA_WIDTH)
            | (estimate & F::MANTISSA_MASK)
            | F::zero(sign).to_bits(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_uses_arm_estimate_not_exact_reciprocal() {
        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_recip_estimate(1.0f32.to_bits(), Fpcr::default(), &mut fpsr),
            0x3f7f_8000
        );
        assert_eq!(fpsr.value(), 0);
    }

    #[test]
    fn zero_sets_divide_by_zero() {
        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_recip_estimate(0u32, Fpcr::default(), &mut fpsr),
            f32::INFINITY.to_bits()
        );
        assert_eq!(fpsr.value(), 1 << 1);
    }

    #[test]
    fn signaling_nan_is_quiet_and_sets_invalid_operation() {
        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_recip_estimate(0x7f80_0001u32, Fpcr::default(), &mut fpsr),
            0x7fc0_0001
        );
        assert_eq!(fpsr.value(), 1);
    }
}

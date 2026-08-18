use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::info::FloatFormat;
use crate::common::fp::process_exception::{process_exception, FpException};
use crate::common::fp::process_nan::process_nan;
use crate::common::fp::unpacked::{fp_unpack, FpType, NORMALIZED_POINT_POSITION};
use crate::common::math_util::recip_sqrt_estimate;

pub fn fp_rsqrt_estimate<F: FloatFormat>(op: F, fpcr: Fpcr, fpsr: &mut Fpsr) -> F {
    let (fp_type, sign, value) = fp_unpack(op, fpcr, fpsr);

    if matches!(fp_type, FpType::SNaN | FpType::QNaN) {
        return process_nan(fp_type, op, fpcr, fpsr);
    }
    if fp_type == FpType::Zero {
        process_exception(FpException::DivideByZero, fpcr, fpsr);
        return F::infinity(sign);
    }
    if sign {
        process_exception(FpException::InvalidOp, fpcr, fpsr);
        return F::default_nan();
    }
    if fp_type == FpType::Infinity {
        return F::zero(false);
    }

    let result_exponent = (-(value.exponent + 1)) >> 1;
    let was_exponent_odd = value.exponent % 2 == 0;
    let scaled =
        value.mantissa >> (NORMALIZED_POINT_POSITION - if was_exponent_odd { 7 } else { 8 });
    let estimate = recip_sqrt_estimate(scaled) as u64;

    let bits_exponent = (result_exponent + F::EXPONENT_BIAS) as u64;
    let bits_mantissa = estimate << (F::EXPLICIT_MANTISSA_WIDTH - 8);
    F::from_bits((bits_exponent << F::EXPLICIT_MANTISSA_WIDTH) | (bits_mantissa & F::MANTISSA_MASK))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_uses_arm_estimate_not_exact_reciprocal_sqrt() {
        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_rsqrt_estimate(1.0f32.to_bits(), Fpcr::default(), &mut fpsr),
            0x3f7f_8000
        );
        assert_eq!(fpsr.value(), 0);
    }

    #[test]
    fn negative_input_sets_invalid_operation() {
        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_rsqrt_estimate((-1.0f32).to_bits(), Fpcr::default(), &mut fpsr),
            0x7fc0_0000
        );
        assert_eq!(fpsr.value(), 1);
    }
}

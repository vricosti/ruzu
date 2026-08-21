use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::fused::fused_mul_add;
use crate::common::fp::info::FloatFormat;
use crate::common::fp::process_exception::{process_exception, FpException};
use crate::common::fp::process_nan::process_nans3;
use crate::common::fp::rounding_mode::RoundingMode;
use crate::common::fp::unpacked::{fp_round_from_fpcr, fp_unpack, FpType};

pub fn fp_mul_add<F: FloatFormat>(addend: F, op1: F, op2: F, fpcr: Fpcr, fpsr: &mut Fpsr) -> F {
    let (type_a, sign_a, value_a) = fp_unpack(addend, fpcr, fpsr);
    let (type1, sign1, value1) = fp_unpack(op1, fpcr, fpsr);
    let (type2, sign2, value2) = fp_unpack(op2, fpcr, fpsr);

    let inf_a = type_a == FpType::Infinity;
    let inf1 = type1 == FpType::Infinity;
    let inf2 = type2 == FpType::Infinity;
    let zero_a = type_a == FpType::Zero;
    let zero1 = type1 == FpType::Zero;
    let zero2 = type2 == FpType::Zero;
    let maybe_nan = process_nans3([type_a, type1, type2], [addend, op1, op2], fpcr, fpsr);

    if type_a == FpType::QNaN && ((inf1 && zero2) || (zero1 && inf2)) {
        process_exception(FpException::InvalidOp, fpcr, fpsr);
        return F::default_nan();
    }
    if let Some(nan) = maybe_nan {
        return nan;
    }

    let sign_product = sign1 != sign2;
    let inf_product = inf1 || inf2;
    let zero_product = zero1 || zero2;
    if (inf1 && zero2) || (zero1 && inf2) || (inf_a && inf_product && sign_a != sign_product) {
        process_exception(FpException::InvalidOp, fpcr, fpsr);
        return F::default_nan();
    }
    if (inf_a && !sign_a) || (inf_product && !sign_product) {
        return F::infinity(false);
    }
    if (inf_a && sign_a) || (inf_product && sign_product) {
        return F::infinity(true);
    }
    if zero_a && zero_product && sign_a == sign_product {
        return F::zero(sign_a);
    }

    let result = fused_mul_add(value_a, value1, value2);
    if result.mantissa == 0 {
        return F::zero(fpcr.rmode() == RoundingMode::TowardsMinusInfinity);
    }
    fp_round_from_fpcr(result, fpcr, fpsr)
}

pub fn fp_mul_sub<F: FloatFormat>(minuend: F, op1: F, op2: F, fpcr: Fpcr, fpsr: &mut Fpsr) -> F {
    fp_mul_add(
        minuend,
        F::from_bits(op1.to_bits() ^ F::SIGN_MASK),
        op2,
        fpcr,
        fpsr,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finite_single_precision_multiply_add_is_fused() {
        let mut fpsr = Fpsr::default();
        let result = fp_mul_add(
            1.0f32.to_bits(),
            2.0f32.to_bits(),
            3.0f32.to_bits(),
            Fpcr::default(),
            &mut fpsr,
        );
        assert_eq!(result, 7.0f32.to_bits());
        assert_eq!(fpsr.value(), 0);
    }

    #[test]
    fn invalid_product_overrides_quiet_nan_addend() {
        let mut fpsr = Fpsr::default();
        let result = fp_mul_add(
            0xffc1_2345u32,
            f32::INFINITY.to_bits(),
            0.0f32.to_bits(),
            Fpcr::default(),
            &mut fpsr,
        );
        assert_eq!(result, 0x7fc0_0000);
        assert_ne!(fpsr.value() & 1, 0);
    }
}

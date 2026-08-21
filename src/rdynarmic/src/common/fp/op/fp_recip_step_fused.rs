use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::fused::fused_mul_add;
use crate::common::fp::info::FloatFormat;
use crate::common::fp::process_nan::process_nans;
use crate::common::fp::rounding_mode::RoundingMode;
use crate::common::fp::unpacked::{fp_round_from_fpcr, fp_unpack, to_normalized, FpType};

pub fn fp_recip_step_fused<F: FloatFormat>(mut op1: F, op2: F, fpcr: Fpcr, fpsr: &mut Fpsr) -> F {
    op1 = F::from_bits(op1.to_bits() ^ F::SIGN_MASK);

    let (type1, sign1, value1) = fp_unpack(op1, fpcr, fpsr);
    let (type2, sign2, value2) = fp_unpack(op2, fpcr, fpsr);
    if let Some(nan) = process_nans(type1, type2, op1, op2, fpcr, fpsr) {
        return nan;
    }

    let inf1 = type1 == FpType::Infinity;
    let inf2 = type2 == FpType::Infinity;
    let zero1 = type1 == FpType::Zero;
    let zero2 = type2 == FpType::Zero;
    if (inf1 && zero2) || (zero1 && inf2) {
        return F::from_bits((1u64 + F::EXPONENT_BIAS as u64) << F::EXPLICIT_MANTISSA_WIDTH);
    }
    if inf1 || inf2 {
        return F::infinity(sign1 != sign2);
    }

    let result = fused_mul_add(to_normalized(false, 0, 2), value1, value2);
    if result.mantissa == 0 {
        return F::zero(fpcr.rmode() == RoundingMode::TowardsMinusInfinity);
    }
    fp_round_from_fpcr(result, fpcr, fpsr)
}

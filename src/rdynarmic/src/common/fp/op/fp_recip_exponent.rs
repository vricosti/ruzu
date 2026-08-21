//! Port of Dynarmic's `common/fp/op/FPRecipExponent.cpp`.

use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::info::FloatFormat;
use crate::common::fp::process_nan::process_nan;
use crate::common::fp::unpacked::{fp_unpack, FpType};

pub fn fp_recip_exponent<F: FloatFormat>(op: F, fpcr: Fpcr, fpsr: &mut Fpsr) -> F {
    let (fp_type, sign, _) = fp_unpack(op, fpcr, fpsr);

    if matches!(fp_type, FpType::SNaN | FpType::QNaN) {
        return process_nan(fp_type, op, fpcr, fpsr);
    }

    let sign_bits = F::zero(sign).to_bits();
    let exponent = (op.to_bits() & F::EXPONENT_MASK) >> F::EXPLICIT_MANTISSA_WIDTH;

    if exponent == 0 {
        let max_exponent = (1u64 << F::EXPONENT_WIDTH) - 2;
        return F::from_bits(sign_bits | (max_exponent << F::EXPLICIT_MANTISSA_WIDTH));
    }

    let adjusted_exponent = ((!exponent) << F::EXPLICIT_MANTISSA_WIDTH) & F::EXPONENT_MASK;
    F::from_bits(sign_bits | adjusted_exponent)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reciprocal_exponent_matches_upstream_special_values() {
        let mut fpsr = Fpsr::default();

        assert_eq!(
            fp_recip_exponent(0.0f32.to_bits(), Fpcr::default(), &mut fpsr),
            0x7f00_0000
        );
        assert_eq!(
            fp_recip_exponent((-0.0f32).to_bits(), Fpcr::default(), &mut fpsr),
            0xff00_0000
        );
        assert_eq!(
            fp_recip_exponent(f32::INFINITY.to_bits(), Fpcr::default(), &mut fpsr),
            0
        );
        assert_eq!(
            fp_recip_exponent(f32::NEG_INFINITY.to_bits(), Fpcr::default(), &mut fpsr),
            0x8000_0000
        );
    }

    #[test]
    fn reciprocal_exponent_processes_nan_and_input_denormal_like_upstream() {
        let mut fpsr = Fpsr::default();
        let snan = 0x7f80_0001u32;
        assert_eq!(
            fp_recip_exponent(snan, Fpcr::default(), &mut fpsr),
            0x7fc0_0001
        );
        assert_eq!(fpsr.value() & 1, 1);

        let mut fpsr = Fpsr::default();
        assert_eq!(
            fp_recip_exponent(1u32, Fpcr::new(1 << 24), &mut fpsr),
            0x7f00_0000
        );
        assert_eq!(fpsr.value() & (1 << 7), 1 << 7);
    }
}

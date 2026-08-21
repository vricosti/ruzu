use crate::common::fp::fpcr::Fpcr;
use crate::common::fp::fpsr::Fpsr;
use crate::common::fp::info::FloatFormat;
use crate::common::fp::process_exception::{process_exception, FpException};
use crate::common::fp::rounding_mode::RoundingMode;
use crate::common::fp::unpacked::{fp_round_cv, fp_unpack_cv, FpType};

fn fp_convert_nan<To: FloatFormat, From: FloatFormat>(op: From) -> To {
    let bits = op.to_bits();
    let sign = bits & From::SIGN_MASK != 0;
    let fraction = match From::TOTAL_WIDTH {
        64 => bits & ((1u64 << 51) - 1),
        32 => (bits & ((1u64 << 22) - 1)) << 29,
        16 => (bits & ((1u64 << 9) - 1)) << 42,
        _ => unreachable!("unsupported floating-point source format"),
    };

    let shifted_sign = (sign as u64) << (To::TOTAL_WIDTH - 1);
    let exponent = (1u64 << (To::TOTAL_WIDTH - To::EXPLICIT_MANTISSA_WIDTH)) - 1;
    let bits = match To::TOTAL_WIDTH {
        64 => shifted_sign | (exponent << 51) | fraction,
        32 => shifted_sign | (exponent << 22) | ((fraction >> 29) & ((1u64 << 22) - 1)),
        16 => shifted_sign | (exponent << 9) | ((fraction >> 42) & ((1u64 << 9) - 1)),
        _ => unreachable!("unsupported floating-point destination format"),
    };
    To::from_bits(bits)
}

pub fn fp_convert<To: FloatFormat, From: FloatFormat>(
    op: From,
    fpcr: Fpcr,
    rounding_mode: RoundingMode,
    fpsr: &mut Fpsr,
) -> To {
    let (fp_type, sign, value) = fp_unpack_cv(op, fpcr, fpsr);
    let is_althp = To::TOTAL_WIDTH == 16 && fpcr.ahp();

    if matches!(fp_type, FpType::SNaN | FpType::QNaN) {
        let result = if is_althp {
            To::zero(sign)
        } else if fpcr.dn() {
            To::default_nan()
        } else {
            fp_convert_nan::<To, From>(op)
        };

        if fp_type == FpType::SNaN || is_althp {
            process_exception(FpException::InvalidOp, fpcr, fpsr);
        }
        return result;
    }

    if fp_type == FpType::Infinity {
        if is_althp {
            process_exception(FpException::InvalidOp, fpcr, fpsr);
            return To::from_bits(((sign as u64) << 15) | 0x7fff);
        }
        return To::infinity(sign);
    }

    if fp_type == FpType::Zero {
        return To::zero(sign);
    }

    fp_round_cv(value, fpcr, rounding_mode, fpsr)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_to_half_sets_underflow_and_inexact() {
        let mut fpsr = Fpsr::default();
        let result = fp_convert::<u16, u32>(
            0x3280_0000,
            Fpcr::default(),
            RoundingMode::ToNearestTieEven,
            &mut fpsr,
        );

        assert_eq!(result, 0x0000);
        assert_eq!(fpsr.value() & 0x18, 0x18);
    }

    #[test]
    fn signaling_nan_is_quieted_and_sets_invalid_operation() {
        let mut fpsr = Fpsr::default();
        let result = fp_convert::<u16, u32>(
            0x7f80_0001,
            Fpcr::default(),
            RoundingMode::ToNearestTieEven,
            &mut fpsr,
        );

        assert_eq!(result, 0x7e00);
        assert_eq!(fpsr.value() & 1, 1);
    }

    #[test]
    fn default_nan_mode_discards_payload() {
        let mut fpsr = Fpsr::default();
        let result = fp_convert::<u16, u32>(
            0xffc1_2345,
            Fpcr::new(1 << 25),
            RoundingMode::ToNearestTieEven,
            &mut fpsr,
        );

        assert_eq!(result, 0x7e00);
        assert_eq!(fpsr.value(), 0);
    }

    #[test]
    fn alternative_half_precision_rejects_infinity() {
        let mut fpsr = Fpsr::default();
        let result = fp_convert::<u16, u32>(
            0xff80_0000,
            Fpcr::new(1 << 26),
            RoundingMode::ToNearestTieEven,
            &mut fpsr,
        );

        assert_eq!(result, 0xffff);
        assert_eq!(fpsr.value() & 1, 1);
    }
}

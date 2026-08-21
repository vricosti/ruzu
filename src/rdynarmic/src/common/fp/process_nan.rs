use super::fpcr::Fpcr;
use super::fpsr::Fpsr;
use super::info::FloatFormat;
use super::process_exception::{process_exception, FpException};
use super::unpacked::FpType;

pub fn process_nan<F: FloatFormat>(fp_type: FpType, op: F, fpcr: Fpcr, fpsr: &mut Fpsr) -> F {
    assert!(matches!(fp_type, FpType::QNaN | FpType::SNaN));

    let mut result = op;
    if fp_type == FpType::SNaN {
        result = F::from_bits(op.to_bits() | F::MANTISSA_MSB);
        process_exception(FpException::InvalidOp, fpcr, fpsr);
    }
    if fpcr.dn() {
        result = F::default_nan();
    }
    result
}

pub fn process_nans<F: FloatFormat>(
    type1: FpType,
    type2: FpType,
    op1: F,
    op2: F,
    fpcr: Fpcr,
    fpsr: &mut Fpsr,
) -> Option<F> {
    if type1 == FpType::SNaN {
        return Some(process_nan(type1, op1, fpcr, fpsr));
    }
    if type2 == FpType::SNaN {
        return Some(process_nan(type2, op2, fpcr, fpsr));
    }
    if type1 == FpType::QNaN {
        return Some(process_nan(type1, op1, fpcr, fpsr));
    }
    if type2 == FpType::QNaN {
        return Some(process_nan(type2, op2, fpcr, fpsr));
    }
    None
}

pub fn process_nans3<F: FloatFormat>(
    types: [FpType; 3],
    operands: [F; 3],
    fpcr: Fpcr,
    fpsr: &mut Fpsr,
) -> Option<F> {
    for index in 0..3 {
        if types[index] == FpType::SNaN {
            return Some(process_nan(types[index], operands[index], fpcr, fpsr));
        }
    }
    for index in 0..3 {
        if types[index] == FpType::QNaN {
            return Some(process_nan(types[index], operands[index], fpcr, fpsr));
        }
    }
    None
}

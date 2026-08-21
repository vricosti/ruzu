use super::fpcr::Fpcr;
use super::fpsr::Fpsr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FpException {
    InvalidOp,
    DivideByZero,
    Overflow,
    Underflow,
    Inexact,
    InputDenorm,
}

pub fn process_exception(exception: FpException, fpcr: Fpcr, fpsr: &mut Fpsr) {
    match exception {
        FpException::InvalidOp => {
            assert!(
                !fpcr.ioe(),
                "raising floating-point exceptions is unimplemented"
            );
            fpsr.set_ioc(true);
        }
        FpException::DivideByZero => {
            assert!(
                !fpcr.dze(),
                "raising floating-point exceptions is unimplemented"
            );
            fpsr.set_dzc(true);
        }
        FpException::Overflow => {
            assert!(
                !fpcr.ofe(),
                "raising floating-point exceptions is unimplemented"
            );
            fpsr.set_ofc(true);
        }
        FpException::Underflow => {
            assert!(
                !fpcr.ufe(),
                "raising floating-point exceptions is unimplemented"
            );
            fpsr.set_ufc(true);
        }
        FpException::Inexact => {
            assert!(
                !fpcr.ixe(),
                "raising floating-point exceptions is unimplemented"
            );
            fpsr.set_ixc(true);
        }
        FpException::InputDenorm => {
            assert!(
                !fpcr.ide(),
                "raising floating-point exceptions is unimplemented"
            );
            fpsr.set_idc(true);
        }
    }
}

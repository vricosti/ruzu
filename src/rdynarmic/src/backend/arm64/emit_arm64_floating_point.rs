//! ARM64 scalar floating-point emission.
//!
//! Upstream owner: `backend/arm64/emit_arm64_floating_point.cpp`.

use crate::backend::arm64::abi::XSCRATCH0;
use crate::backend::arm64::block_of_code::BlockOfCode;
use crate::backend::arm64::emit_context::EmitContext;
use crate::backend::arm64::inst;
use crate::backend::arm64::reg_alloc::RegAlloc;
use crate::ir::value::InstRef;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FpSize {
    Single,
    Double,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RoundingMode {
    ToNearestTieEven,
    TowardsPlusInfinity,
    TowardsMinusInfinity,
    TowardsZero,
    ToNearestTieAwayFromZero,
    ToOdd,
}

impl RoundingMode {
    fn from_u8(value: u8) -> Result<Self, String> {
        match value {
            0 => Ok(Self::ToNearestTieEven),
            1 => Ok(Self::TowardsPlusInfinity),
            2 => Ok(Self::TowardsMinusInfinity),
            3 => Ok(Self::TowardsZero),
            4 => Ok(Self::ToNearestTieAwayFromZero),
            5 => Ok(Self::ToOdd),
            _ => Err(format!(
                "ARM64 floating point: invalid rounding mode {value}"
            )),
        }
    }

    fn fpcr_bits(self) -> Option<u32> {
        match self {
            Self::ToNearestTieEven => Some(0),
            Self::TowardsPlusInfinity => Some(1),
            Self::TowardsMinusInfinity => Some(2),
            Self::TowardsZero => Some(3),
            Self::ToNearestTieAwayFromZero | Self::ToOdd => None,
        }
    }
}

fn emit_mov_w_imm(code: &mut BlockOfCode, reg: u8, imm: u32) -> Result<(), String> {
    code.write_u32(inst::movz_w(reg, (imm & 0xffff) as u16, 0))?;
    let upper = ((imm >> 16) & 0xffff) as u16;
    if upper != 0 {
        code.write_u32(inst::movk_w(reg, upper, 16))?;
    }
    Ok(())
}

fn emit_with_rounding_fpcr(
    code: &mut BlockOfCode,
    ctx: &EmitContext<'_>,
    rounding_mode: RoundingMode,
    emit: impl FnOnce(&mut BlockOfCode) -> Result<(), String>,
) -> Result<(), String> {
    let Some(rounding_bits) = rounding_mode.fpcr_bits() else {
        return Err(format!(
            "ARM64 floating point: fixed to FP rounding mode {:?} is not supported by FPCR",
            rounding_mode
        ));
    };

    let current_fpcr = ctx.fpcr(true).value();
    let target_fpcr = (current_fpcr & !(0b11 << 22)) | (rounding_bits << 22);
    if target_fpcr == current_fpcr {
        return emit(code);
    }

    emit_mov_w_imm(code, XSCRATCH0, target_fpcr)?;
    code.write_u32(inst::msr_fpcr(XSCRATCH0))?;
    emit(code)?;
    emit_mov_w_imm(code, XSCRATCH0, current_fpcr)?;
    code.write_u32(inst::msr_fpcr(XSCRATCH0))?;
    Ok(())
}

fn emit_two_op(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
    emit: impl FnOnce(u8, u8) -> u32,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut result = match size {
        FpSize::Single => ctx.reg_alloc.write_s(inst_ref),
        FpSize::Double => ctx.reg_alloc.write_d(inst_ref),
    };
    let mut operand = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[0]),
        FpSize::Double => ctx.reg_alloc.read_d(args[0]),
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;
    code.write_u32(emit(
        result.index().expect("result realized") as u8,
        operand.index().expect("operand realized") as u8,
    ))?;
    Ok(())
}

fn emit_three_op(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
    emit: impl FnOnce(u8, u8, u8) -> u32,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut result = match size {
        FpSize::Single => ctx.reg_alloc.write_s(inst_ref),
        FpSize::Double => ctx.reg_alloc.write_d(inst_ref),
    };
    let mut a = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[0]),
        FpSize::Double => ctx.reg_alloc.read_d(args[0]),
    };
    let mut b = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[1]),
        FpSize::Double => ctx.reg_alloc.read_d(args[1]),
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut a, &mut b])?;
    ctx.fpsr.load(code)?;
    code.write_u32(emit(
        result.index().expect("result realized") as u8,
        a.index().expect("a realized") as u8,
        b.index().expect("b realized") as u8,
    ))?;
    Ok(())
}

fn emit_four_op(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
    emit: impl FnOnce(u8, u8, u8, u8) -> u32,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut result = match size {
        FpSize::Single => ctx.reg_alloc.write_s(inst_ref),
        FpSize::Double => ctx.reg_alloc.write_d(inst_ref),
    };
    let mut a = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[0]),
        FpSize::Double => ctx.reg_alloc.read_d(args[0]),
    };
    let mut b = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[1]),
        FpSize::Double => ctx.reg_alloc.read_d(args[1]),
    };
    let mut c = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[2]),
        FpSize::Double => ctx.reg_alloc.read_d(args[2]),
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut a, &mut b, &mut c])?;
    ctx.fpsr.load(code)?;
    code.write_u32(emit(
        result.index().expect("result realized") as u8,
        a.index().expect("a realized") as u8,
        b.index().expect("b realized") as u8,
        c.index().expect("c realized") as u8,
    ))?;
    Ok(())
}

fn fpcr_rounding_mode(ctx: &EmitContext<'_>) -> Result<RoundingMode, String> {
    RoundingMode::from_u8(((ctx.fpcr(true).value() >> 22) & 0b11) as u8)
}

fn emit_convert(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    from: FpSize,
    to: FpSize,
    emit: impl FnOnce(u8, u8) -> u32,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let rounding_mode = RoundingMode::from_u8(args[1].get_immediate_u8())?;
    let fpcr_rounding_mode = fpcr_rounding_mode(ctx)?;
    if rounding_mode != fpcr_rounding_mode {
        return Err(format!(
            "ARM64 floating point: convert rounding mode {:?} does not match FPCR {:?}",
            rounding_mode, fpcr_rounding_mode
        ));
    }

    let mut result = match to {
        FpSize::Single => ctx.reg_alloc.write_s(inst_ref),
        FpSize::Double => ctx.reg_alloc.write_d(inst_ref),
    };
    let mut operand = match from {
        FpSize::Single => ctx.reg_alloc.read_s(args[0]),
        FpSize::Double => ctx.reg_alloc.read_d(args[0]),
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;
    code.write_u32(emit(
        result.index().expect("result realized") as u8,
        operand.index().expect("operand realized") as u8,
    ))?;
    Ok(())
}

fn emit_convert_half(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    from_half: bool,
    other_size: FpSize,
    emit: impl FnOnce(u8, u8) -> u32,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let rounding_mode = RoundingMode::from_u8(args[1].get_immediate_u8())?;
    let fpcr_rounding_mode = fpcr_rounding_mode(ctx)?;
    if rounding_mode != fpcr_rounding_mode {
        return Err(format!(
            "ARM64 floating point: convert rounding mode {:?} does not match FPCR {:?}",
            rounding_mode, fpcr_rounding_mode
        ));
    }

    let mut result = if from_half {
        match other_size {
            FpSize::Single => ctx.reg_alloc.write_s(inst_ref),
            FpSize::Double => ctx.reg_alloc.write_d(inst_ref),
        }
    } else {
        ctx.reg_alloc.write_h(inst_ref)
    };
    let mut operand = if from_half {
        ctx.reg_alloc.read_h(args[0])
    } else {
        match other_size {
            FpSize::Single => ctx.reg_alloc.read_s(args[0]),
            FpSize::Double => ctx.reg_alloc.read_d(args[0]),
        }
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;
    code.write_u32(emit(
        result.index().expect("result realized") as u8,
        operand.index().expect("operand realized") as u8,
    ))?;
    Ok(())
}

fn emit_compare(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut flags = ctx.reg_alloc.write_flags(inst_ref);
    let mut a = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[0]),
        FpSize::Double => ctx.reg_alloc.read_d(args[0]),
    };
    let exc_on_qnan = args[2].get_immediate_u1();

    if args[1].is_immediate() && args[1].get_immediate_u64() == 0 {
        RegAlloc::realize_all(code, ctx.block, &mut [&mut flags, &mut a])?;
        ctx.fpsr.load(code)?;
        let a = a.index().expect("a realized") as u8;
        let word = match (size, exc_on_qnan) {
            (FpSize::Single, false) => inst::fcmp_s_zero(a),
            (FpSize::Single, true) => inst::fcmpe_s_zero(a),
            (FpSize::Double, false) => inst::fcmp_d_zero(a),
            (FpSize::Double, true) => inst::fcmpe_d_zero(a),
        };
        code.write_u32(word)?;
        return Ok(());
    }

    let mut b = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[1]),
        FpSize::Double => ctx.reg_alloc.read_d(args[1]),
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut flags, &mut a, &mut b])?;
    ctx.fpsr.load(code)?;
    let a = a.index().expect("a realized") as u8;
    let b = b.index().expect("b realized") as u8;
    let word = match (size, exc_on_qnan) {
        (FpSize::Single, false) => inst::fcmp_s(a, b),
        (FpSize::Single, true) => inst::fcmpe_s(a, b),
        (FpSize::Double, false) => inst::fcmp_d(a, b),
        (FpSize::Double, true) => inst::fcmpe_d(a, b),
    };
    code.write_u32(word)?;
    Ok(())
}

fn emit_to_fixed32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
    signed: bool,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let fbits = args[1].get_immediate_u8();
    let rounding_mode = RoundingMode::from_u8(args[2].get_immediate_u8())?;
    if fbits > 32 {
        return Err(format!(
            "ARM64 floating point: FP to 32-bit fixed with invalid fbits={fbits}"
        ));
    }
    if fbits != 0 && rounding_mode != RoundingMode::TowardsZero {
        return Err(format!(
            "ARM64 floating point: FP to 32-bit fixed with fbits={fbits} and rounding mode {:?} is not ported",
            rounding_mode
        ));
    }
    if rounding_mode == RoundingMode::ToOdd {
        return Err("ARM64 floating point: ToOdd FP to 32-bit fixed is not ported".to_string());
    }

    let mut result = ctx.reg_alloc.write_w(inst_ref);
    let mut operand = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[0]),
        FpSize::Double => ctx.reg_alloc.read_d(args[0]),
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;
    let result = result.index().expect("result realized") as u8;
    let operand = operand.index().expect("operand realized") as u8;
    let word = match (size, signed, rounding_mode, fbits) {
        (FpSize::Single, false, RoundingMode::TowardsZero, fbits) if fbits != 0 => {
            inst::fcvtzu_w_from_s_fixed(result, operand, fbits)
        }
        (FpSize::Double, false, RoundingMode::TowardsZero, fbits) if fbits != 0 => {
            inst::fcvtzu_w_from_d_fixed(result, operand, fbits)
        }
        (FpSize::Single, true, RoundingMode::TowardsZero, fbits) if fbits != 0 => {
            inst::fcvtzs_w_from_s_fixed(result, operand, fbits)
        }
        (FpSize::Double, true, RoundingMode::TowardsZero, fbits) if fbits != 0 => {
            inst::fcvtzs_w_from_d_fixed(result, operand, fbits)
        }
        (FpSize::Single, false, RoundingMode::ToNearestTieEven, _) => {
            inst::fcvtnu_w_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::ToNearestTieEven, _) => {
            inst::fcvtnu_w_from_d(result, operand)
        }
        (FpSize::Single, false, RoundingMode::TowardsPlusInfinity, _) => {
            inst::fcvtpu_w_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::TowardsPlusInfinity, _) => {
            inst::fcvtpu_w_from_d(result, operand)
        }
        (FpSize::Single, false, RoundingMode::TowardsMinusInfinity, _) => {
            inst::fcvtmu_w_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::TowardsMinusInfinity, _) => {
            inst::fcvtmu_w_from_d(result, operand)
        }
        (FpSize::Single, false, RoundingMode::TowardsZero, _) => {
            inst::fcvtzu_w_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::TowardsZero, _) => {
            inst::fcvtzu_w_from_d(result, operand)
        }
        (FpSize::Single, false, RoundingMode::ToNearestTieAwayFromZero, _) => {
            inst::fcvtau_w_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::ToNearestTieAwayFromZero, _) => {
            inst::fcvtau_w_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::ToNearestTieEven, _) => {
            inst::fcvtns_w_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::ToNearestTieEven, _) => {
            inst::fcvtns_w_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::TowardsPlusInfinity, _) => {
            inst::fcvtps_w_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::TowardsPlusInfinity, _) => {
            inst::fcvtps_w_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::TowardsMinusInfinity, _) => {
            inst::fcvtms_w_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::TowardsMinusInfinity, _) => {
            inst::fcvtms_w_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::TowardsZero, _) => {
            inst::fcvtzs_w_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::TowardsZero, _) => {
            inst::fcvtzs_w_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::ToNearestTieAwayFromZero, _) => {
            inst::fcvtas_w_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::ToNearestTieAwayFromZero, _) => {
            inst::fcvtas_w_from_d(result, operand)
        }
        (_, _, RoundingMode::ToOdd, _) => unreachable!(),
    };
    code.write_u32(word)?;
    Ok(())
}

fn emit_to_fixed16(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
    signed: bool,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let fbits = args[1].get_immediate_u8();
    let rounding_mode = RoundingMode::from_u8(args[2].get_immediate_u8())?;
    if fbits > 16 {
        return Err(format!(
            "ARM64 floating point: FP to 16-bit fixed with invalid fbits={fbits}"
        ));
    }
    if rounding_mode != RoundingMode::TowardsZero {
        return Err(format!(
            "ARM64 floating point: FP to 16-bit fixed requires TowardsZero, got {:?}",
            rounding_mode
        ));
    }

    let mut result = ctx.reg_alloc.write_w(inst_ref);
    let mut operand = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[0]),
        FpSize::Double => ctx.reg_alloc.read_d(args[0]),
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;
    let result = result.index().expect("result realized") as u8;
    let operand = operand.index().expect("operand realized") as u8;
    let scaled_fbits = fbits + 16;
    let convert = match (size, signed) {
        (FpSize::Single, false) => inst::fcvtzu_w_from_s_fixed(result, operand, scaled_fbits),
        (FpSize::Double, false) => inst::fcvtzu_w_from_d_fixed(result, operand, scaled_fbits),
        (FpSize::Single, true) => inst::fcvtzs_w_from_s_fixed(result, operand, scaled_fbits),
        (FpSize::Double, true) => inst::fcvtzs_w_from_d_fixed(result, operand, scaled_fbits),
    };
    code.write_u32(convert)?;
    if signed {
        code.write_u32(inst::asr_w_imm(XSCRATCH0, result, 31))?;
        code.write_u32(inst::add_w_reg_lsr(result, result, XSCRATCH0, 16))?;
    }
    code.write_u32(inst::lsr_w_imm(result, result, 16))?;
    Ok(())
}

fn emit_from_fixed32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
    signed: bool,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let fbits = args[1].get_immediate_u8();
    let rounding_mode = RoundingMode::from_u8(args[2].get_immediate_u8())?;
    if fbits > 32 {
        return Err(format!(
            "ARM64 floating point: 32-bit fixed to FP with invalid fbits={fbits}"
        ));
    }

    let mut result = match size {
        FpSize::Single => ctx.reg_alloc.write_s(inst_ref),
        FpSize::Double => ctx.reg_alloc.write_d(inst_ref),
    };
    let mut operand = ctx.reg_alloc.read_w(args[0]);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;
    let result = result.index().expect("result realized") as u8;
    let operand = operand.index().expect("operand realized") as u8;
    let word = match (size, signed) {
        (FpSize::Single, false) if fbits == 0 => inst::ucvtf_s_from_w(result, operand),
        (FpSize::Double, false) if fbits == 0 => inst::ucvtf_d_from_w(result, operand),
        (FpSize::Single, true) if fbits == 0 => inst::scvtf_s_from_w(result, operand),
        (FpSize::Double, true) if fbits == 0 => inst::scvtf_d_from_w(result, operand),
        (FpSize::Single, false) => inst::ucvtf_s_from_w_fixed(result, operand, fbits),
        (FpSize::Double, false) => inst::ucvtf_d_from_w_fixed(result, operand, fbits),
        (FpSize::Single, true) => inst::scvtf_s_from_w_fixed(result, operand, fbits),
        (FpSize::Double, true) => inst::scvtf_d_from_w_fixed(result, operand, fbits),
    };
    emit_with_rounding_fpcr(code, ctx, rounding_mode, |code| {
        code.write_u32(word)?;
        Ok(())
    })
}

fn emit_from_fixed16(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
    signed: bool,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let fbits = args[1].get_immediate_u8();
    let rounding_mode = RoundingMode::from_u8(args[2].get_immediate_u8())?;
    if fbits > 16 {
        return Err(format!(
            "ARM64 floating point: 16-bit fixed to FP with invalid fbits={fbits}"
        ));
    }

    let mut result = match size {
        FpSize::Single => ctx.reg_alloc.write_s(inst_ref),
        FpSize::Double => ctx.reg_alloc.write_d(inst_ref),
    };
    let mut operand = ctx.reg_alloc.read_w(args[0]);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;
    let result = result.index().expect("result realized") as u8;
    let operand = operand.index().expect("operand realized") as u8;
    let scaled_fbits = fbits + 16;
    let word = match (size, signed) {
        (FpSize::Single, false) => inst::ucvtf_s_from_w_fixed(result, XSCRATCH0, scaled_fbits),
        (FpSize::Double, false) => inst::ucvtf_d_from_w_fixed(result, XSCRATCH0, scaled_fbits),
        (FpSize::Single, true) => inst::scvtf_s_from_w_fixed(result, XSCRATCH0, scaled_fbits),
        (FpSize::Double, true) => inst::scvtf_d_from_w_fixed(result, XSCRATCH0, scaled_fbits),
    };
    emit_with_rounding_fpcr(code, ctx, rounding_mode, |code| {
        code.write_u32(inst::lsl_w_imm(XSCRATCH0, operand, 16))?;
        code.write_u32(word)?;
        Ok(())
    })
}

fn emit_to_fixed64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
    signed: bool,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let fbits = args[1].get_immediate_u8();
    let rounding_mode = RoundingMode::from_u8(args[2].get_immediate_u8())?;
    if fbits > 64 {
        return Err(format!(
            "ARM64 floating point: FP to 64-bit fixed with invalid fbits={fbits}"
        ));
    }
    if rounding_mode == RoundingMode::ToOdd {
        return Err("ARM64 floating point: ToOdd FP to 64-bit fixed is not ported".to_string());
    }
    if fbits != 0 && rounding_mode != RoundingMode::TowardsZero {
        return Err(format!(
            "ARM64 floating point: FP to 64-bit fixed with fbits={fbits} and rounding mode {:?} is not ported",
            rounding_mode
        ));
    }

    let mut result = ctx.reg_alloc.write_x(inst_ref);
    let mut operand = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[0]),
        FpSize::Double => ctx.reg_alloc.read_d(args[0]),
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;
    let result = result.index().expect("result realized") as u8;
    let operand = operand.index().expect("operand realized") as u8;
    let word = match (size, signed, rounding_mode, fbits) {
        (FpSize::Single, false, RoundingMode::ToNearestTieEven, 0) => {
            inst::fcvtnu_x_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::ToNearestTieEven, 0) => {
            inst::fcvtnu_x_from_d(result, operand)
        }
        (FpSize::Single, false, RoundingMode::TowardsPlusInfinity, 0) => {
            inst::fcvtpu_x_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::TowardsPlusInfinity, 0) => {
            inst::fcvtpu_x_from_d(result, operand)
        }
        (FpSize::Single, false, RoundingMode::TowardsMinusInfinity, 0) => {
            inst::fcvtmu_x_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::TowardsMinusInfinity, 0) => {
            inst::fcvtmu_x_from_d(result, operand)
        }
        (FpSize::Single, false, RoundingMode::TowardsZero, 0) => {
            inst::fcvtzu_x_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::TowardsZero, 0) => {
            inst::fcvtzu_x_from_d(result, operand)
        }
        (FpSize::Single, false, RoundingMode::ToNearestTieAwayFromZero, 0) => {
            inst::fcvtau_x_from_s(result, operand)
        }
        (FpSize::Double, false, RoundingMode::ToNearestTieAwayFromZero, 0) => {
            inst::fcvtau_x_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::ToNearestTieEven, 0) => {
            inst::fcvtns_x_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::ToNearestTieEven, 0) => {
            inst::fcvtns_x_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::TowardsPlusInfinity, 0) => {
            inst::fcvtps_x_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::TowardsPlusInfinity, 0) => {
            inst::fcvtps_x_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::TowardsMinusInfinity, 0) => {
            inst::fcvtms_x_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::TowardsMinusInfinity, 0) => {
            inst::fcvtms_x_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::TowardsZero, 0) => {
            inst::fcvtzs_x_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::TowardsZero, 0) => {
            inst::fcvtzs_x_from_d(result, operand)
        }
        (FpSize::Single, true, RoundingMode::ToNearestTieAwayFromZero, 0) => {
            inst::fcvtas_x_from_s(result, operand)
        }
        (FpSize::Double, true, RoundingMode::ToNearestTieAwayFromZero, 0) => {
            inst::fcvtas_x_from_d(result, operand)
        }
        (FpSize::Single, false, RoundingMode::TowardsZero, _) => {
            inst::fcvtzu_x_from_s_fixed(result, operand, fbits)
        }
        (FpSize::Double, false, RoundingMode::TowardsZero, _) => {
            inst::fcvtzu_x_from_d_fixed(result, operand, fbits)
        }
        (FpSize::Single, true, RoundingMode::TowardsZero, _) => {
            inst::fcvtzs_x_from_s_fixed(result, operand, fbits)
        }
        (FpSize::Double, true, RoundingMode::TowardsZero, _) => {
            inst::fcvtzs_x_from_d_fixed(result, operand, fbits)
        }
        (_, _, RoundingMode::ToOdd, _) => unreachable!(),
        _ => unreachable!("unsupported fbits/rounding combination checked above"),
    };
    code.write_u32(word)?;
    Ok(())
}

fn emit_from_fixed64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
    signed: bool,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let fbits = args[1].get_immediate_u8();
    let rounding_mode = RoundingMode::from_u8(args[2].get_immediate_u8())?;
    if fbits > 64 {
        return Err(format!(
            "ARM64 floating point: 64-bit fixed to FP with invalid fbits={fbits}"
        ));
    }

    let mut result = match size {
        FpSize::Single => ctx.reg_alloc.write_s(inst_ref),
        FpSize::Double => ctx.reg_alloc.write_d(inst_ref),
    };
    let mut operand = ctx.reg_alloc.read_x(args[0]);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;
    let result = result.index().expect("result realized") as u8;
    let operand = operand.index().expect("operand realized") as u8;
    let word = match (size, signed, fbits) {
        (FpSize::Single, false, 0) => inst::ucvtf_s_from_x(result, operand),
        (FpSize::Double, false, 0) => inst::ucvtf_d_from_x(result, operand),
        (FpSize::Single, true, 0) => inst::scvtf_s_from_x(result, operand),
        (FpSize::Double, true, 0) => inst::scvtf_d_from_x(result, operand),
        (FpSize::Single, false, _) => inst::ucvtf_s_from_x_fixed(result, operand, fbits),
        (FpSize::Double, false, _) => inst::ucvtf_d_from_x_fixed(result, operand, fbits),
        (FpSize::Single, true, _) => inst::scvtf_s_from_x_fixed(result, operand, fbits),
        (FpSize::Double, true, _) => inst::scvtf_d_from_x_fixed(result, operand, fbits),
    };
    emit_with_rounding_fpcr(code, ctx, rounding_mode, |code| {
        code.write_u32(word)?;
        Ok(())
    })
}

pub fn emit_fp_compare32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_compare(code, ctx, inst_ref, FpSize::Single)
}

pub fn emit_fp_compare64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_compare(code, ctx, inst_ref, FpSize::Double)
}

pub fn emit_fp_mul32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::fmul_s)
}

pub fn emit_fp_mul64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::fmul_d)
}

pub fn emit_fp_mul_x32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::fmulx_s)
}

pub fn emit_fp_mul_x64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::fmulx_d)
}

pub fn emit_fp_add32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::fadd_s)
}

pub fn emit_fp_add64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::fadd_d)
}

pub fn emit_fp_sub32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::fsub_s)
}

pub fn emit_fp_sub64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::fsub_d)
}

pub fn emit_fp_div32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::fdiv_s)
}

pub fn emit_fp_div64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::fdiv_d)
}

pub fn emit_fp_abs32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Single, inst::fabs_s)
}

pub fn emit_fp_abs64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Double, inst::fabs_d)
}

pub fn emit_fp_max_numeric32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::fmaxnm_s)
}

pub fn emit_fp_max32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::fmax_s)
}

pub fn emit_fp_max_numeric64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::fmaxnm_d)
}

pub fn emit_fp_max64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::fmax_d)
}

pub fn emit_fp_mul_add32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_four_op(code, ctx, inst_ref, FpSize::Single, |result, a, b, c| {
        inst::fmadd_s(result, b, c, a)
    })
}

pub fn emit_fp_mul_add64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_four_op(code, ctx, inst_ref, FpSize::Double, |result, a, b, c| {
        inst::fmadd_d(result, b, c, a)
    })
}

pub fn emit_fp_mul_sub32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_four_op(code, ctx, inst_ref, FpSize::Single, |result, a, b, c| {
        inst::fmsub_s(result, b, c, a)
    })
}

pub fn emit_fp_mul_sub64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_four_op(code, ctx, inst_ref, FpSize::Double, |result, a, b, c| {
        inst::fmsub_d(result, b, c, a)
    })
}

pub fn emit_fp_min_numeric32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::fminnm_s)
}

pub fn emit_fp_min32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::fmin_s)
}

pub fn emit_fp_min_numeric64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::fminnm_d)
}

pub fn emit_fp_min64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::fmin_d)
}

pub fn emit_fp_neg32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Single, inst::fneg_s)
}

pub fn emit_fp_neg64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Double, inst::fneg_d)
}

pub fn emit_fp_recip_estimate32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Single, inst::frecpe_s)
}

pub fn emit_fp_recip_estimate64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Double, inst::frecpe_d)
}

pub fn emit_fp_recip_exponent32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Single, inst::frecpx_s)
}

pub fn emit_fp_recip_exponent64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Double, inst::frecpx_d)
}

pub fn emit_fp_recip_step_fused32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::frecps_s)
}

pub fn emit_fp_recip_step_fused64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::frecps_d)
}

pub fn emit_fp_rsqrt_estimate32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Single, inst::frsqrte_s)
}

pub fn emit_fp_rsqrt_estimate64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Double, inst::frsqrte_d)
}

pub fn emit_fp_rsqrt_step_fused32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Single, inst::frsqrts_s)
}

pub fn emit_fp_rsqrt_step_fused64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_three_op(code, ctx, inst_ref, FpSize::Double, inst::frsqrts_d)
}

fn emit_fp_round_int(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: FpSize,
) -> Result<(), String> {
    let rounding_mode = RoundingMode::from_u8(ctx.block.get(inst_ref).arg(1).get_u8())?;
    let exact = ctx.block.get(inst_ref).arg(2).get_u1();

    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut result = match size {
        FpSize::Single => ctx.reg_alloc.write_s(inst_ref),
        FpSize::Double => ctx.reg_alloc.write_d(inst_ref),
    };
    let mut operand = match size {
        FpSize::Single => ctx.reg_alloc.read_s(args[0]),
        FpSize::Double => ctx.reg_alloc.read_d(args[0]),
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.fpsr.load(code)?;

    let result = result.index().expect("result realized") as u8;
    let operand = operand.index().expect("operand realized") as u8;

    let word = if exact {
        let fpcr_rounding_mode = fpcr_rounding_mode(ctx)?;
        if fpcr_rounding_mode != rounding_mode {
            return Err(format!(
                "ARM64 floating point: exact FPRoundInt rounding mode {:?} does not match FPCR {:?}",
                rounding_mode, fpcr_rounding_mode
            ));
        }
        match size {
            FpSize::Single => inst::frintx_s(result, operand),
            FpSize::Double => inst::frintx_d(result, operand),
        }
    } else {
        match (size, rounding_mode) {
            (FpSize::Single, RoundingMode::ToNearestTieEven) => inst::frintn_s(result, operand),
            (FpSize::Double, RoundingMode::ToNearestTieEven) => inst::frintn_d(result, operand),
            (FpSize::Single, RoundingMode::TowardsPlusInfinity) => inst::frintp_s(result, operand),
            (FpSize::Double, RoundingMode::TowardsPlusInfinity) => inst::frintp_d(result, operand),
            (FpSize::Single, RoundingMode::TowardsMinusInfinity) => inst::frintm_s(result, operand),
            (FpSize::Double, RoundingMode::TowardsMinusInfinity) => inst::frintm_d(result, operand),
            (FpSize::Single, RoundingMode::TowardsZero) => inst::frintz_s(result, operand),
            (FpSize::Double, RoundingMode::TowardsZero) => inst::frintz_d(result, operand),
            (FpSize::Single, RoundingMode::ToNearestTieAwayFromZero) => {
                inst::frinta_s(result, operand)
            }
            (FpSize::Double, RoundingMode::ToNearestTieAwayFromZero) => {
                inst::frinta_d(result, operand)
            }
            (_, RoundingMode::ToOdd) => {
                return Err("ARM64 floating point: ToOdd FPRoundInt is not ported".to_string());
            }
        }
    };

    code.write_u32(word)?;
    Ok(())
}

pub fn emit_fp_round_int32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_fp_round_int(code, ctx, inst_ref, FpSize::Single)
}

pub fn emit_fp_round_int64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_fp_round_int(code, ctx, inst_ref, FpSize::Double)
}

pub fn emit_fp_sqrt32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Single, inst::fsqrt_s)
}

pub fn emit_fp_sqrt64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_two_op(code, ctx, inst_ref, FpSize::Double, inst::fsqrt_d)
}

pub fn emit_fp_single_to_double(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_convert(
        code,
        ctx,
        inst_ref,
        FpSize::Single,
        FpSize::Double,
        inst::fcvt_d_from_s,
    )
}

pub fn emit_fp_half_to_single(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_convert_half(
        code,
        ctx,
        inst_ref,
        true,
        FpSize::Single,
        inst::fcvt_s_from_h,
    )
}

pub fn emit_fp_half_to_double(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_convert_half(
        code,
        ctx,
        inst_ref,
        true,
        FpSize::Double,
        inst::fcvt_d_from_h,
    )
}

pub fn emit_fp_single_to_half(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_convert_half(
        code,
        ctx,
        inst_ref,
        false,
        FpSize::Single,
        inst::fcvt_h_from_s,
    )
}

pub fn emit_fp_double_to_half(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_convert_half(
        code,
        ctx,
        inst_ref,
        false,
        FpSize::Double,
        inst::fcvt_h_from_d,
    )
}

pub fn emit_fp_double_to_single(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    let rounding_mode = RoundingMode::from_u8(ctx.block.get(inst_ref).arg(1).get_u8())?;

    if rounding_mode == RoundingMode::ToOdd {
        let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
        let mut result = ctx.reg_alloc.write_s(inst_ref);
        let mut operand = ctx.reg_alloc.read_d(args[0]);
        RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
        ctx.fpsr.load(code)?;
        code.write_u32(inst::fcvtxn_s_from_d(
            result.index().expect("result realized") as u8,
            operand.index().expect("operand realized") as u8,
        ))?;
        return Ok(());
    }

    emit_convert(
        code,
        ctx,
        inst_ref,
        FpSize::Double,
        FpSize::Single,
        inst::fcvt_s_from_d,
    )
}

pub fn emit_fp_single_to_fixed_u32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed32(code, ctx, inst_ref, FpSize::Single, false)
}

pub fn emit_fp_single_to_fixed_u16(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed16(code, ctx, inst_ref, FpSize::Single, false)
}

pub fn emit_fp_double_to_fixed_u16(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed16(code, ctx, inst_ref, FpSize::Double, false)
}

pub fn emit_fp_single_to_fixed_s16(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed16(code, ctx, inst_ref, FpSize::Single, true)
}

pub fn emit_fp_double_to_fixed_s16(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed16(code, ctx, inst_ref, FpSize::Double, true)
}

pub fn emit_fp_double_to_fixed_u32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed32(code, ctx, inst_ref, FpSize::Double, false)
}

pub fn emit_fp_single_to_fixed_s32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed32(code, ctx, inst_ref, FpSize::Single, true)
}

pub fn emit_fp_double_to_fixed_s32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed32(code, ctx, inst_ref, FpSize::Double, true)
}

pub fn emit_fp_single_to_fixed_u64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed64(code, ctx, inst_ref, FpSize::Single, false)
}

pub fn emit_fp_double_to_fixed_u64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed64(code, ctx, inst_ref, FpSize::Double, false)
}

pub fn emit_fp_single_to_fixed_s64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed64(code, ctx, inst_ref, FpSize::Single, true)
}

pub fn emit_fp_double_to_fixed_s64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_to_fixed64(code, ctx, inst_ref, FpSize::Double, true)
}

pub fn emit_fp_fixed_u16_to_single(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed16(code, ctx, inst_ref, FpSize::Single, false)
}

pub fn emit_fp_fixed_u16_to_double(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed16(code, ctx, inst_ref, FpSize::Double, false)
}

pub fn emit_fp_fixed_s16_to_single(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed16(code, ctx, inst_ref, FpSize::Single, true)
}

pub fn emit_fp_fixed_s16_to_double(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed16(code, ctx, inst_ref, FpSize::Double, true)
}

pub fn emit_fp_fixed_u32_to_single(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed32(code, ctx, inst_ref, FpSize::Single, false)
}

pub fn emit_fp_fixed_u32_to_double(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed32(code, ctx, inst_ref, FpSize::Double, false)
}

pub fn emit_fp_fixed_s32_to_single(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed32(code, ctx, inst_ref, FpSize::Single, true)
}

pub fn emit_fp_fixed_s32_to_double(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed32(code, ctx, inst_ref, FpSize::Double, true)
}

pub fn emit_fp_fixed_u64_to_single(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed64(code, ctx, inst_ref, FpSize::Single, false)
}

pub fn emit_fp_fixed_u64_to_double(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed64(code, ctx, inst_ref, FpSize::Double, false)
}

pub fn emit_fp_fixed_s64_to_single(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed64(code, ctx, inst_ref, FpSize::Single, true)
}

pub fn emit_fp_fixed_s64_to_double(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_from_fixed64(code, ctx, inst_ref, FpSize::Double, true)
}

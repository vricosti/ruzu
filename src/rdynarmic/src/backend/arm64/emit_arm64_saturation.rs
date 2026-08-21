//! ARM64 scalar saturation emission.
//!
//! Upstream owner: `backend/arm64/emit_arm64_saturation.cpp`.

use crate::backend::arm64::abi::{XSCRATCH0, XSCRATCH1};
use crate::backend::arm64::block_of_code::BlockOfCode;
use crate::backend::arm64::emit_context::EmitContext;
use crate::backend::arm64::inst;
use crate::backend::arm64::reg_alloc::RegAlloc;
use crate::ir::cond::Cond;
use crate::ir::opcode::Opcode;
use crate::ir::value::InstRef;

fn emit_mov_w_imm(code: &mut BlockOfCode, reg: u8, imm: u32) -> Result<(), String> {
    code.write_u32(inst::movz_w(reg, (imm & 0xffff) as u16, 0))?;
    let high = (imm >> 16) as u16;
    if high != 0 {
        code.write_u32(inst::movk_w(reg, high, 16))?;
    }
    Ok(())
}

pub fn emit_signed_saturated_add_with_flag32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    let overflow_inst = ctx
        .block
        .get_associated_pseudo_operation(inst_ref, Opcode::GetOverflowFromOp)
        .expect("SignedSaturatedAddWithFlag32 requires an overflow pseudo-operation");

    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut result = ctx.reg_alloc.write_w(inst_ref);
    let mut a = ctx.reg_alloc.read_w(args[0]);
    let mut b = ctx.reg_alloc.read_w(args[1]);
    let mut overflow = ctx.reg_alloc.write_w(overflow_inst);
    RegAlloc::realize_all(
        code,
        ctx.block,
        &mut [&mut result, &mut a, &mut b, &mut overflow],
    )?;
    ctx.reg_alloc.spill_flags(code)?;

    let result = result.index().expect("realized W result") as u8;
    let a = a.index().expect("realized W a") as u8;
    let b = b.index().expect("realized W b") as u8;
    let overflow = overflow.index().expect("realized W overflow") as u8;
    code.write_u32(inst::adds_w_reg(result, a, b))?;
    code.write_u32(inst::asr_w_imm(XSCRATCH0, result, 31))?;
    emit_mov_w_imm(code, XSCRATCH1, 0x8000_0000)?;
    code.write_u32(inst::eor_w_reg(XSCRATCH0, XSCRATCH0, XSCRATCH1))?;
    code.write_u32(inst::csel_w(result, result, XSCRATCH0, Cond::VC))?;
    code.write_u32(inst::cinc_w(overflow, 31, Cond::VS))?;
    Ok(())
}

pub fn emit_signed_saturated_sub_with_flag32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    let overflow_inst = ctx
        .block
        .get_associated_pseudo_operation(inst_ref, Opcode::GetOverflowFromOp)
        .expect("SignedSaturatedSubWithFlag32 requires an overflow pseudo-operation");

    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut result = ctx.reg_alloc.write_w(inst_ref);
    let mut a = ctx.reg_alloc.read_w(args[0]);
    let mut b = ctx.reg_alloc.read_w(args[1]);
    let mut overflow = ctx.reg_alloc.write_w(overflow_inst);
    RegAlloc::realize_all(
        code,
        ctx.block,
        &mut [&mut result, &mut a, &mut b, &mut overflow],
    )?;
    ctx.reg_alloc.spill_flags(code)?;

    let result = result.index().expect("realized W result") as u8;
    let a = a.index().expect("realized W a") as u8;
    let b = b.index().expect("realized W b") as u8;
    let overflow = overflow.index().expect("realized W overflow") as u8;
    code.write_u32(inst::subs_w_reg(result, a, b))?;
    code.write_u32(inst::asr_w_imm(XSCRATCH0, result, 31))?;
    emit_mov_w_imm(code, XSCRATCH1, 0x8000_0000)?;
    code.write_u32(inst::eor_w_reg(XSCRATCH0, XSCRATCH0, XSCRATCH1))?;
    code.write_u32(inst::csel_w(result, result, XSCRATCH0, Cond::VC))?;
    code.write_u32(inst::cinc_w(overflow, 31, Cond::VS))?;
    Ok(())
}

pub fn emit_signed_saturation(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    let overflow_inst = ctx
        .block
        .get_associated_pseudo_operation(inst_ref, Opcode::GetOverflowFromOp);
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let bit_size = args[1].get_immediate_u8() as usize;
    assert!((1..=32).contains(&bit_size));

    if bit_size == 32 {
        ctx.reg_alloc
            .define_as_existing(ctx.block, inst_ref, args[0]);
        if let Some(overflow_inst) = overflow_inst {
            let mut overflow = ctx.reg_alloc.write_w(overflow_inst);
            RegAlloc::realize_all(code, ctx.block, &mut [&mut overflow])?;
            code.write_u32(inst::mov_w(
                overflow.index().expect("realized W overflow") as u8,
                31,
            ))?;
        }
        return Ok(());
    }

    let positive_saturated_value = (1u32 << (bit_size - 1)) - 1;
    let negative_saturated_value = !0u32 << (bit_size - 1);

    let mut operand = ctx.reg_alloc.read_w(args[0]);
    let mut result = ctx.reg_alloc.write_w(inst_ref);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut operand, &mut result])?;
    ctx.reg_alloc.spill_flags(code)?;

    let operand = operand.index().expect("realized W operand") as u8;
    let result = result.index().expect("realized W result") as u8;
    emit_mov_w_imm(code, XSCRATCH0, negative_saturated_value)?;
    emit_mov_w_imm(code, XSCRATCH1, positive_saturated_value)?;
    code.write_u32(inst::cmp_w_reg(operand, XSCRATCH0))?;
    code.write_u32(inst::csel_w(result, operand, XSCRATCH0, Cond::GT))?;
    code.write_u32(inst::cmp_w_reg(operand, XSCRATCH1))?;
    code.write_u32(inst::csel_w(result, result, XSCRATCH1, Cond::LT))?;

    if let Some(overflow_inst) = overflow_inst {
        let mut overflow = ctx.reg_alloc.write_w(overflow_inst);
        RegAlloc::realize_all(code, ctx.block, &mut [&mut overflow])?;
        let overflow = overflow.index().expect("realized W overflow") as u8;
        code.write_u32(inst::cmp_w_reg(result, operand))?;
        code.write_u32(inst::cinc_w(overflow, 31, Cond::NE))?;
    }
    Ok(())
}

pub fn emit_unsigned_saturation(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    let overflow_inst = ctx
        .block
        .get_associated_pseudo_operation(inst_ref, Opcode::GetOverflowFromOp);
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut result = ctx.reg_alloc.write_w(inst_ref);
    let mut operand = ctx.reg_alloc.read_w(args[0]);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut operand])?;
    ctx.reg_alloc.spill_flags(code)?;

    let bit_size = args[1].get_immediate_u8() as usize;
    assert!(bit_size <= 31);
    let saturated_value = (1u32 << bit_size) - 1;

    let result = result.index().expect("realized W result") as u8;
    let operand = operand.index().expect("realized W operand") as u8;
    emit_mov_w_imm(code, XSCRATCH0, saturated_value)?;
    code.write_u32(inst::cmp_w_imm(operand, 0))?;
    code.write_u32(inst::csel_w(result, operand, 31, Cond::GT))?;
    code.write_u32(inst::cmp_w_reg(operand, XSCRATCH0))?;
    code.write_u32(inst::csel_w(result, result, XSCRATCH0, Cond::LT))?;

    if let Some(overflow_inst) = overflow_inst {
        let mut overflow = ctx.reg_alloc.write_w(overflow_inst);
        RegAlloc::realize_all(code, ctx.block, &mut [&mut overflow])?;
        code.write_u32(inst::cinc_w(
            overflow.index().expect("realized W overflow") as u8,
            31,
            Cond::HI,
        ))?;
    }
    Ok(())
}

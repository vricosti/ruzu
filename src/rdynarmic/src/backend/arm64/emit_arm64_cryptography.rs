//! ARM64 cryptography emission.
//!
//! Upstream owner: `backend/arm64/emit_arm64_cryptography.cpp`.

use crate::backend::arm64::block_of_code::BlockOfCode;
use crate::backend::arm64::emit_context::EmitContext;
use crate::backend::arm64::inst;
use crate::backend::arm64::reg_alloc::RegAlloc;
use crate::ir::value::InstRef;

fn emit_aes_single_round(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    emit: fn(u8, u8) -> u32,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut output = ctx.reg_alloc.write_q(inst_ref);
    let mut input = ctx.reg_alloc.read_q(args[0]);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut output, &mut input])?;

    let output = output.index().expect("AES output realized") as u8;
    let input = input.index().expect("AES input realized") as u8;
    code.write_u32(inst::movi_d_imm0(output))?;
    code.write_u32(emit(output, input))?;
    Ok(())
}

fn emit_aes_mix(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    emit: fn(u8, u8) -> u32,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut output = ctx.reg_alloc.write_q(inst_ref);
    let mut input = ctx.reg_alloc.read_q(args[0]);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut output, &mut input])?;

    code.write_u32(emit(
        output.index().expect("AES output realized") as u8,
        input.index().expect("AES input realized") as u8,
    ))?;
    Ok(())
}

pub fn emit_aes_decrypt_single_round(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_aes_single_round(code, ctx, inst_ref, inst::aesd_v16b)
}

pub fn emit_aes_encrypt_single_round(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_aes_single_round(code, ctx, inst_ref, inst::aese_v16b)
}

pub fn emit_aes_inverse_mix_columns(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_aes_mix(code, ctx, inst_ref, inst::aesimc_v16b)
}

pub fn emit_aes_mix_columns(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_aes_mix(code, ctx, inst_ref, inst::aesmc_v16b)
}

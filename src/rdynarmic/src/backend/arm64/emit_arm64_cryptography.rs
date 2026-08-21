//! ARM64 cryptography emission.
//!
//! Upstream owner: `backend/arm64/emit_arm64_cryptography.cpp`.

use crate::backend::arm64::block_of_code::BlockOfCode;
use crate::backend::arm64::emit_context::EmitContext;
use crate::backend::arm64::inst;
use crate::backend::arm64::reg_alloc::RegAlloc;
use crate::ir::value::InstRef;

fn emit_crc(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    data_is_64_bit: bool,
    emit: fn(u8, u8, u8) -> u32,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut output = ctx.reg_alloc.write_w(inst_ref);
    let mut input = ctx.reg_alloc.read_w(args[0]);
    let mut data = if data_is_64_bit {
        ctx.reg_alloc.read_x(args[1])
    } else {
        ctx.reg_alloc.read_w(args[1])
    };
    RegAlloc::realize_all(code, ctx.block, &mut [&mut output, &mut input, &mut data])?;

    code.write_u32(emit(
        output.index().expect("CRC output realized") as u8,
        input.index().expect("CRC input realized") as u8,
        data.index().expect("CRC data realized") as u8,
    ))?;
    Ok(())
}

pub fn emit_crc32_castagnoli_8(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_crc(code, ctx, inst_ref, false, inst::crc32cb_w)
}

pub fn emit_crc32_castagnoli_16(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_crc(code, ctx, inst_ref, false, inst::crc32ch_w)
}

pub fn emit_crc32_castagnoli_32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_crc(code, ctx, inst_ref, false, inst::crc32cw_w)
}

pub fn emit_crc32_castagnoli_64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_crc(code, ctx, inst_ref, true, inst::crc32cx_x)
}

pub fn emit_crc32_iso_8(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_crc(code, ctx, inst_ref, false, inst::crc32b_w)
}

pub fn emit_crc32_iso_16(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_crc(code, ctx, inst_ref, false, inst::crc32h_w)
}

pub fn emit_crc32_iso_32(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_crc(code, ctx, inst_ref, false, inst::crc32w_w)
}

pub fn emit_crc32_iso_64(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    emit_crc(code, ctx, inst_ref, true, inst::crc32x_x)
}

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

pub fn emit_sha256_hash(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let part1 = args[3].get_immediate_u1();

    if part1 {
        let mut x = ctx.reg_alloc.read_write_q(args[0], inst_ref);
        let mut y = ctx.reg_alloc.read_q(args[1]);
        let mut w = ctx.reg_alloc.read_q(args[2]);
        RegAlloc::realize_all(code, ctx.block, &mut [&mut x, &mut y, &mut w])?;
        code.write_u32(inst::sha256h_q(
            x.index().expect("SHA256 x realized") as u8,
            y.index().expect("SHA256 y realized") as u8,
            w.index().expect("SHA256 w realized") as u8,
        ))?;
    } else {
        let mut x = ctx.reg_alloc.read_q(args[0]);
        let mut y = ctx.reg_alloc.read_write_q(args[1], inst_ref);
        let mut w = ctx.reg_alloc.read_q(args[2]);
        RegAlloc::realize_all(code, ctx.block, &mut [&mut x, &mut y, &mut w])?;
        code.write_u32(inst::sha256h2_q(
            y.index().expect("SHA256 y realized") as u8,
            x.index().expect("SHA256 x realized") as u8,
            w.index().expect("SHA256 w realized") as u8,
        ))?;
    }
    Ok(())
}

pub fn emit_sha256_message_schedule_0(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut a = ctx.reg_alloc.read_write_q(args[0], inst_ref);
    let mut b = ctx.reg_alloc.read_q(args[1]);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut a, &mut b])?;
    code.write_u32(inst::sha256su0_v4s(
        a.index().expect("SHA256 a realized") as u8,
        b.index().expect("SHA256 b realized") as u8,
    ))?;
    Ok(())
}

pub fn emit_sha256_message_schedule_1(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut a = ctx.reg_alloc.read_write_q(args[0], inst_ref);
    let mut b = ctx.reg_alloc.read_q(args[1]);
    let mut c = ctx.reg_alloc.read_q(args[2]);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut a, &mut b, &mut c])?;
    code.write_u32(inst::sha256su1_v4s(
        a.index().expect("SHA256 a realized") as u8,
        b.index().expect("SHA256 b realized") as u8,
        c.index().expect("SHA256 c realized") as u8,
    ))?;
    Ok(())
}

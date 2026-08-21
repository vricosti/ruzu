//! ARM64 vector saturation emission.
//!
//! Upstream owner: `backend/arm64/emit_arm64_vector_saturation.cpp`.

use crate::backend::arm64::block_of_code::BlockOfCode;
use crate::backend::arm64::emit_context::EmitContext;
use crate::backend::arm64::inst;
use crate::backend::arm64::reg_alloc::RegAlloc;
use crate::ir::opcode::Opcode;
use crate::ir::value::InstRef;

type ThreeOpEmitter = fn(u8, u8, u8, u8, bool) -> u32;

fn emit(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
    size: u8,
    emit: ThreeOpEmitter,
) -> Result<(), String> {
    let args = ctx.reg_alloc.get_argument_info(ctx.block, inst_ref);
    let mut result = ctx.reg_alloc.write_q(inst_ref);
    let mut a = ctx.reg_alloc.read_q(args[0]);
    let mut b = ctx.reg_alloc.read_q(args[1]);
    RegAlloc::realize_all(code, ctx.block, &mut [&mut result, &mut a, &mut b])?;
    ctx.fpsr.load(code)?;
    code.write_u32(emit(
        result.index().expect("result realized") as u8,
        a.index().expect("a realized") as u8,
        b.index().expect("b realized") as u8,
        size,
        true,
    ))?;
    Ok(())
}

pub fn emit_vector_saturation_instruction(
    code: &mut BlockOfCode,
    ctx: &mut EmitContext<'_>,
    inst_ref: InstRef,
) -> Result<(), String> {
    match ctx.block.get(inst_ref).opcode {
        Opcode::VectorSignedSaturatedAdd8 => emit(code, ctx, inst_ref, 8, inst::sqadd_v),
        Opcode::VectorSignedSaturatedAdd16 => emit(code, ctx, inst_ref, 16, inst::sqadd_v),
        Opcode::VectorSignedSaturatedAdd32 => emit(code, ctx, inst_ref, 32, inst::sqadd_v),
        Opcode::VectorSignedSaturatedAdd64 => emit(code, ctx, inst_ref, 64, inst::sqadd_v),
        Opcode::VectorSignedSaturatedSub8 => emit(code, ctx, inst_ref, 8, inst::sqsub_v),
        Opcode::VectorSignedSaturatedSub16 => emit(code, ctx, inst_ref, 16, inst::sqsub_v),
        Opcode::VectorSignedSaturatedSub32 => emit(code, ctx, inst_ref, 32, inst::sqsub_v),
        Opcode::VectorSignedSaturatedSub64 => emit(code, ctx, inst_ref, 64, inst::sqsub_v),
        Opcode::VectorUnsignedSaturatedAdd8 => emit(code, ctx, inst_ref, 8, inst::uqadd_v),
        Opcode::VectorUnsignedSaturatedAdd16 => emit(code, ctx, inst_ref, 16, inst::uqadd_v),
        Opcode::VectorUnsignedSaturatedAdd32 => emit(code, ctx, inst_ref, 32, inst::uqadd_v),
        Opcode::VectorUnsignedSaturatedAdd64 => emit(code, ctx, inst_ref, 64, inst::uqadd_v),
        Opcode::VectorUnsignedSaturatedSub8 => emit(code, ctx, inst_ref, 8, inst::uqsub_v),
        Opcode::VectorUnsignedSaturatedSub16 => emit(code, ctx, inst_ref, 16, inst::uqsub_v),
        Opcode::VectorUnsignedSaturatedSub32 => emit(code, ctx, inst_ref, 32, inst::uqsub_v),
        Opcode::VectorUnsignedSaturatedSub64 => emit(code, ctx, inst_ref, 64, inst::uqsub_v),
        opcode => Err(format!(
            "unimplemented ARM64 vector saturation opcode: {opcode:?}"
        )),
    }
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Main GLSL emission entry point.
//!
//! Maps to upstream `backend/glsl/emit_glsl.cpp`.

use crate::shader_recompiler::ir;
use crate::shader_recompiler::ir::opcodes::Opcode;

use super::glsl_emit_context::EmitContext;

/// Walk an IR program and emit GLSL code.
pub fn emit_program(ctx: &mut EmitContext, program: &ir::Program) {
    ctx.add_line("void main() {");
    for block in &program.blocks {
        for inst in &block.instructions {
            emit_inst(ctx, inst);
        }
    }
    ctx.add_line("}");
}

/// Emit a single IR instruction as GLSL.
fn emit_inst(ctx: &mut EmitContext, inst: &ir::instruction::Inst) {
    match inst.opcode {
        // Arithmetic - float
        Opcode::FPAdd32 => ctx.add_line("f_0=f_1+f_2;"),
        Opcode::FPMul32 => ctx.add_line("f_0=f_1*f_2;"),
        Opcode::FPFma32 => ctx.add_line("f_0=fma(f_1,f_2,f_3);"),
        Opcode::FPMax32 => ctx.add_line("f_0=max(f_1,f_2);"),
        Opcode::FPMin32 => ctx.add_line("f_0=min(f_1,f_2);"),
        Opcode::FPNeg32 => ctx.add_line("f_0=-f_1;"),
        Opcode::FPAbs32 => ctx.add_line("f_0=abs(f_1);"),
        Opcode::FPSaturate32 => ctx.add_line("f_0=clamp(f_1,0.0f,1.0f);"),
        Opcode::FPRoundEven32 => ctx.add_line("f_0=roundEven(f_1);"),
        Opcode::FPFloor32 => ctx.add_line("f_0=floor(f_1);"),
        Opcode::FPCeil32 => ctx.add_line("f_0=ceil(f_1);"),
        Opcode::FPTrunc32 => ctx.add_line("f_0=trunc(f_1);"),
        Opcode::FPSin => ctx.add_line("f_0=sin(f_1);"),
        Opcode::FPCos => ctx.add_line("f_0=cos(f_1);"),
        Opcode::FPExp2 => ctx.add_line("f_0=exp2(f_1);"),
        Opcode::FPLog2 => ctx.add_line("f_0=log2(f_1);"),
        Opcode::FPRecip32 => ctx.add_line("f_0=1.0f/f_1;"),
        Opcode::FPRecipSqrt32 => ctx.add_line("f_0=inversesqrt(f_1);"),
        Opcode::FPSqrt32 => ctx.add_line("f_0=sqrt(f_1);"),

        // Arithmetic - integer
        Opcode::IAdd32 => ctx.add_line("u_0=u_1+u_2;"),
        Opcode::ISub32 => ctx.add_line("u_0=u_1-u_2;"),
        Opcode::IMul32 => ctx.add_line("u_0=u_1*u_2;"),
        Opcode::INeg32 => ctx.add_line("u_0=uint(-int(u_1));"),
        Opcode::IAbs32 => ctx.add_line("u_0=uint(abs(int(u_1)));"),
        Opcode::ShiftLeftLogical32 => ctx.add_line("u_0=u_1<<u_2;"),
        Opcode::ShiftRightLogical32 => ctx.add_line("u_0=u_1>>u_2;"),
        Opcode::ShiftRightArithmetic32 => ctx.add_line("u_0=uint(int(u_1)>>int(u_2));"),
        Opcode::BitwiseAnd32 => ctx.add_line("u_0=u_1&u_2;"),
        Opcode::BitwiseOr32 => ctx.add_line("u_0=u_1|u_2;"),
        Opcode::BitwiseXor32 => ctx.add_line("u_0=u_1^u_2;"),
        Opcode::BitwiseNot32 => ctx.add_line("u_0=~u_1;"),
        Opcode::BitReverse32 => ctx.add_line("u_0=bitfieldReverse(u_1);"),
        Opcode::BitCount32 => ctx.add_line("u_0=uint(bitCount(u_1));"),
        Opcode::SMin32 => ctx.add_line("u_0=uint(min(int(u_1),int(u_2)));"),
        Opcode::UMin32 => ctx.add_line("u_0=min(u_1,u_2);"),
        Opcode::SMax32 => ctx.add_line("u_0=uint(max(int(u_1),int(u_2)));"),
        Opcode::UMax32 => ctx.add_line("u_0=max(u_1,u_2);"),
        Opcode::BitFieldInsert => ctx.add_line("u_0=bitfieldInsert(u_1,u_2,int(u_3),int(u_4));"),
        Opcode::BitFieldSExtract => ctx.add_line("u_0=uint(bitfieldExtract(int(u_1),int(u_2),int(u_3)));"),
        Opcode::BitFieldUExtract => ctx.add_line("u_0=bitfieldExtract(u_1,int(u_2),int(u_3));"),

        // Comparisons - float
        Opcode::FPOrdLessThan32 => ctx.add_line("b_0=f_1<f_2;"),
        Opcode::FPOrdEqual32 => ctx.add_line("b_0=f_1==f_2;"),
        Opcode::FPOrdLessThanEqual32 => ctx.add_line("b_0=f_1<=f_2;"),
        Opcode::FPOrdGreaterThan32 => ctx.add_line("b_0=f_1>f_2;"),
        Opcode::FPOrdGreaterThanEqual32 => ctx.add_line("b_0=f_1>=f_2;"),
        Opcode::FPOrdNotEqual32 => ctx.add_line("b_0=f_1!=f_2;"),
        Opcode::FPUnordLessThan32 => ctx.add_line("b_0=!(f_1>=f_2);"),
        Opcode::FPUnordEqual32 => ctx.add_line("b_0=!(f_1!=f_2);"),
        Opcode::FPUnordLessThanEqual32 => ctx.add_line("b_0=!(f_1>f_2);"),
        Opcode::FPUnordGreaterThan32 => ctx.add_line("b_0=!(f_1<=f_2);"),
        Opcode::FPUnordGreaterThanEqual32 => ctx.add_line("b_0=!(f_1<f_2);"),
        Opcode::FPUnordNotEqual32 => ctx.add_line("b_0=!(f_1==f_2);"),
        Opcode::FPIsNan32 => ctx.add_line("b_0=isnan(f_1);"),

        // Comparisons - integer
        Opcode::SLessThan => ctx.add_line("b_0=int(u_1)<int(u_2);"),
        Opcode::ULessThan => ctx.add_line("b_0=u_1<u_2;"),
        Opcode::IEqual => ctx.add_line("b_0=u_1==u_2;"),
        Opcode::SLessThanEqual => ctx.add_line("b_0=int(u_1)<=int(u_2);"),
        Opcode::ULessThanEqual => ctx.add_line("b_0=u_1<=u_2;"),
        Opcode::SGreaterThan => ctx.add_line("b_0=int(u_1)>int(u_2);"),
        Opcode::UGreaterThan => ctx.add_line("b_0=u_1>u_2;"),
        Opcode::INotEqual => ctx.add_line("b_0=u_1!=u_2;"),
        Opcode::SGreaterThanEqual => ctx.add_line("b_0=int(u_1)>=int(u_2);"),
        Opcode::UGreaterThanEqual => ctx.add_line("b_0=u_1>=u_2;"),

        // Logical
        Opcode::LogicalOr => ctx.add_line("b_0=b_1||b_2;"),
        Opcode::LogicalAnd => ctx.add_line("b_0=b_1&&b_2;"),
        Opcode::LogicalXor => ctx.add_line("b_0=b_1^^b_2;"),
        Opcode::LogicalNot => ctx.add_line("b_0=!b_1;"),

        // Select
        Opcode::SelectU32 => ctx.add_line("u_0=b_1?u_2:u_3;"),
        Opcode::SelectF32 => ctx.add_line("f_0=b_1?f_2:f_3;"),

        // Undefined
        Opcode::UndefU1 => ctx.add_line("b_0=false;"),
        Opcode::UndefU8 | Opcode::UndefU16 | Opcode::UndefU32 => ctx.add_line("u_0=0u;"),

        // Barriers
        Opcode::Barrier => ctx.add_line("barrier();"),
        Opcode::WorkgroupMemoryBarrier => ctx.add_line("groupMemoryBarrier();"),
        Opcode::DeviceMemoryBarrier => ctx.add_line("memoryBarrier();"),

        // Control flow
        Opcode::DemoteToHelperInvocation => ctx.add_line("discard;"),

        // Bitwise conversion
        Opcode::BitCastU32F32 => ctx.add_line("u_0=floatBitsToUint(f_1);"),
        Opcode::BitCastF32U32 => ctx.add_line("f_0=uintBitsToFloat(u_1);"),
        Opcode::Identity => {}
        Opcode::BitCastU16F16 | Opcode::BitCastF16U16 => {}
        Opcode::BitCastU64F64 => ctx.add_line("u64_0=doubleBitsToUint64(d_1);"),
        Opcode::BitCastF64U64 => ctx.add_line("d_0=uint64BitsToDouble(u64_1);"),

        // Pack/unpack
        Opcode::PackHalf2x16 => ctx.add_line("u_0=packHalf2x16(f2_1);"),
        Opcode::UnpackHalf2x16 => ctx.add_line("f2_0=unpackHalf2x16(u_1);"),

        // Conversion
        Opcode::ConvertF32S32 => ctx.add_line("f_0=float(int(u_1));"),
        Opcode::ConvertF32U32 => ctx.add_line("f_0=float(u_1);"),
        Opcode::ConvertS32F32 => ctx.add_line("u_0=uint(int(f_1));"),
        Opcode::ConvertU32F32 => ctx.add_line("u_0=uint(f_1);"),
        Opcode::ConvertF32F64 => ctx.add_line("f_0=float(d_1);"),
        Opcode::ConvertF64F32 => ctx.add_line("d_0=double(f_1);"),

        // Special
        Opcode::Prologue | Opcode::Epilogue | Opcode::Void | Opcode::Phi | Opcode::PhiMove => {}
        Opcode::EmitVertex => ctx.add_line("EmitVertex();"),
        Opcode::EndPrimitive => ctx.add_line("EndPrimitive();"),

        // Everything else
        _ => {
            ctx.add_fmt(format!("// {} (not yet emitted)", inst.opcode.name()));
        }
    }
}

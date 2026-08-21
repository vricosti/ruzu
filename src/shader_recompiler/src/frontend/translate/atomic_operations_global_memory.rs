// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `frontend/maxwell/translate/impl/atomic_operations_global_memory.cpp`.

use super::{bit, field, sfield, TranslatorVisitor};
use crate::ir::types::{FmzMode, FpControl, FpRounding};
use crate::ir::value::{Reg, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AtomOp {
    Add,
    Min,
    Max,
    Inc,
    Dec,
    And,
    Or,
    Xor,
    Exch,
    SafeAdd,
}

impl AtomOp {
    fn from_bits(bits: u32) -> Self {
        match bits {
            0 => Self::Add,
            1 => Self::Min,
            2 => Self::Max,
            3 => Self::Inc,
            4 => Self::Dec,
            5 => Self::And,
            6 => Self::Or,
            7 => Self::Xor,
            8 => Self::Exch,
            9 => Self::SafeAdd,
            _ => panic!("Invalid AtomOp {}", bits),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AtomSize {
    U32,
    S32,
    U64,
    F32,
    F16x2,
    S64,
}

impl AtomSize {
    fn from_bits(bits: u32) -> Self {
        match bits {
            0 => Self::U32,
            1 => Self::S32,
            2 => Self::U64,
            3 => Self::F32,
            4 => Self::F16x2,
            5 => Self::S64,
            _ => panic!("Invalid AtomSize {}", bits),
        }
    }
}

fn u32_to_u64(tv: &mut TranslatorVisitor<'_>, value: Value) -> Value {
    let words = tv.ir.composite_construct_u32x2(value, Value::ImmU32(0));
    tv.ir.pack_uint_2x32(words)
}

fn apply_integer_atom_op(
    tv: &mut TranslatorVisitor<'_>,
    offset: Value,
    op_b: Value,
    op: AtomOp,
    is_signed: bool,
    is_64_bit: bool,
) -> Value {
    match op {
        AtomOp::Add => tv.ir.global_atomic_iadd(offset, op_b, is_64_bit),
        AtomOp::Min => tv.ir.global_atomic_imin(offset, op_b, is_signed, is_64_bit),
        AtomOp::Max => tv.ir.global_atomic_imax(offset, op_b, is_signed, is_64_bit),
        AtomOp::Inc => tv.ir.global_atomic_inc_32(offset, op_b),
        AtomOp::Dec => tv.ir.global_atomic_dec_32(offset, op_b),
        AtomOp::And => tv.ir.global_atomic_and(offset, op_b, is_64_bit),
        AtomOp::Or => tv.ir.global_atomic_or(offset, op_b, is_64_bit),
        AtomOp::Xor => tv.ir.global_atomic_xor(offset, op_b, is_64_bit),
        AtomOp::Exch => tv.ir.global_atomic_exchange(offset, op_b, is_64_bit),
        AtomOp::SafeAdd => panic!("Integer Atom Operation SafeAdd"),
    }
}

fn apply_fp_atom_op(
    tv: &mut TranslatorVisitor<'_>,
    offset: Value,
    op_b: Value,
    op: AtomOp,
    size: AtomSize,
) -> Value {
    const F16_CONTROL: FpControl = FpControl {
        no_contraction: false,
        rounding: FpRounding::RN,
        fmz_mode: FmzMode::DontCare,
    };
    const F32_CONTROL: FpControl = FpControl {
        no_contraction: false,
        rounding: FpRounding::RN,
        fmz_mode: FmzMode::FTZ,
    };
    match op {
        AtomOp::Add if size == AtomSize::F32 => {
            tv.ir.global_atomic_f32_add(offset, op_b, F32_CONTROL)
        }
        AtomOp::Add => tv.ir.global_atomic_f16x2_add(offset, op_b, F16_CONTROL),
        AtomOp::Min => tv.ir.global_atomic_f16x2_min(offset, op_b, F16_CONTROL),
        AtomOp::Max => tv.ir.global_atomic_f16x2_max(offset, op_b, F16_CONTROL),
        _ => panic!("FP Atom Operation {:?}", op),
    }
}

fn atom_offset(tv: &mut TranslatorVisitor<'_>, insn: u64) -> Value {
    let addr_reg = field(insn, 8, 8);
    let address = if bit(insn, 48) {
        tv.l(addr_reg)
    } else {
        let addr = tv.x(addr_reg);
        u32_to_u64(tv, addr)
    };
    let addr_offset = if addr_reg == Reg::RZ.0 as u32 {
        u64::from(field(insn, 28, 20))
    } else {
        sfield(insn, 28, 20) as i64 as u64
    };
    tv.ir.iadd_64(address, Value::ImmU64(addr_offset))
}

fn atom_op_not_applicable(size: AtomSize, op: AtomOp) -> bool {
    match size {
        AtomSize::S32 | AtomSize::U64 => matches!(op, AtomOp::Inc | AtomOp::Dec),
        AtomSize::S64 => !matches!(op, AtomOp::Min | AtomOp::Max),
        AtomSize::F32 => op != AtomOp::Add,
        AtomSize::F16x2 => !matches!(op, AtomOp::Add | AtomOp::Min | AtomOp::Max),
        AtomSize::U32 => false,
    }
}

fn load_global(tv: &mut TranslatorVisitor<'_>, offset: Value, size: AtomSize) -> Value {
    match size {
        AtomSize::U32 | AtomSize::S32 | AtomSize::F32 | AtomSize::F16x2 => {
            tv.ir.load_global_32(offset)
        }
        AtomSize::U64 | AtomSize::S64 => {
            let words = tv.ir.load_global_64(offset);
            tv.ir.pack_uint_2x32(words)
        }
    }
}

fn store_result(tv: &mut TranslatorVisitor<'_>, dest_reg: u32, result: Value, size: AtomSize) {
    match size {
        AtomSize::U32 | AtomSize::S32 | AtomSize::F16x2 => tv.set_x(dest_reg, result),
        AtomSize::U64 | AtomSize::S64 => tv.set_l(dest_reg, result),
        AtomSize::F32 => tv.set_f(dest_reg, result),
    }
}

fn apply_atom_op(
    tv: &mut TranslatorVisitor<'_>,
    operand_reg: u32,
    offset: Value,
    size: AtomSize,
    op: AtomOp,
) -> Value {
    match size {
        AtomSize::U32 | AtomSize::S32 => {
            let op_b = tv.x(operand_reg);
            apply_integer_atom_op(tv, offset, op_b, op, size == AtomSize::S32, false)
        }
        AtomSize::U64 | AtomSize::S64 => {
            let op_b = tv.l(operand_reg);
            apply_integer_atom_op(tv, offset, op_b, op, size == AtomSize::S64, true)
        }
        AtomSize::F32 => {
            let op_b = tv.f(operand_reg);
            apply_fp_atom_op(tv, offset, op_b, op, size)
        }
        AtomSize::F16x2 => {
            let packed = tv.x(operand_reg);
            let op_b = tv.ir.unpack_float_2x16(packed);
            apply_fp_atom_op(tv, offset, op_b, op, size)
        }
    }
}

fn global_atomic(
    tv: &mut TranslatorVisitor<'_>,
    dest_reg: u32,
    operand_reg: u32,
    offset: Value,
    size: AtomSize,
    op: AtomOp,
    write_dest: bool,
) {
    let result = if atom_op_not_applicable(size, op) {
        load_global(tv, offset, size)
    } else {
        apply_atom_op(tv, operand_reg, offset, size, op)
    };
    if write_dest {
        store_result(tv, dest_reg, result, size);
    }
}

/// Port of upstream `TranslatorVisitor::ATOM`.
pub fn atom(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    let dest_reg = field(insn, 0, 8);
    let operand_reg = field(insn, 20, 8);
    let size = AtomSize::from_bits(field(insn, 49, 3));
    let op = AtomOp::from_bits(field(insn, 52, 4));
    let offset = atom_offset(tv, insn);
    global_atomic(tv, dest_reg, operand_reg, offset, size, op, true);
}

/// Port of upstream `TranslatorVisitor::RED`.
pub fn red(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    let operand_reg = field(insn, 0, 8);
    let size = AtomSize::from_bits(field(insn, 20, 3));
    let op = AtomOp::from_bits(field(insn, 23, 3));
    let offset = atom_offset(tv, insn);
    global_atomic(tv, Reg::RZ.0 as u32, operand_reg, offset, size, op, true);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    fn translate_atom(size: AtomSize, op: AtomOp) -> Vec<Opcode> {
        let mut program = Program::new(ShaderStage::Compute);
        program.blocks.push(Block::new());
        let mut visitor = TranslatorVisitor::new(&mut program, 0);
        let insn = 2u64 << 20 | (size as u64) << 49 | (op as u64) << 52;

        atom(&mut visitor, insn);

        program
            .block(0)
            .iter()
            .map(|instruction| instruction.opcode)
            .collect()
    }

    #[test]
    fn atom_integer_sizes_select_upstream_opcodes() {
        assert!(translate_atom(AtomSize::U32, AtomOp::Add).contains(&Opcode::GlobalAtomicIAdd32));
        assert!(translate_atom(AtomSize::S64, AtomOp::Min).contains(&Opcode::GlobalAtomicSMin64));
        assert!(translate_atom(AtomSize::U32, AtomOp::Inc).contains(&Opcode::GlobalAtomicInc32));
    }

    #[test]
    fn atom_float_sizes_select_upstream_opcodes() {
        assert!(translate_atom(AtomSize::F32, AtomOp::Add).contains(&Opcode::GlobalAtomicAddF32));
        assert!(
            translate_atom(AtomSize::F16x2, AtomOp::Max).contains(&Opcode::GlobalAtomicMaxF16x2)
        );
    }

    #[test]
    fn unsupported_operation_degrades_to_load_like_upstream() {
        let opcodes = translate_atom(AtomSize::F32, AtomOp::Min);
        assert!(opcodes.contains(&Opcode::LoadGlobal32));
        assert!(!opcodes.contains(&Opcode::GlobalAtomicMinF16x2));
    }
}

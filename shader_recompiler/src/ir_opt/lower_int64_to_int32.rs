// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/lower_int64_to_int32.cpp`.

use crate::ir::basic_block::Block;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::{Program, SyntaxNode};
use crate::ir::value::{InstRef, Value};

fn insert_before(
    block: &mut Block,
    block_index: u32,
    before: u32,
    opcode: Opcode,
    args: Vec<Value>,
) -> Value {
    let inst = block.insert_inst_before(before, Inst::new(opcode, args));
    Value::Inst(InstRef {
        block: block_index,
        inst,
    })
}

fn insert_pseudo_before(
    block: &mut Block,
    block_index: u32,
    before: u32,
    opcode: Opcode,
    parent: Value,
) -> Value {
    let pseudo = insert_before(block, block_index, before, opcode, vec![parent]);
    if let (Value::Inst(parent), Value::Inst(pseudo_ref)) = (parent, pseudo) {
        block
            .inst_mut(parent.inst)
            .set_associated_pseudo(opcode, pseudo_ref);
    }
    pseudo
}

fn unpack(block: &mut Block, block_index: u32, before: u32, packed: Value) -> (Value, Value) {
    if let Value::ImmU64(value) = packed {
        (
            Value::ImmU32(value as u32),
            Value::ImmU32((value >> 32) as u32),
        )
    } else {
        (
            insert_before(
                block,
                block_index,
                before,
                Opcode::CompositeExtractU32x2,
                vec![packed, Value::ImmU32(0)],
            ),
            insert_before(
                block,
                block_index,
                before,
                Opcode::CompositeExtractU32x2,
                vec![packed, Value::ImmU32(1)],
            ),
        )
    }
}

fn replace_uses_with(program: &mut Program, old: InstRef, replacement: Value) {
    let old_value = Value::Inst(old);
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            for arg in &mut inst.args {
                if *arg == old_value {
                    *arg = replacement;
                }
            }
            for (_, value) in &mut inst.phi_args {
                if *value == old_value {
                    *value = replacement;
                }
            }
        }
    }
    for node in &mut program.syntax_list {
        match node {
            SyntaxNode::If { cond, .. }
            | SyntaxNode::Repeat { cond, .. }
            | SyntaxNode::Break { cond, .. } => {
                if *cond == old_value {
                    *cond = replacement;
                }
            }
            _ => {}
        }
    }
}

fn assert_no_pseudo(inst: &Inst, operation: &str) {
    if inst.associated.is_some() {
        panic!("{operation} emulation with pseudo instructions");
    }
}

fn composite_pair(block: &mut Block, block_index: u32, before: u32, lo: Value, hi: Value) -> Value {
    insert_before(
        block,
        block_index,
        before,
        Opcode::CompositeConstructU32x2,
        vec![lo, hi],
    )
}

fn lower_iadd64(program: &mut Program, inst_ref: InstRef, inst: &Inst) {
    assert_no_pseudo(inst, "IAdd64");
    let block = program.block_mut(inst_ref.block);
    let (a_lo, a_hi) = unpack(block, inst_ref.block, inst_ref.inst, inst.args[0]);
    let (b_lo, b_hi) = unpack(block, inst_ref.block, inst_ref.inst, inst.args[1]);
    let ret_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::IAdd32,
        vec![a_lo, b_lo],
    );
    let carry_flag = insert_pseudo_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::GetCarryFromOp,
        ret_lo,
    );
    let carry = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![carry_flag, Value::ImmU32(1), Value::ImmU32(0)],
    );
    let high_sum = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::IAdd32,
        vec![a_hi, b_hi],
    );
    let ret_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::IAdd32,
        vec![high_sum, carry],
    );
    let replacement = composite_pair(block, inst_ref.block, inst_ref.inst, ret_lo, ret_hi);
    replace_uses_with(program, inst_ref, replacement);
}

fn lower_isub64(program: &mut Program, inst_ref: InstRef, inst: &Inst) {
    assert_no_pseudo(inst, "ISub64");
    let block = program.block_mut(inst_ref.block);
    let (a_lo, a_hi) = unpack(block, inst_ref.block, inst_ref.inst, inst.args[0]);
    let (b_lo, b_hi) = unpack(block, inst_ref.block, inst_ref.inst, inst.args[1]);
    let ret_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ISub32,
        vec![a_lo, b_lo],
    );
    let underflow = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::UGreaterThan,
        vec![ret_lo, a_lo],
    );
    let underflow_bit = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![underflow, Value::ImmU32(1), Value::ImmU32(0)],
    );
    let high_difference = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ISub32,
        vec![a_hi, b_hi],
    );
    let ret_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ISub32,
        vec![high_difference, underflow_bit],
    );
    let replacement = composite_pair(block, inst_ref.block, inst_ref.inst, ret_lo, ret_hi);
    replace_uses_with(program, inst_ref, replacement);
}

fn lower_ineg64(program: &mut Program, inst_ref: InstRef, inst: &Inst) {
    assert_no_pseudo(inst, "INeg64");
    let block = program.block_mut(inst_ref.block);
    let (lo, hi) = unpack(block, inst_ref.block, inst_ref.inst, inst.args[0]);
    let lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::BitwiseNot32,
        vec![lo],
    );
    let hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::BitwiseNot32,
        vec![hi],
    );
    let lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::IAdd32,
        vec![lo, Value::ImmU32(1)],
    );
    let carry_flag = insert_pseudo_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::GetCarryFromOp,
        lo,
    );
    let carry = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![carry_flag, Value::ImmU32(1), Value::ImmU32(0)],
    );
    let hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::IAdd32,
        vec![hi, carry],
    );
    let replacement = composite_pair(block, inst_ref.block, inst_ref.inst, lo, hi);
    replace_uses_with(program, inst_ref, replacement);
}

fn lower_shift_left_logical64(program: &mut Program, inst_ref: InstRef, inst: &Inst) {
    assert_no_pseudo(inst, "ShiftLeftLogical64");
    let block = program.block_mut(inst_ref.block);
    let (lo, hi) = unpack(block, inst_ref.block, inst_ref.inst, inst.args[0]);
    let shift = inst.args[1];
    let shifted_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftLeftLogical32,
        vec![lo, shift],
    );
    let shifted_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftLeftLogical32,
        vec![hi, shift],
    );
    let inv_shift = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ISub32,
        vec![shift, Value::ImmU32(32)],
    );
    let is_long = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SGreaterThanEqual,
        vec![inv_shift, Value::ImmU32(0)],
    );
    let is_zero = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::IEqual,
        vec![shift, Value::ImmU32(0)],
    );
    let long_ret_lo = Value::ImmU32(0);
    let long_ret_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftLeftLogical32,
        vec![lo, inv_shift],
    );
    let shift_complement = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ISub32,
        vec![Value::ImmU32(32), shift],
    );
    let lo_extract = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::BitFieldUExtract,
        vec![lo, shift_complement, shift],
    );
    let short_ret_lo = shifted_lo;
    let short_ret_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::BitwiseOr32,
        vec![shifted_hi, lo_extract],
    );
    let non_zero_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_long, long_ret_lo, short_ret_lo],
    );
    let non_zero_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_long, long_ret_hi, short_ret_hi],
    );
    let ret_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_zero, lo, non_zero_lo],
    );
    let ret_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_zero, hi, non_zero_hi],
    );
    let replacement = composite_pair(block, inst_ref.block, inst_ref.inst, ret_lo, ret_hi);
    replace_uses_with(program, inst_ref, replacement);
}

fn lower_shift_right_logical64(program: &mut Program, inst_ref: InstRef, inst: &Inst) {
    assert_no_pseudo(inst, "ShiftRightLogical64");
    let block = program.block_mut(inst_ref.block);
    let (lo, hi) = unpack(block, inst_ref.block, inst_ref.inst, inst.args[0]);
    let shift = inst.args[1];
    let shifted_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftRightLogical32,
        vec![lo, shift],
    );
    let shifted_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftRightLogical32,
        vec![hi, shift],
    );
    let inv_shift = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ISub32,
        vec![shift, Value::ImmU32(32)],
    );
    let is_long = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SGreaterThanEqual,
        vec![inv_shift, Value::ImmU32(0)],
    );
    let is_zero = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::IEqual,
        vec![shift, Value::ImmU32(0)],
    );
    let long_ret_hi = Value::ImmU32(0);
    let long_ret_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftRightLogical32,
        vec![hi, inv_shift],
    );
    let shift_complement = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ISub32,
        vec![Value::ImmU32(32), shift],
    );
    let short_hi_extract = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::BitFieldUExtract,
        vec![hi, Value::ImmU32(0), shift],
    );
    let short_ret_hi = shifted_hi;
    let short_ret_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::BitFieldInsert,
        vec![shifted_lo, short_hi_extract, shift_complement, shift],
    );
    let non_zero_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_long, long_ret_lo, short_ret_lo],
    );
    let non_zero_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_long, long_ret_hi, short_ret_hi],
    );
    let ret_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_zero, lo, non_zero_lo],
    );
    let ret_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_zero, hi, non_zero_hi],
    );
    let replacement = composite_pair(block, inst_ref.block, inst_ref.inst, ret_lo, ret_hi);
    replace_uses_with(program, inst_ref, replacement);
}

fn lower_shift_right_arithmetic64(program: &mut Program, inst_ref: InstRef, inst: &Inst) {
    assert_no_pseudo(inst, "ShiftRightArithmetic64");
    let block = program.block_mut(inst_ref.block);
    let (lo, hi) = unpack(block, inst_ref.block, inst_ref.inst, inst.args[0]);
    let shift = inst.args[1];
    let shifted_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftRightLogical32,
        vec![lo, shift],
    );
    let shifted_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftRightArithmetic32,
        vec![hi, shift],
    );
    let sign_extension = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftRightArithmetic32,
        vec![hi, Value::ImmU32(31)],
    );
    let inv_shift = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ISub32,
        vec![shift, Value::ImmU32(32)],
    );
    let is_long = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SGreaterThanEqual,
        vec![inv_shift, Value::ImmU32(0)],
    );
    let is_zero = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::IEqual,
        vec![shift, Value::ImmU32(0)],
    );
    let long_ret_hi = sign_extension;
    let long_ret_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ShiftRightArithmetic32,
        vec![hi, inv_shift],
    );
    let shift_complement = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::ISub32,
        vec![Value::ImmU32(32), shift],
    );
    let short_hi_extract = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::BitFieldUExtract,
        vec![hi, Value::ImmU32(0), shift],
    );
    let short_ret_hi = shifted_hi;
    let short_ret_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::BitFieldInsert,
        vec![shifted_lo, short_hi_extract, shift_complement, shift],
    );
    let non_zero_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_long, long_ret_lo, short_ret_lo],
    );
    let non_zero_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_long, long_ret_hi, short_ret_hi],
    );
    let ret_lo = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_zero, lo, non_zero_lo],
    );
    let ret_hi = insert_before(
        block,
        inst_ref.block,
        inst_ref.inst,
        Opcode::SelectU32,
        vec![is_zero, hi, non_zero_hi],
    );
    let replacement = composite_pair(block, inst_ref.block, inst_ref.inst, ret_lo, ret_hi);
    replace_uses_with(program, inst_ref, replacement);
}

fn replacement_opcode(opcode: Opcode) -> Option<Opcode> {
    Some(match opcode {
        Opcode::PackUint2x32 | Opcode::UnpackUint2x32 => Opcode::Identity,
        Opcode::SharedAtomicExchange64 => Opcode::SharedAtomicExchange32x2,
        Opcode::GlobalAtomicIAdd64 => Opcode::GlobalAtomicIAdd32x2,
        Opcode::GlobalAtomicSMin64 => Opcode::GlobalAtomicSMin32x2,
        Opcode::GlobalAtomicUMin64 => Opcode::GlobalAtomicUMin32x2,
        Opcode::GlobalAtomicSMax64 => Opcode::GlobalAtomicSMax32x2,
        Opcode::GlobalAtomicUMax64 => Opcode::GlobalAtomicUMax32x2,
        Opcode::GlobalAtomicAnd64 => Opcode::GlobalAtomicAnd32x2,
        Opcode::GlobalAtomicOr64 => Opcode::GlobalAtomicOr32x2,
        Opcode::GlobalAtomicXor64 => Opcode::GlobalAtomicXor32x2,
        Opcode::GlobalAtomicExchange64 => Opcode::GlobalAtomicExchange32x2,
        _ => return None,
    })
}

fn lower(program: &mut Program, inst_ref: InstRef) {
    let inst = program.block(inst_ref.block).inst(inst_ref.inst).clone();
    if let Some(opcode) = replacement_opcode(inst.opcode) {
        program
            .block_mut(inst_ref.block)
            .inst_mut(inst_ref.inst)
            .opcode = opcode;
        return;
    }
    match inst.opcode {
        Opcode::IAdd64 => lower_iadd64(program, inst_ref, &inst),
        Opcode::ISub64 => lower_isub64(program, inst_ref, &inst),
        Opcode::INeg64 => lower_ineg64(program, inst_ref, &inst),
        Opcode::ShiftLeftLogical64 => lower_shift_left_logical64(program, inst_ref, &inst),
        Opcode::ShiftRightLogical64 => lower_shift_right_logical64(program, inst_ref, &inst),
        Opcode::ShiftRightArithmetic64 => lower_shift_right_arithmetic64(program, inst_ref, &inst),
        _ => {}
    }
}

pub fn lower_int64_to_int32(program: &mut Program) {
    for block_index in program.post_order_blocks.clone().into_iter().rev() {
        let instruction_order = program
            .block(block_index)
            .indexed_iter()
            .map(|(inst_index, _)| inst_index)
            .collect::<Vec<_>>();
        for inst_index in instruction_order {
            lower(
                program,
                InstRef {
                    block: block_index,
                    inst: inst_index,
                },
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;

    fn program_with(opcode: Opcode, args: Vec<Value>) -> (Program, InstRef) {
        let mut program = Program::new(crate::ir::types::ShaderStage::Compute);
        program.blocks.push(Block::new());
        program.post_order_blocks.push(0);
        let inst = program.block_mut(0).append_inst(Inst::new(opcode, args));
        (program, InstRef { block: 0, inst })
    }

    #[test]
    fn pack_and_shared_exchange_match_upstream_replacement_opcodes() {
        let (mut pack_program, pack) = program_with(Opcode::PackUint2x32, vec![Value::Void]);
        lower_int64_to_int32(&mut pack_program);
        assert_eq!(
            pack_program.block(0).inst(pack.inst).opcode,
            Opcode::Identity
        );

        let (mut atomic_program, atomic) = program_with(
            Opcode::SharedAtomicExchange64,
            vec![Value::ImmU32(0), Value::ImmU64(1)],
        );
        lower_int64_to_int32(&mut atomic_program);
        assert_eq!(
            atomic_program.block(0).inst(atomic.inst).opcode,
            Opcode::SharedAtomicExchange32x2
        );
    }

    #[test]
    fn iadd64_uses_two_words_and_carry() {
        let (mut program, add) = program_with(
            Opcode::IAdd64,
            vec![Value::ImmU64(0x0000_0001_ffff_ffff), Value::ImmU64(1)],
        );
        let consumer = program
            .block_mut(0)
            .append_inst(Inst::new(Opcode::Identity, vec![Value::Inst(add)]));
        lower_int64_to_int32(&mut program);

        let replacement = program.block(0).inst(consumer).args[0];
        let Value::Inst(replacement) = replacement else {
            panic!("IAdd64 replacement is not an instruction");
        };
        assert_eq!(
            program.block(0).inst(replacement.inst).opcode,
            Opcode::CompositeConstructU32x2
        );
        assert!(program
            .block(0)
            .iter()
            .any(|inst| { inst.opcode == Opcode::GetCarryFromOp }));
    }

    #[test]
    fn all_64_bit_shifts_lower_to_u32_pairs() {
        for opcode in [
            Opcode::ShiftLeftLogical64,
            Opcode::ShiftRightLogical64,
            Opcode::ShiftRightArithmetic64,
        ] {
            let (mut program, shift) =
                program_with(opcode, vec![Value::ImmU64(1), Value::ImmU32(9)]);
            let consumer = program
                .block_mut(0)
                .append_inst(Inst::new(Opcode::Identity, vec![Value::Inst(shift)]));
            lower_int64_to_int32(&mut program);
            let Value::Inst(replacement) = program.block(0).inst(consumer).args[0] else {
                panic!("shift replacement is not an instruction");
            };
            assert_eq!(
                program.block(0).inst(replacement.inst).opcode,
                Opcode::CompositeConstructU32x2
            );
        }
    }
}

// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_scaled_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

fn iscadd_impl(
    tv: &mut TranslatorVisitor<'_>,
    insn: u64,
    mut op_b: Value,
    cc: bool,
    neg_a: bool,
    neg_b: bool,
    scale: u32,
) {
    let dst = tv.dst_reg(insn);
    let mut op_a = tv.x(tv.src_a_reg(insn));
    let po = neg_a && neg_b;

    if po {
        op_b = tv.ir.iadd_32(op_b, Value::ImmU32(1));
    } else {
        if neg_a {
            op_a = tv.ir.ineg_32(op_a);
        }
        if neg_b {
            op_b = tv.ir.ineg_32(op_b);
        }
    }

    let scaled_a = tv.ir.shift_left_logical_32(op_a, Value::ImmU32(scale));
    let result = tv.ir.iadd_32(scaled_a, op_b.clone());

    tv.set_x(dst, result.clone());

    if cc {
        let zero = tv.ir.get_zero_from_op(result.clone());
        let sign = tv.ir.get_sign_from_op(result.clone());
        let mut carry = tv.ir.get_carry_from_op(result.clone());
        let mut overflow = tv.ir.get_overflow_from_op(result);
        if po {
            let op_b_carry = tv.ir.get_carry_from_op(op_b.clone());
            carry = tv.ir.logical_or(carry, op_b_carry);
            let op_b_overflow = tv.ir.get_overflow_from_op(op_b);
            overflow = tv.ir.logical_or(overflow, op_b_overflow);
        }
        tv.ir.set_z_flag(zero);
        tv.ir.set_s_flag(sign);
        tv.ir.set_c_flag(carry);
        tv.ir.set_o_flag(overflow);
    }
}

/// ISCADD_reg / ISCADD_cbuf / ISCADD_imm.
pub fn iscadd(tv: &mut TranslatorVisitor<'_>, insn: u64, opcode: MaxwellOpcode) {
    let op_b = tv.decode_src_b(insn, opcode);
    iscadd_impl(
        tv,
        insn,
        op_b,
        bit(insn, 47),
        bit(insn, 49),
        bit(insn, 48),
        field(insn, 39, 5),
    );
}

/// ISCADD32I.
pub fn iscadd32i(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    let op_b = Value::ImmU32(tv.decode_imm32(insn));
    iscadd_impl(
        tv,
        insn,
        op_b,
        bit(insn, 52),
        false,
        false,
        field(insn, 53, 5),
    );
}

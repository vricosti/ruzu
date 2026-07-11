// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_short_multiply_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

/// IMAD — Integer multiply-add.
pub fn imad(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.x(src_c_reg);

    let product = tv.ir.imul_32(src_a, src_b);
    let result = tv.ir.iadd_32(product, src_c);

    tv.set_x(dst, result);
}

/// IMAD32I — Integer multiply-add with 32-bit immediate.
pub fn imad32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);

    let product = tv.ir.imul_32(src_a, Value::ImmU32(imm));
    let src_c = tv.x(dst); // IMAD32I uses dst as src_c
    let result = tv.ir.iadd_32(product, src_c);

    tv.set_x(dst, result);
}

/// XMAD — Extended multiply-add (16×16+32).
///
/// Implements XMAD_reg, XMAD_rc, XMAD_cr, XMAD_imm.
pub fn xmad(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    fn extract_half(tv: &mut TranslatorVisitor, src: Value, high: bool, signed: bool) -> Value {
        let offset = Value::ImmU32(if high { 16 } else { 0 });
        if signed {
            tv.ir.bit_field_s_extract(src, offset, Value::ImmU32(16))
        } else {
            tv.ir.bit_field_u_extract(src, offset, Value::ImmU32(16))
        }
    }

    let (src_b, src_c, select_mode, half_b, psl, mrg, x) = match opcode {
        MaxwellOpcode::XMAD_reg => (
            tv.get_reg20(insn),
            tv.get_reg39(insn),
            field(insn, 50, 3),
            bit(insn, 35),
            bit(insn, 36),
            bit(insn, 37),
            bit(insn, 38),
        ),
        MaxwellOpcode::XMAD_rc => (
            tv.get_reg39(insn),
            tv.get_cbuf(insn),
            field(insn, 50, 2),
            bit(insn, 52),
            false,
            false,
            bit(insn, 54),
        ),
        MaxwellOpcode::XMAD_cr => (
            tv.get_cbuf(insn),
            tv.get_reg39(insn),
            field(insn, 50, 2),
            bit(insn, 52),
            bit(insn, 55),
            bit(insn, 56),
            bit(insn, 54),
        ),
        MaxwellOpcode::XMAD_imm => (
            Value::ImmU32(field(insn, 20, 16)),
            tv.get_reg39(insn),
            field(insn, 50, 3),
            false,
            bit(insn, 36),
            bit(insn, 37),
            bit(insn, 38),
        ),
        _ => unreachable!("non-XMAD opcode"),
    };
    if x {
        panic!("XMAD X");
    }

    let src_a = tv.x(field(insn, 8, 8));
    let op_a = extract_half(tv, src_a, bit(insn, 53), bit(insn, 48));
    let op_b = extract_half(tv, src_b, half_b, bit(insn, 49));
    let mut product = tv.ir.imul_32(op_a, op_b);
    if psl {
        product = tv.ir.shift_left_logical_32(product, Value::ImmU32(16));
    }
    let op_c = match select_mode {
        0 => src_c,
        1 => extract_half(tv, src_c, false, false),
        2 => extract_half(tv, src_c, true, false),
        3 => panic!("XMAD CSFU"),
        4 => {
            let shifted = tv.ir.shift_left_logical_32(src_b, Value::ImmU32(16));
            tv.ir.iadd_32(shifted, src_c)
        }
        _ => panic!("Invalid XMAD select mode {select_mode}"),
    };
    let mut result = tv.ir.iadd_32(product, op_c);
    if mrg {
        let lsb_b = extract_half(tv, src_b, false, false);
        result = tv
            .ir
            .bit_field_insert(result, lsb_b, Value::ImmU32(16), Value::ImmU32(16));
    }
    if bit(insn, 47) {
        panic!("XMAD CC");
    }
    tv.set_x(field(insn, 0, 8), result);
}

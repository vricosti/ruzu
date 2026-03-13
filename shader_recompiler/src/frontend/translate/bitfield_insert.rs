// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/bitfield_insert.cpp

use super::{field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

pub fn bfi(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.x(src_c_reg);

    // src_a packs offset and count
    let offset = tv.ir.bitwise_and_32(src_a, Value::ImmU32(0xFF));
    let shifted = tv.ir.shift_right_logical_32(src_a, Value::ImmU32(8));
    let count = tv.ir.bitwise_and_32(shifted, Value::ImmU32(0xFF));

    let result = tv.ir.bit_field_insert(src_c, src_b, offset, count);

    tv.set_x(dst, result);
}

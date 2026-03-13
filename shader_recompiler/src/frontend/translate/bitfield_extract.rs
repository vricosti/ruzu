// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/bitfield_extract.cpp

use super::{bit, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

pub fn bfe(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let is_signed = bit(insn, 48);

    // src_b packs offset in low 8 bits, count in bits [15:8]
    let offset = tv.ir.bitwise_and_32(src_b, Value::ImmU32(0xFF));
    let shifted = tv.ir.shift_right_logical_32(src_b, Value::ImmU32(8));
    let count = tv.ir.bitwise_and_32(shifted, Value::ImmU32(0xFF));

    let result = if is_signed {
        tv.ir.bit_field_s_extract(src_a, offset, count)
    } else {
        tv.ir.bit_field_u_extract(src_a, offset, count)
    };

    tv.set_x(dst, result);
}

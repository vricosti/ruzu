// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_to_integer_conversion.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

pub fn i2i(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b(insn, opcode);

    let abs_src = bit(insn, 49);
    let neg_src = bit(insn, 45);
    let is_signed = bit(insn, 13);
    let _dst_size = field(insn, 8, 2); // 0=U8, 1=U16, 2=U32
    let src_size = field(insn, 10, 2);

    // Source size masking
    let masked = match src_size {
        0 => {
            // U8/S8
            if is_signed {
                // Sign-extend from 8 bits
                let shifted = tv.ir.shift_left_logical_32(src, Value::ImmU32(24));
                tv.ir.shift_right_arithmetic_32(shifted, Value::ImmU32(24))
            } else {
                tv.ir.bitwise_and_32(src, Value::ImmU32(0xFF))
            }
        }
        1 => {
            // U16/S16
            if is_signed {
                let shifted = tv.ir.shift_left_logical_32(src, Value::ImmU32(16));
                tv.ir.shift_right_arithmetic_32(shifted, Value::ImmU32(16))
            } else {
                tv.ir.bitwise_and_32(src, Value::ImmU32(0xFFFF))
            }
        }
        _ => src, // U32
    };

    let result = if abs_src {
        tv.ir.iabs_32(masked)
    } else {
        masked
    };

    let result = if neg_src {
        tv.ir.ineg_32(result)
    } else {
        result
    };

    tv.set_x(dst, result);
}

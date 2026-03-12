// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Type conversion translation: F2I, I2F, F2F, I2I.

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

pub fn f2i(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b_f32(insn, opcode);

    let abs_src = bit(insn, 49);
    let neg_src = bit(insn, 45);
    let is_signed = bit(insn, 12);
    let _dst_size = field(insn, 8, 2);  // 0=U8, 1=U16, 2=U32, 3=U64
    let _src_size = field(insn, 10, 2); // 0=F16, 1=F32, 2=F64
    let _rounding = field(insn, 39, 2); // 0=RN, 1=RM, 2=RP, 3=RZ

    let a = tv.ir.fp_abs_neg_32(src, abs_src, neg_src);

    let result = if is_signed {
        tv.ir.convert_s32_from_f32(a)
    } else {
        tv.ir.convert_u32_from_f32(a)
    };

    tv.set_x(dst, result);
}

pub fn i2f(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b(insn, opcode);

    let abs_src = bit(insn, 49);
    let neg_src = bit(insn, 45);
    let is_signed = bit(insn, 13);
    let _dst_size = field(insn, 8, 2);  // 0=F16, 1=F32, 2=F64
    let _src_size = field(insn, 10, 2); // 0=U8, 1=U16, 2=U32, 3=U64

    let result = if is_signed {
        tv.ir.convert_f32_from_s32(src)
    } else {
        tv.ir.convert_f32_from_u32(src)
    };

    let result = if abs_src {
        tv.ir.fp_abs_32(result)
    } else {
        result
    };

    let result = if neg_src {
        tv.ir.fp_neg_32(result)
    } else {
        result
    };

    tv.set_f(dst, result);
}

pub fn f2f(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b_f32(insn, opcode);

    let abs_src = bit(insn, 49);
    let neg_src = bit(insn, 45);
    let sat = bit(insn, 50);
    let _dst_size = field(insn, 8, 2); // 0=F16, 1=F32, 2=F64
    let _src_size = field(insn, 10, 2);
    let rounding = field(insn, 39, 2);

    let a = tv.ir.fp_abs_neg_32(src, abs_src, neg_src);

    // Apply rounding
    let mut result = match rounding {
        0 => a,                             // RN (default)
        1 => tv.ir.fp_floor_32(a),          // RM (floor)
        2 => tv.ir.fp_ceil_32(a),           // RP (ceil)
        3 => tv.ir.fp_trunc_32(a),          // RZ (trunc)
        _ => a,
    };

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

pub fn i2i(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b(insn, opcode);

    let abs_src = bit(insn, 49);
    let neg_src = bit(insn, 45);
    let is_signed = bit(insn, 13);
    let _dst_size = field(insn, 8, 2);  // 0=U8, 1=U16, 2=U32
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

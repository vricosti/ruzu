// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_to_integer_conversion.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

fn width_size(width: u32) -> u32 {
    match width {
        0 => 8,
        1 => 16,
        2 => 32,
        _ => panic!("Invalid integer width {width}"),
    }
}

fn convert_integer(tv: &mut TranslatorVisitor, src: Value, dst_width: u32) -> Value {
    tv.ir
        .bit_field_u_extract(src, Value::ImmU32(0), Value::ImmU32(width_size(dst_width)))
}

fn saturate_integer(
    tv: &mut TranslatorVisitor,
    src: Value,
    dst_width: u32,
    dst_signed: bool,
    src_signed: bool,
) -> Value {
    let (min, max) = match dst_width {
        0 => (
            if dst_signed && src_signed {
                0xffff_ff80
            } else {
                0
            },
            if dst_signed { 0x7f } else { 0xff },
        ),
        1 => (
            if dst_signed && src_signed {
                0xffff_8000
            } else {
                0
            },
            if dst_signed { 0x7fff } else { 0xffff },
        ),
        2 => (
            if dst_signed && src_signed {
                0x8000_0000
            } else {
                0
            },
            if dst_signed { 0x7fff_ffff } else { u32::MAX },
        ),
        _ => panic!("Invalid integer width {dst_width}"),
    };
    let value = if !dst_signed && src_signed {
        tv.ir.s_max_32(Value::ImmU32(0), src)
    } else {
        src
    };
    if dst_signed && src_signed {
        let lower = tv.ir.s_max_32(value, Value::ImmU32(min));
        tv.ir.s_min_32(lower, Value::ImmU32(max))
    } else {
        let lower = tv.ir.u_max_32(value, Value::ImmU32(min));
        tv.ir.u_min_32(lower, Value::ImmU32(max))
    }
}

pub fn i2i(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b(insn, opcode);

    let abs_src = bit(insn, 49);
    let neg_src = bit(insn, 45);
    let cc = bit(insn, 47);
    let sat = bit(insn, 50);
    let dst_signed = bit(insn, 12);
    let src_signed = bit(insn, 13);
    let dst_width = field(insn, 8, 2);
    let src_width = field(insn, 10, 2);
    let selector = field(insn, 41, 3);

    if src_width == 1 && matches!(selector, 1 | 3) {
        panic!("16-bit source format incompatible with selector {selector}");
    }
    if src_width == 2 && selector != 0 {
        panic!("32-bit source format incompatible with selector {selector}");
    }

    let offset = Value::ImmU32(selector * 8);
    let count = Value::ImmU32(width_size(src_width));
    let src_values = if src_signed {
        tv.ir.bit_field_s_extract(src, offset, count)
    } else {
        tv.ir.bit_field_u_extract(src, offset, count)
    };
    let src_values = if abs_src {
        tv.ir.iabs_32(src_values)
    } else {
        src_values
    };
    let result = if neg_src {
        tv.ir.ineg_32(src_values)
    } else {
        src_values
    };
    let result = if sat {
        saturate_integer(tv, result, dst_width, dst_signed, src_signed)
    } else {
        convert_integer(tv, result, dst_width)
    };

    tv.set_x(dst, result);
    if cc {
        let zero = tv.ir.get_zero_from_op(result);
        let sign = tv.ir.get_sign_from_op(result);
        tv.ir.set_z_flag(zero);
        tv.ir.set_s_flag(sign);
        tv.ir.set_c_flag(Value::ImmU1(false));
        tv.ir.set_o_flag(Value::ImmU1(false));
    }
}

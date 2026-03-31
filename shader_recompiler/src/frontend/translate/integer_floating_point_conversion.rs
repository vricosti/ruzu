// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_floating_point_conversion.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;

pub fn i2f(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b(insn, opcode);

    let abs_src = bit(insn, 49);
    let neg_src = bit(insn, 45);
    let is_signed = bit(insn, 13);
    let _dst_size = field(insn, 8, 2); // 0=F16, 1=F32, 2=F64
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

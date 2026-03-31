// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_conversion_integer.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;

pub fn f2i(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b_f32(insn, opcode);

    let abs_src = bit(insn, 49);
    let neg_src = bit(insn, 45);
    let is_signed = bit(insn, 12);
    let _dst_size = field(insn, 8, 2); // 0=U8, 1=U16, 2=U32, 3=U64
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

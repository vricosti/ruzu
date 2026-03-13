// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_conversion_floating_point.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;

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

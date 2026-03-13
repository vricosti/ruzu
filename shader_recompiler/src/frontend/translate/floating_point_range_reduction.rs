// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_range_reduction.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;

pub fn rro(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b_f32(insn, opcode);

    let abs = bit(insn, 49);
    let neg = bit(insn, 45);

    let result = tv.ir.fp_abs_neg_32(src, abs, neg);
    tv.set_f(dst, result);
}

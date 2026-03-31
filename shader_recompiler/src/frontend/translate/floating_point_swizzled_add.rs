// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_swizzled_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

/// FSWZADD — Floating-point swizzled add.
///
/// Upstream: `TranslatorVisitor::FSWZADD(u64 insn)`
pub fn fswzadd(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = field(insn, 0, 8);
    let swizzle = field(insn, 28, 8);
    let _ndv = bit(insn, 38);
    let _ftz = bit(insn, 44);
    let _cc = bit(insn, 47);

    let src_a = tv.get_float_reg8(insn);
    let src_b = tv.get_float_reg20(insn);
    let swizzle_val = Value::ImmU32(swizzle);

    let result = tv.ir.fp_swizzle_add(src_a, src_b, swizzle_val);
    tv.set_f(dst, result);
}

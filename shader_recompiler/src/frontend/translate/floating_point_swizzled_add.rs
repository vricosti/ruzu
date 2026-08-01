// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_swizzled_add.cpp

use super::common_encoding::{cast_fp_rounding, MaxwellFpRounding};
use super::{bit, field, TranslatorVisitor};
use crate::ir::types::{FmzMode, FpControl};
use crate::ir::value::Value;

/// FSWZADD — Floating-point swizzled add.
///
/// Upstream: `TranslatorVisitor::FSWZADD(u64 insn)`
pub fn fswzadd(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = field(insn, 0, 8);
    let swizzle = field(insn, 28, 8);
    let ndv = bit(insn, 38);
    let round = MaxwellFpRounding::from_field(field(insn, 39, 2));
    let ftz = bit(insn, 44);
    let cc = bit(insn, 47);

    if ndv {
        log::warn!("(STUBBED) FSWZADD - NDV mode");
    }

    let src_a = tv.get_float_reg8(insn);
    let src_b = tv.get_float_reg20(insn);
    let swizzle_val = Value::ImmU32(swizzle);

    let control = FpControl {
        no_contraction: false,
        rounding: cast_fp_rounding(round),
        fmz_mode: if ftz { FmzMode::FTZ } else { FmzMode::None },
    };
    let result = tv
        .ir
        .fp_swizzle_add_with_control(src_a, src_b, swizzle_val, control);
    tv.set_f(dst, result);

    if cc {
        panic!("FSWZADD CC");
    }
}

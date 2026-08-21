// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_compare_and_set.cpp

use super::common_funcs::{floating_point_compare_32, predicate_combine};
use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::{FmzMode, FpControl};
use crate::ir::value::{Pred, Value};

pub fn fset(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let abs_a = bit(insn, 54);
    let abs_b = bit(insn, 44);
    let neg_a = bit(insn, 43);
    let neg_b = bit(insn, 53);

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);

    let cmp_op = field(insn, 48, 4);
    let bool_op = field(insn, 45, 2);
    let pred_idx = field(insn, 39, 3);
    let neg_pred = bit(insn, 42);
    let pred39 = tv.ir.get_pred(Pred(pred_idx as u8), neg_pred);
    let bf_mode = bit(insn, 52);
    let cc = bit(insn, 47);
    let ftz = bit(insn, 55);

    let control = FpControl {
        no_contraction: false,
        rounding: Default::default(),
        fmz_mode: if ftz { FmzMode::FTZ } else { FmzMode::None },
    };
    let cmp_result = floating_point_compare_32(tv, a, b, cmp_op, control);
    let result = predicate_combine(tv, cmp_result, pred39, bool_op);

    // Output: if bf_mode, output 1.0f for true, else 0xFFFFFFFF
    let true_val = if bf_mode {
        Value::ImmU32(0x3F800000) // 1.0f
    } else {
        Value::ImmU32(0xFFFFFFFF)
    };
    let output = tv.ir.select_u32(result, true_val, Value::ImmU32(0));

    tv.set_x(dst, output.clone());

    if cc {
        let zero = Value::ImmU32(0);
        let is_zero = tv.ir.i_equal(output, zero);
        tv.ir.set_z_flag(is_zero.clone());
        let sign = if bf_mode {
            tv.ir.imm_u1(false)
        } else {
            tv.ir.logical_not(is_zero)
        };
        tv.ir.set_s_flag(sign);
        tv.ir.set_c_flag(Value::ImmU1(false));
        tv.ir.set_o_flag(Value::ImmU1(false));
    }
}

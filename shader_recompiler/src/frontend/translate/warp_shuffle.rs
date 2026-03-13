// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of upstream warp-level instruction translate files:
//! - `impl/vote.cpp`
//! - `impl/warp_shuffle.cpp`

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

/// VOTE — Warp vote operations (ALL, ANY, EQ).
///
/// Matches upstream `TranslatorVisitor::VOTE(u64 insn)`.
pub fn vote(v: &mut TranslatorVisitor<'_>, insn: u64) {
    let dest_reg = field(insn, 0, 8);
    let pred_a_idx = field(insn, 39, 3);
    let neg_pred_a = bit(insn, 42);
    let pred_b_idx = field(insn, 45, 3);
    let vote_op = (insn >> 48) & 3;

    let vote_pred = v.ir.get_pred(Pred(pred_a_idx as u8), neg_pred_a);

    let result = match vote_op {
        0 => v.ir.vote_all(vote_pred),
        1 => v.ir.vote_any(vote_pred),
        2 => v.ir.vote_equal(vote_pred),
        _ => {
            log::warn!("VOTE: invalid vote op {}", vote_op);
            return;
        }
    };

    v.ir.set_pred(Pred(pred_b_idx as u8), result);

    let ballot = v.ir.subgroup_ballot(vote_pred);
    v.set_x(dest_reg, ballot);
}

/// SHFL — Warp shuffle.
///
/// Matches upstream `TranslatorVisitor::SHFL(u64 insn)`.
pub fn shfl(v: &mut TranslatorVisitor<'_>, insn: u64) {
    let dest_reg = field(insn, 0, 8);
    let src_reg = field(insn, 8, 8);
    let pred_idx = field(insn, 48, 3);
    let mode = (insn >> 30) & 3;

    // src_a: index operand — register or 5-bit immediate
    let src_a_flag = bit(insn, 28);
    let src_a_imm = field(insn, 20, 5);
    let src_a = if src_a_flag {
        Value::ImmU32(src_a_imm)
    } else {
        v.x(field(insn, 20, 8))
    };

    // src_b: mask operand — register or 13-bit immediate
    let src_b_flag = bit(insn, 29);
    let src_b_imm = field(insn, 34, 13);
    let src_b = if src_b_flag {
        Value::ImmU32(src_b_imm)
    } else {
        v.x(field(insn, 39, 8))
    };

    // clamp = mask[4:0], seg_mask = mask[12:8]
    let clamp = v.ir.bit_field_u_extract(src_b, Value::ImmU32(0), Value::ImmU32(5));
    let seg_mask = v.ir.bit_field_u_extract(src_b, Value::ImmU32(8), Value::ImmU32(5));

    let value = v.x(src_reg);
    let result = match mode {
        0 => v.ir.shuffle_index(value, src_a, seg_mask),
        1 | 2 | 3 => {
            // UP/DOWN/BFLY shuffle variants — emit ShuffleIndex as fallback
            // (upstream has full ShuffleUp/Down/Butterfly but emitter only exposes ShuffleIndex)
            log::warn!("SHFL mode {} not fully implemented, using IDX fallback", mode);
            let _ = clamp; // used in full implementation
            v.ir.shuffle_index(value, src_a, seg_mask)
        }
        _ => unreachable!(),
    };

    // Set destination predicate (in-bounds flag) — emit true as placeholder
    v.ir.set_pred(Pred(pred_idx as u8), Value::ImmU1(true));
    v.set_x(dest_reg, result);
}

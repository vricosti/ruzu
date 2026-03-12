// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! VOTE instruction translation — maps to zuyu's
//! `frontend/maxwell/translate/impl/vote.cpp`.
//!
//! Handles the VOTE and VOTE_vtg instructions for subgroup voting operations.

use super::{field, TranslatorVisitor};
use crate::ir::value::Value;

/// VOTE operation types, matching upstream `VoteOp` enum.
#[repr(u64)]
#[derive(Debug, Clone, Copy)]
enum VoteOp {
    All = 0,
    Any = 1,
    Eq = 2,
}

impl VoteOp {
    fn from_raw(val: u64) -> Self {
        match val {
            0 => VoteOp::All,
            1 => VoteOp::Any,
            2 => VoteOp::Eq,
            _ => panic!("Invalid VOTE op {}", val),
        }
    }
}

impl<'a> TranslatorVisitor<'a> {
    /// Translate the VOTE instruction.
    ///
    /// Matches upstream `TranslatorVisitor::VOTE(u64)`.
    ///
    /// Encoding:
    /// - bits [7:0]:   dest_reg
    /// - bits [41:39]: pred_a (source predicate)
    /// - bit  42:      neg_pred_a
    /// - bits [47:45]: pred_b (destination predicate)
    /// - bits [49:48]: vote_op (ALL=0, ANY=1, EQ=2)
    pub fn translate_vote(&mut self, insn: u64) {
        let dest_reg = field(insn, 0, 8);
        let pred_a_idx = field(insn, 39, 3);
        let neg_pred_a = (insn >> 42) & 1 != 0;
        let pred_b_idx = field(insn, 45, 3);
        let vote_op = VoteOp::from_raw((insn >> 48) & 3);

        // Get the source predicate
        let vote_pred = self.ir.get_pred(
            crate::ir::value::Pred(pred_a_idx as u8),
            neg_pred_a,
        );

        // Perform the vote operation
        let result = match vote_op {
            VoteOp::All => self.ir.vote_all(vote_pred),
            VoteOp::Any => self.ir.vote_any(vote_pred),
            VoteOp::Eq => self.ir.vote_equal(vote_pred),
        };

        // Set the destination predicate
        self.ir.set_pred(
            crate::ir::value::Pred(pred_b_idx as u8),
            result,
        );

        // Set the destination register to the ballot result
        let ballot = self.ir.subgroup_ballot(vote_pred);
        self.set_x(dest_reg, ballot);
    }

    /// Translate the VOTE_vtg instruction (stubbed).
    ///
    /// Matches upstream `TranslatorVisitor::VOTE_vtg(u64)`.
    pub fn translate_vote_vtg(&mut self, _insn: u64) {
        log::warn!("(STUBBED) VOTE_vtg called");
    }
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Move/select translation: MOV, MOV32I, SEL, S2R, CS2R.

use super::{field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

pub fn mov(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b(insn, opcode);
    tv.set_x(dst, src);
}

pub fn mov32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let imm = tv.decode_imm32(insn);
    tv.set_x(dst, Value::ImmU32(imm));
}

pub fn sel(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let pred_idx = field(insn, 39, 3);
    let pred_neg = (insn >> 42) & 1 != 0;

    let cond = tv.ir.get_pred(Pred(pred_idx as u8), pred_neg);
    let result = tv.ir.select_u32(cond, src_a, src_b);

    tv.set_x(dst, result);
}

/// S2R — System Register to Register.
pub fn s2r(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let sr = field(insn, 20, 8);

    let result = match sr {
        0x00 => {
            // SR_LANEID
            let inv_info = tv.ir.invocation_info();
            tv.ir.bitwise_and_32(inv_info, Value::ImmU32(0x1F))
        }
        0x02 => {
            // SR_VIRTCFG
            Value::ImmU32(0)
        }
        0x03 => {
            // SR_VIRTID
            Value::ImmU32(0)
        }
        0x05 => {
            // SR_PM0 (perf counter)
            Value::ImmU32(0)
        }
        0x10 => {
            // SR_ORDERING_TICKET (for threadgroup ordering)
            Value::ImmU32(0)
        }
        0x21 => {
            // SR_TID.X (ThreadId.X)
            let lid = tv.ir.local_invocation_id();
            tv.ir.composite_extract_u32x4(lid, Value::ImmU32(0))
        }
        0x22 => {
            // SR_TID.Y (ThreadId.Y)
            let lid = tv.ir.local_invocation_id();
            tv.ir.composite_extract_u32x4(lid, Value::ImmU32(1))
        }
        0x23 => {
            // SR_TID.Z (ThreadId.Z)
            let lid = tv.ir.local_invocation_id();
            tv.ir.composite_extract_u32x4(lid, Value::ImmU32(2))
        }
        0x25 => {
            // SR_CTAID.X (BlockId.X)
            let wgid = tv.ir.workgroup_id();
            tv.ir.composite_extract_u32x4(wgid, Value::ImmU32(0))
        }
        0x26 => {
            // SR_CTAID.Y
            let wgid = tv.ir.workgroup_id();
            tv.ir.composite_extract_u32x4(wgid, Value::ImmU32(1))
        }
        0x27 => {
            // SR_CTAID.Z
            let wgid = tv.ir.workgroup_id();
            tv.ir.composite_extract_u32x4(wgid, Value::ImmU32(2))
        }
        0x37 => {
            // SR_THREAD_KILL (helper invocation)
            let is_helper = tv.ir.is_helper_invocation();
            tv.ir.select_u32(is_helper, Value::ImmU32(1), Value::ImmU32(0))
        }
        0x50 => {
            // SR_CLOCKLO (low 32 bits of clock)
            Value::ImmU32(0)
        }
        0x51 => {
            // SR_CLOCKHI (high 32 bits of clock)
            Value::ImmU32(0)
        }
        _ => {
            log::trace!("S2R: unknown system register 0x{:02X}", sr);
            Value::ImmU32(0)
        }
    };

    tv.set_x(dst, result);
}

// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/attribute_memory_to_physical.cpp

use super::{sfield, field, TranslatorVisitor};
use crate::ir::value::Value;

/// AL2P — Attribute memory to physical address.
///
/// Converts an attribute byte offset (with optional indexing register) to a
/// physical attribute word offset.
///
/// Upstream: `TranslatorVisitor::AL2P(u64 inst)`
pub fn al2p(tv: &mut TranslatorVisitor, insn: u64) {
    let result_reg    = field(insn, 0, 8);
    let indexing_reg  = field(insn, 8, 8);
    let offset        = sfield(insn, 20, 11) as i32;
    // bitsize field at [47:48]: 0 = B32, others not yet implemented.
    let bitsize = field(insn, 47, 2);
    if bitsize != 0 {
        log::warn!("AL2P: BitSize {} not B32 — approximating (insn={:#018x})", bitsize, insn);
    }

    let converted_offset = Value::ImmU32(offset as u32);
    let index_val = tv.x(indexing_reg);
    let result = tv.ir.iadd_32(index_val, converted_offset);
    tv.set_x(result_reg, result);
}

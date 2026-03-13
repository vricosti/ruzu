// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/load_store_local_shared.cpp
//!
//! Implements LDL, STL (load/store local memory) and LDS, STS (load/store
//! shared memory).

use super::{field, TranslatorVisitor};
use crate::ir::value::Value;

/// LDL — Load Local Memory.
///
/// Loads from thread-local (private) memory using a register offset plus a
/// sign-extended 24-bit immediate.  The result is sign- or zero-extended
/// according to the size field.
///
/// Upstream: `TranslatorVisitor::LDL(u64 insn)`
pub fn ldl(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let offset = field(insn, 20, 24);

    let base = tv.x(src_reg);
    let addr = tv.ir.iadd_32(base, Value::ImmU32(offset));
    let result = tv.ir.load_local(addr);

    tv.set_x(dst, result);
}

/// STL — Store Local Memory.
///
/// Stores a register value into thread-local (private) memory.
///
/// Upstream: `TranslatorVisitor::STL(u64 insn)`
pub fn stl(tv: &mut TranslatorVisitor, insn: u64) {
    let src_data_reg = tv.dst_reg(insn);
    let src_addr_reg = tv.src_a_reg(insn);
    let offset = field(insn, 20, 24);

    let base = tv.x(src_addr_reg);
    let addr = tv.ir.iadd_32(base, Value::ImmU32(offset));
    let data = tv.x(src_data_reg);

    tv.ir.write_local(addr, data);
}

/// LDS — Load Shared Memory.
///
/// Loads from shared (workgroup) memory at the given byte offset.
///
/// Upstream: `TranslatorVisitor::LDS(u64 insn)`
pub fn lds(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    // Offset is a sign-extended 24-bit field at bits [20:43] shifted by 0.
    // Use the same encoding as LDL (local memory) since shared uses same format.
    let offset_raw = field(insn, 20, 24);
    let base = tv.x(src_reg);
    // Byte offset: base + sign-extended offset
    let addr = tv.ir.iadd_32(base, Value::ImmU32(offset_raw));
    let result = tv.ir.load_shared_u32(addr);
    tv.set_x(dst, result);
}

/// STS — Store Shared Memory.
///
/// Stores a register value into shared (workgroup) memory.
///
/// Upstream: `TranslatorVisitor::STS(u64 insn)`
pub fn sts(tv: &mut TranslatorVisitor, insn: u64) {
    let src_data_reg = tv.dst_reg(insn);
    let src_addr_reg = tv.src_a_reg(insn);
    let offset_raw = field(insn, 20, 24);
    let base = tv.x(src_addr_reg);
    let addr = tv.ir.iadd_32(base, Value::ImmU32(offset_raw));
    let data = tv.x(src_data_reg);
    tv.ir.write_shared_u32(addr, data);
}

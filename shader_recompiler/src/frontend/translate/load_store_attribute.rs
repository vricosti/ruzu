// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/load_store_attribute.cpp
//!
//! Implements ALD, AST (attribute load/store for tessellation/geometry stages)
//! and IPA (interpolate pixel attribute for fragment shaders).

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Attribute, Reg, Value};

/// ALD — Attribute Load.
///
/// Upstream: `TranslatorVisitor::ALD(u64 insn)`
pub fn ald(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let index_reg = field(insn, 8, 8);
    let attr_offset = field(insn, 20, 10);
    let patch = bit(insn, 31);
    let vertex_reg = field(insn, 39, 8);
    let size = field(insn, 47, 2);

    if attr_offset % 4 != 0 {
        log::warn!("ALD unaligned absolute offset {}", attr_offset);
        return;
    }
    if patch {
        log::warn!("ALD patch read not yet implemented");
        return;
    }
    if index_reg != Reg::RZ.0 as u32 {
        log::warn!("ALD indexed attribute read not yet implemented");
        return;
    }

    let vertex = tv.x(vertex_reg);
    let attr_base = attr_offset / 4;
    for element in 0..num_elements(size) {
        let attr = Attribute(attr_base + element);
        let result = tv.ir.get_attribute(attr, vertex);
        tv.set_f(dst + element, result);
        tv.ir.program.info.loads.set(attr.0 as usize, true);
    }
}

/// AST — Attribute Store.
///
/// Upstream: `TranslatorVisitor::AST(u64 insn)`
pub fn ast(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = tv.dst_reg(insn);
    let index_reg = field(insn, 8, 8);
    let attr_offset = field(insn, 20, 10);
    let patch = bit(insn, 31);
    let vertex_reg = field(insn, 39, 8);
    let size = field(insn, 47, 2);

    if index_reg != Reg::RZ.0 as u32 {
        log::warn!("AST indexed attribute store not yet implemented");
        return;
    }
    if attr_offset % 4 != 0 {
        log::warn!("AST unaligned absolute offset {}", attr_offset);
        return;
    }
    if patch {
        log::warn!("AST patch store not yet implemented");
        return;
    }

    let vertex = tv.x(vertex_reg);
    let attr_base = attr_offset / 4;
    for element in 0..num_elements(size) {
        let attr = Attribute(attr_base + element);
        let value = tv.f(src_reg + element);
        tv.ir.set_attribute(attr, value, vertex);
        tv.ir.program.info.stores.set(attr.0 as usize, true);
    }
}

/// IPA — Interpolate Pixel Attribute.
///
/// Upstream: `TranslatorVisitor::IPA(u64 insn)`
pub fn ipa(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let attr = Attribute(field(insn, 30, 8));
    let vertex = Value::ImmU32(0);
    let result = tv.ir.get_attribute(attr, vertex);
    tv.set_f(dst, result);
    tv.ir.program.info.loads.set(attr.0 as usize, true);
}

fn num_elements(size: u32) -> u32 {
    match size {
        0 => 1,
        1 => 2,
        2 => 3,
        3 => 4,
        _ => unreachable!(),
    }
}

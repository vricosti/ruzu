// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/load_store_attribute.cpp
//!
//! Implements ALD, AST (attribute load/store for tessellation/geometry stages)
//! and IPA (interpolate pixel attribute for fragment shaders).

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Attribute, Value};

/// ALD — Attribute Load.
///
/// Upstream: `TranslatorVisitor::ALD(u64 insn)`
pub fn ald(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let attr_offset = field(insn, 20, 10);
    let _patch = bit(insn, 31);
    let _input_vertex = field(insn, 8, 8);

    let attr_idx = attr_offset / 4;
    let vertex = Value::ImmU32(0);

    if attr_idx >= 8 && attr_idx < 8 + 4 {
        let attr = Attribute::position(attr_idx - 8);
        let result = tv.ir.get_attribute(attr, vertex);
        tv.set_f(dst, result);
        tv.ir.program.info.loads.set(attr.0 as usize, true);
    } else if attr_idx >= 16 {
        let mapped_generic = (attr_idx - 16) / 4;
        let mapped_comp = (attr_idx - 16) % 4;
        if mapped_generic < 32 {
            let attr = Attribute::generic(mapped_generic, mapped_comp);
            let result = tv.ir.get_attribute(attr, vertex);
            tv.set_f(dst, result);
            tv.ir.program.info.loads.set(attr.0 as usize, true);
        }
    } else {
        let result = Value::ImmF32(0.0);
        tv.set_f(dst, result);
    }
}

/// AST — Attribute Store.
///
/// Upstream: `TranslatorVisitor::AST(u64 insn)`
pub fn ast(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = tv.dst_reg(insn);
    let attr_offset = field(insn, 20, 10);

    let attr_idx = attr_offset / 4;
    let vertex = Value::ImmU32(0);

    if attr_idx >= 8 && attr_idx < 8 + 4 {
        let comp = attr_idx - 8;
        let attr = Attribute::position(comp);
        let value = tv.f(src_reg);
        tv.ir.set_attribute(attr, value, vertex);
        tv.ir.program.info.stores.set(attr.0 as usize, true);
    } else if attr_idx >= 16 {
        let mapped_generic = (attr_idx - 16) / 4;
        let mapped_comp = (attr_idx - 16) % 4;
        if mapped_generic < 32 {
            let attr = Attribute::generic(mapped_generic, mapped_comp);
            let value = tv.f(src_reg);
            tv.ir.set_attribute(attr, value, vertex);
            tv.ir.program.info.stores.set(attr.0 as usize, true);
        }
    }
}

/// IPA — Interpolate Pixel Attribute.
///
/// Upstream: `TranslatorVisitor::IPA(u64 insn)`
pub fn ipa(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let attr_offset = field(insn, 28, 10);
    let _mode = field(insn, 54, 2);
    let _sample = bit(insn, 46);

    let attr_idx = attr_offset / 4;
    let vertex = Value::ImmU32(0);

    if attr_idx >= 8 && attr_idx < 8 + 4 {
        let comp = attr_idx - 8;
        let attr = Attribute::position(comp);
        let result = tv.ir.get_attribute(attr, vertex);
        tv.set_f(dst, result);
        tv.ir.program.info.loads.set(attr.0 as usize, true);
    } else if attr_idx >= 16 {
        let mapped_generic = (attr_idx - 16) / 4;
        let mapped_comp = (attr_idx - 16) % 4;
        if mapped_generic < 32 {
            let attr = Attribute::generic(mapped_generic, mapped_comp);
            let result = tv.ir.get_attribute(attr, vertex);
            tv.set_f(dst, result);
            tv.ir.program.info.loads.set(attr.0 as usize, true);
        }
    } else {
        tv.set_f(dst, Value::ImmF32(0.0));
    }
}

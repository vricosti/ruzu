// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Attribute I/O translation: ALD, AST, IPA.

use super::{bit, field, TranslatorVisitor};
use crate::shader_recompiler::ir::value::{Attribute, Value};

/// ALD — Attribute Load.
pub fn ald(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let attr_offset = field(insn, 20, 10);
    let patch = bit(insn, 31);
    let _input_vertex = field(insn, 8, 8);

    // attr_offset is in bytes; divide by 4 for component index
    let attr_idx = attr_offset / 4;
    let generic_idx = attr_idx / 4;
    let component = attr_idx % 4;

    let vertex = Value::ImmU32(0);

    if attr_idx >= 8 && attr_idx < 8 + 4 {
        // Position (offset 0x70..0x7F → attr 28..31)
        let attr = Attribute::position(attr_idx - 8);
        let result = tv.ir.get_attribute(attr, vertex);
        tv.set_f(dst, result);
        tv.ir.program.info.loads_position = true;
    } else if generic_idx < 32 && attr_idx >= 16 {
        // Generic attributes (offset 0x80+ → generic 0..31)
        let mapped_generic = (attr_idx - 16) / 4;
        let mapped_comp = (attr_idx - 16) % 4;
        if mapped_generic < 32 {
            let attr = Attribute::generic(mapped_generic, mapped_comp);
            let result = tv.ir.get_attribute(attr, vertex);
            tv.set_f(dst, result);
            tv.ir.program.info.loads_generics |= 1 << mapped_generic;
        }
    } else {
        // Other system attributes
        let result = Value::ImmF32(0.0);
        tv.set_f(dst, result);
    }
}

/// AST — Attribute Store.
pub fn ast(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = tv.dst_reg(insn);
    let attr_offset = field(insn, 20, 10);

    let attr_idx = attr_offset / 4;
    let vertex = Value::ImmU32(0);

    if attr_idx >= 8 && attr_idx < 8 + 4 {
        // Position output
        let comp = attr_idx - 8;
        let attr = Attribute::position(comp);
        let value = tv.f(src_reg);
        tv.ir.set_attribute(attr, value, vertex);
        tv.ir.program.info.stores_position = true;
    } else if attr_idx >= 16 {
        // Generic output
        let mapped_generic = (attr_idx - 16) / 4;
        let mapped_comp = (attr_idx - 16) % 4;
        if mapped_generic < 32 {
            let attr = Attribute::generic(mapped_generic, mapped_comp);
            let value = tv.f(src_reg);
            tv.ir.set_attribute(attr, value, vertex);
            tv.ir.program.info.stores_generics |= 1 << mapped_generic;
        }
    }
}

/// IPA — Interpolate Pixel Attribute (fragment shader input interpolation).
pub fn ipa(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let attr_offset = field(insn, 28, 10);
    let _mode = field(insn, 54, 2); // 0=Perspective, 1=Constant, 2=ScreenLinear
    let _sample = bit(insn, 46);

    let attr_idx = attr_offset / 4;
    let vertex = Value::ImmU32(0);

    if attr_idx >= 8 && attr_idx < 8 + 4 {
        // Fragment position
        let comp = attr_idx - 8;
        let attr = Attribute::position(comp);
        let result = tv.ir.get_attribute(attr, vertex);
        tv.set_f(dst, result);
        tv.ir.program.info.loads_position = true;
    } else if attr_idx >= 16 {
        let mapped_generic = (attr_idx - 16) / 4;
        let mapped_comp = (attr_idx - 16) % 4;
        if mapped_generic < 32 {
            let attr = Attribute::generic(mapped_generic, mapped_comp);
            let result = tv.ir.get_attribute(attr, vertex);
            tv.set_f(dst, result);
            tv.ir.program.info.loads_generics |= 1 << mapped_generic;
        }
    } else {
        tv.set_f(dst, Value::ImmF32(0.0));
    }
}

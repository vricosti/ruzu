// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/load_store_attribute.cpp
//!
//! Implements ALD, AST (attribute load/store for tessellation/geometry stages)
//! and IPA (interpolate pixel attribute for fragment shaders).

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Attribute, Patch, Reg, Value};
use crate::program_header::PixelImap;

const INTERPOLATION_MODE_MULTIPLY: u32 = 1;

/// Walk the indexed-attribute element loop, computing
/// `final_offset = index_value + (element * 4)`. Port of upstream's
/// `HandleIndexed` helper lambda in `load_store_attribute.cpp`.
fn handle_indexed<F>(tv: &mut TranslatorVisitor, index_reg: u32, num_elements: u32, mut f: F)
where
    F: FnMut(&mut TranslatorVisitor, u32, Value),
{
    let index_value = tv.x(index_reg);
    for element in 0..num_elements {
        let final_offset = if element == 0 {
            index_value.clone()
        } else {
            let imm = Value::ImmU32(element * 4);
            tv.ir.iadd_32(index_value.clone(), imm)
        };
        f(tv, element, final_offset);
    }
}

/// ALD — Attribute Load.
///
/// Upstream: `TranslatorVisitor::ALD(u64 insn)`. Supports both direct
/// attribute reads (via `GetAttribute`/`GetPatch`) and indexed reads
/// (via `GetAttributeIndexed`). Indirect patch reads panic — upstream
/// throws `NotImplementedException("Indirect patch read")`.
pub fn ald(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let index_reg = field(insn, 8, 8);
    let attr_offset = field(insn, 20, 10);
    let patch = bit(insn, 31);
    let vertex_reg = field(insn, 39, 8);
    let size = field(insn, 47, 2);

    if attr_offset % 4 != 0 {
        panic!("Unaligned absolute offset {}", attr_offset);
    }
    let vertex = tv.x(vertex_reg);
    let num = num_elements(size);
    if index_reg == Reg::RZ.0 as u32 {
        let attr_base = attr_offset / 4;
        for element in 0..num {
            if patch {
                let patch_id = Patch(attr_base + element);
                let value = tv.ir.get_patch(patch_id);
                tv.set_f(dst + element, value);
            } else {
                let attr = Attribute(attr_base + element);
                let value = tv.ir.get_attribute(attr, vertex.clone());
                tv.set_f(dst + element, value);
                tv.ir.program.info.loads.set(attr.0 as usize, true);
            }
        }
        return;
    }
    if patch {
        panic!("Indirect patch read");
    }
    handle_indexed(tv, index_reg, num, |tv, element, final_offset| {
        let value = tv.ir.get_attribute_indexed(final_offset, vertex.clone());
        tv.set_f(dst + element, value);
    });
}

/// AST — Attribute Store.
///
/// Upstream: `TranslatorVisitor::AST(u64 insn)`. Same patch + indexed
/// dispatch as ALD; indexed patch store panics matching upstream's
/// `NotImplementedException("Indexed tessellation patch store")`.
pub fn ast(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = tv.dst_reg(insn);
    let index_reg = field(insn, 8, 8);
    let attr_offset = field(insn, 20, 10);
    let patch = bit(insn, 31);
    let vertex_reg = field(insn, 39, 8);
    let size = field(insn, 47, 2);

    if attr_offset % 4 != 0 {
        panic!("Unaligned absolute offset {}", attr_offset);
    }
    if index_reg != Reg::RZ.0 as u32 {
        panic!("Indexed store");
    }
    let vertex = tv.x(vertex_reg);
    let num = num_elements(size);
    if index_reg == Reg::RZ.0 as u32 {
        let attr_base = attr_offset / 4;
        for element in 0..num {
            let value = tv.f(src_reg + element);
            if patch {
                let patch_id = Patch(attr_base + element);
                tv.ir.set_patch(patch_id, value);
            } else {
                let attr = Attribute(attr_base + element);
                tv.ir.set_attribute(attr, value, vertex.clone());
                tv.ir.program.info.stores.set(attr.0 as usize, true);
            }
        }
        return;
    }
}

/// IPA — Interpolate Pixel Attribute.
///
/// Upstream: `TranslatorVisitor::IPA(u64 insn)`
pub fn ipa(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let index_reg = field(insn, 8, 8);
    let multiplier = field(insn, 20, 8);
    let attr = Attribute(field(insn, 30, 8));
    let indexed = bit(insn, 38);
    let saturated = bit(insn, 51);
    let interpolation_mode = field(insn, 54, 2);

    let is_indexed = indexed && index_reg != Reg::RZ.0 as u32;
    let vertex = Value::ImmU32(0);
    let mut result = if is_indexed {
        let index_val = tv.x(index_reg);
        tv.ir.get_attribute_indexed(index_val, vertex)
    } else {
        tv.ir.get_attribute(attr, vertex)
    };

    if attr.is_generic() {
        if let Some(sph) = tv.sph.as_ref() {
            let input_map = sph.ps_generic_input_map(attr.generic_index());
            let element = attr.generic_element() as usize;
            if input_map[element] == PixelImap::Perspective {
                let position_w = tv.ir.get_attribute(Attribute::POSITION_W, Value::ImmU32(0));
                result = tv.ir.fp_mul_32(result, position_w);
            }
        }
    }

    if interpolation_mode == INTERPOLATION_MODE_MULTIPLY {
        let multiplier = tv.f(multiplier);
        result = tv.ir.fp_mul_32(result, multiplier);
    }

    if saturated {
        result = tv.ir.fp_saturate_32(result);
    }

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

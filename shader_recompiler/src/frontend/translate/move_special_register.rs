// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/move_special_register.cpp

use super::{field, TranslatorVisitor};
use crate::ir::value::Value;

/// Special register indices.
/// Matches upstream `enum class SpecialRegister : u64`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SpecialRegister {
    LaneId,
    InvocationId,
    YDirection,
    ThreadKill,
    Affinity,
    InvocationInfo,
    WscalefactorXy,
    WscalefactorZ,
    Tid,
    TidX,
    TidY,
    TidZ,
    CtaidX,
    CtaidY,
    CtaidZ,
    EqMask,
    LtMask,
    LeMask,
    GtMask,
    GeMask,
    Unknown(u32),
}

impl SpecialRegister {
    fn from_u32(v: u32) -> Self {
        match v {
            0 => SpecialRegister::LaneId,
            17 => SpecialRegister::InvocationId,
            18 => SpecialRegister::YDirection,
            19 => SpecialRegister::ThreadKill,
            28 => SpecialRegister::Affinity,
            29 => SpecialRegister::InvocationInfo,
            30 => SpecialRegister::WscalefactorXy,
            31 => SpecialRegister::WscalefactorZ,
            32 => SpecialRegister::Tid,
            33 => SpecialRegister::TidX,
            34 => SpecialRegister::TidY,
            35 => SpecialRegister::TidZ,
            37 => SpecialRegister::CtaidX,
            38 => SpecialRegister::CtaidY,
            39 => SpecialRegister::CtaidZ,
            56 => SpecialRegister::EqMask,
            57 => SpecialRegister::LtMask,
            58 => SpecialRegister::LeMask,
            59 => SpecialRegister::GtMask,
            60 => SpecialRegister::GeMask,
            other => SpecialRegister::Unknown(other),
        }
    }
}

/// S2R — Move special register to general-purpose register.
///
/// Matches upstream `TranslatorVisitor::S2R(u64 insn)`.
pub fn s2r(tv: &mut TranslatorVisitor, insn: u64) {
    let dest_reg = field(insn, 0, 8);
    let sr_idx = field(insn, 20, 8);
    let sr = SpecialRegister::from_u32(sr_idx);

    let result: Value = match sr {
        SpecialRegister::InvocationId => tv.ir.invocation_id(),
        SpecialRegister::ThreadKill => {
            let is_helper = tv.ir.is_helper_invocation();
            tv.ir
                .select_u32(is_helper, Value::ImmU32(u32::MAX), Value::ImmU32(0))
        }
        SpecialRegister::InvocationInfo => tv.ir.invocation_info(),
        SpecialRegister::Tid => {
            // Pack TID.x[7:0] in [7:0], TID.y[7:0] in [23:16], TID.z[5:0] in [31:26]
            // Matches upstream S2R / SR_TID handling.
            let tid = tv.ir.local_invocation_id();
            let x = tv.ir.composite_extract_u32x2(tid, Value::ImmU32(0));
            let y = tv.ir.composite_extract_u32x2(tid, Value::ImmU32(1));
            let z = tv.ir.composite_extract_u32x2(tid, Value::ImmU32(2));
            let xy = tv
                .ir
                .bit_field_insert(x, y, Value::ImmU32(16), Value::ImmU32(8));
            tv.ir
                .bit_field_insert(xy, z, Value::ImmU32(26), Value::ImmU32(6))
        }
        SpecialRegister::TidX => {
            let tid = tv.ir.local_invocation_id();
            tv.ir.composite_extract_u32x2(tid, Value::ImmU32(0))
        }
        SpecialRegister::TidY => {
            let tid = tv.ir.local_invocation_id();
            tv.ir.composite_extract_u32x2(tid, Value::ImmU32(1))
        }
        SpecialRegister::TidZ => {
            let tid = tv.ir.local_invocation_id();
            tv.ir.composite_extract_u32x2(tid, Value::ImmU32(2))
        }
        SpecialRegister::CtaidX => {
            let wg = tv.ir.workgroup_id();
            tv.ir.composite_extract_u32x2(wg, Value::ImmU32(0))
        }
        SpecialRegister::CtaidY => {
            let wg = tv.ir.workgroup_id();
            tv.ir.composite_extract_u32x2(wg, Value::ImmU32(1))
        }
        SpecialRegister::CtaidZ => {
            let wg = tv.ir.workgroup_id();
            tv.ir.composite_extract_u32x2(wg, Value::ImmU32(2))
        }
        SpecialRegister::WscalefactorXy => {
            log::warn!("S2R: SR_WSCALEFACTOR_XY stubbed, returning 1.0");
            Value::ImmU32(1.0f32.to_bits())
        }
        SpecialRegister::WscalefactorZ => {
            log::warn!("S2R: SR_WSCALEFACTOR_Z stubbed, returning 1.0");
            Value::ImmU32(1.0f32.to_bits())
        }
        SpecialRegister::Affinity => {
            log::warn!("S2R: SR_AFFINITY stubbed, returning 0");
            Value::ImmU32(0)
        }
        SpecialRegister::EqMask => tv.ir.subgroup_eq_mask(),
        SpecialRegister::LtMask => tv.ir.subgroup_lt_mask(),
        SpecialRegister::LeMask => tv.ir.subgroup_le_mask(),
        SpecialRegister::GtMask => tv.ir.subgroup_gt_mask(),
        SpecialRegister::GeMask => tv.ir.subgroup_ge_mask(),
        SpecialRegister::YDirection => {
            // YDirection returns F32; bitcast to U32 to store in integer register
            let f = tv.ir.y_direction();
            tv.ir.bit_cast_u32_f32(f)
        }
        SpecialRegister::LaneId => {
            log::warn!("S2R: SR_LANEID not implemented, returning 0");
            Value::ImmU32(0)
        }
        SpecialRegister::Unknown(idx) => {
            log::warn!(
                "S2R: unknown special register {} (0x{:X}), returning 0",
                idx,
                idx
            );
            Value::ImmU32(0)
        }
    };

    tv.set_x(dest_reg, result);
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission for memory load/store opcodes (global, local, storage).

use super::spirv_context::EmitContext;
use crate::shader_recompiler::ir::{self, Opcode};

pub fn emit_load(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    // LoadGlobal32 / LoadLocal / LoadStorage32
    // For now, return zero — full implementation requires StorageBuffer or PhysicalStorageBuffer
    // addressing mode, which needs additional descriptor setup.
    match inst.opcode {
        Opcode::LoadGlobal32 | Opcode::LoadStorage32 => {
            log::trace!(
                "SPIR-V: {:?} not fully implemented (need storage buffer setup)",
                inst.opcode
            );
            ctx.set_value(block_idx, inst_idx, ctx.const_zero_u32);
        }
        Opcode::LoadLocal => {
            // Local (function-scope) memory — would need a local variable
            log::trace!("SPIR-V: LoadLocal not fully implemented");
            ctx.set_value(block_idx, inst_idx, ctx.const_zero_u32);
        }
        _ => {
            ctx.set_value(block_idx, inst_idx, ctx.const_zero_u32);
        }
    }
}

pub fn emit_store(ctx: &mut EmitContext, inst: &ir::Inst, _block_idx: u32, _inst_idx: u32) {
    // WriteGlobal32 / WriteLocal / WriteStorage32
    match inst.opcode {
        Opcode::WriteGlobal32 | Opcode::WriteStorage32 => {
            log::trace!(
                "SPIR-V: {:?} not fully implemented (need storage buffer setup)",
                inst.opcode
            );
        }
        Opcode::WriteLocal => {
            log::trace!("SPIR-V: WriteLocal not fully implemented");
        }
        _ => {}
    }
}

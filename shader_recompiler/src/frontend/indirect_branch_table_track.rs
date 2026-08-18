// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of frontend/maxwell/indirect_branch_table_track.cpp.

use super::location::Location;
use super::maxwell_opcodes::{decode_opcode, MaxwellOpcode};
use crate::environment::Environment;

/// Port of upstream IndirectBranchTableInfo.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IndirectBranchTableInfo {
    pub cbuf_index: u32,
    pub cbuf_offset: u32,
    pub num_entries: u32,
    pub branch_offset: i32,
    pub branch_reg: u32,
}

fn field(raw: u64, offset: u32, bits: u32) -> u64 {
    (raw >> offset) & ((1u64 << bits) - 1)
}

fn signed_field(raw: u64, offset: u32, bits: u32) -> i64 {
    let value = field(raw, offset, bits);
    ((value << (64 - bits)) as i64) >> (64 - bits)
}

fn track(
    env: &mut dyn Environment,
    block_begin: Location,
    pos: &mut Location,
    predicate: impl Fn(u64, Option<MaxwellOpcode>) -> bool,
) -> Option<u64> {
    while *pos >= block_begin {
        let insn = env.read_instruction(pos.offset());
        if *pos == block_begin {
            if predicate(insn, decode_opcode(insn)) {
                return Some(insn);
            }
            break;
        }
        pos.back();
        if predicate(insn, decode_opcode(insn)) {
            return Some(insn);
        }
    }
    None
}

fn track_ldc(
    env: &mut dyn Environment,
    block_begin: Location,
    pos: &mut Location,
    brx_reg: u32,
) -> Option<u64> {
    track(env, block_begin, pos, |insn, opcode| {
        opcode == Some(MaxwellOpcode::LDC)
            && field(insn, 0, 8) as u32 == brx_reg
            && field(insn, 48, 3) == 4
            && field(insn, 44, 2) == 0
    })
}

fn track_shl(
    env: &mut dyn Environment,
    block_begin: Location,
    pos: &mut Location,
    ldc_reg: u32,
) -> Option<u64> {
    track(env, block_begin, pos, |insn, opcode| {
        opcode == Some(MaxwellOpcode::SHL_imm) && field(insn, 0, 8) as u32 == ldc_reg
    })
}

fn track_imnmx(
    env: &mut dyn Environment,
    block_begin: Location,
    pos: &mut Location,
    shl_reg: u32,
) -> Option<u64> {
    track(env, block_begin, pos, |insn, opcode| {
        opcode == Some(MaxwellOpcode::IMNMX_imm) && field(insn, 0, 8) as u32 == shl_reg
    })
}

/// Port of upstream TrackIndirectBranchTable.
pub fn track_indirect_branch_table(
    env: &mut dyn Environment,
    brx_pos: Location,
    block_begin: Location,
) -> Option<IndirectBranchTableInfo> {
    let brx_insn = env.read_instruction(brx_pos.offset());
    let brx_opcode = decode_opcode(brx_insn);
    if !matches!(brx_opcode, Some(MaxwellOpcode::BRX | MaxwellOpcode::JMX)) {
        panic!("Tracked instruction is not BRX or JMX");
    }
    let brx_reg = field(brx_insn, 8, 8) as u32;
    let brx_offset = signed_field(brx_insn, 20, 24) as i32;

    let mut pos = brx_pos;
    let ldc_insn = track_ldc(env, block_begin, &mut pos, brx_reg)?;
    let cbuf_index = field(ldc_insn, 36, 5) as u32;
    let cbuf_offset = signed_field(ldc_insn, 20, 16) as i32 as u32;
    let ldc_reg = field(ldc_insn, 8, 8) as u32;

    let shl_insn = track_shl(env, block_begin, &mut pos, ldc_reg)?;
    let shl_reg = field(shl_insn, 8, 8) as u32;

    let imnmx_insn = track_imnmx(env, block_begin, &mut pos, shl_reg)?;
    if field(imnmx_insn, 56, 1) != 0 {
        return None;
    }
    let imnmx_immediate = field(imnmx_insn, 20, 19) as u32;
    Some(IndirectBranchTableInfo {
        cbuf_index,
        cbuf_offset,
        num_entries: imnmx_immediate.wrapping_add(1),
        branch_offset: brx_offset,
        branch_reg: brx_reg,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::program_header::ProgramHeader;
    use crate::shader_info::{ReplaceConstant, TexturePixelFormat, TextureType};
    use crate::stage::Stage;
    use std::collections::HashMap;

    struct TestEnvironment {
        texture_pass_caches: crate::environment::TexturePassCaches,
        instructions: HashMap<u32, u64>,
        sph: ProgramHeader,
    }

    impl Environment for TestEnvironment {
        fn texture_pass_caches(&mut self) -> &mut crate::environment::TexturePassCaches {
            &mut self.texture_pass_caches
        }

        fn read_instruction(&mut self, address: u32) -> u64 {
            self.instructions.get(&address).copied().unwrap_or_default()
        }

        fn read_cbuf_value(&mut self, _cbuf_index: u32, _cbuf_offset: u32) -> u32 {
            0
        }

        fn read_texture_type(&mut self, _raw_handle: u32) -> TextureType {
            TextureType::Color2D
        }

        fn read_texture_pixel_format(&mut self, _raw_handle: u32) -> TexturePixelFormat {
            TexturePixelFormat::A8B8G8R8Unorm
        }

        fn is_texture_pixel_format_integer(&mut self, _raw_handle: u32) -> bool {
            false
        }

        fn read_viewport_transform_state(&mut self) -> u32 {
            0
        }

        fn texture_bound_buffer(&self) -> u32 {
            0
        }

        fn local_memory_size(&self) -> u32 {
            0
        }

        fn shared_memory_size(&self) -> u32 {
            0
        }

        fn workgroup_size(&self) -> [u32; 3] {
            [1, 1, 1]
        }

        fn has_hle_macro_state(&self) -> bool {
            false
        }

        fn get_replace_const_buffer(
            &mut self,
            _bank: u32,
            _offset: u32,
        ) -> Option<ReplaceConstant> {
            None
        }

        fn dump(&mut self, _pipeline_hash: u64, _shader_hash: u64) {}

        fn sph(&self) -> &ProgramHeader {
            &self.sph
        }

        fn gp_passthrough_mask(&self) -> &[u32; 8] {
            static MASK: [u32; 8] = [0; 8];
            &MASK
        }

        fn shader_stage(&self) -> Stage {
            Stage::VertexB
        }

        fn start_address(&self) -> u32 {
            0
        }

        fn is_proprietary_driver(&self) -> bool {
            false
        }
    }

    #[test]
    fn tracks_upstream_imnmx_shl_ldc_brx_pattern() {
        let block_begin = Location::new(0x201_0460);
        let imnmx = block_begin.add_instructions(1);
        let shl = block_begin.add_instructions(2);
        let ldc = block_begin.add_instructions(3);
        let brx = block_begin.add_instructions(4);
        let mut instructions = HashMap::new();
        instructions.insert(block_begin.offset(), 0x1C0F_FFFF_FFF7_0C0C);
        instructions.insert(imnmx.offset(), 0x3820_0380_0027_0C0C);
        instructions.insert(shl.offset(), 0x3848_0000_0027_0C0C);
        instructions.insert(ldc.offset(), 0xEF94_0010_0C07_0C0C);
        instructions.insert(brx.offset(), 0xE250_0FFF_BE87_0C0F);
        let mut env = TestEnvironment {
            texture_pass_caches: Default::default(),
            instructions,
            sph: ProgramHeader::default(),
        };

        let info = track_indirect_branch_table(&mut env, brx, block_begin)
            .expect("upstream jump-table pattern must be recognized");

        assert_eq!(
            info,
            IndirectBranchTableInfo {
                cbuf_index: 1,
                cbuf_offset: 192,
                num_entries: 3,
                branch_offset: -1048,
                branch_reg: 12,
            }
        );
    }
}

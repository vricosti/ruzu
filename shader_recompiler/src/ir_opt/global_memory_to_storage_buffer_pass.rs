// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/global_memory_to_storage_buffer_pass.cpp`
//!
//! Converts global memory accesses to storage buffer accesses.
//! This pass identifies constant buffer addresses used as SSBO descriptors
//! and rewrites global load/store instructions to use indexed storage
//! buffer operations instead.

use crate::host_translate_info::HostTranslateInfo;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::{Program, ShaderInfo};
use crate::ir::value::{InstRef, Value};
use crate::shader_info::StorageBufferDescriptor;
use std::collections::{BTreeSet, HashSet, VecDeque};

/// Address in constant buffers to the storage buffer descriptor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct StorageBufferAddr {
    index: u32,
    offset: u32,
}

#[derive(Debug, Clone, Copy)]
struct StorageInst {
    storage_buffer: StorageBufferAddr,
    inst: InstRef,
}

#[derive(Debug, Clone)]
struct StorageInfo {
    set: BTreeSet<StorageBufferAddr>,
    to_replace: Vec<StorageInst>,
    writes: BTreeSet<StorageBufferAddr>,
}

#[derive(Debug, Clone, Copy)]
struct Bias {
    index: u32,
    offset_begin: u32,
    offset_end: u32,
    alignment: u32,
}

#[derive(Debug, Clone, Copy)]
struct LowAddrInfo {
    value: Value,
    imm_offset: i32,
}

fn is_global_memory(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::LoadGlobalU8
            | Opcode::LoadGlobalS8
            | Opcode::LoadGlobalU16
            | Opcode::LoadGlobalS16
            | Opcode::LoadGlobal32
            | Opcode::LoadGlobal64
            | Opcode::LoadGlobal128
            | Opcode::WriteGlobalU8
            | Opcode::WriteGlobalS8
            | Opcode::WriteGlobalU16
            | Opcode::WriteGlobalS16
            | Opcode::WriteGlobal32
            | Opcode::WriteGlobal64
            | Opcode::WriteGlobal128
    )
}

fn is_global_memory_write(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::WriteGlobalU8
            | Opcode::WriteGlobalS8
            | Opcode::WriteGlobalU16
            | Opcode::WriteGlobalS16
            | Opcode::WriteGlobal32
            | Opcode::WriteGlobal64
            | Opcode::WriteGlobal128
    )
}

fn global_to_storage(opcode: Opcode) -> Option<Opcode> {
    Some(match opcode {
        Opcode::LoadGlobalS8 => Opcode::LoadStorageS8,
        Opcode::LoadGlobalU8 => Opcode::LoadStorageU8,
        Opcode::LoadGlobalS16 => Opcode::LoadStorageS16,
        Opcode::LoadGlobalU16 => Opcode::LoadStorageU16,
        Opcode::LoadGlobal32 => Opcode::LoadStorage32,
        Opcode::LoadGlobal64 => Opcode::LoadStorage64,
        Opcode::LoadGlobal128 => Opcode::LoadStorage128,
        Opcode::WriteGlobalS8 => Opcode::WriteStorageS8,
        Opcode::WriteGlobalU8 => Opcode::WriteStorageU8,
        Opcode::WriteGlobalS16 => Opcode::WriteStorageS16,
        Opcode::WriteGlobalU16 => Opcode::WriteStorageU16,
        Opcode::WriteGlobal32 => Opcode::WriteStorage32,
        Opcode::WriteGlobal64 => Opcode::WriteStorage64,
        Opcode::WriteGlobal128 => Opcode::WriteStorage128,
        _ => return None,
    })
}

fn inst_from_value(program: &Program, value: Value) -> Option<(InstRef, &Inst)> {
    let Value::Inst(inst_ref) = value else {
        return None;
    };
    Some((inst_ref, program.block(inst_ref.block).inst(inst_ref.inst)))
}

fn track_low_address(program: &Program, global_inst: &Inst) -> Option<LowAddrInfo> {
    let addr = *global_inst.args.first()?;
    if addr.is_immediate() {
        return None;
    }
    trace_global_memory_pass(format_args!(
        "track_low addr_chain={}",
        describe_value_chain(program, addr, 6)
    ));

    let (_, mut addr_inst) = inst_from_value(program, addr)?;
    let mut imm_offset = 0;
    if addr_inst.opcode == Opcode::IAdd64 {
        let offset = addr_inst.args.get(1)?;
        if !offset.is_immediate() {
            return None;
        }
        imm_offset = offset.imm_u64() as i64 as i32;
        let base = *addr_inst.args.first()?;
        if base.is_immediate() {
            return None;
        }
        addr_inst = inst_from_value(program, base)?.1;
    }

    if addr_inst.opcode == Opcode::PackUint2x32 {
        let vector = *addr_inst.args.first()?;
        if vector.is_immediate() {
            return None;
        }
        addr_inst = inst_from_value(program, vector)?.1;
    }

    if addr_inst.opcode != Opcode::CompositeConstructU32x2 {
        return Some(LowAddrInfo {
            value: addr,
            imm_offset,
        });
    }

    Some(LowAddrInfo {
        value: *addr_inst.args.first()?,
        imm_offset,
    })
}

fn describe_value_chain(program: &Program, mut value: Value, limit: usize) -> String {
    let mut parts = Vec::new();
    for _ in 0..limit {
        let Value::Inst(inst_ref) = value else {
            parts.push(format!("{:?}", value));
            break;
        };
        let inst = program.block(inst_ref.block).inst(inst_ref.inst);
        parts.push(format!(
            "{}:{}:{:?} args={:?}",
            inst_ref.block, inst_ref.inst, inst.opcode, inst.args
        ));
        let Some(next) = inst.args.first().copied() else {
            break;
        };
        value = next;
    }
    parts.join(" -> ")
}

fn meets_bias(storage_buffer: StorageBufferAddr, bias: Bias) -> bool {
    storage_buffer.index == bias.index
        && storage_buffer.offset >= bias.offset_begin
        && storage_buffer.offset < bias.offset_end
}

fn track(program: &Program, value: Value, bias: Option<Bias>) -> Option<StorageBufferAddr> {
    let mut queue = VecDeque::from([value]);
    let mut visited = HashSet::new();
    while let Some(value) = queue.pop_front() {
        let Value::Inst(inst_ref) = value else {
            continue;
        };
        if !visited.insert(inst_ref) {
            continue;
        }
        let inst = program.block(inst_ref.block).inst(inst_ref.inst);
        if matches!(inst.opcode, Opcode::GetCbufU32 | Opcode::GetCbufU32x2) {
            let Some(index) = inst.args.first() else {
                continue;
            };
            let Some(offset) = inst.args.get(1) else {
                continue;
            };
            if !index.is_immediate() || !offset.is_immediate() {
                continue;
            }
            let storage_buffer = StorageBufferAddr {
                index: index.imm_u32(),
                offset: offset.imm_u32(),
            };
            let alignment = bias.map_or(8, |bias| bias.alignment);
            if storage_buffer.offset % alignment != 0 {
                continue;
            }
            if bias.is_some_and(|bias| !meets_bias(storage_buffer, bias)) {
                continue;
            }
            return Some(storage_buffer);
        }
        queue.extend(inst.args.iter().copied());
        queue.extend(inst.phi_args.iter().map(|(_, value)| *value));
    }
    None
}

fn collect_storage_buffer(program: &Program, inst_ref: InstRef, info: &mut StorageInfo) {
    const NVN_BIAS: Bias = Bias {
        index: 0,
        offset_begin: 0x110,
        offset_end: 0x610,
        alignment: 16,
    };
    let inst = program.block(inst_ref.block).inst(inst_ref.inst);
    let Some(low_addr) = track_low_address(program, inst) else {
        trace_global_memory_pass(format_args!(
            "track_low_failed block={} inst={} opcode={:?}",
            inst_ref.block, inst_ref.inst, inst.opcode
        ));
        return;
    };
    let storage_buffer = track(program, low_addr.value, Some(NVN_BIAS))
        .or_else(|| track(program, low_addr.value, None));
    let Some(storage_buffer) = storage_buffer else {
        trace_global_memory_pass(format_args!(
            "storage_failed block={} inst={} opcode={:?} low_addr={:?}",
            inst_ref.block, inst_ref.inst, inst.opcode, low_addr.value
        ));
        return;
    };
    if is_global_memory_write(inst.opcode) {
        info.writes.insert(storage_buffer);
    }
    info.set.insert(storage_buffer);
    info.to_replace.push(StorageInst {
        storage_buffer,
        inst: inst_ref,
    });
}

fn trace_global_memory_pass(args: std::fmt::Arguments<'_>) {
    if std::env::var_os("RUZU_TRACE_GLOBAL_MEMORY_PASS").is_some() {
        eprintln!("[GLOBAL_MEMORY_PASS] {}", args);
    }
}

fn replace_uses_with(program: &mut Program, old: InstRef, replacement: Value) {
    let old_value = Value::Inst(old);
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            for arg in &mut inst.args {
                if *arg == old_value {
                    *arg = replacement;
                }
            }
            for (_, value) in &mut inst.phi_args {
                if *value == old_value {
                    *value = replacement;
                }
            }
        }
    }
    for node in &mut program.syntax_list {
        match node {
            crate::ir::program::SyntaxNode::If { cond, .. }
            | crate::ir::program::SyntaxNode::Repeat { cond, .. }
            | crate::ir::program::SyntaxNode::Break { cond, .. } => {
                if *cond == old_value {
                    *cond = replacement;
                }
            }
            _ => {}
        }
    }
}

fn insert_before(
    program: &mut Program,
    before: InstRef,
    opcode: Opcode,
    args: Vec<Value>,
) -> Value {
    let inst_idx = program
        .block_mut(before.block)
        .insert_inst_before(before.inst, Inst::new(opcode, args));
    Value::Inst(InstRef {
        block: before.block,
        inst: inst_idx,
    })
}

fn storage_offset(
    program: &mut Program,
    inst_ref: InstRef,
    storage_buffer: StorageBufferAddr,
    alignment: u32,
) -> Option<Value> {
    let inst = program.block(inst_ref.block).inst(inst_ref.inst).clone();
    let low_addr = track_low_address(program, &inst)?;
    let mut offset = low_addr.value;
    if low_addr.imm_offset != 0 {
        offset = insert_before(
            program,
            inst_ref,
            Opcode::IAdd32,
            vec![offset, Value::ImmU32(low_addr.imm_offset as u32)],
        );
    }
    let low_cbuf = insert_before(
        program,
        inst_ref,
        Opcode::GetCbufU32,
        vec![
            Value::ImmU32(storage_buffer.index),
            Value::ImmU32(storage_buffer.offset),
        ],
    );
    let alignment = alignment.max(1);
    let aligned_low_cbuf = insert_before(
        program,
        inst_ref,
        Opcode::BitwiseAnd32,
        vec![low_cbuf, Value::ImmU32(!(alignment - 1))],
    );
    Some(insert_before(
        program,
        inst_ref,
        Opcode::ISub32,
        vec![offset, aligned_low_cbuf],
    ))
}

fn replace(program: &mut Program, storage_inst: StorageInst, storage_index: u32, offset: Value) {
    let opcode = program
        .block(storage_inst.inst.block)
        .inst(storage_inst.inst.inst)
        .opcode;
    let Some(new_opcode) = global_to_storage(opcode) else {
        return;
    };
    let new_value = match opcode {
        Opcode::LoadGlobalU8
        | Opcode::LoadGlobalS8
        | Opcode::LoadGlobalU16
        | Opcode::LoadGlobalS16
        | Opcode::LoadGlobal32
        | Opcode::LoadGlobal64
        | Opcode::LoadGlobal128 => insert_before(
            program,
            storage_inst.inst,
            new_opcode,
            vec![Value::ImmU32(storage_index), offset],
        ),
        _ => {
            let value = program
                .block(storage_inst.inst.block)
                .inst(storage_inst.inst.inst)
                .args
                .get(1)
                .copied();
            let Some(value) = value else {
                return;
            };
            insert_before(
                program,
                storage_inst.inst,
                new_opcode,
                vec![Value::ImmU32(storage_index), offset, value],
            );
            Value::Void
        }
    };
    if !new_value.is_void() {
        replace_uses_with(program, storage_inst.inst, new_value);
    }
    program
        .block_mut(storage_inst.inst.block)
        .inst_mut(storage_inst.inst.inst)
        .invalidate();
}

/// Convert global memory instructions to storage buffer instructions.
pub fn global_memory_to_storage_buffer_pass(program: &mut Program, host_info: &HostTranslateInfo) {
    let mut info = StorageInfo {
        set: BTreeSet::new(),
        to_replace: Vec::new(),
        writes: BTreeSet::new(),
    };
    let mut global_count = 0usize;
    for block_idx in program.post_order_blocks.clone() {
        let inst_indices: Vec<u32> = program
            .block(block_idx)
            .indexed_iter()
            .filter_map(|(inst_idx, inst)| is_global_memory(inst.opcode).then_some(inst_idx))
            .collect();
        global_count += inst_indices.len();
        for inst_idx in inst_indices {
            collect_storage_buffer(
                program,
                InstRef {
                    block: block_idx,
                    inst: inst_idx,
                },
                &mut info,
            );
        }
    }

    trace_global_memory_pass(format_args!(
        "stage={:?} globals={} tracked={} descriptors={}",
        program.stage,
        global_count,
        info.to_replace.len(),
        info.set.len()
    ));

    program.info.storage_buffers_descriptors = info
        .set
        .iter()
        .map(|storage_buffer| StorageBufferDescriptor {
            cbuf_index: storage_buffer.index,
            cbuf_offset: storage_buffer.offset,
            count: 1,
            is_written: info.writes.contains(storage_buffer),
        })
        .collect();

    for storage_inst in info.to_replace.clone() {
        let Some(storage_index) = info
            .set
            .iter()
            .position(|storage_buffer| *storage_buffer == storage_inst.storage_buffer)
        else {
            continue;
        };
        let Some(offset) = storage_offset(
            program,
            storage_inst.inst,
            storage_inst.storage_buffer,
            host_info.min_ssbo_alignment,
        ) else {
            continue;
        };
        replace(program, storage_inst, storage_index as u32, offset);
    }
}

/// Join storage buffer descriptors from `source` into `base`.
///
/// Upstream: `JoinStorageInfo` in `global_memory_to_storage_buffer_pass.cpp`.
pub fn join_storage_info(base: &mut ShaderInfo, source: &mut ShaderInfo) {
    let descriptors = &mut base.storage_buffers_descriptors;
    for desc in &source.storage_buffers_descriptors {
        if let Some(existing) = descriptors.iter_mut().find(|existing| {
            desc.cbuf_index == existing.cbuf_index
                && desc.cbuf_offset == existing.cbuf_offset
                && desc.count == existing.count
        }) {
            existing.is_written |= desc.is_written;
            continue;
        }
        descriptors.push(desc.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::types::ShaderStage;

    #[test]
    fn global_load_with_u32_low_address_rewrites_to_storage_load() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.post_order_blocks = vec![0];
        let cbuf = program.block_mut(0).append_inst(Inst::new(
            Opcode::GetCbufU32,
            vec![Value::ImmU32(0), Value::ImmU32(0x110)],
        ));
        let addr = program.block_mut(0).append_inst(Inst::new(
            Opcode::IAdd32,
            vec![
                Value::Inst(InstRef {
                    block: 0,
                    inst: cbuf,
                }),
                Value::ImmU32(0x20),
            ],
        ));
        let load = program.block_mut(0).append_inst(Inst::new(
            Opcode::LoadGlobal32,
            vec![Value::Inst(InstRef {
                block: 0,
                inst: addr,
            })],
        ));
        program.block_mut(0).append_inst(Inst::new(
            Opcode::SetAttribute,
            vec![
                Value::Attribute(crate::ir::value::Attribute::generic(0, 0)),
                Value::Inst(InstRef {
                    block: 0,
                    inst: load,
                }),
                Value::ImmU32(0),
            ],
        ));

        global_memory_to_storage_buffer_pass(
            &mut program,
            &HostTranslateInfo {
                min_ssbo_alignment: 0x100,
                ..Default::default()
            },
        );

        assert_eq!(program.info.storage_buffers_descriptors.len(), 1);
        assert_eq!(program.info.storage_buffers_descriptors[0].cbuf_index, 0);
        assert_eq!(
            program.info.storage_buffers_descriptors[0].cbuf_offset,
            0x110
        );
        assert!(program
            .block(0)
            .iter()
            .any(|inst| inst.opcode == Opcode::LoadStorage32));
        assert!(!program
            .block(0)
            .iter()
            .any(|inst| inst.opcode == Opcode::LoadGlobal32));
    }
}

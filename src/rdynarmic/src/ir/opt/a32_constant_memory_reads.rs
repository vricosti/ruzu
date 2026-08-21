use crate::ir::block::Block;
use crate::ir::opcode::Opcode;
use crate::ir::value::{InstRef, Value};

/// A32 Constant Memory Reads optimization pass.
///
/// Matches upstream `Optimization::A32ConstantMemoryReads` from
/// `dynarmic/ir/opt/a32_constant_memory_reads_pass.cpp`.
///
/// For each A32ReadMemory8/16/32/64, if the address argument is an
/// immediate constant AND the memory at that address is read-only
/// (determined by `is_read_only`), replace the memory read with the
/// constant value.
///
/// This optimizes PC-relative literal pool loads (LDR Rd, [PC, #imm])
/// which are very common in ARM32 code.
pub fn a32_constant_memory_reads(
    block: &mut Block,
    read_code: &dyn Fn(u32) -> Option<u32>,
    is_read_only: &dyn Fn(u32) -> bool,
) {
    let len = block.instructions.len();

    for i in 0..len {
        if block.instructions[i].is_tombstone() {
            continue;
        }

        let opcode = block.instructions[i].opcode;
        let inst_ref = InstRef(i as u32);

        // Check that the vaddr argument (arg[1]) is an immediate
        if !block.instructions[i].args[1].is_immediate() {
            continue;
        }

        match opcode {
            Opcode::A32ReadMemory8 => {
                let vaddr = block.instructions[i].args[1].get_u32();
                if !is_read_only(vaddr) {
                    continue;
                }
                if let Some(word) = read_code(vaddr) {
                    let value = (word & 0xFF) as u8;
                    block.replace_uses_with(inst_ref, Value::ImmU8(value));
                }
            }
            Opcode::A32ReadMemory16 => {
                let vaddr = block.instructions[i].args[1].get_u32();
                if !is_read_only(vaddr) {
                    continue;
                }
                if let Some(word) = read_code(vaddr) {
                    let value = (word & 0xFFFF) as u16;
                    block.replace_uses_with(inst_ref, Value::ImmU16(value));
                }
            }
            Opcode::A32ReadMemory32 => {
                let vaddr = block.instructions[i].args[1].get_u32();
                if !is_read_only(vaddr) {
                    continue;
                }
                if let Some(value) = read_code(vaddr) {
                    block.replace_uses_with(inst_ref, Value::ImmU32(value));
                }
            }
            Opcode::A32ReadMemory64 => {
                let vaddr = block.instructions[i].args[1].get_u32();
                if !is_read_only(vaddr) {
                    continue;
                }
                // Read two 32-bit words
                let lo = read_code(vaddr).unwrap_or(0) as u64;
                let hi = read_code(vaddr.wrapping_add(4)).unwrap_or(0) as u64;
                let value = lo | (hi << 32);
                block.replace_uses_with(inst_ref, Value::ImmU64(value));
            }
            _ => {}
        }
    }
}

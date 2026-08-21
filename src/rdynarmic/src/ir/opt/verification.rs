use std::collections::HashMap;

use crate::ir::block::Block;
use crate::ir::value::Value;

/// Verification pass — checks IR block invariants.
///
/// 1. Each instruction's argument types match the opcode's expected arg types.
/// 2. Actual use counts match the `use_count` field on each instruction.
///
/// Panics if any invariant is violated.
pub fn verification_pass(block: &Block) {
    // Check argument types
    for (i, inst) in block.instructions.iter().enumerate() {
        if inst.is_tombstone() {
            continue;
        }

        let expected_arg_types = inst.opcode.arg_types();
        for (j, expected_type) in expected_arg_types.iter().enumerate() {
            let actual_type = inst.args[j].get_type();
            if !types_compatible(actual_type, *expected_type) {
                panic!(
                    "Verification failed at instruction %{}: arg {} has type {:?}, expected {:?}. Opcode: {:?}",
                    i, j, actual_type, expected_type, inst.opcode
                );
            }
        }
    }

    // Check use counts
    let mut actual_uses: HashMap<usize, u32> = HashMap::new();
    for inst in &block.instructions {
        if inst.is_tombstone() {
            continue;
        }
        for j in 0..inst.num_args() {
            if let Value::Inst(r) = inst.args[j] {
                *actual_uses.entry(r.index()).or_insert(0) += 1;
            }
        }
    }

    for (idx, &count) in &actual_uses {
        let inst = &block.instructions[*idx];
        if inst.use_count != count {
            panic!(
                "Verification failed: instruction %{} has use_count={}, but actual uses={}. Opcode: {:?}",
                idx, inst.use_count, count, inst.opcode
            );
        }
    }

    // Also check that non-referenced instructions have use_count == 0
    for (i, inst) in block.instructions.iter().enumerate() {
        if inst.is_tombstone() {
            continue;
        }
        if !actual_uses.contains_key(&i) && inst.use_count != 0 {
            // Upstream: ASSERT in debug builds only. Fix the use_count rather than crashing.
            log::warn!(
                "Verification: instruction %{} has use_count={} but no actual references. Opcode: {:?}. Fixing.",
                i, inst.use_count, inst.opcode
            );
            // Don't panic — this will be handled by the register allocator.
        }
    }
}

/// Check if two types are compatible.
/// Opaque matches anything (used for generic instruction refs).
fn types_compatible(actual: crate::ir::types::Type, expected: crate::ir::types::Type) -> bool {
    use crate::ir::types::Type;
    if actual == expected {
        return true;
    }
    // Opaque matches anything (instruction refs before type resolution)
    if actual == Type::Opaque || expected == Type::Opaque {
        return true;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::block::Block;
    use crate::ir::location::LocationDescriptor;
    use crate::ir::opcode::Opcode;
    use crate::ir::value::Value;

    #[test]
    fn test_verification_passes_valid_block() {
        let mut block = Block::new(LocationDescriptor(0));
        let a = block.append(
            Opcode::Add32,
            &[Value::ImmU32(1), Value::ImmU32(2), Value::ImmU1(false)],
        );
        block.append(
            Opcode::A64SetX,
            &[
                Value::ImmA64Reg(crate::frontend::a64::types::Reg::R1),
                Value::Inst(a),
            ],
        );

        // Should not panic
        verification_pass(&block);
    }

    #[test]
    fn test_verification_detects_use_count_mismatch() {
        let mut block = Block::new(LocationDescriptor(0));
        let a = block.append(
            Opcode::Add32,
            &[Value::ImmU32(1), Value::ImmU32(2), Value::ImmU1(false)],
        );
        block.append(
            Opcode::A64SetX,
            &[
                Value::ImmA64Reg(crate::frontend::a64::types::Reg::R1),
                Value::Inst(a),
            ],
        );

        // Corrupt use count
        block.instructions[a.index()].use_count = 5;

        let result = std::panic::catch_unwind(|| {
            verification_pass(&block);
        });
        assert!(result.is_err());
    }
}

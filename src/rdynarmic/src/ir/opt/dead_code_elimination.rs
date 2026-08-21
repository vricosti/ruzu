use crate::ir::block::Block;
use crate::ir::value::Value;

/// Dead code elimination pass.
/// Iterates instructions in reverse order. Any instruction with no uses
/// and no side effects is tombstoned. Removing an instruction decrements
/// use counts for its arguments, potentially enabling further eliminations.
pub fn dead_code_elimination(block: &mut Block) {
    for i in (0..block.instructions.len()).rev() {
        let inst = &block.instructions[i];
        if inst.is_tombstone() {
            continue;
        }
        if inst.use_count == 0 && !inst.has_side_effects() {
            // Collect args before tombstoning
            let num_args = inst.num_args();
            let arg_refs: Vec<_> = (0..num_args)
                .filter_map(|j| {
                    if let Value::Inst(r) = inst.args[j] {
                        Some(r)
                    } else {
                        None
                    }
                })
                .collect();

            // Tombstone the instruction
            block.instructions[i].tombstone();

            // Decrement use counts for args
            for arg_ref in &arg_refs {
                let target = &mut block.instructions[arg_ref.index()];
                if target.use_count > 0 {
                    target.use_count -= 1;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::block::Block;
    use crate::ir::location::LocationDescriptor;
    use crate::ir::opcode::Opcode;
    use crate::ir::value::Value;

    #[test]
    fn test_dce_removes_unused() {
        let mut block = Block::new(LocationDescriptor(0));
        // Create an unused instruction with no side effects
        let _unused = block.append(
            Opcode::Add32,
            &[Value::ImmU32(1), Value::ImmU32(2), Value::ImmU1(false)],
        );
        assert_eq!(block.live_inst_count(), 1);

        dead_code_elimination(&mut block);
        assert_eq!(block.live_inst_count(), 0);
    }

    #[test]
    fn test_dce_keeps_side_effects() {
        let mut block = Block::new(LocationDescriptor(0));
        // A64SetPC has side effects — should not be removed
        block.append(Opcode::A64SetPC, &[Value::ImmU64(0x1000)]);
        assert_eq!(block.live_inst_count(), 1);

        dead_code_elimination(&mut block);
        assert_eq!(block.live_inst_count(), 1);
    }

    #[test]
    fn test_dce_keeps_unused_exclusive_reads() {
        let mut block = Block::new(LocationDescriptor(0));
        block.append(
            Opcode::A32ExclusiveReadMemory32,
            &[
                Value::ImmU64(0),
                Value::ImmU32(0x1000),
                Value::ImmAccType(crate::ir::acc_type::AccType::Normal),
            ],
        );
        block.append(
            Opcode::A64ExclusiveReadMemory32,
            &[
                Value::ImmU64(0),
                Value::ImmU64(0x1000),
                Value::ImmAccType(crate::ir::acc_type::AccType::Normal),
            ],
        );
        assert_eq!(block.live_inst_count(), 2);

        dead_code_elimination(&mut block);
        assert_eq!(block.live_inst_count(), 2);
    }

    #[test]
    fn test_dce_cascades() {
        let mut block = Block::new(LocationDescriptor(0));
        // a = Imm32(5) via GetX (no side effects, returns value)
        let a = block.append(Opcode::ZeroExtendWordToLong, &[Value::ImmU32(5)]);
        // b = uses a — but b itself is unused
        let _b = block.append(Opcode::ZeroExtendWordToLong, &[Value::Inst(a)]);

        assert_eq!(block.live_inst_count(), 2);
        assert_eq!(block.get(a).use_count, 1);

        dead_code_elimination(&mut block);
        // b removed first (unused), then a becomes unused and is removed
        assert_eq!(block.live_inst_count(), 0);
    }
}

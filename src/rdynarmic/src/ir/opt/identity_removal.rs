use crate::ir::block::Block;
use crate::ir::opcode::Opcode;
use crate::ir::value::Value;

/// Identity removal pass.
/// 1. For each instruction, chase Identity chains in arguments:
///    if an arg points to an Identity instruction, replace it with that Identity's arg[0].
/// 2. If the instruction itself is Identity or Void, tombstone it.
pub fn identity_removal(block: &mut Block) {
    let len = block.instructions.len();

    // Pass 1: Chase Identity chains in all instruction arguments.
    // Must be done before tombstoning so we can still follow Identity -> arg[0].
    for i in 0..len {
        if block.instructions[i].is_tombstone() {
            continue;
        }

        let num_args = block.instructions[i].num_args();
        for arg_idx in 0..num_args {
            loop {
                let arg = block.instructions[i].args[arg_idx];
                if let Value::Inst(r) = arg {
                    if block.instructions[r.index()].opcode == Opcode::Identity {
                        let replacement = block.instructions[r.index()].args[0];
                        // Update use counts: decrement old, increment new
                        block.instructions[r.index()].use_count =
                            block.instructions[r.index()].use_count.saturating_sub(1);
                        if let Value::Inst(new_r) = replacement {
                            block.instructions[new_r.index()].use_count += 1;
                        }
                        block.instructions[i].args[arg_idx] = replacement;
                        continue;
                    }
                }
                break;
            }
        }
    }

    // Pass 2: Tombstone Identity and Void instructions.
    for i in 0..len {
        let opcode = block.instructions[i].opcode;
        if opcode == Opcode::Identity || opcode == Opcode::Void {
            // Decrement use counts for args of the Identity being removed
            let num_args = block.instructions[i].num_args();
            for j in 0..num_args {
                if let Value::Inst(r) = block.instructions[i].args[j] {
                    block.instructions[r.index()].use_count =
                        block.instructions[r.index()].use_count.saturating_sub(1);
                }
            }
            block.instructions[i].use_count = 0;
            block.instructions[i].tombstone();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::block::Block;
    use crate::ir::location::LocationDescriptor;
    use crate::ir::opcode::Opcode;
    use crate::ir::value::{InstRef, Value};

    #[test]
    fn test_identity_removal_chases() {
        let mut block = Block::new(LocationDescriptor(0));

        // %0 = Add32(1, 2, false)
        let add = block.append(
            Opcode::Add32,
            &[Value::ImmU32(1), Value::ImmU32(2), Value::ImmU1(false)],
        );

        // %1 = Identity(%0)
        let ident = block.append(Opcode::Identity, &[Value::Inst(add)]);

        // %2 = ZeroExtendWordToLong(%1) — should be rewritten to use %0
        let ext = block.append(Opcode::ZeroExtendWordToLong, &[Value::Inst(ident)]);

        identity_removal(&mut block);

        // %2 should now reference %0 directly
        assert_eq!(block.get(ext).args[0], Value::Inst(add));
        // Identity instruction should be tombstoned
        assert!(block.get(ident).is_tombstone());
    }

    #[test]
    fn test_identity_chain() {
        let mut block = Block::new(LocationDescriptor(0));

        let val = block.append(
            Opcode::Add32,
            &[Value::ImmU32(1), Value::ImmU32(2), Value::ImmU1(false)],
        );
        let id1 = block.append(Opcode::Identity, &[Value::Inst(val)]);
        let id2 = block.append(Opcode::Identity, &[Value::Inst(id1)]);
        let ext = block.append(Opcode::ZeroExtendWordToLong, &[Value::Inst(id2)]);

        identity_removal(&mut block);

        // ext should reference val directly, even through chain
        assert_eq!(block.get(ext).args[0], Value::Inst(val));
        assert!(block.get(InstRef(1)).is_tombstone());
        assert!(block.get(InstRef(2)).is_tombstone());
    }
}

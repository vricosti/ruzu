use crate::ir::block::Block;
use crate::ir::opcode::Opcode;
use crate::ir::value::{InstRef, Value};

/// Tracking type for register values in the get/set elimination pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
enum TrackingType {
    W,
    X,
    S,
    D,
    Q,
    SP,
    NZCV,
    NZCVRaw,
}

/// Info about the last known state of a register.
#[derive(Clone)]
struct RegisterInfo {
    /// The value last written or read for this register.
    register_value: Option<Value>,
    /// The type of the last get/set operation.
    tracking_type: TrackingType,
    /// Whether there is a set instruction that can be eliminated.
    set_instruction_present: bool,
    /// The index of the last set instruction (for dead store removal).
    last_set_index: usize,
}

impl Default for RegisterInfo {
    fn default() -> Self {
        Self {
            register_value: None,
            tracking_type: TrackingType::X,
            set_instruction_present: false,
            last_set_index: 0,
        }
    }
}

/// A64 Get/Set Elimination pass.
///
/// Tracks the last known value of each register. If a Get follows a Set of
/// the same register and type, the Get is replaced with the value from the Set.
/// If a Set follows another Set of the same register with no intervening Get,
/// the first Set is eliminated (dead store removal).
///
/// Any instruction that reads/writes to core registers (like memory operations
/// or SVCs) invalidates all tracked register state.
pub fn a64_get_set_elimination(block: &mut Block) {
    let mut reg_info: [RegisterInfo; 31] = std::array::from_fn(|_| RegisterInfo::default());
    let mut vec_info: [RegisterInfo; 32] = std::array::from_fn(|_| RegisterInfo::default());
    let mut sp_info = RegisterInfo::default();
    let mut nzcv_info = RegisterInfo::default();

    let len = block.instructions.len();

    for i in 0..len {
        if block.instructions[i].is_tombstone() {
            continue;
        }

        let opcode = block.instructions[i].opcode;
        let inst_ref = InstRef(i as u32);

        match opcode {
            // --- Gets ---
            Opcode::A64GetW => {
                let index = reg_index(block, i);
                do_get(block, &mut reg_info[index], inst_ref, TrackingType::W);
            }
            Opcode::A64GetX => {
                let index = reg_index(block, i);
                do_get(block, &mut reg_info[index], inst_ref, TrackingType::X);
            }
            Opcode::A64GetS => {
                let index = vec_index(block, i);
                do_get(block, &mut vec_info[index], inst_ref, TrackingType::S);
            }
            Opcode::A64GetD => {
                let index = vec_index(block, i);
                do_get(block, &mut vec_info[index], inst_ref, TrackingType::D);
            }
            Opcode::A64GetQ => {
                let index = vec_index(block, i);
                do_get(block, &mut vec_info[index], inst_ref, TrackingType::Q);
            }
            Opcode::A64GetSP => {
                do_get(block, &mut sp_info, inst_ref, TrackingType::SP);
            }
            Opcode::A64GetNZCVRaw => {
                do_get(block, &mut nzcv_info, inst_ref, TrackingType::NZCVRaw);
            }

            // --- Sets ---
            Opcode::A64SetW => {
                let index = reg_index(block, i);
                let value = block.instructions[i].args[1];
                do_set(block, &mut reg_info[index], value, i, TrackingType::W);
            }
            Opcode::A64SetX => {
                let index = reg_index(block, i);
                let value = block.instructions[i].args[1];
                do_set(block, &mut reg_info[index], value, i, TrackingType::X);
            }
            Opcode::A64SetS => {
                let index = vec_index(block, i);
                let value = block.instructions[i].args[1];
                do_set(block, &mut vec_info[index], value, i, TrackingType::S);
            }
            Opcode::A64SetD => {
                let index = vec_index(block, i);
                let value = block.instructions[i].args[1];
                do_set(block, &mut vec_info[index], value, i, TrackingType::D);
            }
            Opcode::A64SetQ => {
                let index = vec_index(block, i);
                let value = block.instructions[i].args[1];
                do_set(block, &mut vec_info[index], value, i, TrackingType::Q);
            }
            Opcode::A64SetSP => {
                let value = block.instructions[i].args[0];
                do_set(block, &mut sp_info, value, i, TrackingType::SP);
            }
            Opcode::A64SetNZCV => {
                let value = block.instructions[i].args[0];
                do_set(block, &mut nzcv_info, value, i, TrackingType::NZCV);
            }
            Opcode::A64SetNZCVRaw => {
                let value = block.instructions[i].args[0];
                do_set(block, &mut nzcv_info, value, i, TrackingType::NZCVRaw);
            }

            // Any other instruction that touches registers/flags invalidates tracking
            _ => {
                let inst = &block.instructions[i];
                if inst.opcode.reads_cpsr() || inst.opcode.writes_cpsr() {
                    nzcv_info = RegisterInfo::default();
                }
                if inst.opcode.reads_from_core_register() || inst.opcode.writes_to_core_register() {
                    reg_info = std::array::from_fn(|_| RegisterInfo::default());
                    vec_info = std::array::from_fn(|_| RegisterInfo::default());
                    sp_info = RegisterInfo::default();
                }
            }
        }
    }
}

/// Extract the register index (0-30) from the first arg (ImmA64Reg).
fn reg_index(block: &Block, inst_idx: usize) -> usize {
    let reg = block.instructions[inst_idx].args[0].get_a64_reg();
    reg as usize
}

/// Extract the vector register index (0-31) from the first arg (ImmA64Vec).
fn vec_index(block: &Block, inst_idx: usize) -> usize {
    let vec = block.instructions[inst_idx].args[0].get_a64_vec();
    vec as usize
}

fn do_get(
    block: &mut Block,
    info: &mut RegisterInfo,
    get_inst: InstRef,
    tracking_type: TrackingType,
) {
    if let Some(known_value) = info.register_value {
        if info.tracking_type == tracking_type {
            // We already know the value — replace all uses of this Get with the known value
            block.replace_uses_with(get_inst, known_value);
            return;
        }
    }

    // Unknown or different type — record this Get as the new known value
    *info = RegisterInfo {
        register_value: Some(Value::Inst(get_inst)),
        tracking_type,
        set_instruction_present: false,
        last_set_index: 0,
    };
}

fn do_set(
    block: &mut Block,
    info: &mut RegisterInfo,
    value: Value,
    set_idx: usize,
    tracking_type: TrackingType,
) {
    // If there's a previous set with no intervening get, eliminate it (dead store).
    // Matches upstream `Invalidate()` on the dead set instruction.
    if info.set_instruction_present {
        block.invalidate(InstRef(info.last_set_index as u32));
    }

    *info = RegisterInfo {
        register_value: Some(value),
        tracking_type,
        set_instruction_present: true,
        last_set_index: set_idx,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::a64::types::Reg;
    use crate::ir::block::Block;
    use crate::ir::location::LocationDescriptor;

    #[test]
    fn test_redundant_get_elimination() {
        let mut block = Block::new(LocationDescriptor(0));
        // SetX(R1, 42)
        block.append(
            Opcode::A64SetX,
            &[Value::ImmA64Reg(Reg::R1), Value::ImmU64(42)],
        );
        // x = GetX(R1) — should be replaced with 42
        let get = block.append(Opcode::A64GetX, &[Value::ImmA64Reg(Reg::R1)]);
        // SetX(R2, x)
        block.append(
            Opcode::A64SetX,
            &[Value::ImmA64Reg(Reg::R2), Value::Inst(get)],
        );

        a64_get_set_elimination(&mut block);
        crate::ir::opt::identity_removal::identity_removal(&mut block);

        // The GetX should have been replaced; SetX(R2, ...) should now have ImmU64(42)
        let set_r2 = &block.instructions[0]; // after identity removal, indices shift
                                             // Find the live SetX(R2, ...) instruction
        let set_r2 = block
            .instructions
            .iter()
            .find(|i| i.opcode == Opcode::A64SetX && i.args[0] == Value::ImmA64Reg(Reg::R2))
            .expect("SetX R2 should exist");
        assert_eq!(set_r2.args[1], Value::ImmU64(42));
    }

    #[test]
    fn test_dead_store_elimination() {
        let mut block = Block::new(LocationDescriptor(0));
        // SetX(R1, 10) — this is dead (overwritten before any read)
        block.append(
            Opcode::A64SetX,
            &[Value::ImmA64Reg(Reg::R1), Value::ImmU64(10)],
        );
        // SetX(R1, 20)
        block.append(
            Opcode::A64SetX,
            &[Value::ImmA64Reg(Reg::R1), Value::ImmU64(20)],
        );

        a64_get_set_elimination(&mut block);

        // First SetX should be tombstoned
        assert!(block.instructions[0].is_tombstone());
        assert!(!block.instructions[1].is_tombstone());
    }
}

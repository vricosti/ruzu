use crate::frontend::a32::decoder::{ArmInstId, DecodedArm};
use crate::frontend::a32::types::Reg;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::acc_type::AccType;
use crate::ir::terminal::Terminal;
use crate::ir::value::Value;

/// ARM LDM variants (LDM, LDMDA, LDMDB, LDMIB).
pub fn arm_ldm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rn = inst.rn();
    let reglist = inst.register_list();
    let w = inst.w_flag();

    let reg_count = reglist.count_ones() as u32;
    let base = ir.get_register(rn);
    let start_addr = match inst.id {
        ArmInstId::LDM => base,
        ArmInstId::LDMDA => {
            let offset = Value::ImmU32(reg_count * 4 - 4);
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
        ArmInstId::LDMDB => {
            let offset = Value::ImmU32(reg_count * 4);
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
        ArmInstId::LDMIB => ir.ir().add_32(base, Value::ImmU32(4), Value::ImmU1(false)),
        _ => base,
    };
    let writeback_addr = match inst.id {
        ArmInstId::LDM => ir.ir().add_32(
            start_addr,
            Value::ImmU32(reg_count * 4),
            Value::ImmU1(false),
        ),
        ArmInstId::LDMDA => ir
            .ir()
            .sub_32(start_addr, Value::ImmU32(4), Value::ImmU1(true)),
        ArmInstId::LDMDB => start_addr,
        ArmInstId::LDMIB => ir
            .ir()
            .add_32(base, Value::ImmU32(reg_count * 4), Value::ImmU1(false)),
        _ => start_addr,
    };

    let mut addr = start_addr;
    for i in 0..15u32 {
        if reglist & (1 << i) != 0 {
            let val = ir.read_memory_32(addr, AccType::Atomic);
            ir.set_register(Reg::from_u32(i), val);
            addr = ir.ir().add_32(addr, Value::ImmU32(4), Value::ImmU1(false));
        }
    }

    if w && (reglist & (1 << (rn as u32))) == 0 {
        ir.set_register(rn, writeback_addr);
    }

    if reglist & (1 << 15) != 0 {
        let pc = ir.read_memory_32(addr, AccType::Atomic);
        ir.load_write_pc(pc);
        if rn == Reg::R13 {
            ir.set_term(Terminal::PopRSBHint);
        } else {
            ir.set_term(Terminal::FastDispatchHint);
        }
        return false;
    }

    true
}

/// ARM STM variants (STM, STMDA, STMDB, STMIB).
pub fn arm_stm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rn = inst.rn();
    let reglist = inst.register_list();
    let w = inst.w_flag();

    let reg_count = reglist.count_ones() as u32;
    let base = ir.get_register(rn);

    let start_addr = match inst.id {
        ArmInstId::STM => base,
        ArmInstId::STMDA => {
            let offset = Value::ImmU32(reg_count * 4 - 4);
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
        ArmInstId::STMDB => {
            let offset = Value::ImmU32(reg_count * 4);
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
        ArmInstId::STMIB => ir.ir().add_32(base, Value::ImmU32(4), Value::ImmU1(false)),
        _ => base,
    };
    let writeback_addr = match inst.id {
        ArmInstId::STM => ir.ir().add_32(
            start_addr,
            Value::ImmU32(reg_count * 4),
            Value::ImmU1(false),
        ),
        ArmInstId::STMDA => ir
            .ir()
            .sub_32(start_addr, Value::ImmU32(4), Value::ImmU1(true)),
        ArmInstId::STMDB => start_addr,
        ArmInstId::STMIB => ir
            .ir()
            .add_32(base, Value::ImmU32(reg_count * 4), Value::ImmU1(false)),
        _ => start_addr,
    };

    let mut addr = start_addr;
    for i in 0..15u32 {
        if reglist & (1 << i) != 0 {
            let reg = Reg::from_u32(i);
            let val = ir.get_register(reg);
            ir.write_memory_32(addr, val, AccType::Atomic);
            addr = ir.ir().add_32(addr, Value::ImmU32(4), Value::ImmU1(false));
        }
    }

    if w {
        ir.set_register(rn, writeback_addr);
    }

    if reglist & (1 << 15) != 0 {
        ir.write_memory_32(addr, Value::ImmU32(ir.pc()), AccType::Atomic);
    }

    true
}

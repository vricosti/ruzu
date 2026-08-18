use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::terminal::Terminal;
use crate::ir::value::Value;

/// ARM DMB - data memory barrier.
pub fn arm_dmb(ir: &mut A32IREmitter) -> bool {
    ir.data_memory_barrier();
    true
}

/// ARM DSB - data synchronization barrier.
pub fn arm_dsb(ir: &mut A32IREmitter) -> bool {
    ir.data_synchronization_barrier();
    true
}

/// ARM ISB - instruction synchronization barrier.
pub fn arm_isb(ir: &mut A32IREmitter) -> bool {
    ir.instruction_synchronization_barrier();
    let next_pc = ir
        .current_location
        .expect("location not set")
        .pc()
        .wrapping_add(4);
    ir.branch_write_pc(Value::ImmU32(next_pc));
    ir.set_term(Terminal::ReturnToDispatch);
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::a32::fpscr::FPSCR;
    use crate::frontend::a32::psr::PSR;
    use crate::ir::block::Block;
    use crate::ir::location::A32LocationDescriptor;
    use crate::ir::opcode::Opcode;
    use crate::ir::value::Value;

    #[test]
    fn arm_isb_uses_branch_write_pc() {
        let loc = A32LocationDescriptor::new(0x3000, PSR::default(), FPSCR::default(), false);
        let mut block = Block::new(loc.to_location());
        let mut ir = A32IREmitter::with_location(&mut block, loc);

        assert!(!arm_isb(&mut ir));
        assert_eq!(
            block.instructions.last().map(|inst| inst.opcode),
            Some(Opcode::A32SetRegister)
        );
        assert_eq!(
            block.instructions.last().map(|inst| inst.args[0]),
            Some(Value::ImmA32Reg(crate::frontend::a32::types::Reg::R15))
        );
    }
}

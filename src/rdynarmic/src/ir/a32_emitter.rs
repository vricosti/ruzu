use crate::frontend::a32::types::{ExtReg, Reg};
use crate::ir::acc_type::AccType;
use crate::ir::block::Block;
use crate::ir::emitter::IREmitter;
use crate::ir::location::A32LocationDescriptor;
use crate::ir::opcode::Opcode;
use crate::ir::terminal::Terminal;
use crate::ir::types::Type;
use crate::ir::value::Value;

/// A32-specific IR emitter. Extends IREmitter with A32 register/memory/system operations.
pub struct A32IREmitter<'a> {
    pub base: IREmitter<'a>,
    pub current_location: Option<A32LocationDescriptor>,
}

impl<'a> A32IREmitter<'a> {
    pub fn new(block: &'a mut Block) -> Self {
        Self {
            base: IREmitter::new(block),
            current_location: None,
        }
    }

    pub fn with_location(block: &'a mut Block, location: A32LocationDescriptor) -> Self {
        Self {
            base: IREmitter::new(block),
            current_location: Some(location),
        }
    }

    /// Returns the PC value as seen during ARM execution (with pipeline offset).
    /// ARM mode: PC + 8, Thumb mode: PC + 4 (matching dynarmic's IREmitter::PC()).
    pub fn pc(&self) -> u32 {
        let loc = self.current_location.expect("current_location not set");
        let offset = if loc.t_flag() { 4u32 } else { 8u32 };
        loc.pc().wrapping_add(offset)
    }

    pub fn ir(&mut self) -> &mut IREmitter<'a> {
        &mut self.base
    }

    pub fn set_term(&mut self, terminal: Terminal) {
        self.base.set_term(terminal);
    }

    // --- Internal helpers ---

    fn emit(&mut self, opcode: Opcode, args: &[Value]) -> Value {
        let r = self.base.block.append(opcode, args);
        Value::Inst(r)
    }

    fn emit_void(&mut self, opcode: Opcode, args: &[Value]) {
        self.base.block.append(opcode, args);
    }

    fn imm_current_location_descriptor(&mut self) -> Value {
        let loc = self.current_location.expect("current_location not set");
        Value::ImmU64(loc.unique_hash())
    }

    fn value_type(&self, value: Value) -> Type {
        match value {
            Value::Inst(inst_ref) => self.base.block.inst_real_return_type(inst_ref),
            value => value.get_type(),
        }
    }

    fn coerce_to_u8(&mut self, value: Value) -> Value {
        match self.value_type(value) {
            Type::U8 => value,
            Type::U32 => match value {
                Value::ImmU32(imm) => Value::ImmU8(imm as u8),
                value => self.base.least_significant_byte(value),
            },
            ty => panic!("A32 WriteMemory8 value must be U8 or U32, got {:?}", ty),
        }
    }

    fn coerce_to_u16(&mut self, value: Value) -> Value {
        match self.value_type(value) {
            Type::U16 => value,
            Type::U32 => match value {
                Value::ImmU32(imm) => Value::ImmU16(imm as u16),
                value => self.base.least_significant_half(value),
            },
            ty => panic!("A32 WriteMemory16 value must be U16 or U32, got {:?}", ty),
        }
    }

    // --- A32 register getters/setters ---

    pub fn get_register(&mut self, reg: Reg) -> Value {
        if reg == Reg::R15 {
            // PC reads return current instruction address + 8 (ARM) or + 4 (Thumb)
            let loc = self.current_location.expect("current_location not set");
            let offset = if loc.t_flag() { 4u32 } else { 8u32 };
            return Value::ImmU32(loc.pc().wrapping_add(offset));
        }
        self.emit(Opcode::A32GetRegister, &[Value::ImmA32Reg(reg)])
    }

    pub fn set_register(&mut self, reg: Reg, value: Value) {
        if reg == Reg::R15 {
            // Write to R15 = branch
            self.bx_write_pc(value);
            return;
        }
        self.emit_void(Opcode::A32SetRegister, &[Value::ImmA32Reg(reg), value]);
    }

    pub fn get_extended_register_32(&mut self, reg: ExtReg) -> Value {
        self.emit(
            Opcode::A32GetExtendedRegister32,
            &[Value::ImmA32ExtReg(reg)],
        )
    }

    pub fn get_extended_register_64(&mut self, reg: ExtReg) -> Value {
        self.emit(
            Opcode::A32GetExtendedRegister64,
            &[Value::ImmA32ExtReg(reg)],
        )
    }

    pub fn set_extended_register_32(&mut self, reg: ExtReg, value: Value) {
        self.emit_void(
            Opcode::A32SetExtendedRegister32,
            &[Value::ImmA32ExtReg(reg), value],
        );
    }

    pub fn set_extended_register_64(&mut self, reg: ExtReg, value: Value) {
        self.emit_void(
            Opcode::A32SetExtendedRegister64,
            &[Value::ImmA32ExtReg(reg), value],
        );
    }

    pub fn get_vector(&mut self, reg: ExtReg) -> Value {
        self.emit(Opcode::A32GetVector, &[Value::ImmA32ExtReg(reg)])
    }

    pub fn set_vector(&mut self, reg: ExtReg, value: Value) {
        self.emit_void(Opcode::A32SetVector, &[Value::ImmA32ExtReg(reg), value]);
    }

    // --- PC writes ---

    pub fn bx_write_pc(&mut self, value: Value) {
        self.emit_void(Opcode::A32BXWritePC, &[value]);
    }

    pub fn update_upper_location_descriptor(&mut self) {
        self.emit_void(Opcode::A32UpdateUpperLocationDescriptor, &[]);
    }

    // --- Flags ---

    pub fn set_check_bit(&mut self, value: Value) {
        self.emit_void(Opcode::A32SetCheckBit, &[value]);
    }

    pub fn get_c_flag(&mut self) -> Value {
        self.emit(Opcode::A32GetCFlag, &[])
    }

    pub fn get_cpsr(&mut self) -> Value {
        self.emit(Opcode::A32GetCpsr, &[])
    }

    pub fn set_cpsr(&mut self, value: Value) {
        self.emit_void(Opcode::A32SetCpsr, &[value]);
    }

    pub fn set_cpsr_nzcv_raw(&mut self, value: Value) {
        self.emit_void(Opcode::A32SetCpsrNZCVRaw, &[value]);
    }

    pub fn set_cpsr_nzcv(&mut self, nzcv: Value) {
        self.emit_void(Opcode::A32SetCpsrNZCV, &[nzcv]);
    }

    pub fn set_cpsr_nzcvq(&mut self, value: Value) {
        self.emit_void(Opcode::A32SetCpsrNZCVQ, &[value]);
    }

    pub fn set_cpsr_nz(&mut self, nzcv: Value) {
        self.emit_void(Opcode::A32SetCpsrNZ, &[nzcv]);
    }

    pub fn set_cpsr_nzc(&mut self, nzcv: Value, carry: Value) {
        self.emit_void(Opcode::A32SetCpsrNZC, &[nzcv, carry]);
    }

    pub fn or_q_flag(&mut self, value: Value) {
        self.emit_void(Opcode::A32OrQFlag, &[value]);
    }

    pub fn get_ge_flags(&mut self) -> Value {
        self.emit(Opcode::A32GetGEFlags, &[])
    }

    pub fn set_ge_flags(&mut self, value: Value) {
        self.emit_void(Opcode::A32SetGEFlags, &[value]);
    }

    pub fn set_ge_flags_compressed(&mut self, value: Value) {
        self.emit_void(Opcode::A32SetGEFlagsCompressed, &[value]);
    }

    // --- System ---

    pub fn call_supervisor(&mut self, imm: u32) {
        self.emit_void(Opcode::A32CallSupervisor, &[Value::ImmU32(imm)]);
    }

    /// Debug-only per-instruction PC execution hook (RUZU_A32_PC_EXEC). `pc` is
    /// the guest PC of the instruction this hook precedes; it becomes the hook's
    /// aggregation tag. Only emitted by the translator when the PC is in the
    /// configured target set, so there is zero cost when the env var is unset.
    pub fn pc_exec_hook(&mut self, pc: u32) {
        let args = [
            Value::ImmU32(pc),
            self.get_register(Reg::R0),
            self.get_register(Reg::R1),
            self.get_register(Reg::R2),
            self.get_register(Reg::LR),
        ];
        self.emit_void(Opcode::A32PcExecHook, &args);
    }

    pub fn exception_raised(&mut self, exception: crate::frontend::a32::types::Exception) {
        let loc_desc = self.imm_current_location_descriptor();
        let pc = match loc_desc {
            Value::ImmU64(raw) => Value::ImmU32(raw as u32),
            _ => panic!("A32 current location descriptor must be an immediate"),
        };
        self.emit_void(
            Opcode::A32ExceptionRaised,
            &[pc, Value::ImmU64(exception.as_u32() as u64)],
        );
    }

    pub fn data_synchronization_barrier(&mut self) {
        self.emit_void(Opcode::A32DataSynchronizationBarrier, &[]);
    }

    pub fn data_memory_barrier(&mut self) {
        self.emit_void(Opcode::A32DataMemoryBarrier, &[]);
    }

    pub fn instruction_synchronization_barrier(&mut self) {
        self.emit_void(Opcode::A32InstructionSynchronizationBarrier, &[]);
    }

    // --- FPSCR ---

    pub fn get_fpscr(&mut self) -> Value {
        self.emit(Opcode::A32GetFpscr, &[])
    }

    pub fn set_fpscr(&mut self, value: Value) {
        self.emit_void(Opcode::A32SetFpscr, &[value]);
    }

    pub fn get_fpscr_nzcv(&mut self) -> Value {
        self.emit(Opcode::A32GetFpscrNZCV, &[])
    }

    pub fn set_fpscr_nzcv(&mut self, nzcv: Value) {
        self.emit_void(Opcode::A32SetFpscrNZCV, &[nzcv]);
    }

    // --- Memory ---

    pub fn clear_exclusive(&mut self) {
        self.emit_void(Opcode::A32ClearExclusive, &[]);
    }

    pub fn read_memory_8(&mut self, vaddr: Value, acc_type: AccType) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ReadMemory8,
            &[upper, vaddr, Value::ImmAccType(acc_type)],
        )
    }

    pub fn read_memory_16(&mut self, vaddr: Value, acc_type: AccType) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ReadMemory16,
            &[upper, vaddr, Value::ImmAccType(acc_type)],
        )
    }

    pub fn read_memory_32(&mut self, vaddr: Value, acc_type: AccType) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ReadMemory32,
            &[upper, vaddr, Value::ImmAccType(acc_type)],
        )
    }

    pub fn read_memory_64(&mut self, vaddr: Value, acc_type: AccType) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ReadMemory64,
            &[upper, vaddr, Value::ImmAccType(acc_type)],
        )
    }

    pub fn exclusive_read_memory_8(&mut self, vaddr: Value, acc_type: AccType) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ExclusiveReadMemory8,
            &[upper, vaddr, Value::ImmAccType(acc_type)],
        )
    }

    pub fn exclusive_read_memory_16(&mut self, vaddr: Value, acc_type: AccType) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ExclusiveReadMemory16,
            &[upper, vaddr, Value::ImmAccType(acc_type)],
        )
    }

    pub fn exclusive_read_memory_32(&mut self, vaddr: Value, acc_type: AccType) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ExclusiveReadMemory32,
            &[upper, vaddr, Value::ImmAccType(acc_type)],
        )
    }

    pub fn exclusive_read_memory_64(&mut self, vaddr: Value, acc_type: AccType) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ExclusiveReadMemory64,
            &[upper, vaddr, Value::ImmAccType(acc_type)],
        )
    }

    pub fn write_memory_8(&mut self, vaddr: Value, value: Value, acc_type: AccType) {
        let upper = self.imm_current_location_descriptor();
        let value = self.coerce_to_u8(value);
        self.emit_void(
            Opcode::A32WriteMemory8,
            &[upper, vaddr, value, Value::ImmAccType(acc_type)],
        );
    }

    pub fn write_memory_16(&mut self, vaddr: Value, value: Value, acc_type: AccType) {
        let upper = self.imm_current_location_descriptor();
        let value = self.coerce_to_u16(value);
        self.emit_void(
            Opcode::A32WriteMemory16,
            &[upper, vaddr, value, Value::ImmAccType(acc_type)],
        );
    }

    pub fn write_memory_32(&mut self, vaddr: Value, value: Value, acc_type: AccType) {
        let upper = self.imm_current_location_descriptor();
        self.emit_void(
            Opcode::A32WriteMemory32,
            &[upper, vaddr, value, Value::ImmAccType(acc_type)],
        );
    }

    pub fn write_memory_64(&mut self, vaddr: Value, value: Value, acc_type: AccType) {
        let upper = self.imm_current_location_descriptor();
        self.emit_void(
            Opcode::A32WriteMemory64,
            &[upper, vaddr, value, Value::ImmAccType(acc_type)],
        );
    }

    pub fn exclusive_write_memory_8(
        &mut self,
        vaddr: Value,
        value: Value,
        acc_type: AccType,
    ) -> Value {
        let upper = self.imm_current_location_descriptor();
        let value = self.coerce_to_u8(value);
        self.emit(
            Opcode::A32ExclusiveWriteMemory8,
            &[upper, vaddr, value, Value::ImmAccType(acc_type)],
        )
    }

    pub fn exclusive_write_memory_16(
        &mut self,
        vaddr: Value,
        value: Value,
        acc_type: AccType,
    ) -> Value {
        let upper = self.imm_current_location_descriptor();
        let value = self.coerce_to_u16(value);
        self.emit(
            Opcode::A32ExclusiveWriteMemory16,
            &[upper, vaddr, value, Value::ImmAccType(acc_type)],
        )
    }

    pub fn exclusive_write_memory_32(
        &mut self,
        vaddr: Value,
        value: Value,
        acc_type: AccType,
    ) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ExclusiveWriteMemory32,
            &[upper, vaddr, value, Value::ImmAccType(acc_type)],
        )
    }

    pub fn exclusive_write_memory_64(
        &mut self,
        vaddr: Value,
        value: Value,
        acc_type: AccType,
    ) -> Value {
        let upper = self.imm_current_location_descriptor();
        self.emit(
            Opcode::A32ExclusiveWriteMemory64,
            &[upper, vaddr, value, Value::ImmAccType(acc_type)],
        )
    }

    // --- Coprocessor ---

    pub fn coproc_internal_operation(&mut self, coproc_info: u64) {
        self.emit_void(
            Opcode::A32CoprocInternalOperation,
            &[Value::ImmCoprocInfo(coproc_info)],
        );
    }

    pub fn coproc_send_one_word(&mut self, coproc_info: u64, word: Value) {
        self.emit_void(
            Opcode::A32CoprocSendOneWord,
            &[Value::ImmCoprocInfo(coproc_info), word],
        );
    }

    pub fn coproc_send_two_words(&mut self, coproc_info: u64, word1: Value, word2: Value) {
        self.emit_void(
            Opcode::A32CoprocSendTwoWords,
            &[Value::ImmCoprocInfo(coproc_info), word1, word2],
        );
    }

    pub fn coproc_get_one_word(&mut self, coproc_info: u64) -> Value {
        self.emit(
            Opcode::A32CoprocGetOneWord,
            &[Value::ImmCoprocInfo(coproc_info)],
        )
    }

    pub fn coproc_get_two_words(&mut self, coproc_info: u64) -> Value {
        self.emit(
            Opcode::A32CoprocGetTwoWords,
            &[Value::ImmCoprocInfo(coproc_info)],
        )
    }

    pub fn coproc_load_words(&mut self, coproc_info: u64, address: Value, is_64bit: bool) {
        self.emit_void(
            Opcode::A32CoprocLoadWords,
            &[
                Value::ImmCoprocInfo(coproc_info),
                address,
                Value::ImmU1(is_64bit),
            ],
        );
    }

    pub fn coproc_store_words(&mut self, coproc_info: u64, address: Value, is_64bit: bool) {
        self.emit_void(
            Opcode::A32CoprocStoreWords,
            &[
                Value::ImmCoprocInfo(coproc_info),
                address,
                Value::ImmU1(is_64bit),
            ],
        );
    }

    // --- Additional PC write helpers (matching C++ dynarmic) ---

    pub fn branch_write_pc(&mut self, value: Value) {
        let loc = self.current_location.expect("current_location not set");
        let mask = if loc.t_flag() {
            0xFFFFFFFEu32
        } else {
            0xFFFFFFFCu32
        };
        let masked = self.base.and_32(value, Value::ImmU32(mask));
        self.emit_void(
            Opcode::A32SetRegister,
            &[Value::ImmA32Reg(Reg::R15), masked],
        );
    }

    pub fn alu_write_pc(&mut self, value: Value) {
        let loc = self.current_location.expect("current_location not set");
        // ruzu currently executes Switch AArch32 codepaths, which follow
        // dynarmic's ArchVersion() >= 7 behavior here.
        if !loc.t_flag() {
            self.bx_write_pc(value);
        } else {
            self.branch_write_pc(value);
        }
    }

    pub fn load_write_pc(&mut self, value: Value) {
        self.bx_write_pc(value);
    }

    pub fn align_pc(&self, alignment: u32) -> u32 {
        self.pc() & !(alignment - 1)
    }

    // --- Convenience flag helpers ---

    pub fn nz_from(&mut self, value: Value) -> Value {
        self.base.get_nzcv_from_op(value)
    }

    pub fn get_overflow_from(&mut self, value: Value) -> Value {
        self.base.get_overflow_from_op(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::a32::types::Reg;
    use crate::ir::block::Block;
    use crate::ir::value::InstRef;

    #[test]
    fn test_a32_emitter_register_ops() {
        let loc = A32LocationDescriptor::at(0x1000);
        let mut block = Block::new(loc.to_location());
        {
            let mut e = A32IREmitter::with_location(&mut block, loc);
            let r0 = e.get_register(Reg::R0);
            let r1 = e.get_register(Reg::R1);
            let carry = e.ir().imm1(false);
            let result = e.ir().add_32(r0, r1, carry);
            e.set_register(Reg::R2, result);
            e.set_term(Terminal::ReturnToDispatch);
        }
        assert_eq!(block.inst_count(), 4);
        assert_eq!(block.get(InstRef(0)).opcode, Opcode::A32GetRegister);
        assert_eq!(block.get(InstRef(1)).opcode, Opcode::A32GetRegister);
        assert_eq!(block.get(InstRef(2)).opcode, Opcode::Add32);
        assert_eq!(block.get(InstRef(3)).opcode, Opcode::A32SetRegister);
    }

    #[test]
    fn test_a32_emitter_pc_read() {
        let loc = A32LocationDescriptor::at(0x1000);
        let mut block = Block::new(loc.to_location());
        {
            let mut e = A32IREmitter::with_location(&mut block, loc);
            let pc = e.get_register(Reg::R15);
            // ARM mode: PC = current_pc + 8
            assert_eq!(pc, Value::ImmU32(0x1008));
        }
    }

    #[test]
    fn test_a32_emitter_memory() {
        let loc = A32LocationDescriptor::at(0x2000);
        let mut block = Block::new(loc.to_location());
        {
            let mut e = A32IREmitter::with_location(&mut block, loc);
            let addr = e.get_register(Reg::R0);
            let val = e.read_memory_32(addr, AccType::Normal);
            e.set_register(Reg::R1, val);
        }
        assert_eq!(block.inst_count(), 3);
        assert_eq!(block.get(InstRef(1)).opcode, Opcode::A32ReadMemory32);
    }

    #[test]
    fn test_a32_emitter_flags() {
        let loc = A32LocationDescriptor::at(0x3000);
        let mut block = Block::new(loc.to_location());
        {
            let mut e = A32IREmitter::with_location(&mut block, loc);
            let r0 = e.get_register(Reg::R0);
            let r1 = e.get_register(Reg::R1);
            let carry = e.ir().imm1(true);
            let result = e.ir().add_32(r0, r1, carry);
            let nzcv = e.ir().get_nzcv_from_op(result);
            e.set_cpsr_nzcv_raw(nzcv);
        }
        assert_eq!(block.inst_count(), 5);
    }

    #[test]
    fn test_a32_emitter_svc() {
        let loc = A32LocationDescriptor::at(0x4000);
        let mut block = Block::new(loc.to_location());
        {
            let mut e = A32IREmitter::with_location(&mut block, loc);
            e.call_supervisor(0x21);
            e.set_term(Terminal::ReturnToDispatch);
        }
        assert_eq!(block.inst_count(), 1);
        assert_eq!(block.get(InstRef(0)).opcode, Opcode::A32CallSupervisor);
    }

    #[test]
    fn test_a32_emitter_alu_write_pc_arm_uses_bx_write_pc() {
        let loc = A32LocationDescriptor::at(0x5000).set_t_flag(false);
        let mut block = Block::new(loc.to_location());
        {
            let mut e = A32IREmitter::with_location(&mut block, loc);
            e.alu_write_pc(Value::ImmU32(0x1235));
        }
        assert_eq!(block.inst_count(), 1);
        assert_eq!(block.get(InstRef(0)).opcode, Opcode::A32BXWritePC);
    }

    #[test]
    fn test_a32_emitter_alu_write_pc_thumb_uses_branch_write_pc() {
        let loc = A32LocationDescriptor::at(0x6000).set_t_flag(true);
        let mut block = Block::new(loc.to_location());
        {
            let mut e = A32IREmitter::with_location(&mut block, loc);
            e.alu_write_pc(Value::ImmU32(0x1237));
        }
        assert_eq!(block.inst_count(), 2);
        assert_eq!(block.get(InstRef(0)).opcode, Opcode::And32);
        assert_eq!(block.get(InstRef(1)).opcode, Opcode::A32SetRegister);
    }
}

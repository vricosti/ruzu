// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/macro/macro_interpreter.h` and `macro_interpreter.cpp`.
//!
//! Software interpreter for Maxwell macro programs. This is the fallback
//! execution backend when JIT compilation is unavailable or disabled.

use super::macro_engine::{
    AluOperation, BranchCondition, CachedMacro, MethodAddress, Opcode, Operation, ResultOperation,
    NUM_MACRO_REGISTERS,
};

// ── MacroInterpreterImpl ─────────────────────────────────────────────────────

/// A software-interpreted macro program.
///
/// Port of `MacroInterpreterImpl` from `macro_interpreter.cpp`.
///
/// This struct holds the macro code and executes it instruction by instruction,
/// maintaining register state, carry flag, and method address register.
pub struct MacroInterpreterImpl {
    /// The macro code words.
    code: Vec<u32>,

    /// Current program counter (byte offset).
    pc: u32,

    /// Program counter to execute at after the delay slot.
    delayed_pc: Option<u32>,

    /// General purpose macro registers.
    registers: [u32; NUM_MACRO_REGISTERS],

    /// Method address register with auto-increment.
    method_address: MethodAddress,

    /// Input parameters of the current macro.
    parameters: Vec<u32>,

    /// Index of the next parameter that will be fetched by the 'parm' instruction.
    next_parameter_index: usize,

    /// Carry flag from ALU operations.
    carry_flag: bool,

    /// Temporary bounded trace budget for targeted macro debugging.
    trace_steps_remaining: u32,

    /// Callback for writing a GPU method register.
    /// Signature: (address, value, is_last_call)
    method_writer: Option<Box<dyn FnMut(u32, u32, bool) + Send>>,

    /// Callback for reading a GPU method register.
    /// Signature: (method) -> value
    method_reader: Option<Box<dyn Fn(u32) -> u32 + Send>>,
}

impl MacroInterpreterImpl {
    /// Create a new interpreter for the given macro code.
    ///
    /// Port of `MacroInterpreterImpl::MacroInterpreterImpl`.
    pub fn new(code: Vec<u32>) -> Self {
        Self {
            code,
            pc: 0,
            delayed_pc: None,
            registers: [0; NUM_MACRO_REGISTERS],
            method_address: MethodAddress::new(0),
            parameters: Vec::new(),
            next_parameter_index: 0,
            carry_flag: false,
            trace_steps_remaining: 0,
            method_writer: None,
            method_reader: None,
        }
    }

    /// Set the callback for writing GPU method registers.
    pub fn set_method_writer<F: FnMut(u32, u32, bool) + Send + 'static>(&mut self, writer: F) {
        self.method_writer = Some(Box::new(writer));
    }

    /// Set the callback for reading GPU method registers.
    pub fn set_method_reader<F: Fn(u32) -> u32 + Send + 'static>(&mut self, reader: F) {
        self.method_reader = Some(Box::new(reader));
    }

    /// Reset the execution engine state, zeroing registers, etc.
    ///
    /// Port of `MacroInterpreterImpl::Reset`.
    fn reset(&mut self) {
        self.registers = [0; NUM_MACRO_REGISTERS];
        self.pc = 0;
        self.delayed_pc = None;
        self.method_address = MethodAddress::new(0);
        // The next parameter index starts at 1, because $r1 already has the
        // value of the first parameter.
        self.next_parameter_index = 1;
        self.carry_flag = false;
        self.trace_steps_remaining = 0;
    }

    /// Execute a single macro instruction at the current PC.
    /// Returns whether the interpreter should keep running.
    ///
    /// Port of `MacroInterpreterImpl::Step`.
    fn step(&mut self, is_delay_slot: bool) -> bool {
        let base_address = self.pc;
        let opcode = self.get_opcode();
        if self.trace_steps_remaining > 0 {
            log::info!(
                "MacroTrace pre pc=0x{:X} raw=0x{:08X} op={:?} res={:?} dst={} src_a={} src_b={} imm={} delay={} regs={:08X?} method_raw=0x{:08X} next_param={}",
                base_address,
                opcode.raw,
                opcode.operation(),
                opcode.result_operation(),
                opcode.dst(),
                opcode.src_a(),
                opcode.src_b(),
                opcode.immediate(),
                is_delay_slot,
                self.registers,
                self.method_address.raw,
                self.next_parameter_index
            );
            self.trace_steps_remaining -= 1;
        }
        self.pc += 4;

        // Update the program counter if we were delayed
        if let Some(delayed) = self.delayed_pc.take() {
            assert!(is_delay_slot, "delayed_pc set but not in a delay slot");
            self.pc = delayed;
        }

        match opcode.operation() {
            Operation::Alu => {
                let result = self.get_alu_result(
                    opcode.alu_operation(),
                    self.get_register(opcode.src_a()),
                    self.get_register(opcode.src_b()),
                );
                self.process_result(opcode.result_operation(), opcode.dst(), result);
            }
            Operation::AddImmediate => {
                let result = self
                    .get_register(opcode.src_a())
                    .wrapping_add(opcode.immediate() as u32);
                self.process_result(opcode.result_operation(), opcode.dst(), result);
            }
            Operation::ExtractInsert => {
                let mut dst = self.get_register(opcode.src_a());
                let src = self.get_register(opcode.src_b());

                let extracted = (src >> opcode.bf_src_bit()) & opcode.get_bitfield_mask();
                dst &= !(opcode.get_bitfield_mask() << opcode.bf_dst_bit());
                dst |= extracted << opcode.bf_dst_bit();
                self.process_result(opcode.result_operation(), opcode.dst(), dst);
            }
            Operation::ExtractShiftLeftImmediate => {
                let dst_val = self.get_register(opcode.src_a());
                let src = self.get_register(opcode.src_b());

                let result = ((src >> dst_val) & opcode.get_bitfield_mask()) << opcode.bf_dst_bit();
                self.process_result(opcode.result_operation(), opcode.dst(), result);
            }
            Operation::ExtractShiftLeftRegister => {
                let dst_val = self.get_register(opcode.src_a());
                let src = self.get_register(opcode.src_b());

                let result = ((src >> opcode.bf_src_bit()) & opcode.get_bitfield_mask()) << dst_val;
                self.process_result(opcode.result_operation(), opcode.dst(), result);
            }
            Operation::Read => {
                let method = self
                    .get_register(opcode.src_a())
                    .wrapping_add(opcode.immediate() as u32);
                let result = self.read(method);
                self.process_result(opcode.result_operation(), opcode.dst(), result);
            }
            Operation::Branch => {
                assert!(
                    !is_delay_slot,
                    "Executing a branch in a delay slot is not valid"
                );
                let value = self.get_register(opcode.src_a());
                let taken = self.evaluate_branch_condition(opcode.branch_condition(), value);
                if taken {
                    // Ignore the delay slot if the branch has the annul bit.
                    if opcode.branch_annul() {
                        self.pc = (base_address as i32 + opcode.get_branch_target()) as u32;
                        return true;
                    }

                    self.delayed_pc =
                        Some((base_address as i32 + opcode.get_branch_target()) as u32);
                    // Execute one more instruction due to the delay slot.
                    return self.step(true);
                }
            }
            Operation::Unused => {
                log::warn!(
                    "MacroInterpreter: unused operation at PC=0x{:x}",
                    base_address
                );
            }
        }

        // An instruction with the Exit flag will not actually
        // cause an exit if it's executed inside a delay slot.
        if opcode.is_exit() && !is_delay_slot {
            // Exit has a delay slot, execute the next instruction
            self.step(true);
            return false;
        }

        true
    }

    /// Calculate the result of an ALU operation.
    ///
    /// Port of `MacroInterpreterImpl::GetALUResult`.
    fn get_alu_result(&mut self, operation: AluOperation, src_a: u32, src_b: u32) -> u32 {
        match operation {
            AluOperation::Add => {
                let result = src_a as u64 + src_b as u64;
                self.carry_flag = result > 0xFFFFFFFF;
                result as u32
            }
            AluOperation::AddWithCarry => {
                let carry = if self.carry_flag { 1u64 } else { 0u64 };
                let result = src_a as u64 + src_b as u64 + carry;
                self.carry_flag = result > 0xFFFFFFFF;
                result as u32
            }
            AluOperation::Subtract => {
                let result = src_a as u64 - src_b as u64;
                self.carry_flag = result < 0x100000000;
                result as u32
            }
            AluOperation::SubtractWithBorrow => {
                let borrow = if self.carry_flag { 0u64 } else { 1u64 };
                let result = (src_a as u64)
                    .wrapping_sub(src_b as u64)
                    .wrapping_sub(borrow);
                self.carry_flag = result < 0x100000000;
                result as u32
            }
            AluOperation::Xor => src_a ^ src_b,
            AluOperation::Or => src_a | src_b,
            AluOperation::And => src_a & src_b,
            AluOperation::AndNot => src_a & !src_b,
            AluOperation::Nand => !(src_a & src_b),
        }
    }

    /// Perform the result operation on the input result.
    ///
    /// Port of `MacroInterpreterImpl::ProcessResult`.
    fn process_result(&mut self, operation: ResultOperation, reg: u32, result: u32) {
        match operation {
            ResultOperation::IgnoreAndFetch => {
                // Fetch parameter and ignore result.
                let param = self.fetch_parameter();
                self.set_register(reg, param);
            }
            ResultOperation::Move => {
                // Move result.
                self.set_register(reg, result);
            }
            ResultOperation::MoveAndSetMethod => {
                // Move result and use as Method Address.
                self.set_register(reg, result);
                self.set_method_address(result);
            }
            ResultOperation::FetchAndSend => {
                // Fetch parameter and send result.
                let param = self.fetch_parameter();
                self.set_register(reg, param);
                self.send(result);
            }
            ResultOperation::MoveAndSend => {
                // Move and send result.
                self.set_register(reg, result);
                self.send(result);
            }
            ResultOperation::FetchAndSetMethod => {
                // Fetch parameter and use result as Method Address.
                let param = self.fetch_parameter();
                self.set_register(reg, param);
                self.set_method_address(result);
            }
            ResultOperation::MoveAndSetMethodFetchAndSend => {
                // Move result and use as Method Address, then fetch and send parameter.
                self.set_register(reg, result);
                self.set_method_address(result);
                let param = self.fetch_parameter();
                self.send(param);
            }
            ResultOperation::MoveAndSetMethodSend => {
                // Move result and use as Method Address, then send bits 12:17 of result.
                self.set_register(reg, result);
                self.set_method_address(result);
                self.send((result >> 12) & 0b111111);
            }
        }
    }

    /// Evaluate branch condition.
    ///
    /// Port of `MacroInterpreterImpl::EvaluateBranchCondition`.
    fn evaluate_branch_condition(&self, cond: BranchCondition, value: u32) -> bool {
        match cond {
            BranchCondition::Zero => value == 0,
            BranchCondition::NotZero => value != 0,
        }
    }

    /// Read an opcode at the current program counter.
    ///
    /// Port of `MacroInterpreterImpl::GetOpcode`.
    fn get_opcode(&self) -> Opcode {
        assert!(self.pc % 4 == 0, "PC not aligned: 0x{:x}", self.pc);
        let index = (self.pc / 4) as usize;
        assert!(
            index < self.code.len(),
            "PC out of bounds: 0x{:x} (code size: {})",
            self.pc,
            self.code.len()
        );
        Opcode::new(self.code[index])
    }

    /// Returns the specified register's value. Register 0 always returns 0.
    ///
    /// Port of `MacroInterpreterImpl::GetRegister`.
    fn get_register(&self, register_id: u32) -> u32 {
        self.registers[register_id as usize]
    }

    /// Set a register value. Register 0 writes are ignored.
    ///
    /// Port of `MacroInterpreterImpl::SetRegister`.
    fn set_register(&mut self, register_id: u32, value: u32) {
        // Register 0 is hardwired as the zero register.
        if register_id == 0 {
            return;
        }
        self.registers[register_id as usize] = value;
    }

    /// Set the method address register.
    ///
    /// Port of `MacroInterpreterImpl::SetMethodAddress`.
    fn set_method_address(&mut self, address: u32) {
        self.method_address = MethodAddress::new(address);
    }

    /// Send a value to the GPU via the current method address.
    ///
    /// Port of `MacroInterpreterImpl::Send`.
    fn send(&mut self, value: u32) {
        let address = self.method_address.address();
        if self.trace_steps_remaining > 0 {
            log::info!(
                "MacroTrace send addr=0x{:X} incr={} value=0x{:08X}",
                address,
                self.method_address.increment(),
                value
            );
        }
        if let Some(ref mut writer) = self.method_writer {
            writer(address, value, true);
        }
        // Increment the method address by the method increment.
        let new_addr = address + self.method_address.increment();
        self.method_address.set_address(new_addr);
    }

    /// Read a GPU register.
    ///
    /// Port of `MacroInterpreterImpl::Read`.
    fn read(&self, method: u32) -> u32 {
        let value = if let Some(ref reader) = self.method_reader {
            reader(method)
        } else {
            0
        };
        if self.trace_steps_remaining > 0 {
            log::info!(
                "MacroTrace read method=0x{:X} value=0x{:08X}",
                method,
                value
            );
        }
        value
    }

    /// Fetch the next parameter.
    ///
    /// Port of `MacroInterpreterImpl::FetchParameter`.
    fn fetch_parameter(&mut self) -> u32 {
        assert!(
            self.next_parameter_index < self.parameters.len(),
            "Macro parameter index out of bounds: {} >= {}",
            self.next_parameter_index,
            self.parameters.len()
        );
        let value = self.parameters[self.next_parameter_index];
        self.next_parameter_index += 1;
        if self.trace_steps_remaining > 0 {
            log::info!(
                "MacroTrace fetch_parameter idx={} value=0x{:08X}",
                self.next_parameter_index - 1,
                value
            );
        }
        value
    }
}

impl CachedMacro for MacroInterpreterImpl {
    /// Execute the macro with the given parameters.
    ///
    /// Port of `MacroInterpreterImpl::Execute`.
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        self.reset();

        self.registers[1] = parameters[0];
        self.parameters = parameters.to_vec();
        if self.code.len() == 26 && self.code.first().copied() == Some(0x0011_0071) {
            self.trace_steps_remaining = 64;
            log::info!(
                "MacroTrace start params={:08X?} code={:08X?}",
                self.parameters,
                self.code
            );
        }

        // Execute the code until we hit an exit condition.
        let mut keep_executing = true;
        let mut steps: u64 = 0;
        while keep_executing {
            keep_executing = self.step(false);
            steps += 1;
            if steps == 1_000_000 {
                log::warn!(
                    "MacroInterpreterImpl::execute still running after {} steps pc=0x{:X} code_len={} first=0x{:08X}",
                    steps,
                    self.pc,
                    self.code.len(),
                    self.code.first().copied().unwrap_or(0)
                );
            }
        }
        if steps > 100 {
            log::info!(
                "MacroInterpreterImpl::execute finished steps={} code_len={} first=0x{:08X}",
                steps,
                self.code.len(),
                self.code.first().copied().unwrap_or(0)
            );
        }

        // Assert that the macro used all the input parameters
        assert_eq!(
            self.next_parameter_index,
            self.parameters.len(),
            "Macro did not consume all parameters: used {}, total {}",
            self.next_parameter_index,
            self.parameters.len()
        );
    }
}

// ── MacroInterpreter Engine ──────────────────────────────────────────────────

/// Software interpreter macro engine backend.
///
/// Port of `Tegra::MacroInterpreter`.
pub struct MacroInterpreter;

impl MacroInterpreter {
    pub fn new() -> Self {
        Self
    }

    /// Compile macro code into an interpreted cached program.
    ///
    /// Port of `MacroInterpreter::Compile`.
    pub fn compile(&self, code: &[u32]) -> Box<dyn CachedMacro> {
        Box::new(MacroInterpreterImpl::new(code.to_vec()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interpreter_basic_exit() {
        // Encode an instruction that just exits immediately:
        // operation = AddImmediate (1), result_operation = Move (1),
        // is_exit = 1 (bit 7), dst = 1 (bits 10:8), src_a = 0 (bits 13:11),
        // immediate = 0 (bits 31:14)
        //
        // raw = operation(1) | result_op(1 << 4) | is_exit(1 << 7) | dst(1 << 8)
        let exit_nop = 0b1 | (0b001 << 4) | (1 << 7) | (1 << 8);
        // The exit instruction has a delay slot, so we need a second NOP
        let nop = 0b1 | (0b001 << 4) | (1 << 8); // AddImmediate, Move, dst=1

        let code = vec![exit_nop, nop];
        let engine = MacroInterpreter::new();
        let mut program = engine.compile(&code);
        // Execute with 2 parameters (minimum: $r1 = params[0], plus one for fetch)
        // Actually, with Move result operations, no parameters are fetched.
        // We only need 1 parameter since $r1 is set from params[0].
        // But the exit delay slot also runs, so we need the parameter count to match.
        // With Move operations, next_parameter_index stays at 1.
        program.execute(&[0x42], 0);
    }

    #[test]
    fn alu_add() {
        let mut interp = MacroInterpreterImpl::new(vec![]);
        let result = interp.get_alu_result(AluOperation::Add, 5, 3);
        assert_eq!(result, 8);
        assert!(!interp.carry_flag);
    }

    #[test]
    fn alu_add_overflow() {
        let mut interp = MacroInterpreterImpl::new(vec![]);
        let result = interp.get_alu_result(AluOperation::Add, 0xFFFFFFFF, 2);
        assert_eq!(result, 1);
        assert!(interp.carry_flag);
    }

    #[test]
    fn alu_subtract() {
        let mut interp = MacroInterpreterImpl::new(vec![]);
        let result = interp.get_alu_result(AluOperation::Subtract, 10, 3);
        assert_eq!(result, 7);
        assert!(interp.carry_flag); // No borrow
    }

    #[test]
    fn alu_bitwise() {
        let mut interp = MacroInterpreterImpl::new(vec![]);
        assert_eq!(
            interp.get_alu_result(AluOperation::Xor, 0xFF00, 0x0FF0),
            0xF0F0
        );
        assert_eq!(
            interp.get_alu_result(AluOperation::Or, 0xFF00, 0x0FF0),
            0xFFF0
        );
        assert_eq!(
            interp.get_alu_result(AluOperation::And, 0xFF00, 0x0FF0),
            0x0F00
        );
        assert_eq!(
            interp.get_alu_result(AluOperation::AndNot, 0xFF00, 0x0FF0),
            0xF000
        );
        assert_eq!(
            interp.get_alu_result(AluOperation::Nand, 0xFF00, 0x0FF0),
            !0x0F00u32
        );
    }

    #[test]
    fn register_zero_is_hardwired() {
        let mut interp = MacroInterpreterImpl::new(vec![]);
        interp.set_register(0, 42);
        assert_eq!(interp.get_register(0), 0);
        interp.set_register(1, 42);
        assert_eq!(interp.get_register(1), 42);
    }

    #[test]
    fn branch_condition() {
        let interp = MacroInterpreterImpl::new(vec![]);
        assert!(interp.evaluate_branch_condition(BranchCondition::Zero, 0));
        assert!(!interp.evaluate_branch_condition(BranchCondition::Zero, 1));
        assert!(!interp.evaluate_branch_condition(BranchCondition::NotZero, 0));
        assert!(interp.evaluate_branch_condition(BranchCondition::NotZero, 1));
    }
}

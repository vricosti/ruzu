// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! x86-64 native compiler for Maxwell macro programs.
//!
//! Port of `MacroJITx64Impl` in current upstream `video_core/macro.cpp`.
//! Upstream can address `Maxwell3D::regs` by C++ field offset. Rust's
//! `Maxwell3D` is not a stable-layout type, so JIT state carries the stable
//! address of its boxed register array. Register reads remain native indexed
//! loads; no Rust callback is introduced on that path.

use std::mem::offset_of;

use rxbyak::{
    byte_ptr, dword_ptr, qword_ptr, CodeAssembler, JmpType, Label, Reg, RegExp, EAX, ECX, R10,
    R10D, R11, R12, R14, R14D, R15, RAX, RBP, RBX, RCX, RDI, RDX, RSI, RSP,
};

#[cfg(target_os = "windows")]
use rxbyak::R8;

use super::macro_engine::{
    AluOperation, BranchCondition, CachedMacro, Opcode, Operation, ResultOperation,
    NUM_MACRO_REGISTERS,
};
use crate::engines::engine_interface::EngineInterface;
use crate::engines::maxwell_3d::Maxwell3D;

/// Upstream `MAX_CODE_SIZE`.
const MAX_CODE_SIZE: usize = 0x10000;

const STATE: Reg = RBX;
const RESULT: Reg = R10D;
const MAX_PARAMETER: Reg = R11;
const PARAMETERS: Reg = R12;
const METHOD_ADDRESS: Reg = R14D;
const BRANCH_HOLDER: Reg = R15;

#[cfg(not(target_os = "windows"))]
const ABI_PARAM1: Reg = RDI;
#[cfg(not(target_os = "windows"))]
const ABI_PARAM2: Reg = RSI;
#[cfg(not(target_os = "windows"))]
const ABI_PARAM3: Reg = RDX;

#[cfg(target_os = "windows")]
const ABI_PARAM1: Reg = rxbyak::RCX;
#[cfg(target_os = "windows")]
const ABI_PARAM2: Reg = RDX;
#[cfg(target_os = "windows")]
const ABI_PARAM3: Reg = R8;

#[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
type ProgramType = unsafe extern "sysv64" fn(*mut JitState, *const u32, *const u32);
#[cfg(all(target_arch = "x86_64", target_os = "windows"))]
type ProgramType = unsafe extern "win64" fn(*mut JitState, *const u32, *const u32);

/// Port of upstream `MacroJITx64Impl::JITState`.
#[repr(C)]
struct JitState {
    maxwell3d: *mut Maxwell3D,
    /// Stable pointer to `Maxwell3D::regs`, replacing upstream's C++ member
    /// offset load while preserving the same native indexed read.
    register_array: *const u32,
    registers: [u32; NUM_MACRO_REGISTERS],
    carry_flag: u32,
}

const _: () = assert!(offset_of!(JitState, maxwell3d) == 0);

/// Port of upstream `MacroJITx64Impl::OptimizerState`.
#[derive(Debug, Clone, Copy, Default)]
struct OptimizerState {
    can_skip_carry: bool,
    has_delayed_pc: bool,
    zero_reg_skip: bool,
    skip_dummy_addimmediate: bool,
    optimize_for_method_move: bool,
    enable_asserts: bool,
}

#[cfg(not(target_os = "windows"))]
unsafe extern "sysv64" fn macro_jit_send_thunk(
    maxwell3d: *mut Maxwell3D,
    method_address: u32,
    value: u32,
) {
    (&mut *maxwell3d).call_method(method_address & 0xfff, value, true);
}

#[cfg(target_os = "windows")]
unsafe extern "win64" fn macro_jit_send_thunk(
    maxwell3d: *mut Maxwell3D,
    method_address: u32,
    value: u32,
) {
    (&mut *maxwell3d).call_method(method_address & 0xfff, value, true);
}

#[cfg(not(target_os = "windows"))]
unsafe extern "sysv64" fn macro_jit_error_thunk(parameter: usize, max_parameter: usize) {
    log::error!(
        "Macro JIT: invalid parameter access {parameter:#x} ({:#x} is the last parameter)",
        max_parameter.wrapping_sub(std::mem::size_of::<u32>())
    );
}

#[cfg(target_os = "windows")]
unsafe extern "win64" fn macro_jit_error_thunk(parameter: usize, max_parameter: usize) {
    log::error!(
        "Macro JIT: invalid parameter access {parameter:#x} ({:#x} is the last parameter)",
        max_parameter.wrapping_sub(std::mem::size_of::<u32>())
    );
}

/// Port of upstream `MacroJITx64Impl`.
pub(crate) struct MacroJitX64Impl {
    assembler: CodeAssembler,
    code: Vec<u32>,
    optimizer: OptimizerState,
    next_opcode: Option<Opcode>,
    labels: Vec<Label>,
    delay_skip: Vec<Label>,
    end_of_code: Label,
    is_delay_slot: bool,
    pc: usize,
    program: Option<ProgramType>,
    maxwell3d: *mut Maxwell3D,
}

// Upstream owns compiled macros on the serialized GPU thread. The executable
// mapping and the non-owning Maxwell pointer move with that owner but are not
// accessed concurrently.
unsafe impl Send for MacroJitX64Impl {}

impl MacroJitX64Impl {
    /// Construct and compile a macro without a Maxwell owner (unit tests).
    #[cfg(test)]
    fn new(code: Vec<u32>) -> Self {
        Self::new_with_maxwell(code, std::ptr::null_mut())
    }

    /// Construct and compile a macro for the channel-owned Maxwell engine.
    pub(crate) fn new_with_maxwell(code: Vec<u32>, maxwell3d: *mut Maxwell3D) -> Self {
        let mut assembler = CodeAssembler::new(MAX_CODE_SIZE)
            .expect("MacroJITx64 must allocate its upstream-sized code buffer");
        let labels = (0..MAX_CODE_SIZE)
            .map(|_| assembler.create_label())
            .collect();
        let delay_skip = (0..MAX_CODE_SIZE)
            .map(|_| assembler.create_label())
            .collect();
        let end_of_code = assembler.create_label();
        let mut jit = Self {
            assembler,
            code,
            optimizer: OptimizerState::default(),
            next_opcode: None,
            labels,
            delay_skip,
            end_of_code,
            is_delay_slot: false,
            pc: 0,
            program: None,
            maxwell3d,
        };
        jit.compile()
            .expect("MacroJITx64 must compile valid uploaded macro code");
        jit.program = Some(unsafe { jit.assembler.get_code::<ProgramType>() });
        jit
    }

    /// Port of `MacroJITx64Impl::Optimizer_ScanFlags`.
    fn optimizer_scan_flags(&mut self) {
        self.optimizer.can_skip_carry = true;
        self.optimizer.has_delayed_pc = false;
        for &raw_op in &self.code {
            let opcode = Opcode::new(raw_op);
            if opcode.operation() == Operation::Alu
                && matches!(
                    opcode.alu_operation(),
                    AluOperation::AddWithCarry | AluOperation::SubtractWithBorrow
                )
            {
                self.optimizer.can_skip_carry = false;
            }
            if opcode.operation() == Operation::Branch && !opcode.branch_annul() {
                self.optimizer.has_delayed_pc = true;
            }
        }
    }

    fn state_offset(field: usize) -> i32 {
        i32::try_from(field).expect("JIT state offset must fit x86 displacement")
    }

    fn registers_offset(index: u32) -> i32 {
        Self::state_offset(offset_of!(JitState, registers) + index as usize * size_of::<u32>())
    }

    fn emit_prologue(&mut self) -> rxbyak::Result<()> {
        #[cfg(not(target_os = "windows"))]
        let callee_saved = [RBX, RBP, R12, rxbyak::R13, R14, R15];
        #[cfg(target_os = "windows")]
        let callee_saved = [RBX, RBP, RSI, RDI, R12, rxbyak::R13, R14, R15];
        for register in callee_saved {
            self.assembler.push(register)?;
        }
        self.assembler.sub(RSP, 8i32)?;
        self.assembler.mov(STATE, ABI_PARAM1)?;
        self.assembler.mov(PARAMETERS, ABI_PARAM2)?;
        self.assembler.mov(MAX_PARAMETER, ABI_PARAM3)?;
        self.assembler.xor_(RESULT, RESULT)?;
        self.assembler.xor_(METHOD_ADDRESS, METHOD_ADDRESS)?;
        self.assembler.xor_(BRANCH_HOLDER, BRANCH_HOLDER)?;
        let first_parameter = self.compile_fetch_parameter()?;
        self.assembler.mov(
            dword_ptr(RegExp::from(STATE) + Self::registers_offset(1)),
            first_parameter,
        )?;
        Ok(())
    }

    fn emit_epilogue(&mut self) -> rxbyak::Result<()> {
        self.assembler.add(RSP, 8i32)?;
        #[cfg(not(target_os = "windows"))]
        let callee_saved = [RBX, RBP, R12, rxbyak::R13, R14, R15];
        #[cfg(target_os = "windows")]
        let callee_saved = [RBX, RBP, RSI, RDI, R12, rxbyak::R13, R14, R15];
        for register in callee_saved.into_iter().rev() {
            self.assembler.pop(register)?;
        }
        self.assembler.ret()
    }

    fn push_persistent_caller_saved(&mut self) -> rxbyak::Result<()> {
        self.assembler.push(R10)?;
        self.assembler.push(R11)?;
        #[cfg(target_os = "windows")]
        self.assembler.sub(RSP, 32i32)?;
        Ok(())
    }

    fn pop_persistent_caller_saved(&mut self) -> rxbyak::Result<()> {
        #[cfg(target_os = "windows")]
        self.assembler.add(RSP, 32i32)?;
        self.assembler.pop(R11)?;
        self.assembler.pop(R10)
    }

    fn emit_far_call(&mut self, function: usize) -> rxbyak::Result<()> {
        self.assembler.mov(RAX, function as i64)?;
        self.assembler.call_reg(RAX)
    }

    /// Port of `MacroJITx64Impl::Compile`.
    fn compile(&mut self) -> rxbyak::Result<()> {
        self.emit_prologue()?;
        self.optimizer.zero_reg_skip = true;
        self.optimizer.skip_dummy_addimmediate = true;
        self.optimizer.optimize_for_method_move = true;
        self.optimizer.enable_asserts = false;
        self.optimizer_scan_flags();

        for index in 0..self.code.len() {
            self.next_opcode = self.code.get(index + 1).copied().map(Opcode::new);
            self.pc = index;
            self.compile_next_instruction()?;
        }
        self.assembler.bind(&self.end_of_code)?;
        self.emit_epilogue()?;
        self.assembler.ready()
    }

    /// Port of `MacroJITx64Impl::Compile_NextInstruction`.
    fn compile_next_instruction(&mut self) -> rxbyak::Result<bool> {
        let opcode = self.get_opcode();
        self.assembler.bind(&self.labels[self.pc])?;
        match opcode.operation() {
            Operation::Alu => self.compile_alu(opcode)?,
            Operation::AddImmediate => self.compile_add_immediate(opcode)?,
            Operation::ExtractInsert => self.compile_extract_insert(opcode)?,
            Operation::ExtractShiftLeftImmediate => {
                self.compile_extract_shift_left_immediate(opcode)?
            }
            Operation::ExtractShiftLeftRegister => {
                self.compile_extract_shift_left_register(opcode)?
            }
            Operation::Read => self.compile_read(opcode)?,
            Operation::Branch => self.compile_branch(opcode)?,
            Operation::Unused => log::warn!("Unimplemented macro opcode Unused"),
        }

        if self.optimizer.has_delayed_pc {
            if opcode.is_exit() {
                self.assembler.lea_label(RAX, &self.end_of_code)?;
                self.assembler.test(BRANCH_HOLDER, BRANCH_HOLDER)?;
                self.assembler.cmove(BRANCH_HOLDER, RAX)?;
                self.assembler
                    .je(&self.labels[self.pc + 1], JmpType::Near)?;
            } else {
                let no_delay_slot = self.assembler.create_label();
                self.assembler.test(BRANCH_HOLDER, BRANCH_HOLDER)?;
                self.assembler.je(&no_delay_slot, JmpType::Near)?;
                self.assembler.mov(RAX, BRANCH_HOLDER)?;
                self.assembler.xor_(BRANCH_HOLDER, BRANCH_HOLDER)?;
                self.assembler.jmp_reg(RAX)?;
                self.assembler.bind(&no_delay_slot)?;
            }
            self.assembler.bind(&self.delay_skip[self.pc])?;
            if opcode.is_exit() {
                return Ok(false);
            }
        } else {
            self.assembler.test(BRANCH_HOLDER, BRANCH_HOLDER)?;
            self.assembler.jne(&self.end_of_code, JmpType::Near)?;
            if opcode.is_exit() {
                self.assembler.inc(BRANCH_HOLDER)?;
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Port of `MacroJITx64Impl::Compile_ALU`.
    fn compile_alu(&mut self, opcode: Opcode) -> rxbyak::Result<()> {
        let is_a_zero = opcode.src_a() == 0;
        let is_b_zero = opcode.src_b() == 0;
        let valid_operation = !is_a_zero && !is_b_zero;
        let has_zero_register = is_a_zero || is_b_zero;
        let no_zero_reg_skip = matches!(
            opcode.alu_operation(),
            AluOperation::AddWithCarry | AluOperation::SubtractWithBorrow
        );
        let mut src_a = RESULT;
        let mut src_b = EAX;
        if !self.optimizer.zero_reg_skip || no_zero_reg_skip {
            src_a = self.compile_get_register(opcode.src_a(), RESULT)?;
            src_b = self.compile_get_register(opcode.src_b(), EAX)?;
        } else {
            if !is_a_zero {
                src_a = self.compile_get_register(opcode.src_a(), RESULT)?;
            }
            if !is_b_zero {
                src_b = self.compile_get_register(opcode.src_b(), EAX)?;
            }
        }
        let mut has_emitted = false;
        match opcode.alu_operation() {
            AluOperation::Add => {
                if !self.optimizer.zero_reg_skip || valid_operation {
                    self.assembler.add(src_a, src_b)?;
                }
                if !self.optimizer.can_skip_carry {
                    self.assembler.setc(byte_ptr(
                        RegExp::from(STATE) + Self::state_offset(offset_of!(JitState, carry_flag)),
                    ))?;
                }
            }
            AluOperation::AddWithCarry => {
                self.assembler.bt_imm(
                    dword_ptr(
                        RegExp::from(STATE) + Self::state_offset(offset_of!(JitState, carry_flag)),
                    ),
                    0,
                )?;
                self.assembler.adc(src_a, src_b)?;
                self.assembler.setc(byte_ptr(
                    RegExp::from(STATE) + Self::state_offset(offset_of!(JitState, carry_flag)),
                ))?;
            }
            AluOperation::Subtract => {
                if !self.optimizer.zero_reg_skip || valid_operation {
                    self.assembler.sub(src_a, src_b)?;
                    has_emitted = true;
                }
                if !self.optimizer.can_skip_carry && has_emitted {
                    self.assembler.setc(byte_ptr(
                        RegExp::from(STATE) + Self::state_offset(offset_of!(JitState, carry_flag)),
                    ))?;
                }
            }
            AluOperation::SubtractWithBorrow => {
                self.assembler.bt_imm(
                    dword_ptr(
                        RegExp::from(STATE) + Self::state_offset(offset_of!(JitState, carry_flag)),
                    ),
                    0,
                )?;
                self.assembler.sbb(src_a, src_b)?;
                self.assembler.setc(byte_ptr(
                    RegExp::from(STATE) + Self::state_offset(offset_of!(JitState, carry_flag)),
                ))?;
            }
            AluOperation::Xor => {
                if !self.optimizer.zero_reg_skip || valid_operation {
                    self.assembler.xor_(src_a, src_b)?;
                }
            }
            AluOperation::Or => {
                if !self.optimizer.zero_reg_skip || valid_operation {
                    self.assembler.or_(src_a, src_b)?;
                }
            }
            AluOperation::And => {
                if !self.optimizer.zero_reg_skip || !has_zero_register {
                    self.assembler.and_(src_a, src_b)?;
                }
            }
            AluOperation::AndNot => {
                if !self.optimizer.zero_reg_skip || !is_a_zero {
                    self.assembler.not_(src_b)?;
                    self.assembler.and_(src_a, src_b)?;
                }
            }
            AluOperation::Nand => {
                if !self.optimizer.zero_reg_skip || !is_a_zero {
                    self.assembler.and_(src_a, src_b)?;
                    self.assembler.not_(src_a)?;
                }
            }
            AluOperation::Invalid => log::warn!("Unimplemented ALU operation"),
        }
        self.compile_process_result(opcode.result_operation(), opcode.dst())
    }

    /// Port of `MacroJITx64Impl::Compile_AddImmediate`.
    fn compile_add_immediate(&mut self, opcode: Opcode) -> rxbyak::Result<()> {
        if self.optimizer.skip_dummy_addimmediate
            && opcode.result_operation() == ResultOperation::Move
            && opcode.dst() == 0
        {
            return Ok(());
        }
        if self.optimizer.optimize_for_method_move
            && opcode.result_operation() == ResultOperation::MoveAndSetMethod
            && self.next_opcode.is_some_and(|next| {
                next.result_operation() == ResultOperation::MoveAndSetMethod
                    && opcode.dst() == next.dst()
            })
        {
            return Ok(());
        }
        self.compile_register_plus_immediate(opcode)?;
        self.compile_process_result(opcode.result_operation(), opcode.dst())
    }

    fn compile_register_plus_immediate(&mut self, opcode: Opcode) -> rxbyak::Result<()> {
        let immediate = opcode.immediate();
        if self.optimizer.zero_reg_skip && opcode.src_a() == 0 {
            if immediate == 0 {
                self.assembler.xor_(RESULT, RESULT)?;
            } else {
                self.assembler.mov(RESULT, immediate)?;
            }
        } else {
            let result = self.compile_get_register(opcode.src_a(), RESULT)?;
            if immediate > 2 {
                self.assembler.add(result, immediate)?;
            } else if immediate == 1 {
                self.assembler.inc(result)?;
            } else if immediate < 0 {
                self.assembler.sub(result, immediate.wrapping_neg())?;
            }
        }
        Ok(())
    }

    /// Port of `MacroJITx64Impl::Compile_ExtractInsert`.
    fn compile_extract_insert(&mut self, opcode: Opcode) -> rxbyak::Result<()> {
        let dst = self.compile_get_register(opcode.src_a(), RESULT)?;
        let src = self.compile_get_register(opcode.src_b(), EAX)?;
        let mask = !(opcode.get_bitfield_mask() << opcode.bf_dst_bit());
        self.assembler.and_(dst, mask as i32)?;
        self.assembler.shr(src, opcode.bf_src_bit() as u8)?;
        self.assembler
            .and_(src, opcode.get_bitfield_mask() as i32)?;
        self.assembler.shl(src, opcode.bf_dst_bit() as u8)?;
        self.assembler.or_(dst, src)?;
        self.compile_process_result(opcode.result_operation(), opcode.dst())
    }

    /// Port of `MacroJITx64Impl::Compile_ExtractShiftLeftImmediate`.
    fn compile_extract_shift_left_immediate(&mut self, opcode: Opcode) -> rxbyak::Result<()> {
        self.compile_get_register(opcode.src_a(), ECX)?;
        let src = self.compile_get_register(opcode.src_b(), RESULT)?;
        self.assembler.shr_cl(src)?;
        self.assembler
            .and_(src, opcode.get_bitfield_mask() as i32)?;
        self.assembler.shl(src, opcode.bf_dst_bit() as u8)?;
        self.compile_process_result(opcode.result_operation(), opcode.dst())
    }

    /// Port of `MacroJITx64Impl::Compile_ExtractShiftLeftRegister`.
    fn compile_extract_shift_left_register(&mut self, opcode: Opcode) -> rxbyak::Result<()> {
        self.compile_get_register(opcode.src_a(), ECX)?;
        let src = self.compile_get_register(opcode.src_b(), RESULT)?;
        self.assembler.shr(src, opcode.bf_src_bit() as u8)?;
        self.assembler
            .and_(src, opcode.get_bitfield_mask() as i32)?;
        self.assembler.shl_cl(src)?;
        self.compile_process_result(opcode.result_operation(), opcode.dst())
    }

    /// Port of `MacroJITx64Impl::Compile_Read`.
    fn compile_read(&mut self, opcode: Opcode) -> rxbyak::Result<()> {
        self.compile_register_plus_immediate(opcode)?;
        if self.optimizer.enable_asserts {
            let pass_range_check = self.assembler.create_label();
            self.assembler
                .cmp(RESULT, crate::engines::ENGINE_REG_COUNT as i32)?;
            self.assembler.jb(&pass_range_check, JmpType::Near)?;
            self.assembler.int3()?;
            self.assembler.bind(&pass_range_check)?;
        }
        self.assembler.mov(
            RAX,
            qword_ptr(
                RegExp::from(STATE) + Self::state_offset(offset_of!(JitState, register_array)),
            ),
        )?;
        self.assembler.mov(
            RESULT,
            dword_ptr(RegExp::from(RAX) + RESULT.cvt64()? * size_of::<u32>() as u8),
        )?;
        self.compile_process_result(opcode.result_operation(), opcode.dst())
    }

    /// Port of `MacroJITx64Impl::Compile_Send`.
    fn compile_send(&mut self, value: Reg) -> rxbyak::Result<()> {
        self.push_persistent_caller_saved()?;
        self.assembler.mov(
            ABI_PARAM1,
            qword_ptr(RegExp::from(STATE) + Self::state_offset(offset_of!(JitState, maxwell3d))),
        )?;
        self.assembler.mov(ABI_PARAM2.cvt32()?, METHOD_ADDRESS)?;
        self.assembler.mov(ABI_PARAM3.cvt32()?, value)?;
        self.emit_far_call(macro_jit_send_thunk as usize)?;
        self.pop_persistent_caller_saved()?;

        let dont_process = self.assembler.create_label();
        self.assembler.test(METHOD_ADDRESS, 0x3f000i32)?;
        self.assembler.je(&dont_process, JmpType::Near)?;
        self.assembler.mov(ECX, METHOD_ADDRESS)?;
        self.assembler.and_(METHOD_ADDRESS, 0xfffi32)?;
        self.assembler.shr(ECX, 12)?;
        self.assembler.and_(ECX, 0x3fi32)?;
        self.assembler
            .lea(EAX, qword_ptr(RegExp::from(RCX) + R14))?;
        self.assembler.shl(ECX, 12)?;
        self.assembler.or_(EAX, ECX)?;
        self.assembler.mov(METHOD_ADDRESS, EAX)?;
        self.assembler.bind(&dont_process)
    }

    /// Port of `MacroJITx64Impl::Compile_Branch`.
    fn compile_branch(&mut self, opcode: Opcode) -> rxbyak::Result<()> {
        assert!(!self.is_delay_slot, "branch in a delay slot is invalid");
        let jump_address = (self.pc as i32 + opcode.get_branch_target() / 4) as usize;
        assert!(
            jump_address < self.labels.len(),
            "macro branch target out of range"
        );
        let end = self.assembler.create_label();
        let value = self.compile_get_register(opcode.src_a(), EAX)?;
        self.assembler.cmp(value, 0i32)?;
        if self.optimizer.has_delayed_pc {
            match opcode.branch_condition() {
                BranchCondition::Zero => self.assembler.jne(&end, JmpType::Near)?,
                BranchCondition::NotZero => self.assembler.je(&end, JmpType::Near)?,
            }
            if opcode.branch_annul() {
                self.assembler.xor_(BRANCH_HOLDER, BRANCH_HOLDER)?;
                self.assembler
                    .jmp(&self.labels[jump_address], JmpType::Near)?;
            } else {
                let handle_post_exit = self.assembler.create_label();
                let skip = self.assembler.create_label();
                self.assembler.jmp(&skip, JmpType::Near)?;
                self.assembler.bind(&handle_post_exit)?;
                self.assembler.xor_(BRANCH_HOLDER, BRANCH_HOLDER)?;
                self.assembler
                    .jmp(&self.labels[jump_address], JmpType::Near)?;
                self.assembler.bind(&skip)?;
                self.assembler.lea_label(BRANCH_HOLDER, &handle_post_exit)?;
                self.assembler
                    .jmp(&self.delay_skip[self.pc], JmpType::Near)?;
            }
        } else {
            match opcode.branch_condition() {
                BranchCondition::Zero => self
                    .assembler
                    .je(&self.labels[jump_address], JmpType::Near)?,
                BranchCondition::NotZero => self
                    .assembler
                    .jne(&self.labels[jump_address], JmpType::Near)?,
            }
        }
        self.assembler.bind(&end)
    }

    /// Port of `MacroJITx64Impl::Compile_FetchParameter`.
    fn compile_fetch_parameter(&mut self) -> rxbyak::Result<Reg> {
        let parameter_ok = self.assembler.create_label();
        self.assembler.cmp(PARAMETERS, MAX_PARAMETER)?;
        self.assembler.jb(&parameter_ok, JmpType::Near)?;
        self.push_persistent_caller_saved()?;
        self.assembler.mov(ABI_PARAM1, PARAMETERS)?;
        self.assembler.mov(ABI_PARAM2, MAX_PARAMETER)?;
        self.emit_far_call(macro_jit_error_thunk as usize)?;
        self.pop_persistent_caller_saved()?;
        self.assembler.bind(&parameter_ok)?;
        self.assembler
            .mov(EAX, dword_ptr(RegExp::from(PARAMETERS)))?;
        self.assembler.add(PARAMETERS, size_of::<u32>() as i32)?;
        Ok(EAX)
    }

    /// Port of `MacroJITx64Impl::Compile_GetRegister`.
    fn compile_get_register(&mut self, index: u32, dst: Reg) -> rxbyak::Result<Reg> {
        if index == 0 {
            self.assembler.xor_(dst, dst)?;
        } else {
            self.assembler.mov(
                dst,
                dword_ptr(RegExp::from(STATE) + Self::registers_offset(index)),
            )?;
        }
        Ok(dst)
    }

    fn compile_set_register(&mut self, index: u32, result: Reg) -> rxbyak::Result<()> {
        if index != 0 {
            self.assembler.mov(
                dword_ptr(RegExp::from(STATE) + Self::registers_offset(index)),
                result,
            )?;
        }
        Ok(())
    }

    /// Port of `MacroJITx64Impl::Compile_ProcessResult`.
    fn compile_process_result(
        &mut self,
        operation: ResultOperation,
        register: u32,
    ) -> rxbyak::Result<()> {
        match operation {
            ResultOperation::IgnoreAndFetch => {
                let parameter = self.compile_fetch_parameter()?;
                self.compile_set_register(register, parameter)?;
            }
            ResultOperation::Move => self.compile_set_register(register, RESULT)?,
            ResultOperation::MoveAndSetMethod => {
                self.compile_set_register(register, RESULT)?;
                self.assembler.mov(METHOD_ADDRESS, RESULT)?;
            }
            ResultOperation::FetchAndSend => {
                let parameter = self.compile_fetch_parameter()?;
                self.compile_set_register(register, parameter)?;
                self.compile_send(RESULT)?;
            }
            ResultOperation::MoveAndSend => {
                self.compile_set_register(register, RESULT)?;
                self.compile_send(RESULT)?;
            }
            ResultOperation::FetchAndSetMethod => {
                let parameter = self.compile_fetch_parameter()?;
                self.compile_set_register(register, parameter)?;
                self.assembler.mov(METHOD_ADDRESS, RESULT)?;
            }
            ResultOperation::MoveAndSetMethodFetchAndSend => {
                self.compile_set_register(register, RESULT)?;
                self.assembler.mov(METHOD_ADDRESS, RESULT)?;
                let parameter = self.compile_fetch_parameter()?;
                self.compile_send(parameter)?;
            }
            ResultOperation::MoveAndSetMethodSend => {
                self.compile_set_register(register, RESULT)?;
                self.assembler.mov(METHOD_ADDRESS, RESULT)?;
                self.assembler.shr(RESULT, 12)?;
                self.assembler.and_(RESULT, 0b111111i32)?;
                self.compile_send(RESULT)?;
            }
        }
        Ok(())
    }

    fn get_opcode(&self) -> Opcode {
        assert!(self.pc < self.code.len());
        Opcode::new(self.code[self.pc])
    }

    fn run(&mut self, parameters: &[u32]) -> JitState {
        let register_array = if self.maxwell3d.is_null() {
            std::ptr::null()
        } else {
            unsafe { (&*self.maxwell3d).register_array_ptr() }
        };
        self.run_with_register_array(parameters, register_array)
    }

    fn run_with_register_array(
        &mut self,
        parameters: &[u32],
        register_array: *const u32,
    ) -> JitState {
        let mut state = JitState {
            maxwell3d: self.maxwell3d,
            register_array,
            registers: [0; NUM_MACRO_REGISTERS],
            carry_flag: 0,
        };
        let end = unsafe { parameters.as_ptr().add(parameters.len()) };
        let program = self
            .program
            .expect("MacroJITx64 program must exist after successful compilation");
        unsafe { program(&mut state, parameters.as_ptr(), end) };
        state
    }
}

impl CachedMacro for MacroJitX64Impl {
    /// Port of `MacroJITx64Impl::Execute`.
    fn execute(&mut self, parameters: &mut [u32], _method: u32) {
        let _ = self.run(parameters);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::macro_engine::macro_interpreter::MacroInterpreterImpl;

    fn add_immediate(
        dst: u32,
        src_a: u32,
        immediate: i32,
        result: ResultOperation,
        exit: bool,
    ) -> u32 {
        Operation::AddImmediate as u32
            | ((result as u32) << 4)
            | ((exit as u32) << 7)
            | (dst << 8)
            | (src_a << 11)
            | (((immediate as u32) & 0x3ffff) << 14)
    }

    fn alu(
        dst: u32,
        src_a: u32,
        src_b: u32,
        operation: AluOperation,
        result: ResultOperation,
        exit: bool,
    ) -> u32 {
        Operation::Alu as u32
            | ((result as u32) << 4)
            | ((exit as u32) << 7)
            | (dst << 8)
            | (src_a << 11)
            | (src_b << 14)
            | ((operation as u32) << 17)
    }

    fn branch(src_a: u32, immediate: i32, condition: BranchCondition, annul: bool) -> u32 {
        Operation::Branch as u32
            | ((condition as u32) << 4)
            | ((annul as u32) << 5)
            | (src_a << 11)
            | (((immediate as u32) & 0x3ffff) << 14)
    }

    fn extract_insert(
        dst: u32,
        src_a: u32,
        src_b: u32,
        src_bit: u32,
        size: u32,
        dst_bit: u32,
        exit: bool,
    ) -> u32 {
        Operation::ExtractInsert as u32
            | ((ResultOperation::Move as u32) << 4)
            | ((exit as u32) << 7)
            | (dst << 8)
            | (src_a << 11)
            | (src_b << 14)
            | (src_bit << 17)
            | (size << 22)
            | (dst_bit << 27)
    }

    fn read(dst: u32, src_a: u32, immediate: i32, exit: bool) -> u32 {
        Operation::Read as u32
            | ((ResultOperation::Move as u32) << 4)
            | ((exit as u32) << 7)
            | (dst << 8)
            | (src_a << 11)
            | (((immediate as u32) & 0x3ffff) << 14)
    }

    fn assert_jit_matches_interpreter(code: Vec<u32>, parameters: &[u32]) -> JitState {
        let mut interpreter = MacroInterpreterImpl::new(code.clone());
        let mut interpreter_parameters = parameters.to_vec();
        interpreter.execute(&mut interpreter_parameters, 0);

        let mut jit = MacroJitX64Impl::new(code);
        let state = jit.run(parameters);
        assert_eq!(state.registers, interpreter.registers_for_test());
        state
    }

    #[test]
    fn jit_state_layout_matches_upstream_prefix() {
        assert_eq!(offset_of!(JitState, maxwell3d), 0);
        assert_eq!(
            offset_of!(JitState, register_array),
            std::mem::size_of::<*mut Maxwell3D>()
        );
        assert_eq!(
            offset_of!(JitState, registers),
            2 * std::mem::size_of::<*mut Maxwell3D>()
        );
        assert_eq!(offset_of!(JitState, carry_flag), 48);
        assert_eq!(size_of::<JitState>(), 56);
    }

    #[test]
    fn optimizer_scan_flags_matches_upstream() {
        let exit = add_immediate(0, 0, 0, ResultOperation::Move, true);
        let delay = add_immediate(0, 0, 0, ResultOperation::Move, false);
        let jit = MacroJitX64Impl::new(vec![exit, delay]);
        assert!(jit.optimizer.can_skip_carry);
        assert!(!jit.optimizer.has_delayed_pc);
    }

    #[test]
    fn native_add_immediate_executes_exit_delay_slot() {
        let exit = add_immediate(2, 1, 5, ResultOperation::Move, true);
        let delay = add_immediate(3, 2, 7, ResultOperation::Move, false);
        let mut jit = MacroJitX64Impl::new(vec![exit, delay]);
        let state = jit.run(&[42]);
        assert_eq!(state.registers[1], 42);
        assert_eq!(state.registers[2], 47);
        assert_eq!(state.registers[3], 54);
    }

    #[test]
    fn native_alu_carry_chain_matches_interpreter() {
        let code = vec![
            add_immediate(2, 0, -1, ResultOperation::Move, false),
            alu(3, 1, 2, AluOperation::Add, ResultOperation::Move, false),
            alu(
                4,
                0,
                0,
                AluOperation::AddWithCarry,
                ResultOperation::Move,
                true,
            ),
            add_immediate(5, 4, 7, ResultOperation::Move, false),
        ];
        let state = assert_jit_matches_interpreter(code, &[1]);
        assert_eq!(state.registers[3], 0);
        assert_eq!(state.registers[4], 1);
        assert_eq!(state.registers[5], 8);
    }

    #[test]
    fn native_taken_branch_executes_delay_slot() {
        let code = vec![
            add_immediate(2, 0, 0, ResultOperation::Move, false),
            branch(2, 3, BranchCondition::Zero, false),
            add_immediate(3, 0, 10, ResultOperation::Move, false),
            add_immediate(3, 0, 99, ResultOperation::Move, false),
            add_immediate(4, 3, 1, ResultOperation::Move, true),
            add_immediate(5, 4, 1, ResultOperation::Move, false),
        ];
        let state = assert_jit_matches_interpreter(code, &[0]);
        assert_eq!(state.registers[3], 10);
        assert_eq!(state.registers[4], 11);
        assert_eq!(state.registers[5], 12);
    }

    #[test]
    fn native_annulled_branch_skips_delay_slot() {
        let code = vec![
            add_immediate(2, 0, 0, ResultOperation::Move, false),
            branch(2, 2, BranchCondition::Zero, true),
            add_immediate(3, 0, 55, ResultOperation::Move, false),
            add_immediate(4, 0, 7, ResultOperation::Move, true),
            add_immediate(5, 4, 1, ResultOperation::Move, false),
        ];
        let state = assert_jit_matches_interpreter(code, &[0]);
        assert_eq!(state.registers[3], 0);
        assert_eq!(state.registers[4], 7);
        assert_eq!(state.registers[5], 8);
    }

    #[test]
    fn native_extract_insert_matches_interpreter() {
        let code = vec![
            add_immediate(2, 0, 0x1234, ResultOperation::Move, false),
            extract_insert(3, 1, 2, 8, 8, 16, true),
            add_immediate(4, 3, 0, ResultOperation::Move, false),
        ];
        let state = assert_jit_matches_interpreter(code, &[0xaaaa_bbbb]);
        assert_eq!(state.registers[3], 0xaa12_bbbb);
        assert_eq!(state.registers[4], 0xaa12_bbbb);
    }

    #[test]
    fn native_read_uses_direct_register_array_load() {
        let code = vec![
            read(2, 0, 7, true),
            add_immediate(3, 2, 1, ResultOperation::Move, false),
        ];
        let mut registers = [0u32; crate::engines::ENGINE_REG_COUNT];
        registers[7] = 0x1234_5678;
        let mut jit = MacroJitX64Impl::new(code);
        let state = jit.run_with_register_array(&[0], registers.as_ptr());
        assert_eq!(state.registers[2], 0x1234_5678);
        assert_eq!(state.registers[3], 0x1234_5679);
    }

    #[test]
    fn native_send_uses_maxwell_method_and_increment() {
        let code = vec![
            add_immediate(2, 0, 0x1100, ResultOperation::MoveAndSetMethod, false),
            add_immediate(3, 0, 0x55, ResultOperation::MoveAndSend, false),
            add_immediate(4, 0, 0x66, ResultOperation::MoveAndSend, true),
            add_immediate(0, 0, 0, ResultOperation::Move, false),
        ];
        let mut maxwell3d = Maxwell3D::new();
        let maxwell3d_ptr = std::ptr::from_mut(&mut maxwell3d);
        let mut jit = MacroJitX64Impl::new_with_maxwell(code, maxwell3d_ptr);
        jit.run(&[0]);
        assert_eq!(maxwell3d.get_register_value(0x100), 0x55);
        assert_eq!(maxwell3d.get_register_value(0x101), 0x66);
    }
}

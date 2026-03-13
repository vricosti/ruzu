// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/macro/macro_jit_x64.h` and `macro_jit_x64.cpp`.
//!
//! x86-64 JIT compiler for Maxwell macro programs. The upstream implementation
//! uses Xbyak to generate native x64 code at runtime. This module provides the
//! structural port with stub implementations — actual JIT compilation would
//! require an x64 code generation library (e.g., dynasm-rs or cranelift).

use super::macro_engine::{CachedMacro, NUM_MACRO_REGISTERS};

// ── Constants ────────────────────────────────────────────────────────────────

/// Maximum code buffer size for JIT-compiled macros.
///
/// Port of `MAX_CODE_SIZE` from `macro_jit_x64.cpp`.
const MAX_CODE_SIZE: usize = 0x10000;

// ── JIT State ────────────────────────────────────────────────────────────────

/// Runtime state for JIT-compiled macro execution.
///
/// Port of `MacroJITx64Impl::JITState` from `macro_jit_x64.cpp`.
#[repr(C)]
pub struct JitState {
    /// Pointer to Maxwell3D engine (offset 0x0 in upstream).
    pub maxwell3d: usize, // *mut Maxwell3D in practice
    /// General-purpose macro registers.
    pub registers: [u32; NUM_MACRO_REGISTERS],
    /// Carry flag from ALU operations.
    pub carry_flag: u32,
}

// Upstream: `static_assert(offsetof(JITState, maxwell3d) == 0)`
const _: () = assert!(std::mem::offset_of!(JitState, maxwell3d) == 0);

/// Optimizer flags for the JIT compiler.
///
/// Port of `MacroJITx64Impl::OptimizerState` from `macro_jit_x64.cpp`.
#[derive(Debug, Clone, Copy, Default)]
pub struct OptimizerState {
    /// Can skip carry flag handling if no AddWithCarry/SubtractWithBorrow ops exist.
    pub can_skip_carry: bool,
    /// Whether any branch has a delay slot (non-annul branch).
    pub has_delayed_pc: bool,
    /// Optimize register 0 reads as zero without loading.
    pub zero_reg_skip: bool,
    /// Skip dummy AddImmediate instructions (NOP pattern).
    pub skip_dummy_addimmediate: bool,
    /// Optimize redundant method move sequences.
    pub optimize_for_method_move: bool,
    /// Enable runtime assertions in generated code.
    pub enable_asserts: bool,
}

// ── JIT Compiled Macro ───────────────────────────────────────────────────────

/// A JIT-compiled macro program.
///
/// Port of `MacroJITx64Impl` from `macro_jit_x64.cpp`.
///
/// This is a stub — actual x64 code generation requires a JIT library.
pub struct MacroJitX64Impl {
    code: Vec<u32>,
    optimizer: OptimizerState,
    // In a full implementation, this would hold the generated native code buffer
    // and a function pointer to the compiled program.
}

impl MacroJitX64Impl {
    /// Create and compile a new JIT macro.
    ///
    /// Port of `MacroJITx64Impl::MacroJITx64Impl`.
    pub fn new(code: Vec<u32>) -> Self {
        let mut jit = Self {
            code,
            optimizer: OptimizerState::default(),
        };
        jit.compile();
        jit
    }

    /// Scan the macro code for optimization opportunities.
    ///
    /// Port of `MacroJITx64Impl::Optimizer_ScanFlags`.
    fn optimizer_scan_flags(&mut self) {
        use super::macro_engine::{AluOperation, Opcode, Operation};

        self.optimizer.can_skip_carry = true;
        self.optimizer.has_delayed_pc = false;

        for &raw_op in &self.code {
            let op = Opcode::new(raw_op);
            if op.operation() == Operation::Alu {
                let alu_op = op.alu_operation();
                if alu_op == AluOperation::AddWithCarry
                    || alu_op == AluOperation::SubtractWithBorrow
                {
                    self.optimizer.can_skip_carry = false;
                }
            }
            if op.operation() == Operation::Branch && !op.branch_annul() {
                self.optimizer.has_delayed_pc = true;
            }
        }
    }

    /// Compile the macro code into native x64 instructions.
    ///
    /// Port of `MacroJITx64Impl::Compile`.
    fn compile(&mut self) {
        self.optimizer.zero_reg_skip = true;
        self.optimizer.skip_dummy_addimmediate = true;
        self.optimizer.optimize_for_method_move = true;
        self.optimizer.enable_asserts = false;

        self.optimizer_scan_flags();

        // TODO: Actual x64 code generation using a JIT library.
        // The upstream uses Xbyak; a Rust port would use dynasm-rs or cranelift.
        log::warn!("MacroJITx64: JIT compilation is stubbed; falling back behavior expected");
    }
}

impl CachedMacro for MacroJitX64Impl {
    /// Execute the JIT-compiled macro.
    ///
    /// Port of `MacroJITx64Impl::Execute`.
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — actual execution requires a generated native code buffer and a
        // function pointer to the compiled program (Xbyak in upstream;
        // dynasm-rs or cranelift in a full Rust port).
        // Upstream: MacroJITx64Impl::Execute() in video_core/macro/macro_jit_x64.cpp
        log::warn!("MacroJITx64::execute: JIT execution not yet implemented (requires x64 code generation)");
    }
}

// ── MacroJITx64 Engine ───────────────────────────────────────────────────────

/// x64 JIT macro engine backend.
///
/// Port of `Tegra::MacroJITx64`.
pub struct MacroJitX64;

impl MacroJitX64 {
    pub fn new() -> Self {
        Self
    }

    /// Compile macro code into a JIT-compiled cached program.
    ///
    /// Port of `MacroJITx64::Compile`.
    pub fn compile(&self, code: &[u32]) -> Box<dyn CachedMacro> {
        Box::new(MacroJitX64Impl::new(code.to_vec()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_state_layout() {
        assert_eq!(std::mem::offset_of!(JitState, maxwell3d), 0);
        assert_eq!(
            std::mem::offset_of!(JitState, registers),
            std::mem::size_of::<usize>()
        );
    }

    #[test]
    fn optimizer_scan_flags_no_carry() {
        // Simple AddImmediate opcode: operation=1, no carry ops
        let code = vec![0b001]; // operation = AddImmediate
        let jit = MacroJitX64Impl::new(code);
        assert!(jit.optimizer.can_skip_carry);
    }

    #[test]
    fn macro_jit_x64_compile() {
        let engine = MacroJitX64::new();
        let _program = engine.compile(&[0, 0, 0]);
        // Just verify it doesn't panic during compilation
    }
}

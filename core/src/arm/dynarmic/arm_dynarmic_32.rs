// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic_32.h and arm_dynarmic_32.cpp
//! ARM32 dynarmic backend.

use crate::arm::arm_interface::{
    ArmInterface, ArmInterfaceBase, Architecture, DebugWatchpoint, HaltReason, KProcess,
    KThread, ThreadContext,
};

/// ARM32 Dynarmic JIT backend.
///
/// Corresponds to upstream `Core::ArmDynarmic32`.
///
/// This is a structural port. The actual JIT (Dynarmic::A32::Jit) is represented
/// as an opaque type until rdynarmic is integrated.
pub struct ArmDynarmic32 {
    pub base: ArmInterfaceBase,

    // TODO: Replace with actual System reference
    // m_system: &System,

    // TODO: Replace with actual DynarmicExclusiveMonitor reference
    // m_exclusive_monitor: &DynarmicExclusiveMonitor,

    /// Core index for this CPU
    core_index: usize,

    /// SVC callback number
    svc_swi: u32,

    /// Watchpoint that caused a halt
    halted_watchpoint: Option<DebugWatchpoint>,

    /// Context saved at breakpoint
    breakpoint_context: ThreadContext,

    // TODO: Opaque JIT handle
    // m_jit: Dynarmic::A32::Jit,
    // m_cb: DynarmicCallbacks32,
    // m_cp15: DynarmicCP15,
}

impl ArmDynarmic32 {
    /// Create a new ARM32 dynarmic backend.
    ///
    /// Corresponds to upstream `ArmDynarmic32::ArmDynarmic32`.
    pub fn new(
        _system: &dyn std::any::Any,
        uses_wall_clock: bool,
        _process: &KProcess,
        _exclusive_monitor: &dyn std::any::Any,
        core_index: usize,
    ) -> Self {
        // TODO: Create JIT, callbacks, and CP15 coprocessor
        // auto& page_table_impl = process->GetPageTable().GetBasePageTable().GetImpl();
        // m_jit = MakeJit(&page_table_impl);
        // ScopedJitExecution::RegisterHandler();

        Self {
            base: ArmInterfaceBase::new(uses_wall_clock),
            core_index,
            svc_swi: 0,
            halted_watchpoint: None,
            breakpoint_context: ThreadContext::default(),
        }
    }

    /// Check if CPU is in Thumb mode.
    ///
    /// Corresponds to upstream `ArmDynarmic32::IsInThumbMode`.
    pub fn is_in_thumb_mode(&self) -> bool {
        // return (m_jit->Cpsr() & 0x20) != 0;
        todo!("Requires JIT integration")
    }

    /// Convert FPSCR to separate FPSR and FPCR values.
    ///
    /// Corresponds to upstream `FpscrToFpsrFpcr`.
    fn fpscr_to_fpsr_fpcr(fpscr: u32) -> (u32, u32) {
        // FPSCR bits [31:27] -> FPSR[31:27]
        // FPSCR bit [7] -> FPSR[7]
        // FPSCR bits [4:0] -> FPSR[4:0]
        let nzcv = fpscr & 0xf800_0000;
        let idc = fpscr & 0x80;
        let fiq = fpscr & 0x1f;
        let fpsr = nzcv | idc | fiq;

        // FPSCR bits [26:15] -> FPCR[26:15]
        // FPSCR bits [12:8] -> FPCR[12:8]
        let round = fpscr & 0x07ff_8000;
        let trap = fpscr & 0x1f00;
        let fpcr = round | trap;

        (fpsr, fpcr)
    }

    /// Convert separate FPSR and FPCR values back to FPSCR.
    ///
    /// Corresponds to upstream `FpsrFpcrToFpscr`.
    fn fpsr_fpcr_to_fpscr(fpsr: u64, fpcr: u64) -> u32 {
        let combined = (fpsr as u32) | (fpcr as u32);
        let (s, c) = Self::fpscr_to_fpsr_fpcr(combined);
        s | c
    }
}

impl ArmInterface for ArmDynarmic32 {
    fn run_thread(&mut self, _thread: &mut KThread) -> HaltReason {
        // ScopedJitExecution sj(thread->GetOwnerProcess());
        // m_jit->ClearExclusiveState();
        // return TranslateHaltReason(m_jit->Run());
        todo!("Requires JIT integration")
    }

    fn step_thread(&mut self, _thread: &mut KThread) -> HaltReason {
        // ScopedJitExecution sj(thread->GetOwnerProcess());
        // m_jit->ClearExclusiveState();
        // return TranslateHaltReason(m_jit->Step());
        todo!("Requires JIT integration")
    }

    fn clear_instruction_cache(&mut self) {
        // m_jit->ClearCache();
        todo!("Requires JIT integration")
    }

    fn invalidate_cache_range(&mut self, _addr: u64, _size: usize) {
        // m_jit->InvalidateCacheRange(static_cast<u32>(addr), size);
        todo!("Requires JIT integration")
    }

    fn get_architecture(&self) -> Architecture {
        Architecture::AArch32
    }

    fn get_context(&self, _ctx: &mut ThreadContext) {
        // Upstream reads from JIT GPRs, ExtRegs, Cpsr, Fpscr
        // and maps them into ThreadContext fields.
        todo!("Requires JIT integration")
    }

    fn set_context(&mut self, _ctx: &ThreadContext) {
        // Upstream writes JIT GPRs, ExtRegs, Cpsr, Fpscr from ThreadContext.
        todo!("Requires JIT integration")
    }

    fn set_tpidrro_el0(&mut self, _value: u64) {
        // m_cp15->uro = static_cast<u32>(value);
        todo!("Requires JIT integration")
    }

    fn get_svc_arguments(&self, _args: &mut [u64; 8]) {
        // Reads GPR[0..8] from JIT
        todo!("Requires JIT integration")
    }

    fn set_svc_arguments(&mut self, _args: &[u64; 8]) {
        // Writes GPR[0..8] to JIT as u32
        todo!("Requires JIT integration")
    }

    fn get_svc_number(&self) -> u32 {
        self.svc_swi
    }

    fn signal_interrupt(&mut self, _thread: &mut KThread) {
        // m_jit->HaltExecution(BreakLoop);
        todo!("Requires JIT integration")
    }

    fn halted_watchpoint(&self) -> Option<&DebugWatchpoint> {
        self.halted_watchpoint.as_ref()
    }

    fn rewind_breakpoint_instruction(&mut self) {
        // this->SetContext(m_breakpoint_context);
        todo!("Requires JIT integration")
    }
}

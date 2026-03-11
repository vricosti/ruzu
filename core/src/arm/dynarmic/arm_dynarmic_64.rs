// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic_64.h and arm_dynarmic_64.cpp
//! ARM64 dynarmic backend.

use crate::arm::arm_interface::{
    ArmInterface, ArmInterfaceBase, Architecture, DebugWatchpoint, HaltReason, KProcess,
    KThread, ThreadContext,
};

/// ARM64 Dynarmic JIT backend.
///
/// Corresponds to upstream `Core::ArmDynarmic64`.
///
/// This is a structural port. The actual JIT (Dynarmic::A64::Jit) is represented
/// as an opaque type until rdynarmic is integrated.
pub struct ArmDynarmic64 {
    pub base: ArmInterfaceBase,

    // TODO: Replace with actual System reference
    // m_system: &System,

    // TODO: Replace with actual DynarmicExclusiveMonitor reference
    // m_exclusive_monitor: &DynarmicExclusiveMonitor,

    /// Core index for this CPU
    core_index: usize,

    /// SVC callback number
    svc: u32,

    /// Watchpoint that caused a halt
    halted_watchpoint: Option<DebugWatchpoint>,

    /// Context saved at breakpoint
    breakpoint_context: ThreadContext,

    // TODO: Opaque JIT handle
    // m_jit: Dynarmic::A64::Jit,
    // m_cb: DynarmicCallbacks64,
}

impl ArmDynarmic64 {
    /// Create a new ARM64 dynarmic backend.
    ///
    /// Corresponds to upstream `ArmDynarmic64::ArmDynarmic64`.
    pub fn new(
        _system: &dyn std::any::Any,
        uses_wall_clock: bool,
        _process: &KProcess,
        _exclusive_monitor: &dyn std::any::Any,
        core_index: usize,
    ) -> Self {
        // TODO: Create JIT and callbacks
        // auto& page_table = process->GetPageTable().GetBasePageTable();
        // auto& page_table_impl = page_table.GetImpl();
        // m_jit = MakeJit(&page_table_impl, page_table.GetAddressSpaceWidth());
        // ScopedJitExecution::RegisterHandler();

        Self {
            base: ArmInterfaceBase::new(uses_wall_clock),
            core_index,
            svc: 0,
            halted_watchpoint: None,
            breakpoint_context: ThreadContext::default(),
        }
    }
}

impl ArmInterface for ArmDynarmic64 {
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
        // m_jit->InvalidateCacheRange(addr, size);
        todo!("Requires JIT integration")
    }

    fn get_architecture(&self) -> Architecture {
        Architecture::AArch64
    }

    fn get_context(&self, _ctx: &mut ThreadContext) {
        // Upstream reads GPRs[0..29], fp=GPR[29], lr=GPR[30],
        // SP, PC, Pstate, Vectors, Fpcr, Fpsr, tpidr_el0
        todo!("Requires JIT integration")
    }

    fn set_context(&mut self, _ctx: &ThreadContext) {
        // Upstream writes GPRs, SP, PC, Pstate, Vectors, Fpcr, Fpsr, tpidr_el0
        todo!("Requires JIT integration")
    }

    fn set_tpidrro_el0(&mut self, _value: u64) {
        // m_cb->m_tpidrro_el0 = value;
        todo!("Requires JIT integration")
    }

    fn get_svc_arguments(&self, _args: &mut [u64; 8]) {
        // Reads j.GetRegister(0..8)
        todo!("Requires JIT integration")
    }

    fn set_svc_arguments(&mut self, _args: &[u64; 8]) {
        // Writes j.SetRegister(0..8, args[i])
        todo!("Requires JIT integration")
    }

    fn get_svc_number(&self) -> u32 {
        self.svc
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

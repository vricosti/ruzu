//! Port of zuyu/src/core/hle/kernel/physical_core.h/.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! PhysicalCore: represents a single emulated CPU core, responsible for
//! running guest threads and handling interrupts. Full implementation
//! requires KernelCore, KThread, KProcess, ArmInterface, Debugger.

use std::sync::{Arc, Condvar, Mutex};

use crate::arm::arm_interface::{ArmInterface, HaltReason, KThread as OpaqueKThread, ThreadContext};
use crate::core::System;
use crate::hle::kernel::svc_dispatch::{self, SvcArgs};

use super::{k_process::KProcess, k_scheduler::KScheduler, k_thread::KThread};
#[cfg(feature = "debug-logs")]
use super::physical_core_log;

pub enum PhysicalCoreExecutionControl {
    Continue,
    Yield,
    Break,
}

/// Represents a single emulated physical CPU core.
pub struct PhysicalCore {
    m_core_index: usize,
    m_guard: Mutex<PhysicalCoreState>,
    m_on_interrupt: Condvar,
    m_runtime: Mutex<Option<PhysicalCoreRuntime>>,
}

struct PhysicalCoreState {
    m_is_interrupted: bool,
    m_is_single_core: bool,
    /// Raw pointer to the ArmInterface currently executing on this core.
    /// Set at the start of RunThread, cleared on exit.
    /// Upstream: `Core::ArmInterface* m_arm_interface{}`.
    m_arm_interface: Option<*mut dyn crate::arm::arm_interface::ArmInterface>,
    /// Raw pointer to the KThread currently executing on this core.
    /// Upstream: `KThread* m_current_thread{}`.
    m_current_thread: Option<*mut KThread>,
}

// Safety: Raw pointers in PhysicalCoreState are only accessed under m_guard mutex,
// matching upstream's std::scoped_lock lk{m_guard} pattern.
unsafe impl Send for PhysicalCoreState {}
unsafe impl Sync for PhysicalCoreState {}

struct PhysicalCoreRuntime {
    m_current_thread: Arc<Mutex<KThread>>,
}

pub enum PhysicalCoreExecutionEvent {
    SupervisorCall { svc_num: u32, svc_args: SvcArgs },
    Halted(HaltReason),
}

impl PhysicalCore {
    fn drain_current_thread_termination(&self, scheduler: &Arc<Mutex<KScheduler>>) -> bool {
        let runtime_guard = self.m_runtime.lock().unwrap();
        let Some(runtime) = runtime_guard.as_ref() else {
            return false;
        };

        if {
            let thread = runtime.m_current_thread.lock().unwrap();
            thread.is_termination_requested() && !thread.is_signaled()
        } {
            runtime.m_current_thread.lock().unwrap().exit();
            scheduler.lock().unwrap().request_schedule();
            return true;
        }

        false
    }

    fn event_from_halt(
        &self,
        jit: &mut dyn ArmInterface,
        halt_reason: HaltReason,
    ) -> PhysicalCoreExecutionEvent {
        if halt_reason.contains(HaltReason::SUPERVISOR_CALL) {
            let svc_num = jit.get_svc_number();
            let mut svc_args = [0u64; 8];
            jit.get_svc_arguments(&mut svc_args);
            PhysicalCoreExecutionEvent::SupervisorCall { svc_num, svc_args }
        } else {
            PhysicalCoreExecutionEvent::Halted(halt_reason)
        }
    }

    pub fn new(core_index: usize, is_multicore: bool) -> Self {
        Self {
            m_core_index: core_index,
            m_guard: Mutex::new(PhysicalCoreState {
                m_is_interrupted: false,
                m_is_single_core: !is_multicore,
                m_arm_interface: None,
                m_current_thread: None,
            }),
            m_on_interrupt: Condvar::new(),
            m_runtime: Mutex::new(None),
        }
    }

    /// Execute guest code until the next runtime event.
    pub fn run_thread(
        &self,
        jit: &mut dyn ArmInterface,
        thread: &mut OpaqueKThread,
    ) -> PhysicalCoreExecutionEvent {
        let halt_reason = jit.run_thread(thread);
        self.event_from_halt(jit, halt_reason)
    }

    pub fn step_thread(
        &self,
        jit: &mut dyn ArmInterface,
        thread: &mut OpaqueKThread,
    ) -> PhysicalCoreExecutionEvent {
        let halt_reason = jit.step_thread(thread);
        self.event_from_halt(jit, halt_reason)
    }

    pub fn initialize_guest_runtime(
        &self,
        main_thread: Arc<Mutex<KThread>>,
        jit: &mut dyn ArmInterface,
        thread_context: &mut ThreadContext,
    ) {
        self.restore_thread_to_jit(jit, thread_context, &main_thread);
        *self.m_runtime.lock().unwrap() = Some(PhysicalCoreRuntime {
            m_current_thread: main_thread,
        });
    }

    pub fn handoff_after_svc(
        &self,
        jit: &mut dyn ArmInterface,
        thread_context: &mut ThreadContext,
        scheduler: &Arc<Mutex<KScheduler>>,
        process: &Arc<Mutex<KProcess>>,
    ) {
        let mut runtime_guard = self.m_runtime.lock().unwrap();
        let Some(runtime) = runtime_guard.as_mut() else {
            return;
        };

        jit.get_context(thread_context);
        let current_thread_id = {
            let mut current_thread = runtime.m_current_thread.lock().unwrap();
            current_thread.capture_guest_context(thread_context);
            current_thread.get_thread_id()
        };
        let next_thread = scheduler
            .lock()
            .unwrap()
            .wait_for_next_thread(process, current_thread_id);
        let Some(next_thread) = next_thread else {
            return;
        };

        let next_thread_id = next_thread.lock().unwrap().get_thread_id();
        if next_thread_id == current_thread_id {
            return;
        }

        self.restore_thread_to_jit(jit, thread_context, &next_thread);
        runtime.m_current_thread = next_thread;
    }

    pub fn dispatch_supervisor_call(
        &self,
        jit: &mut dyn ArmInterface,
        thread_context: &mut ThreadContext,
        scheduler: &Arc<Mutex<KScheduler>>,
        process: &Arc<Mutex<KProcess>>,
        svc_num: u32,
        svc_count: u32,
        is_64bit: bool,
        svc_args: &mut SvcArgs,
        system: &System,
    ) {
        svc_dispatch::call(system, svc_num, is_64bit, svc_args);
        jit.set_svc_arguments(svc_args);
        log::trace!("dispatch_supervisor_call: before handoff (svc=0x{:x})", svc_num);
        self.handoff_after_svc(jit, thread_context, scheduler, process);
        if let Some(threshold) = std::env::var("RUZU_LOG_AFTER_SVC")
            .ok()
            .and_then(|value| value.parse::<u32>().ok())
        {
            if svc_count >= threshold {
                jit.get_context(thread_context);
                let insn = process
                    .lock()
                    .unwrap()
                    .process_memory
                    .read()
                    .unwrap()
                    .read_32(thread_context.pc);
                log::info!(
                    "PhysicalCore::dispatch_supervisor_call after handoff: core={} svc_count={} svc=0x{:x} pc=0x{:08X} sp=0x{:08X} insn=0x{:08X}",
                    self.m_core_index,
                    svc_count,
                    svc_num,
                    thread_context.pc,
                    thread_context.sp,
                    insn,
                );
            }
        }
        log::trace!("dispatch_supervisor_call: after handoff (svc=0x{:x})", svc_num);
    }

    pub fn run_loop<FSvc, FHalt>(
        &self,
        jit: &mut dyn ArmInterface,
        thread: &mut OpaqueKThread,
        thread_context: &mut ThreadContext,
        scheduler: &Arc<Mutex<KScheduler>>,
        process: &Arc<Mutex<KProcess>>,
        is_64bit: bool,
        system: &System,
        mut on_supervisor_call: FSvc,
        mut on_halted: FHalt,
    ) -> (u32, u32, PhysicalCoreExecutionControl)
    where
        FSvc: FnMut(u32, &mut SvcArgs, &ThreadContext, u32, u32) -> PhysicalCoreExecutionControl,
        FHalt: FnMut(HaltReason, Option<u64>, &ThreadContext, u32, u32) -> PhysicalCoreExecutionControl,
    {
        let mut svc_count = 0u32;
        let mut iteration = 0u32;
        let mut step_after_svc = std::env::var("RUZU_STEP_AFTER_SVC")
            .ok()
            .and_then(|value| value.parse::<u32>().ok());
        let log_step_interval = std::env::var("RUZU_LOG_STEP_INTERVAL")
            .ok()
            .and_then(|value| value.parse::<u32>().ok())
            .filter(|value| *value > 0);

        #[cfg(feature = "debug-logs")]
        let mut ring_buf = physical_core_log::InstructionRingBuffer::new();
        loop {
            let use_step = step_after_svc.is_some_and(|threshold| svc_count >= threshold);
            let event = if use_step {
                self.step_thread(jit, thread)
            } else {
                self.run_thread(jit, thread)
            };
            iteration += 1;

            if use_step {
                jit.get_context(thread_context);
                if let Some(interval) = log_step_interval {
                    if iteration % interval == 0 {
                        let insn = process
                            .lock()
                            .unwrap()
                            .process_memory
                            .read()
                            .unwrap()
                            .read_32(thread_context.pc);
                        log::info!(
                            "PhysicalCore::run_loop step trace: core={} svc_count={} iteration={} pc=0x{:08X} sp=0x{:08X} insn=0x{:08X}",
                            self.m_core_index,
                            svc_count,
                            iteration,
                            thread_context.pc,
                            thread_context.sp,
                            insn,
                        );
                    }
                }
                #[cfg(feature = "debug-logs")]
                {
                    let insn = process.lock().unwrap().process_memory.read().unwrap().read_32(thread_context.pc);
                    ring_buf.record(thread_context, insn);
                    ring_buf.check_initlibc0_entry(thread_context);
                    ring_buf.check_rtld_init_return(thread_context);
                    if ring_buf.check_and_dump_abort(thread_context) {
                        step_after_svc = None;
                    }
                }
            }

            match event {
                PhysicalCoreExecutionEvent::SupervisorCall { svc_num, mut svc_args } => {
                    svc_count += 1;
                    jit.get_context(thread_context);

                    // After SetHeapSize (SVC ~#89), dump module memory for comparison with zuyu
                    #[cfg(feature = "debug-logs")]
                    if svc_count == 90 {
                        physical_core_log::dump_module_memory(process);
                    }

                    match on_supervisor_call(
                        svc_num,
                        &mut svc_args,
                        thread_context,
                        svc_count,
                        iteration,
                    ) {
                        PhysicalCoreExecutionControl::Break => {
                            return (iteration, svc_count, PhysicalCoreExecutionControl::Break);
                        }
                        PhysicalCoreExecutionControl::Yield => {
                            return (iteration, svc_count, PhysicalCoreExecutionControl::Yield);
                        }
                        PhysicalCoreExecutionControl::Continue => {}
                    }

                    self.dispatch_supervisor_call(
                        jit,
                        thread_context,
                        scheduler,
                        process,
                        svc_num,
                        svc_count,
                        is_64bit,
                        &mut svc_args,
                        system,
                    );
                }
                PhysicalCoreExecutionEvent::Halted(halt_reason) => {
                    jit.get_context(thread_context);
                    let exception_address = jit.get_last_exception_address();
                    match on_halted(
                        halt_reason,
                        exception_address,
                        thread_context,
                        svc_count,
                        iteration,
                    ) {
                        PhysicalCoreExecutionControl::Break => {
                            return (iteration, svc_count, PhysicalCoreExecutionControl::Break);
                        }
                        PhysicalCoreExecutionControl::Yield => {
                            return (iteration, svc_count, PhysicalCoreExecutionControl::Yield);
                        }
                        PhysicalCoreExecutionControl::Continue => {}
                    }

                    // Upstream observes the same condition when returning from an
                    // interrupt/timer tick and re-entering scheduling. In this
                    // cooperative port, the dispatch quantum boundary is the
                    // equivalent place to drain a pending termination request.
                    if self.drain_current_thread_termination(scheduler) {
                        self.handoff_after_svc(jit, thread_context, scheduler, process);
                    }
                }
            }
        }

        (iteration, svc_count, PhysicalCoreExecutionControl::Break)
    }

    /// Load context from thread to current core.
    /// Port of upstream `PhysicalCore::LoadContext(const KThread* thread)`.
    pub fn load_context(&self, thread: &KThread) {
        // Upstream: PhysicalCore::LoadContext (physical_core.cpp:148-167)
        // Gets process from thread, gets arm_interface from process,
        // calls interface->SetContext(thread->GetContext()),
        //       interface->SetTpidrroEl0(thread->GetTlsAddress()).
        let parent = match thread.parent.as_ref().and_then(|w| w.upgrade()) {
            Some(p) => p,
            None => return, // Kernel threads don't run on emulated CPU cores.
        };

        let mut process = parent.lock().unwrap();
        if let Some(jit) = process.get_arm_interface_mut(self.m_core_index) {
            let k_ctx = &thread.thread_context;
            // Safety: both ThreadContext types have identical layout.
            let arm_ctx: &crate::arm::arm_interface::ThreadContext =
                unsafe { &*(k_ctx as *const super::k_thread::ThreadContext
                    as *const crate::arm::arm_interface::ThreadContext) };
            jit.set_context(arm_ctx);
            jit.set_tpidrro_el0(thread.get_tls_address().get());
            log::info!(
                "PhysicalCore::load_context: core={} r15/PC=0x{:X} r13/SP=0x{:X} ctx.pc=0x{:X} ctx.sp=0x{:X}",
                self.m_core_index, k_ctx.r[15], k_ctx.r[13], k_ctx.pc, k_ctx.sp,
            );
        }
    }

    /// Save context from current core to thread.
    /// Port of upstream `PhysicalCore::SaveContext(KThread* thread)`.
    pub fn save_context(&self, thread: &mut KThread) {
        // Upstream: gets process from thread, gets arm_interface,
        // calls interface->GetContext(thread->GetContext()).
        log::trace!(
            "PhysicalCore::save_context: core={}",
            self.m_core_index,
        );
    }

    /// Log backtrace of current processor state.
    /// Port of upstream `PhysicalCore::LogBacktrace()`.
    pub fn log_backtrace(&self) {
        // Upstream: gets current process, gets arm_interface,
        // calls interface->LogBacktrace(process).
        log::debug!("PhysicalCore::log_backtrace: core={}", self.m_core_index);
    }

    /// Wait for an interrupt.
    pub fn idle(&self) {
        let mut state = self.m_guard.lock().unwrap();
        while !state.m_is_interrupted {
            state = self.m_on_interrupt.wait(state).unwrap();
        }
    }

    /// Set the arm interface and thread currently running on this core.
    /// Called at the start of RunThread, matching upstream's pattern.
    pub fn set_running(&self, arm_interface: *mut dyn crate::arm::arm_interface::ArmInterface, thread: *mut KThread) {
        let mut state = self.m_guard.lock().unwrap();
        state.m_arm_interface = Some(arm_interface);
        state.m_current_thread = Some(thread);
    }

    /// Clear the arm interface and thread (no longer running on this core).
    /// Called at the end of RunThread.
    pub fn clear_running(&self) {
        let mut state = self.m_guard.lock().unwrap();
        state.m_arm_interface = None;
        state.m_current_thread = None;
    }

    /// Interrupt this core.
    /// Port of upstream `PhysicalCore::Interrupt()`.
    pub fn interrupt(&self) {
        let mut state = self.m_guard.lock().unwrap();

        // Load members.
        let arm_interface = state.m_arm_interface;
        let current_thread = state.m_current_thread;

        // Add interrupt flag.
        state.m_is_interrupted = true;

        // Interrupt ourselves (wake from idle).
        self.m_on_interrupt.notify_one();

        // If there is no thread running, we are done.
        let (Some(arm_ptr), Some(thread_ptr)) = (arm_interface, current_thread) else {
            return;
        };

        // Interrupt the CPU.
        // Safety: arm_interface and current_thread are valid while m_guard is held,
        // matching upstream's scoped_lock pattern.
        // arm_interface::KThread is an opaque placeholder for the real k_thread::KThread.
        unsafe {
            let thread_as_arm =
                &mut *(thread_ptr as *mut KThread as *mut crate::arm::arm_interface::KThread);
            (*arm_ptr).signal_interrupt(thread_as_arm);
        }
    }

    /// Clear this core's interrupt flag.
    pub fn clear_interrupt(&self) {
        let mut state = self.m_guard.lock().unwrap();
        state.m_is_interrupted = false;
    }

    /// Check if this core is interrupted.
    pub fn is_interrupted(&self) -> bool {
        let state = self.m_guard.lock().unwrap();
        state.m_is_interrupted
    }

    /// Get the core index.
    pub fn core_index(&self) -> usize {
        self.m_core_index
    }

    fn restore_thread_to_jit(
        &self,
        jit: &mut dyn ArmInterface,
        thread_context: &mut ThreadContext,
        thread: &Arc<Mutex<KThread>>,
    ) {
        let thread = thread.lock().unwrap();
        thread.restore_guest_context(thread_context);
        jit.set_context(thread_context);
        jit.set_tpidrro_el0(thread.get_tls_address().get());
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::arm::arm_interface::{Architecture, DebugWatchpoint, KThread as OpaqueKThread};
    use crate::core::System;
    use crate::hle::kernel::k_thread::ThreadState;
    use crate::hle::kernel::k_worker_task_manager::KWorkerTaskManager;

    use super::*;

    struct TestArmInterface {
        halt_reasons: VecDeque<HaltReason>,
        context: ThreadContext,
        tpidrro_el0: u64,
    }

    impl TestArmInterface {
        fn new(halt_reasons: impl Into<VecDeque<HaltReason>>) -> Self {
            Self {
                halt_reasons: halt_reasons.into(),
                context: ThreadContext::default(),
                tpidrro_el0: 0,
            }
        }
    }

    impl ArmInterface for TestArmInterface {
        fn run_thread(&mut self, _thread: &mut OpaqueKThread) -> HaltReason {
            self.halt_reasons
                .pop_front()
                .unwrap_or(HaltReason::BREAK_LOOP)
        }

        fn step_thread(&mut self, thread: &mut OpaqueKThread) -> HaltReason {
            self.run_thread(thread)
        }

        fn clear_instruction_cache(&mut self) {}

        fn invalidate_cache_range(&mut self, _addr: u64, _size: usize) {}

        fn get_architecture(&self) -> Architecture {
            Architecture::AArch64
        }

        fn get_context(&self, ctx: &mut ThreadContext) {
            *ctx = self.context.clone();
        }

        fn set_context(&mut self, ctx: &ThreadContext) {
            self.context = ctx.clone();
        }

        fn set_tpidrro_el0(&mut self, value: u64) {
            self.tpidrro_el0 = value;
        }

        fn get_svc_arguments(&self, args: &mut [u64; 8]) {
            *args = [0; 8];
        }

        fn set_svc_arguments(&mut self, _args: &[u64; 8]) {}

        fn get_svc_number(&self) -> u32 {
            0
        }

        fn signal_interrupt(&mut self, _thread: &mut OpaqueKThread) {}

        fn halted_watchpoint(&self) -> Option<&DebugWatchpoint> {
            None
        }

        fn rewind_breakpoint_instruction(&mut self) {}
    }

    fn test_context() -> (
        PhysicalCore,
        Arc<Mutex<KProcess>>,
        Arc<Mutex<KScheduler>>,
        Arc<Mutex<KThread>>,
        System,
    ) {
        let mut system = System::new_for_test();

        let mut process = KProcess::new();
        process.capabilities.core_mask = 0xF;
        process.capabilities.priority_mask = u64::MAX;
        process.flags = 0;
        process.allocate_code_memory(0x200000, 0x20000);
        process.initialize_handle_table();
        process.initialize_thread_local_region_base(0x240000);

        let process = Arc::new(Mutex::new(process));
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        let other_thread = Arc::new(Mutex::new(KThread::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.initialize_main_thread(0x200000, 0x250000, 0, 0x23f000, &process, 1, 1, false);
            thread.set_priority(44);
            thread.set_base_priority(44);
        }
        {
            let mut thread = other_thread.lock().unwrap();
            thread.initialize_main_thread(0x201000, 0x260000, 0, 0x24f000, &process, 2, 2, false);
            thread.set_priority(44);
            thread.set_base_priority(44);
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(current_thread.clone());
        process
            .lock()
            .unwrap()
            .register_thread_object(other_thread.clone());
        process.lock().unwrap().push_back_to_priority_queue(1);
        process.lock().unwrap().push_back_to_priority_queue(2);
        scheduler.lock().unwrap().initialize(1, 0, 0);

        let shared_memory = process.lock().unwrap().get_shared_memory();
        system.set_current_process_arc(process.clone());
        system.set_scheduler_arc(scheduler.clone());
        system.set_shared_process_memory(shared_memory);
        system.set_runtime_program_id(1);
        system.set_runtime_64bit(false);

        (
            PhysicalCore::new(0, false),
            process,
            scheduler,
            current_thread,
            system,
        )
    }

    #[test]
    fn run_loop_drains_current_thread_termination_on_halt_boundary() {
        let (physical_core, process, scheduler, current_thread, system) = test_context();
        let mut thread_context = ThreadContext::default();
        let mut jit = TestArmInterface::new(VecDeque::from([
            HaltReason::BREAK_LOOP,
            HaltReason::BREAK_LOOP,
        ]));
        physical_core.initialize_guest_runtime(current_thread.clone(), &mut jit, &mut thread_context);

        current_thread.lock().unwrap().request_terminate();

        let mut opaque_thread = unsafe { std::mem::zeroed::<OpaqueKThread>() };
        let (_iterations, _svc_count, control) = physical_core.run_loop(
            &mut jit,
            &mut opaque_thread,
            &mut thread_context,
            &scheduler,
            &process,
            false,
            &system,
            |_svc_num, _svc_args, _thread_context, _svc_count, _iteration| {
                PhysicalCoreExecutionControl::Continue
            },
            |_halt_reason, _exception_address, _thread_context, _svc_count, iteration| {
                if iteration >= 2 {
                    PhysicalCoreExecutionControl::Break
                } else {
                    PhysicalCoreExecutionControl::Continue
                }
            },
        );
        KWorkerTaskManager::wait_for_global_idle();

        assert!(matches!(control, PhysicalCoreExecutionControl::Break));
        let thread = current_thread.lock().unwrap();
        assert_eq!(thread.get_state(), ThreadState::TERMINATED);
        assert!(thread.is_signaled());
        assert_eq!(
            scheduler.lock().unwrap().get_scheduler_current_thread_id(),
            Some(2)
        );
    }
}

//! Port of zuyu/src/core/hle/kernel/physical_core.h/.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! PhysicalCore: represents a single emulated CPU core, responsible for
//! running guest threads and handling interrupts. Full implementation
//! requires KernelCore, KThread, KProcess, ArmInterface, Debugger.

use std::sync::{Arc, Condvar, Mutex};

use crate::arm::arm_interface::{ArmInterface, HaltReason, KThread as OpaqueKThread, ThreadContext};
use crate::hle::kernel::svc_dispatch::{self, SvcArgs, SvcContext};

use super::{k_process::KProcess, k_scheduler::KScheduler, k_thread::KThread};

pub enum PhysicalCoreExecutionControl {
    Continue,
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
    // m_arm_interface: Option<&mut ArmInterface>,
    // m_current_thread: Option<&mut KThread>,
}

struct PhysicalCoreRuntime {
    m_current_thread: Arc<Mutex<KThread>>,
}

pub enum PhysicalCoreExecutionEvent {
    SupervisorCall { svc_num: u32, svc_args: SvcArgs },
    Halted(HaltReason),
}

impl PhysicalCore {
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
        is_64bit: bool,
        svc_args: &mut SvcArgs,
        svc_context: &SvcContext,
    ) {
        svc_dispatch::call(svc_num, is_64bit, svc_args, svc_context);
        jit.set_svc_arguments(svc_args);
        log::trace!("dispatch_supervisor_call: before handoff (svc=0x{:x})", svc_num);
        self.handoff_after_svc(jit, thread_context, scheduler, process);
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
        svc_context: &SvcContext,
        mut on_supervisor_call: FSvc,
        mut on_halted: FHalt,
    ) -> (u32, u32)
    where
        FSvc: FnMut(u32, &mut SvcArgs, &ThreadContext, u32, u32) -> PhysicalCoreExecutionControl,
        FHalt: FnMut(HaltReason, &ThreadContext, u32, u32) -> PhysicalCoreExecutionControl,
    {
        let mut svc_count = 0u32;
        let mut iteration = 0u32;
        let step_after_svc = std::env::var("RUZU_STEP_AFTER_SVC")
            .ok()
            .and_then(|value| value.parse::<u32>().ok());

        let mut post_svc_trace_count = 0u32;
        loop {
            let use_step = step_after_svc.is_some_and(|threshold| svc_count >= threshold);
            let event = if use_step {
                self.step_thread(jit, thread)
            } else {
                self.run_thread(jit, thread)
            };
            iteration += 1;

            // Log first 200 steps with registers and instruction
            if use_step && post_svc_trace_count < 200 {
                post_svc_trace_count += 1;
                jit.get_context(thread_context);
                // Check TLS word 3 (handle offset)
                let tls_word3 = if let Some(ref runtime) = *self.m_runtime.lock().unwrap() {
                    let tls = runtime.m_current_thread.lock().unwrap().get_tls_address().get();
                    process.lock().unwrap().process_memory.read().unwrap().read_32(tls + 12)
                } else { 0 };
                let insn = process.lock().unwrap().process_memory.read().unwrap().read_32(thread_context.pc);
                log::info!(
                    "[S#{}/i={}] PC={:#x} [{:#010x}] R0={:#x} R1={:#x} R2={:#x} R3={:#x} R4={:#x} R5={:#x} R6={:#x} R7={:#x}",
                    post_svc_trace_count, iteration,
                    thread_context.pc, insn,
                    thread_context.r[0], thread_context.r[1],
                    thread_context.r[2], thread_context.r[3],
                    thread_context.r[4], thread_context.r[5],
                    thread_context.r[6], thread_context.r[7],
                );
            }

            match event {
                PhysicalCoreExecutionEvent::SupervisorCall { svc_num, mut svc_args } => {
                    svc_count += 1;
                    jit.get_context(thread_context);

                    if matches!(
                        on_supervisor_call(
                            svc_num,
                            &mut svc_args,
                            thread_context,
                            svc_count,
                            iteration,
                        ),
                        PhysicalCoreExecutionControl::Break
                    ) {
                        break;
                    }

                    self.dispatch_supervisor_call(
                        jit,
                        thread_context,
                        scheduler,
                        process,
                        svc_num,
                        is_64bit,
                        &mut svc_args,
                        svc_context,
                    );
                }
                PhysicalCoreExecutionEvent::Halted(halt_reason) => {
                    jit.get_context(thread_context);
                    if matches!(
                        on_halted(halt_reason, thread_context, svc_count, iteration),
                        PhysicalCoreExecutionControl::Break
                    ) {
                        break;
                    }
                }
            }
        }

        (iteration, svc_count)
    }

    /// Load context from thread to current core.
    pub fn load_context(&self) {
        // TODO: Implement once KThread, ArmInterface are available.
    }

    /// Save context from current core to thread.
    pub fn save_context(&self) {
        // TODO: Implement once KThread, ArmInterface are available.
    }

    /// Log backtrace of current processor state.
    pub fn log_backtrace(&self) {
        // TODO: Implement once KProcess, ArmInterface are available.
    }

    /// Wait for an interrupt.
    pub fn idle(&self) {
        let mut state = self.m_guard.lock().unwrap();
        while !state.m_is_interrupted {
            state = self.m_on_interrupt.wait(state).unwrap();
        }
    }

    /// Interrupt this core.
    pub fn interrupt(&self) {
        let mut state = self.m_guard.lock().unwrap();
        state.m_is_interrupted = true;
        self.m_on_interrupt.notify_one();
        // TODO: If arm_interface is set, signal interrupt.
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

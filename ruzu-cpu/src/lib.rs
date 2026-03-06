// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

pub mod arm_dynarmic_32;
pub mod arm_dynarmic_64;
pub mod cpu_settings;
pub mod state;

pub use arm_dynarmic_32::ArmDynarmic32;
pub use arm_dynarmic_64::ArmDynarmic64;
pub use arm_dynarmic_64::MemoryVtable;
pub use cpu_settings::{CpuAccuracy, CpuSettings};
pub use rdynarmic::halt_reason::HaltReason;
pub use state::CpuState;

/// Common interface for ARM JIT executors (A32 and A64).
///
/// Allows the main CPU loop to be generic over the architecture.
pub trait ArmJit {
    fn run(&mut self) -> HaltReason;
    fn step(&mut self) -> HaltReason;
    fn halt(&self);
    fn set_ticks_remaining(&mut self, ticks: u64);
    fn get_svc_number(&self) -> Option<u32>;
    fn load_context(&mut self, state: &CpuState);
    fn save_context(&self, state: &mut CpuState);
    fn get_pc(&self) -> u64;
    fn set_pc(&mut self, pc: u64);
    fn clear_halt(&self, reason: HaltReason);
    fn invalidate_cache_range(&mut self, addr: u64, size: u64);
}

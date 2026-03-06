// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! CPU settings for JIT optimization control.
//!
//! Port of zuyu's CPU accuracy / debug settings from `arm_dynarmic_32.cpp`
//! and `arm_dynarmic_64.cpp`. Controls which dynarmic optimization passes
//! are enabled, matching zuyu's `CpuAccuracy` enum and `cpuopt_*` settings.

use rdynarmic::jit_config::OptimizationFlag;

/// CPU accuracy mode, matching zuyu's `Settings::CpuAccuracy`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CpuAccuracy {
    /// Curated safe + some unsafe optimizations (default).
    Auto,
    /// Safe optimizations only.
    Accurate,
    /// All optimizations including unsafe ones.
    Unsafe,
    /// No optimizations at all — for debugging JIT bugs.
    Paranoid,
}

/// CPU JIT settings matching zuyu's `Settings::values.cpuopt_*`.
///
/// Controls individual optimization passes. When `cpu_debug_mode` is true,
/// each setting individually enables/disables its optimization. When false,
/// the `cpu_accuracy` mode determines the set.
#[derive(Debug, Clone)]
pub struct CpuSettings {
    pub cpu_accuracy: CpuAccuracy,
    pub cpu_debug_mode: bool,

    // -- Safe optimizations (individually toggleable in debug mode) --
    pub cpuopt_block_linking: bool,
    pub cpuopt_return_stack_buffer: bool,
    pub cpuopt_fast_dispatcher: bool,
    pub cpuopt_context_elimination: bool,
    pub cpuopt_const_prop: bool,
    pub cpuopt_misc_ir: bool,

    // -- Unsafe optimizations --
    pub cpuopt_unsafe_unfuse_fma: bool,
    pub cpuopt_unsafe_reduce_fp_error: bool,
    pub cpuopt_unsafe_ignore_standard_fpcr: bool,
    pub cpuopt_unsafe_inaccurate_nan: bool,
    pub cpuopt_unsafe_ignore_global_monitor: bool,
}

impl Default for CpuSettings {
    fn default() -> Self {
        Self {
            cpu_accuracy: CpuAccuracy::Auto,
            cpu_debug_mode: false,
            cpuopt_block_linking: true,
            cpuopt_return_stack_buffer: true,
            cpuopt_fast_dispatcher: true,
            cpuopt_context_elimination: true,
            cpuopt_const_prop: true,
            cpuopt_misc_ir: true,
            cpuopt_unsafe_unfuse_fma: true,
            cpuopt_unsafe_reduce_fp_error: true,
            cpuopt_unsafe_ignore_standard_fpcr: true,
            cpuopt_unsafe_inaccurate_nan: true,
            cpuopt_unsafe_ignore_global_monitor: true,
        }
    }
}

impl CpuSettings {
    /// Build optimization flags matching zuyu's `MakeJit()` logic.
    ///
    /// Returns `(optimizations, unsafe_optimizations)`.
    pub fn build_optimization_flags(&self) -> (OptimizationFlag, bool) {
        if self.cpu_debug_mode {
            // Debug mode: start with all safe, individually disable
            let mut opts = OptimizationFlag::ALL_SAFE_OPTIMIZATIONS;

            if !self.cpuopt_block_linking {
                opts = opts & !OptimizationFlag::BLOCK_LINKING;
            }
            if !self.cpuopt_return_stack_buffer {
                opts = opts & !OptimizationFlag::RETURN_STACK_BUFFER;
            }
            if !self.cpuopt_fast_dispatcher {
                opts = opts & !OptimizationFlag::FAST_DISPATCH;
            }
            if !self.cpuopt_context_elimination {
                opts = opts & !OptimizationFlag::GET_SET_ELIMINATION;
            }
            if !self.cpuopt_const_prop {
                opts = opts & !OptimizationFlag::CONST_PROP;
            }
            if !self.cpuopt_misc_ir {
                opts = opts & !OptimizationFlag::MISC_IR_OPT;
            }

            (opts, false)
        } else {
            match self.cpu_accuracy {
                CpuAccuracy::Paranoid => {
                    // No optimizations at all
                    (OptimizationFlag::NO_OPTIMIZATIONS, false)
                }
                CpuAccuracy::Accurate => {
                    // Safe optimizations only
                    (OptimizationFlag::ALL_SAFE_OPTIMIZATIONS, false)
                }
                CpuAccuracy::Unsafe => {
                    // Safe + all unsafe
                    let mut opts = OptimizationFlag::ALL_SAFE_OPTIMIZATIONS;
                    if self.cpuopt_unsafe_unfuse_fma {
                        opts = opts | OptimizationFlag::UNSAFE_UNFUSE_FMA;
                    }
                    if self.cpuopt_unsafe_reduce_fp_error {
                        opts = opts | OptimizationFlag::UNSAFE_REDUCED_ERROR_FP;
                    }
                    if self.cpuopt_unsafe_ignore_standard_fpcr {
                        opts = opts | OptimizationFlag::UNSAFE_IGNORE_STANDARD_FPCR_VALUE;
                    }
                    if self.cpuopt_unsafe_inaccurate_nan {
                        opts = opts | OptimizationFlag::UNSAFE_INACCURATE_NAN;
                    }
                    if self.cpuopt_unsafe_ignore_global_monitor {
                        opts = opts | OptimizationFlag::UNSAFE_IGNORE_GLOBAL_MONITOR;
                    }
                    (opts, true)
                }
                CpuAccuracy::Auto => {
                    // Curated: safe + selected unsafe (matching zuyu's Auto)
                    let opts = OptimizationFlag::ALL_SAFE_OPTIMIZATIONS
                        | OptimizationFlag::UNSAFE_UNFUSE_FMA
                        | OptimizationFlag::UNSAFE_IGNORE_STANDARD_FPCR_VALUE
                        | OptimizationFlag::UNSAFE_INACCURATE_NAN
                        | OptimizationFlag::UNSAFE_IGNORE_GLOBAL_MONITOR;
                    (opts, true)
                }
            }
        }
    }

    /// Create Paranoid settings (no optimizations).
    pub fn paranoid() -> Self {
        Self {
            cpu_accuracy: CpuAccuracy::Paranoid,
            ..Default::default()
        }
    }

    /// Build settings from ruzu-common's `Values` struct.
    ///
    /// Reads `cpu_accuracy`, `cpu_debug_mode`, and all `cpuopt_*` settings,
    /// matching zuyu's `MakeJit()` configuration flow.
    pub fn from_values(values: &common::settings::Values) -> Self {
        let cpu_accuracy = match *values.cpu_accuracy.get_value() {
            common::settings_enums::CpuAccuracy::Auto => CpuAccuracy::Auto,
            common::settings_enums::CpuAccuracy::Accurate => CpuAccuracy::Accurate,
            common::settings_enums::CpuAccuracy::Unsafe => CpuAccuracy::Unsafe,
            common::settings_enums::CpuAccuracy::Paranoid => CpuAccuracy::Paranoid,
        };

        Self {
            cpu_accuracy,
            cpu_debug_mode: *values.cpu_debug_mode.get_value(),
            cpuopt_block_linking: *values.cpuopt_block_linking.get_value(),
            cpuopt_return_stack_buffer: *values.cpuopt_return_stack_buffer.get_value(),
            cpuopt_fast_dispatcher: *values.cpuopt_fast_dispatcher.get_value(),
            cpuopt_context_elimination: *values.cpuopt_context_elimination.get_value(),
            cpuopt_const_prop: *values.cpuopt_const_prop.get_value(),
            cpuopt_misc_ir: *values.cpuopt_misc_ir.get_value(),
            cpuopt_unsafe_unfuse_fma: *values.cpuopt_unsafe_unfuse_fma.get_value(),
            cpuopt_unsafe_reduce_fp_error: *values.cpuopt_unsafe_reduce_fp_error.get_value(),
            cpuopt_unsafe_ignore_standard_fpcr: *values.cpuopt_unsafe_ignore_standard_fpcr.get_value(),
            cpuopt_unsafe_inaccurate_nan: *values.cpuopt_unsafe_inaccurate_nan.get_value(),
            cpuopt_unsafe_ignore_global_monitor: *values.cpuopt_unsafe_ignore_global_monitor.get_value(),
        }
    }
}

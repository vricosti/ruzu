// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/fatal/fatal.h
//! Port of zuyu/src/core/hle/service/fatal/fatal.cpp
//!
//! Fatal error service -- Module::Interface, FatalInfo, error report generation.

/// Architecture of the faulting process.
///
/// Corresponds to `FatalInfo::Architecture` in upstream fatal.cpp.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Architecture {
    AArch64 = 0,
    AArch32 = 1,
}

impl Architecture {
    pub fn as_str(self) -> &'static str {
        match self {
            Architecture::AArch64 => "AArch64",
            Architecture::AArch32 => "AArch32",
        }
    }
}

/// CPU context captured at the time of fatal error.
///
/// Corresponds to `FatalInfo` in upstream fatal.cpp.
/// `static_assert(sizeof(FatalInfo) == 0x250)`.
#[repr(C)]
#[derive(Clone)]
pub struct FatalInfo {
    pub registers: [u64; 31],
    pub sp: u64,
    pub pc: u64,
    pub pstate: u64,
    pub afsr0: u64,
    pub afsr1: u64,
    pub esr: u64,
    pub far: u64,
    pub backtrace: [u64; 32],
    pub program_entry_point: u64,
    /// Bit flags indicating which registers have been set with values.
    pub set_flags: u64,
    pub backtrace_size: u32,
    pub arch: Architecture,
    pub unk10: u32,
}
const _: () = assert!(std::mem::size_of::<FatalInfo>() == 0x250);

impl Default for FatalInfo {
    fn default() -> Self {
        // Zero-initialize the entire struct, matching upstream memset behavior.
        // SAFETY: FatalInfo is repr(C) with all-numeric fields.
        unsafe { std::mem::zeroed() }
    }
}

/// Fatal error type policy.
///
/// Corresponds to `FatalType` in upstream fatal.cpp.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FatalType {
    ErrorReportAndScreen = 0,
    ErrorReport = 1,
    ErrorScreen = 2,
}

impl TryFrom<u32> for FatalType {
    type Error = u32;
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(FatalType::ErrorReportAndScreen),
            1 => Ok(FatalType::ErrorReport),
            2 => Ok(FatalType::ErrorScreen),
            v => Err(v),
        }
    }
}

/// Generate a human-readable crash report from fatal info.
///
/// Corresponds to `GenerateErrorReport` in upstream fatal.cpp.
pub fn generate_error_report(title_id: u64, error_code: u32, info: &FatalInfo) {
    let module = error_code & 0x1FF;
    let description = (error_code >> 9) & 0x1FFF;
    let mut crash_report = format!(
        "Ruzu crash report\n\
         Title ID:                        {:016x}\n\
         Result:                          0x{:X} ({:04}-{:04})\n\
         Set flags:                       0x{:16X}\n\
         Program entry point:             0x{:16X}\n\
         \n",
        title_id,
        error_code,
        2000 + module,
        description,
        info.set_flags,
        info.program_entry_point
    );

    if info.backtrace_size != 0 {
        crash_report += "Registers:\n";
        for i in 0..info.registers.len() {
            crash_report += &format!(
                "    X[{:02}]:                       {:016x}\n",
                i, info.registers[i]
            );
        }
        crash_report += &format!("    SP:                          {:016x}\n", info.sp);
        crash_report += &format!("    PC:                          {:016x}\n", info.pc);
        crash_report += &format!("    PSTATE:                      {:016x}\n", info.pstate);
        crash_report += &format!("    AFSR0:                       {:016x}\n", info.afsr0);
        crash_report += &format!("    AFSR1:                       {:016x}\n", info.afsr1);
        crash_report += &format!("    ESR:                         {:016x}\n", info.esr);
        crash_report += &format!("    FAR:                         {:016x}\n", info.far);
        crash_report += "\nBacktrace:\n";
        for i in 0..std::cmp::min(info.backtrace_size, 32) {
            crash_report += &format!(
                "    Backtrace[{:02}]:               {:016x}\n",
                i, info.backtrace[i as usize]
            );
        }
        crash_report += &format!("Architecture:                    {}\n", info.arch.as_str());
        crash_report += &format!("Unknown 10:                      0x{:016x}\n", info.unk10);
    }

    log::error!("{}", crash_report);

    // TODO: system.GetReporter().SaveCrashReport(...)
}

/// Process a fatal error according to the error type policy.
///
/// Corresponds to `ThrowFatalError` in upstream fatal.cpp.
pub fn throw_fatal_error(title_id: u64, error_code: u32, fatal_type: FatalType, info: &FatalInfo) {
    log::error!(
        "Threw fatal error type {:?} with error code 0x{:X}",
        fatal_type,
        error_code
    );

    match fatal_type {
        FatalType::ErrorReportAndScreen => {
            generate_error_report(title_id, error_code, info);
            // Since we have no fatal:u error screen, just assert in debug
            debug_assert!(false, "Fatal error screen would be shown here");
        }
        FatalType::ErrorScreen => {
            // Should show error screen; since we have none, just assert
            debug_assert!(false, "Fatal error screen would be shown here");
        }
        FatalType::ErrorReport => {
            generate_error_report(title_id, error_code, info);
        }
    }
}

/// Module for fatal service, shared by fatal:p and fatal:u.
///
/// Corresponds to `Module` in upstream fatal.h.
pub struct Module;

impl Module {
    pub fn new() -> Self {
        Self
    }
}

/// Module::Interface -- base type for fatal:p and fatal:u.
///
/// Corresponds to `Module::Interface` in upstream fatal.h / fatal.cpp.
pub struct Interface {
    pub system: crate::core::SystemRef,
    pub module: std::sync::Arc<Module>,
    pub name: &'static str,
}

impl Interface {
    pub fn new(system: crate::core::SystemRef, module: std::sync::Arc<Module>, name: &'static str) -> Self {
        Self { system, module, name }
    }

    /// ThrowFatal (cmd 0).
    ///
    /// Corresponds to `Module::Interface::ThrowFatal` in upstream fatal.cpp.
    pub fn throw_fatal(&self, error_code: u32) {
        log::error!("fatal ThrowFatal called");
        throw_fatal_error(0, error_code, FatalType::ErrorScreen, &FatalInfo::default());
    }

    /// ThrowFatalWithPolicy (cmd 1).
    ///
    /// Corresponds to `Module::Interface::ThrowFatalWithPolicy` in upstream fatal.cpp.
    pub fn throw_fatal_with_policy(&self, error_code: u32, fatal_type: FatalType) {
        log::error!("fatal ThrowFatalWithPolicy called");
        throw_fatal_error(0, error_code, fatal_type, &FatalInfo::default());
    }

    /// ThrowFatalWithCpuContext (cmd 2).
    ///
    /// Corresponds to `Module::Interface::ThrowFatalWithCpuContext` in upstream fatal.cpp.
    pub fn throw_fatal_with_cpu_context(
        &self,
        error_code: u32,
        fatal_type: FatalType,
        fatal_info_buffer: &[u8],
    ) {
        log::error!("fatal ThrowFatalWithCpuContext called");
        let mut info = FatalInfo::default();

        assert!(
            fatal_info_buffer.len() == std::mem::size_of::<FatalInfo>(),
            "Invalid fatal info buffer size!"
        );
        // SAFETY: FatalInfo is repr(C), buffer size is checked
        unsafe {
            std::ptr::copy_nonoverlapping(
                fatal_info_buffer.as_ptr(),
                &mut info as *mut FatalInfo as *mut u8,
                std::mem::size_of::<FatalInfo>(),
            );
        }

        throw_fatal_error(0, error_code, fatal_type, &info);
    }
}

/// LoopProcess -- registers fatal:p and fatal:u services.
///
/// Corresponds to `LoopProcess` in upstream fatal.cpp.
pub fn loop_process() {
    log::debug!("Fatal::LoopProcess called");
    // TODO: Register fatal:p and fatal:u with ServerManager
}

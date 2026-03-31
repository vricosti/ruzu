// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pm/pm.h and pm.cpp
//!
//! Process Manager services.

use crate::hle::result::{ErrorModule, ResultCode};

// --- PM result codes (matching upstream pm.cpp) ---

/// Upstream: `ResultProcessNotFound{ErrorModule::PM, 1}`
pub const RESULT_PROCESS_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::PM, 1);
/// Upstream: `ResultAlreadyStarted{ErrorModule::PM, 2}`
pub const RESULT_ALREADY_STARTED: ResultCode =
    ResultCode::from_module_description(ErrorModule::PM, 2);
/// Upstream: `ResultNotTerminated{ErrorModule::PM, 3}`
pub const RESULT_NOT_TERMINATED: ResultCode =
    ResultCode::from_module_description(ErrorModule::PM, 3);
/// Upstream: `ResultDebugHookInUse{ErrorModule::PM, 4}`
pub const RESULT_DEBUG_HOOK_IN_USE: ResultCode =
    ResultCode::from_module_description(ErrorModule::PM, 4);
/// Upstream: `ResultApplicationRunning{ErrorModule::PM, 5}`
pub const RESULT_APPLICATION_RUNNING: ResultCode =
    ResultCode::from_module_description(ErrorModule::PM, 5);
/// Upstream: `ResultInvalidSize{ErrorModule::PM, 6}`
pub const RESULT_INVALID_SIZE: ResultCode = ResultCode::from_module_description(ErrorModule::PM, 6);

/// Upstream: `NO_PROCESS_FOUND_PID`
const NO_PROCESS_FOUND_PID: u64 = 0;

/// SystemBootMode enum. Upstream: `SystemBootMode` in `pm.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SystemBootMode {
    Normal = 0,
    Maintenance = 1,
}

/// Minimal process info for the process list.
///
/// In upstream this uses `Kernel::KProcess` objects. Here we track the
/// essential fields needed by PM services.
#[derive(Debug, Clone)]
pub struct ProcessInfo {
    pub process_id: u64,
    pub program_id: u64,
    pub is_application: bool,
}

/// Shared process list. Upstream uses `Kernel::KernelCore::GetProcessList()`.
pub type ProcessList = Vec<ProcessInfo>;

/// Search the process list with a predicate.
///
/// Corresponds to upstream `SearchProcessList` template function.
fn search_process_list<F>(process_list: &ProcessList, predicate: F) -> Option<&ProcessInfo>
where
    F: Fn(&ProcessInfo) -> bool,
{
    process_list.iter().find(|p| predicate(p))
}

/// Get the application process ID from the process list.
///
/// Corresponds to upstream `GetApplicationPidGeneric`.
fn get_application_pid_generic(process_list: &ProcessList) -> u64 {
    match search_process_list(process_list, |p| p.is_application) {
        Some(p) => p.process_id,
        None => NO_PROCESS_FOUND_PID,
    }
}

/// Atmosphere process info structures used by AtmosphereGetProcessInfo.
///
/// Corresponds to upstream `ProgramLocation` in pm.cpp.
#[repr(C)]
pub struct ProgramLocation {
    pub program_id: u64,
    pub storage_id: u8,
    pub _padding: [u8; 7],
}

const _: () = assert!(std::mem::size_of::<ProgramLocation>() == 0x10);

/// Corresponds to upstream `OverrideStatus` in pm.cpp.
#[repr(C)]
pub struct OverrideStatus {
    pub keys_held: u64,
    pub flags: u64,
}

const _: () = assert!(std::mem::size_of::<OverrideStatus>() == 0x10);

// --- IPC command tables ---

/// IPC command IDs for BootMode ("pm:bm")
pub mod boot_mode_commands {
    pub const GET_BOOT_MODE: u32 = 0;
    pub const SET_MAINTENANCE_BOOT: u32 = 1;
}

/// IPC command IDs for DebugMonitor ("pm:dmnt")
pub mod debug_monitor_commands {
    pub const GET_JIT_DEBUG_PROCESS_ID_LIST: u32 = 0;
    pub const START_PROCESS: u32 = 1;
    pub const GET_PROCESS_ID: u32 = 2;
    pub const HOOK_TO_CREATE_PROCESS: u32 = 3;
    pub const GET_APPLICATION_PROCESS_ID: u32 = 4;
    pub const HOOK_TO_CREATE_APPLICATION_PROGRESS: u32 = 5;
    pub const CLEAR_HOOK: u32 = 6;
    pub const ATMOSPHERE_GET_PROCESS_INFO: u32 = 65000;
    pub const ATMOSPHERE_GET_CURRENT_LIMIT_INFO: u32 = 65001;
}

/// IPC command IDs for Info ("pm:info")
pub mod info_commands {
    pub const GET_PROGRAM_ID: u32 = 0;
    pub const ATMOSPHERE_GET_PROCESS_ID: u32 = 65000;
    pub const ATMOSPHERE_HAS_LAUNCHED_PROGRAM: u32 = 65001;
    pub const ATMOSPHERE_GET_PROCESS_INFO: u32 = 65002;
}

/// IPC command IDs for Shell ("pm:shell")
pub mod shell_commands {
    pub const LAUNCH_PROGRAM: u32 = 0;
    pub const TERMINATE_PROCESS: u32 = 1;
    pub const TERMINATE_PROGRAM: u32 = 2;
    pub const GET_PROCESS_EVENT_HANDLE: u32 = 3;
    pub const GET_PROCESS_EVENT_INFO: u32 = 4;
    pub const NOTIFY_BOOT_FINISHED: u32 = 5;
    pub const GET_APPLICATION_PROCESS_ID_FOR_SHELL: u32 = 6;
    pub const BOOST_SYSTEM_MEMORY_RESOURCE_LIMIT: u32 = 7;
    pub const BOOST_APPLICATION_THREAD_RESOURCE_LIMIT: u32 = 8;
    pub const GET_BOOT_FINISHED_EVENT_HANDLE: u32 = 9;
}

/// BootMode service ("pm:bm").
///
/// Corresponds to `BootMode` in upstream pm.cpp.
pub struct BootMode {
    boot_mode: SystemBootMode,
}

impl BootMode {
    pub fn new() -> Self {
        Self {
            boot_mode: SystemBootMode::Normal,
        }
    }

    /// GetBootMode (cmd 0).
    ///
    /// Corresponds to `BootMode::GetBootMode` in upstream.
    pub fn get_boot_mode(&self) -> SystemBootMode {
        log::debug!("BootMode::GetBootMode called");
        self.boot_mode
    }

    /// SetMaintenanceBoot (cmd 1).
    ///
    /// Corresponds to `BootMode::SetMaintenanceBoot` in upstream.
    pub fn set_maintenance_boot(&mut self) {
        log::debug!("BootMode::SetMaintenanceBoot called");
        self.boot_mode = SystemBootMode::Maintenance;
    }
}

/// DebugMonitor service ("pm:dmnt").
///
/// Corresponds to `DebugMonitor` in upstream pm.cpp.
pub struct DebugMonitor {
    process_list: ProcessList,
}

impl DebugMonitor {
    pub fn new() -> Self {
        Self {
            process_list: Vec::new(),
        }
    }

    /// For test/integration purposes: provide a process list.
    pub fn with_process_list(process_list: ProcessList) -> Self {
        Self { process_list }
    }

    /// GetProcessId (cmd 2).
    ///
    /// Corresponds to `DebugMonitor::GetProcessId` in upstream.
    pub fn get_process_id(&self, program_id: u64) -> Result<u64, ResultCode> {
        log::debug!(
            "DebugMonitor::GetProcessId called, program_id={:016X}",
            program_id
        );

        match search_process_list(&self.process_list, |p| p.program_id == program_id) {
            Some(p) => Ok(p.process_id),
            None => Err(RESULT_PROCESS_NOT_FOUND),
        }
    }

    /// GetApplicationProcessId (cmd 4).
    ///
    /// Corresponds to `DebugMonitor::GetApplicationProcessId` in upstream.
    pub fn get_application_process_id(&self) -> u64 {
        log::debug!("DebugMonitor::GetApplicationProcessId called");
        get_application_pid_generic(&self.process_list)
    }

    /// AtmosphereGetProcessInfo (cmd 65000).
    ///
    /// Partial implementation matching upstream.
    pub fn atmosphere_get_process_info(
        &self,
        pid: u64,
    ) -> Result<(ProgramLocation, OverrideStatus), ResultCode> {
        log::warn!(
            "DebugMonitor::AtmosphereGetProcessInfo (Partial Implementation) called, pid={:016X}",
            pid
        );

        match search_process_list(&self.process_list, |p| p.process_id == pid) {
            Some(p) => {
                let program_location = ProgramLocation {
                    program_id: p.program_id,
                    storage_id: 0,
                    _padding: [0u8; 7],
                };
                let override_status = OverrideStatus {
                    keys_held: 0,
                    flags: 0,
                };
                Ok((program_location, override_status))
            }
            None => Err(RESULT_PROCESS_NOT_FOUND),
        }
    }
}

/// Info service ("pm:info").
///
/// Corresponds to `Info` in upstream pm.cpp.
pub struct Info {
    process_list: ProcessList,
}

impl Info {
    pub fn new() -> Self {
        Self {
            process_list: Vec::new(),
        }
    }

    pub fn with_process_list(process_list: ProcessList) -> Self {
        Self { process_list }
    }

    /// GetProgramId (cmd 0).
    ///
    /// Corresponds to `Info::GetProgramId` in upstream.
    pub fn get_program_id(&self, process_id: u64) -> Result<u64, ResultCode> {
        log::debug!("Info::GetProgramId called, process_id={:016X}", process_id);

        match search_process_list(&self.process_list, |p| p.process_id == process_id) {
            Some(p) => Ok(p.program_id),
            None => Err(RESULT_PROCESS_NOT_FOUND),
        }
    }

    /// AtmosphereGetProcessId (cmd 65000).
    ///
    /// Corresponds to `Info::AtmosphereGetProcessId` in upstream.
    pub fn atmosphere_get_process_id(&self, program_id: u64) -> Result<u64, ResultCode> {
        log::debug!(
            "Info::AtmosphereGetProcessId called, program_id={:016X}",
            program_id
        );

        match search_process_list(&self.process_list, |p| p.program_id == program_id) {
            Some(p) => Ok(p.process_id),
            None => Err(RESULT_PROCESS_NOT_FOUND),
        }
    }
}

/// Shell service ("pm:shell").
///
/// Corresponds to `Shell` in upstream pm.cpp.
pub struct Shell {
    process_list: ProcessList,
}

impl Shell {
    pub fn new() -> Self {
        Self {
            process_list: Vec::new(),
        }
    }

    pub fn with_process_list(process_list: ProcessList) -> Self {
        Self { process_list }
    }

    /// GetApplicationProcessIdForShell (cmd 6).
    ///
    /// Corresponds to `Shell::GetApplicationProcessIdForShell` in upstream.
    pub fn get_application_process_id_for_shell(&self) -> u64 {
        log::debug!("Shell::GetApplicationProcessIdForShell called");
        get_application_pid_generic(&self.process_list)
    }
}

/// Registers "pm:bm", "pm:dmnt", "pm:info", "pm:shell" services.
///
/// Corresponds to `LoopProcess` in upstream `pm.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let mut server_manager = ServerManager::new(system);

    let stub_names = &["pm:bm", "pm:dmnt", "pm:info", "pm:shell"];
    for &name in stub_names {
        let svc_name = name.to_string();
        server_manager.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(crate::hle::service::services::GenericStubService::new(
                    &svc_name,
                ))
            }),
            16,
        );
    }

    ServerManager::run_server(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_result_codes() {
        assert!(RESULT_PROCESS_NOT_FOUND.is_error());
        assert_eq!(RESULT_PROCESS_NOT_FOUND.get_module(), ErrorModule::PM);
        assert_eq!(RESULT_PROCESS_NOT_FOUND.get_description(), 1);
        assert!(RESULT_ALREADY_STARTED.is_error());
        assert_eq!(RESULT_ALREADY_STARTED.get_description(), 2);
    }

    fn make_test_process_list() -> ProcessList {
        vec![
            ProcessInfo {
                process_id: 100,
                program_id: 0x0100000000001000,
                is_application: true,
            },
            ProcessInfo {
                process_id: 200,
                program_id: 0x0100000000002000,
                is_application: false,
            },
        ]
    }

    #[test]
    fn test_boot_mode() {
        let mut bm = BootMode::new();
        assert_eq!(bm.get_boot_mode(), SystemBootMode::Normal);
        bm.set_maintenance_boot();
        assert_eq!(bm.get_boot_mode(), SystemBootMode::Maintenance);
    }

    #[test]
    fn test_debug_monitor_get_process_id() {
        let dm = DebugMonitor::with_process_list(make_test_process_list());
        assert_eq!(dm.get_process_id(0x0100000000001000), Ok(100));
        assert_eq!(dm.get_process_id(0x0100000000002000), Ok(200));
        assert_eq!(dm.get_process_id(0xDEAD), Err(RESULT_PROCESS_NOT_FOUND));
    }

    #[test]
    fn test_debug_monitor_get_application_process_id() {
        let dm = DebugMonitor::with_process_list(make_test_process_list());
        assert_eq!(dm.get_application_process_id(), 100);

        let dm_empty = DebugMonitor::new();
        assert_eq!(dm_empty.get_application_process_id(), NO_PROCESS_FOUND_PID);
    }

    #[test]
    fn test_info_get_program_id() {
        let info = Info::with_process_list(make_test_process_list());
        assert_eq!(info.get_program_id(100), Ok(0x0100000000001000));
        assert_eq!(info.get_program_id(999), Err(RESULT_PROCESS_NOT_FOUND));
    }

    #[test]
    fn test_info_atmosphere_get_process_id() {
        let info = Info::with_process_list(make_test_process_list());
        assert_eq!(info.atmosphere_get_process_id(0x0100000000001000), Ok(100));
        assert_eq!(
            info.atmosphere_get_process_id(0xDEAD),
            Err(RESULT_PROCESS_NOT_FOUND)
        );
    }

    #[test]
    fn test_shell_get_application_pid() {
        let shell = Shell::with_process_list(make_test_process_list());
        assert_eq!(shell.get_application_process_id_for_shell(), 100);
    }
}

// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pcie/pcie.cpp
//!
//! PCIe service and ISession. All commands are unimplemented stubs.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ISession
pub mod session_commands {
    pub const QUERY_FUNCTIONS: u32 = 0;
    pub const ACQUIRE_FUNCTION: u32 = 1;
    pub const RELEASE_FUNCTION: u32 = 2;
    pub const GET_FUNCTION_STATE: u32 = 3;
    pub const GET_BAR_PROFILE: u32 = 4;
    pub const READ_CONFIG: u32 = 5;
    pub const WRITE_CONFIG: u32 = 6;
    pub const READ_BAR_REGION: u32 = 7;
    pub const WRITE_BAR_REGION: u32 = 8;
    pub const FIND_CAPABILITY: u32 = 9;
    pub const FIND_EXTENDED_CAPABILITY: u32 = 10;
    pub const MAP_DMA: u32 = 11;
    pub const UNMAP_DMA: u32 = 12;
    pub const UNMAP_DMA_BUS_ADDRESS: u32 = 13;
    pub const GET_DMA_BUS_ADDRESS: u32 = 14;
    pub const GET_DMA_BUS_ADDRESS_RANGE: u32 = 15;
    pub const SET_DMA_ENABLE: u32 = 16;
    pub const ACQUIRE_IRQ: u32 = 17;
    pub const RELEASE_IRQ: u32 = 18;
    pub const SET_IRQ_ENABLE: u32 = 19;
    pub const SET_ASPM_ENABLE: u32 = 20;
    pub const SET_RESET_UPON_RESUME_ENABLE: u32 = 21;
    pub const RESET_FUNCTION: u32 = 22;
    pub const UNKNOWN23: u32 = 23;
}

/// IPC command IDs for PCIe
pub mod pcie_commands {
    pub const REGISTER_CLASS_DRIVER: u32 = 0;
    pub const QUERY_FUNCTIONS_UNREGISTERED: u32 = 1;
}

pub struct ISession {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISession {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (session_commands::QUERY_FUNCTIONS, None, "QueryFunctions"),
                (session_commands::ACQUIRE_FUNCTION, None, "AcquireFunction"),
                (session_commands::RELEASE_FUNCTION, None, "ReleaseFunction"),
                (
                    session_commands::GET_FUNCTION_STATE,
                    None,
                    "GetFunctionState",
                ),
                (session_commands::GET_BAR_PROFILE, None, "GetBarProfile"),
                (session_commands::READ_CONFIG, None, "ReadConfig"),
                (session_commands::WRITE_CONFIG, None, "WriteConfig"),
                (session_commands::READ_BAR_REGION, None, "ReadBarRegion"),
                (session_commands::WRITE_BAR_REGION, None, "WriteBarRegion"),
                (session_commands::FIND_CAPABILITY, None, "FindCapability"),
                (
                    session_commands::FIND_EXTENDED_CAPABILITY,
                    None,
                    "FindExtendedCapability",
                ),
                (session_commands::MAP_DMA, None, "MapDma"),
                (session_commands::UNMAP_DMA, None, "UnmapDma"),
                (
                    session_commands::UNMAP_DMA_BUS_ADDRESS,
                    None,
                    "UnmapDmaBusAddress",
                ),
                (
                    session_commands::GET_DMA_BUS_ADDRESS,
                    None,
                    "GetDmaBusAddress",
                ),
                (
                    session_commands::GET_DMA_BUS_ADDRESS_RANGE,
                    None,
                    "GetDmaBusAddressRange",
                ),
                (session_commands::SET_DMA_ENABLE, None, "SetDmaEnable"),
                (session_commands::ACQUIRE_IRQ, None, "AcquireIrq"),
                (session_commands::RELEASE_IRQ, None, "ReleaseIrq"),
                (session_commands::SET_IRQ_ENABLE, None, "SetIrqEnable"),
                (session_commands::SET_ASPM_ENABLE, None, "SetAspmEnable"),
                (
                    session_commands::SET_RESET_UPON_RESUME_ENABLE,
                    None,
                    "SetResetUponResumeEnable",
                ),
                (session_commands::RESET_FUNCTION, None, "ResetFunction"),
                (session_commands::UNKNOWN23, None, "Unknown23"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ISession {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ISession"
    }
}

impl ServiceFramework for ISession {
    fn get_service_name(&self) -> &str {
        "ISession"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

pub struct PCIe {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl PCIe {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    pcie_commands::REGISTER_CLASS_DRIVER,
                    None,
                    "RegisterClassDriver",
                ),
                (
                    pcie_commands::QUERY_FUNCTIONS_UNREGISTERED,
                    None,
                    "QueryFunctionsUnregistered",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for PCIe {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "pcie"
    }
}

impl ServiceFramework for PCIe {
    fn get_service_name(&self) -> &str {
        "pcie"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "pcie" service.
///
/// Corresponds to `LoopProcess` in upstream `pcie.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "pcie",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(PCIe::new()) }),
            16,
        );
    }

    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pcie_service_tables_match_upstream_command_counts() {
        assert_eq!(PCIe::new().handlers.len(), 2);
        assert_eq!(ISession::new().handlers.len(), 24);
    }
}

// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pcv/pcv.h
//! Port of zuyu/src/core/hle/service/pcv/pcv.cpp
//!
//! PCV, CLKRST, and CLKRST_A services.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU32, Ordering};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// DeviceCode enum. Upstream: `DeviceCode` in `pcv.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum DeviceCode {
    Cpu = 0x40000001,
    Gpu = 0x40000002,
    I2s1 = 0x40000003,
    I2s2 = 0x40000004,
    I2s3 = 0x40000005,
    Pwm = 0x40000006,
    I2c1 = 0x02000001,
    I2c2 = 0x02000002,
    I2c3 = 0x02000003,
    I2c4 = 0x02000004,
    I2c5 = 0x02000005,
    I2c6 = 0x02000006,
    Spi1 = 0x07000000,
    Spi2 = 0x07000001,
    Spi3 = 0x07000002,
    Spi4 = 0x07000003,
    Disp1 = 0x40000011,
    Disp2 = 0x40000012,
    Isp = 0x40000013,
    Vi = 0x40000014,
    Sdmmc1 = 0x40000015,
    Sdmmc2 = 0x40000016,
    Sdmmc3 = 0x40000017,
    Sdmmc4 = 0x40000018,
    Owr = 0x40000019,
    Csite = 0x4000001A,
    Tsec = 0x4000001B,
    Mselect = 0x4000001C,
    Hda2codec2x = 0x4000001D,
    Actmon = 0x4000001E,
    I2cSlow = 0x4000001F,
    Sor1 = 0x40000020,
    Sata = 0x40000021,
    Hda = 0x40000022,
    XusbCoreHostSrc = 0x40000023,
    XusbFalconSrc = 0x40000024,
    XusbFsSrc = 0x40000025,
    XusbCoreDevSrc = 0x40000026,
    XusbSsSrc = 0x40000027,
    UartA = 0x03000001,
    UartB = 0x35000405,
    UartC = 0x3500040F,
    UartD = 0x37000001,
    Host1x = 0x4000002C,
    Entropy = 0x4000002D,
    SocTherm = 0x4000002E,
    Vic = 0x4000002F,
    Nvenc = 0x40000030,
    Nvjpg = 0x40000031,
    Nvdec = 0x40000032,
    Qspi = 0x40000033,
    ViI2c = 0x40000034,
    Tsecb = 0x40000035,
    Ape = 0x40000036,
    AudioDsp = 0x40000037,
    AudioUart = 0x40000038,
    Emc = 0x40000039,
    Plle = 0x4000003A,
    PlleHwSeq = 0x4000003B,
    Dsi = 0x4000003C,
    Maud = 0x4000003D,
    Dpaux1 = 0x4000003E,
    MipiCal = 0x4000003F,
    UartFstMipiCal = 0x40000040,
    Osc = 0x40000041,
    SysBus = 0x40000042,
    SorSafe = 0x40000043,
    XusbSs = 0x40000044,
    XusbHost = 0x40000045,
    XusbDevice = 0x40000046,
    Extperiph1 = 0x40000047,
    Ahub = 0x40000048,
    Hda2hdmicodec = 0x40000049,
    Gpuaux = 0x4000004A,
    UsbD = 0x4000004B,
    Usb2 = 0x4000004C,
    Pcie = 0x4000004D,
    Afi = 0x4000004E,
    PciExClk = 0x4000004F,
    PExUsbPhy = 0x40000050,
    XUsbPadCtl = 0x40000051,
    Apbdma = 0x40000052,
    Usb2TrkClk = 0x40000053,
    XUsbIoPll = 0x40000054,
    XUsbIoPllHwSeq = 0x40000055,
    Cec = 0x40000056,
    Extperiph2 = 0x40000057,
    OscClk = 0x40000080,
}

/// IPC command IDs for PCV
pub mod pcv_commands {
    pub const SET_POWER_ENABLED: u32 = 0;
    pub const SET_CLOCK_ENABLED: u32 = 1;
    pub const SET_CLOCK_RATE: u32 = 2;
    pub const GET_CLOCK_RATE: u32 = 3;
    pub const GET_STATE: u32 = 4;
    pub const GET_POSSIBLE_CLOCK_RATES: u32 = 5;
    pub const SET_MIN_V_CLOCK_RATE: u32 = 6;
    pub const SET_RESET: u32 = 7;
    pub const SET_VOLTAGE_ENABLED: u32 = 8;
    pub const GET_VOLTAGE_ENABLED: u32 = 9;
    pub const GET_VOLTAGE_RANGE: u32 = 10;
    pub const SET_VOLTAGE_VALUE: u32 = 11;
    pub const GET_VOLTAGE_VALUE: u32 = 12;
    pub const GET_TEMPERATURE_THRESHOLDS: u32 = 13;
    pub const SET_TEMPERATURE: u32 = 14;
    pub const INITIALIZE: u32 = 15;
    pub const IS_INITIALIZED: u32 = 16;
    pub const FINALIZE: u32 = 17;
    pub const POWER_ON: u32 = 18;
    pub const POWER_OFF: u32 = 19;
    pub const CHANGE_VOLTAGE: u32 = 20;
    pub const GET_POWER_CLOCK_INFO_EVENT: u32 = 21;
    pub const GET_OSCILLATOR_CLOCK: u32 = 22;
    pub const GET_DVFS_TABLE: u32 = 23;
    pub const GET_MODULE_STATE_TABLE: u32 = 24;
    pub const GET_POWER_DOMAIN_STATE_TABLE: u32 = 25;
    pub const GET_FUSE_INFO: u32 = 26;
    pub const GET_DRAM_ID: u32 = 27;
    pub const IS_POWERED_ON: u32 = 28;
    pub const GET_VOLTAGE: u32 = 29;
}

/// IPC command IDs for CLKRST
pub mod clkrst_commands {
    pub const OPEN_SESSION: u32 = 0;
    pub const GET_TEMPERATURE_THRESHOLDS: u32 = 1;
    pub const SET_TEMPERATURE: u32 = 2;
    pub const GET_MODULE_STATE_TABLE: u32 = 3;
    pub const GET_MODULE_STATE_TABLE_EVENT: u32 = 4;
    pub const GET_MODULE_STATE_TABLE_MAX_COUNT: u32 = 5;
}

/// IPC command IDs for IClkrstSession
pub mod clkrst_session_commands {
    pub const SET_CLOCK_ENABLED: u32 = 0;
    pub const SET_CLOCK_DISABLED: u32 = 1;
    pub const SET_RESET_ASSERTED: u32 = 2;
    pub const SET_RESET_DEASSERTED: u32 = 3;
    pub const SET_POWER_ENABLED: u32 = 4;
    pub const SET_POWER_DISABLED: u32 = 5;
    pub const GET_STATE: u32 = 6;
    pub const SET_CLOCK_RATE: u32 = 7;
    pub const GET_CLOCK_RATE: u32 = 8;
    pub const SET_MIN_V_CLOCK_RATE: u32 = 9;
    pub const GET_POSSIBLE_CLOCK_RATES: u32 = 10;
    pub const GET_DVFS_TABLE: u32 = 11;
}

pub mod clkrst_a_commands {
    pub const RELEASE_CONTROL: u32 = 0;
}

/// PCV service ("pcv"). All commands are unimplemented stubs.
pub struct PCV {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl PCV {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (pcv_commands::SET_POWER_ENABLED, None, "SetPowerEnabled"),
                (pcv_commands::SET_CLOCK_ENABLED, None, "SetClockEnabled"),
                (pcv_commands::SET_CLOCK_RATE, None, "SetClockRate"),
                (pcv_commands::GET_CLOCK_RATE, None, "GetClockRate"),
                (pcv_commands::GET_STATE, None, "GetState"),
                (
                    pcv_commands::GET_POSSIBLE_CLOCK_RATES,
                    None,
                    "GetPossibleClockRates",
                ),
                (pcv_commands::SET_MIN_V_CLOCK_RATE, None, "SetMinVClockRate"),
                (pcv_commands::SET_RESET, None, "SetReset"),
                (pcv_commands::SET_VOLTAGE_ENABLED, None, "SetVoltageEnabled"),
                (pcv_commands::GET_VOLTAGE_ENABLED, None, "GetVoltageEnabled"),
                (pcv_commands::GET_VOLTAGE_RANGE, None, "GetVoltageRange"),
                (pcv_commands::SET_VOLTAGE_VALUE, None, "SetVoltageValue"),
                (pcv_commands::GET_VOLTAGE_VALUE, None, "GetVoltageValue"),
                (
                    pcv_commands::GET_TEMPERATURE_THRESHOLDS,
                    None,
                    "GetTemperatureThresholds",
                ),
                (pcv_commands::SET_TEMPERATURE, None, "SetTemperature"),
                (pcv_commands::INITIALIZE, None, "Initialize"),
                (pcv_commands::IS_INITIALIZED, None, "IsInitialized"),
                (pcv_commands::FINALIZE, None, "Finalize"),
                (pcv_commands::POWER_ON, None, "PowerOn"),
                (pcv_commands::POWER_OFF, None, "PowerOff"),
                (pcv_commands::CHANGE_VOLTAGE, None, "ChangeVoltage"),
                (
                    pcv_commands::GET_POWER_CLOCK_INFO_EVENT,
                    None,
                    "GetPowerClockInfoEvent",
                ),
                (
                    pcv_commands::GET_OSCILLATOR_CLOCK,
                    None,
                    "GetOscillatorClock",
                ),
                (pcv_commands::GET_DVFS_TABLE, None, "GetDvfsTable"),
                (
                    pcv_commands::GET_MODULE_STATE_TABLE,
                    None,
                    "GetModuleStateTable",
                ),
                (
                    pcv_commands::GET_POWER_DOMAIN_STATE_TABLE,
                    None,
                    "GetPowerDomainStateTable",
                ),
                (pcv_commands::GET_FUSE_INFO, None, "GetFuseInfo"),
                (pcv_commands::GET_DRAM_ID, None, "GetDramId"),
                (pcv_commands::IS_POWERED_ON, None, "IsPoweredOn"),
                (pcv_commands::GET_VOLTAGE, None, "GetVoltage"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for PCV {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "pcv"
    }
}

impl ServiceFramework for PCV {
    fn get_service_name(&self) -> &str {
        "pcv"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IClkrstSession.
pub struct IClkrstSession {
    device_code: u32,
    clock_rate: AtomicU32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IClkrstSession {
    pub fn new(device_code: u32) -> Self {
        Self {
            device_code,
            clock_rate: AtomicU32::new(0),
            handlers: build_handler_map(&[
                (
                    clkrst_session_commands::SET_CLOCK_ENABLED,
                    None,
                    "SetClockEnabled",
                ),
                (
                    clkrst_session_commands::SET_CLOCK_DISABLED,
                    None,
                    "SetClockDisabled",
                ),
                (
                    clkrst_session_commands::SET_RESET_ASSERTED,
                    None,
                    "SetResetAsserted",
                ),
                (
                    clkrst_session_commands::SET_RESET_DEASSERTED,
                    None,
                    "SetResetDeasserted",
                ),
                (
                    clkrst_session_commands::SET_POWER_ENABLED,
                    None,
                    "SetPowerEnabled",
                ),
                (
                    clkrst_session_commands::SET_POWER_DISABLED,
                    None,
                    "SetPowerDisabled",
                ),
                (clkrst_session_commands::GET_STATE, None, "GetState"),
                (
                    clkrst_session_commands::SET_CLOCK_RATE,
                    Some(IClkrstSession::set_clock_rate_handler),
                    "SetClockRate",
                ),
                (
                    clkrst_session_commands::GET_CLOCK_RATE,
                    Some(IClkrstSession::get_clock_rate_handler),
                    "GetClockRate",
                ),
                (
                    clkrst_session_commands::SET_MIN_V_CLOCK_RATE,
                    None,
                    "SetMinVClockRate",
                ),
                (
                    clkrst_session_commands::GET_POSSIBLE_CLOCK_RATES,
                    None,
                    "GetPossibleClockRates",
                ),
                (
                    clkrst_session_commands::GET_DVFS_TABLE,
                    None,
                    "GetDvfsTable",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn set_clock_rate(&self, clock_rate: u32) {
        log::debug!(
            "(STUBBED) IClkrstSession::set_clock_rate called, device_code={:#x}, clock_rate={}",
            self.device_code,
            clock_rate,
        );
        self.clock_rate.store(clock_rate, Ordering::Relaxed);
    }

    pub fn get_clock_rate(&self) -> u32 {
        log::debug!("(STUBBED) IClkrstSession::get_clock_rate called");
        self.clock_rate.load(Ordering::Relaxed)
    }

    fn set_clock_rate_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IClkrstSession) };
        let mut rp = RequestParser::new(ctx);
        let clock_rate = rp.pop_u32();
        service.set_clock_rate(clock_rate);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_clock_rate_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IClkrstSession) };
        let clock_rate = service.get_clock_rate();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(clock_rate);
    }
}

impl SessionRequestHandler for IClkrstSession {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IClkrstSession"
    }
}

impl ServiceFramework for IClkrstSession {
    fn get_service_name(&self) -> &str {
        "IClkrstSession"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// CLKRST service ("clkrst", "clkrst:i").
pub struct CLKRST {
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl CLKRST {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            handlers: build_handler_map(&[
                (
                    clkrst_commands::OPEN_SESSION,
                    Some(CLKRST::open_session_handler),
                    "OpenSession",
                ),
                (
                    clkrst_commands::GET_TEMPERATURE_THRESHOLDS,
                    None,
                    "GetTemperatureThresholds",
                ),
                (clkrst_commands::SET_TEMPERATURE, None, "SetTemperature"),
                (
                    clkrst_commands::GET_MODULE_STATE_TABLE,
                    None,
                    "GetModuleStateTable",
                ),
                (
                    clkrst_commands::GET_MODULE_STATE_TABLE_EVENT,
                    None,
                    "GetModuleStateTableEvent",
                ),
                (
                    clkrst_commands::GET_MODULE_STATE_TABLE_MAX_COUNT,
                    None,
                    "GetModuleStateTableMaxCount",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn open_session(&self, device_code: u32, _unknown_input: u32) -> IClkrstSession {
        log::debug!(
            "CLKRST({})::open_session called, device_code={:#x}",
            self.name,
            device_code
        );
        IClkrstSession::new(device_code)
    }

    fn open_session_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const CLKRST) };
        let mut rp = RequestParser::new(ctx);
        let device_code = rp.pop_u32();
        let unknown_input = rp.pop_u32();
        let session = service.open_session(device_code, unknown_input);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(std::sync::Arc::new(session));
    }
}

impl SessionRequestHandler for CLKRST {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl ServiceFramework for CLKRST {
    fn get_service_name(&self) -> &str {
        &self.name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// CLKRST_A service ("clkrst:a"). All stubs.
pub struct ClkrstA {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ClkrstA {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(
                clkrst_a_commands::RELEASE_CONTROL,
                None,
                "ReleaseControl",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ClkrstA {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "clkrst:a"
    }
}

impl ServiceFramework for ClkrstA {
    fn get_service_name(&self) -> &str {
        "clkrst:a"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "pcv", "clkrst", "clkrst:i", "clkrst:a" services.
///
/// Corresponds to `LoopProcess` in upstream `pcv.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "pcv",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(PCV::new()) }),
            16,
        );
        server_manager.register_named_service(
            "clkrst",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(CLKRST::new("clkrst")) }),
            16,
        );
        server_manager.register_named_service(
            "clkrst:i",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(CLKRST::new("clkrst:i"))
            }),
            16,
        );
        server_manager.register_named_service(
            "clkrst:a",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(ClkrstA::new()) }),
            16,
        );
    }

    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn device_code_values_match_upstream() {
        assert_eq!(DeviceCode::Cpu as u32, 0x40000001);
        assert_eq!(DeviceCode::I2c1 as u32, 0x02000001);
        assert_eq!(DeviceCode::Vic as u32, 0x4000002F);
        assert_eq!(DeviceCode::OscClk as u32, 0x40000080);
    }

    #[test]
    fn clkrst_session_roundtrips_clock_rate() {
        let session = IClkrstSession::new(DeviceCode::Gpu as u32);
        session.set_clock_rate(768_000_000);
        assert_eq!(session.get_clock_rate(), 768_000_000);
    }
}

// SPDX-FileCopyrightText: 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: 2021 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/nvdrv_interface.h
//! Port of zuyu/src/core/hle/service/nvdrv/nvdrv_interface.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use super::core::container::SessionId;
use super::nvdata::*;
use super::nvdrv::Module;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::svc_common::PseudoHandle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for NVDRV:
/// - 0: Open
/// - 1: Ioctl
/// - 2: Close
/// - 3: Initialize
/// - 4: QueryEvent
/// - 5: MapSharedMem (unimplemented)
/// - 6: GetStatus
/// - 7: SetAruidForTest (unimplemented)
/// - 8: SetAruid
/// - 9: DumpGraphicsMemoryInfo
/// - 10: InitializeDevtools (unimplemented)
/// - 11: Ioctl2
/// - 12: Ioctl3
/// - 13: SetGraphicsFirmwareMemoryMarginEnabled
pub struct NvdrvInterface {
    nvdrv: Arc<Module>,
    pid: u64,
    is_initialized: bool,
    session_id: SessionId,
    output_buffer: Vec<u8>,
    inline_output_buffer: Vec<u8>,
}

impl NvdrvInterface {
    pub fn new(nvdrv: Arc<Module>) -> Self {
        Self {
            nvdrv,
            pid: 0,
            is_initialized: false,
            session_id: SessionId { id: 0 },
            output_buffer: Vec::new(),
            inline_output_buffer: Vec::new(),
        }
    }

    pub fn get_module(&self) -> Arc<Module> {
        Arc::clone(&self.nvdrv)
    }

    /// Port of NVDRV::Open
    pub fn open(&mut self, device_name: &str) -> (DeviceFD, NvResult) {
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return (0, NvResult::NotInitialized);
        }

        if device_name == "/dev/nvhost-prof-gpu" {
            log::warn!("/dev/nvhost-prof-gpu cannot be opened in production");
            return (0, NvResult::NotSupported);
        }

        let fd = self.nvdrv.open(device_name, self.session_id);
        let result = if fd != INVALID_NVDRV_FD {
            NvResult::Success
        } else {
            NvResult::FileOperationFailed
        };
        (fd, result)
    }

    /// Port of NVDRV::Ioctl1
    pub fn ioctl1(&mut self, fd: DeviceFD, command: Ioctl, input: &[u8], output: &mut [u8]) -> NvResult {
        log::debug!("Ioctl1 called fd={}, ioctl=0x{:08X}", fd, command.raw);
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return NvResult::NotInitialized;
        }

        let nv_result = self.nvdrv.ioctl1(fd, command, input, output);
        nv_result
    }

    /// Port of NVDRV::Ioctl2
    pub fn ioctl2(
        &mut self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        inline_input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        log::debug!("Ioctl2 called fd={}, ioctl=0x{:08X}", fd, command.raw);
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return NvResult::NotInitialized;
        }

        let nv_result = self.nvdrv.ioctl2(fd, command, input, inline_input, output);
        nv_result
    }

    /// Port of NVDRV::Ioctl3
    pub fn ioctl3(
        &mut self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
        inline_output: &mut [u8],
    ) -> NvResult {
        log::debug!("Ioctl3 called fd={}, ioctl=0x{:08X}", fd, command.raw);
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return NvResult::NotInitialized;
        }

        let nv_result = self.nvdrv.ioctl3(fd, command, input, output, inline_output);
        nv_result
    }

    /// Port of NVDRV::Close
    pub fn close(&mut self, fd: DeviceFD) -> NvResult {
        log::debug!("Close called");
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return NvResult::NotInitialized;
        }

        self.nvdrv.close(fd)
    }

    /// Port of NVDRV::Initialize
    pub fn initialize(&mut self, process: &Arc<Mutex<KProcess>>) -> NvResult {
        log::warn!("(STUBBED) Initialize called");
        if self.is_initialized {
            return NvResult::Success;
        }

        let container = self.nvdrv.get_container();
        self.session_id = container.open_session(process);
        self.is_initialized = true;
        NvResult::Success
    }

    /// Port of NVDRV::QueryEvent
    pub fn query_event(&self, fd: DeviceFD, event_id: u32) -> (NvResult, Option<u32>) {
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return (NvResult::NotInitialized, None);
        }

        self.nvdrv.query_event(fd, event_id)
    }

    /// Port of NVDRV::SetAruid
    pub fn set_aruid(&mut self, pid: u64) {
        self.pid = pid;
        log::warn!("(STUBBED) SetAruid called, pid=0x{:X}", pid);
    }

    /// Port of NVDRV::SetGraphicsFirmwareMemoryMarginEnabled
    pub fn set_graphics_firmware_memory_margin_enabled(&self) {
        log::warn!("(STUBBED) SetGraphicsFirmwareMemoryMarginEnabled called");
    }

    /// Port of NVDRV::GetStatus
    pub fn get_status(&self) -> NvResult {
        log::warn!("(STUBBED) GetStatus called");
        NvResult::Success
    }

    /// Port of NVDRV::DumpGraphicsMemoryInfo
    pub fn dump_graphics_memory_info(&self) {
        log::debug!("DumpGraphicsMemoryInfo called");
    }
}

impl Drop for NvdrvInterface {
    fn drop(&mut self) {
        if self.is_initialized {
            let container = self.nvdrv.get_container();
            container.close_session(self.session_id);
        }
    }
}

pub struct NvdrvService {
    name: String,
    interface: Mutex<NvdrvInterface>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NvdrvService {
    pub fn new(nvdrv: Arc<Module>, name: &str) -> Self {
        Self {
            name: name.to_string(),
            interface: Mutex::new(NvdrvInterface::new(nvdrv)),
            handlers: build_handler_map(&[
                (0, Some(Self::open_handler), "Open"),
                (1, Some(Self::ioctl1_handler), "Ioctl"),
                (2, Some(Self::close_handler), "Close"),
                (3, Some(Self::initialize_handler), "Initialize"),
                (4, Some(Self::query_event_handler), "QueryEvent"),
                (6, Some(Self::get_status_handler), "GetStatus"),
                (8, Some(Self::set_aruid_handler), "SetAruid"),
                (9, Some(Self::dump_graphics_memory_info_handler), "DumpGraphicsMemoryInfo"),
                (11, Some(Self::ioctl2_handler), "Ioctl2"),
                (12, Some(Self::ioctl3_handler), "Ioctl3"),
                (
                    13,
                    Some(Self::set_graphics_firmware_memory_margin_enabled_handler),
                    "SetGraphicsFirmwareMemoryMarginEnabled",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    // read_buffer / write_buffer removed — use ctx.read_buffer() / ctx.write_buffer()
    // from HLERequestContext (matches upstream where these are methods on HLERequestContext,
    // not on individual service classes).

    fn push_nv_result(ctx: &mut HLERequestContext, nv_result: NvResult) {
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(nv_result as u32);
    }

    fn open_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        let device_name_bytes = ctx.read_buffer(0);
        let end = device_name_bytes
            .iter()
            .position(|&b| b == 0)
            .unwrap_or(device_name_bytes.len());
        let device_name = String::from_utf8_lossy(&device_name_bytes[..end]).to_string();

        let (fd, nv_result) = service.interface.lock().unwrap().open(&device_name);

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i32(fd);
        rb.push_u32(nv_result as u32);
    }

    fn ioctl1_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        let mut rp = RequestParser::new(ctx);
        let fd = rp.pop_i32();
        let command = rp.pop_raw::<Ioctl>();
        let input = ctx.read_buffer(0);
        let mut output = vec![0; ctx.get_write_buffer_size(0)];
        let nv_result = service
            .interface
            .lock()
            .unwrap()
            .ioctl1(fd, command, &input, &mut output);
        if command.is_out() {
            ctx.write_buffer(&output, 0);
        }
        Self::push_nv_result(ctx, nv_result);
    }

    fn ioctl2_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        let mut rp = RequestParser::new(ctx);
        let fd = rp.pop_i32();
        let command = rp.pop_raw::<Ioctl>();
        let input = ctx.read_buffer(0);
        let inline_input = ctx.read_buffer(1);
        let mut output = vec![0; ctx.get_write_buffer_size(0)];
        let nv_result = service
            .interface
            .lock()
            .unwrap()
            .ioctl2(fd, command, &input, &inline_input, &mut output);
        if command.is_out() {
            ctx.write_buffer(&output, 0);
        }
        Self::push_nv_result(ctx, nv_result);
    }

    fn ioctl3_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        let mut rp = RequestParser::new(ctx);
        let fd = rp.pop_i32();
        let command = rp.pop_raw::<Ioctl>();
        let input = ctx.read_buffer(0);
        let mut output = vec![0; ctx.get_write_buffer_size(0)];
        let mut inline_output = vec![0; ctx.get_write_buffer_size(1)];
        let nv_result = service
            .interface
            .lock()
            .unwrap()
            .ioctl3(fd, command, &input, &mut output, &mut inline_output);
        if command.is_out() {
            ctx.write_buffer(&output, 0);
            ctx.write_buffer(&inline_output, 1);
        }
        Self::push_nv_result(ctx, nv_result);
    }

    fn close_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        let mut rp = RequestParser::new(ctx);
        let fd = rp.pop_i32();
        let nv_result = service.interface.lock().unwrap().close(fd);
        Self::push_nv_result(ctx, nv_result);
    }

    fn initialize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        let mut rp = RequestParser::new(ctx);
        let process_handle = ctx.get_copy_handle(0);
        let _transfer_memory_handle = ctx.get_copy_handle(1);
        let _transfer_memory_size = rp.pop_u32();
        let nv_result = match Self::resolve_process_from_handle(ctx, process_handle) {
            Some(process) => service.interface.lock().unwrap().initialize(&process),
            None => {
                log::error!(
                    "NVDRV::Initialize could not resolve process handle {:#x}",
                    process_handle
                );
                NvResult::BadParameter
            }
        };
        Self::push_nv_result(ctx, nv_result);
    }

    fn resolve_process_from_handle(
        ctx: &HLERequestContext,
        process_handle: u32,
    ) -> Option<Arc<Mutex<KProcess>>> {
        let thread = ctx.get_thread()?;
        let parent = {
            let thread_guard = thread.lock().unwrap();
            thread_guard.parent.as_ref()?.upgrade()?
        };

        if process_handle == PseudoHandle::CurrentProcess as u32 || process_handle == 0 {
            return Some(parent);
        }

        let is_valid = {
            let parent_guard = parent.lock().unwrap();
            parent_guard.handle_table.get_object(process_handle).is_some()
        };

        if !is_valid {
            log::error!(
                "NVDRV::Initialize handle {:#x} is not valid in the current process handle table",
                process_handle
            );
            return None;
        }

        Some(parent)
    }

    fn query_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        let mut rp = RequestParser::new(ctx);
        let fd = rp.pop_i32();
        let event_id = rp.pop_u32();
        let (nv_result, maybe_event) = service.interface.lock().unwrap().query_event(fd, event_id);
        let copy_handle = if nv_result == NvResult::Success && maybe_event.is_some() {
            ctx.create_readable_event_handle(false)
        } else {
            None
        };

        if let Some(handle) = copy_handle {
            let mut rb = ResponseBuilder::new(ctx, 3, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
            rb.push_u32(nv_result as u32);
        } else {
            Self::push_nv_result(ctx, nv_result);
        }
    }

    fn set_aruid_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        let mut rp = RequestParser::new(ctx);
        let pid = rp.pop_u64();
        service.interface.lock().unwrap().set_aruid(pid);
        Self::push_nv_result(ctx, NvResult::Success);
    }

    fn get_status_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        let result = service.interface.lock().unwrap().get_status();
        Self::push_nv_result(ctx, result);
    }

    fn dump_graphics_memory_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        service.interface.lock().unwrap().dump_graphics_memory_info();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_graphics_firmware_memory_margin_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const NvdrvService) };
        service
            .interface
            .lock()
            .unwrap()
            .set_graphics_firmware_memory_margin_enabled();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for NvdrvService {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl ServiceFramework for NvdrvService {
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

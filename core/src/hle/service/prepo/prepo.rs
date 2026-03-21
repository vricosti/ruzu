// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/prepo/prepo.cpp
//!
//! PlayReport service -- "prepo:a", "prepo:a2", "prepo:m", "prepo:s", "prepo:u".

use std::collections::BTreeMap;
use std::sync::Arc;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use crate::reporter;

/// Play report type, matching upstream Reporter::PlayReportType.
///
/// Corresponds to `Core::Reporter::PlayReportType` used in upstream prepo.cpp.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PlayReportType {
    Old = 0,
    Old2 = 1,
    New = 2,
    System = 3,
}

/// PlayReport service ("prepo:a", "prepo:a2", "prepo:m", "prepo:s", "prepo:u").
///
/// Corresponds to `PlayReport` in upstream prepo.cpp.
pub struct PlayReport {
    name: String,
    reporter: Arc<reporter::Reporter>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl PlayReport {
    pub fn new(name: &str, reporter: Arc<reporter::Reporter>) -> Self {
        let handlers = build_handler_map(&[
            (10100, Some(PlayReport::save_report_old_handler), "SaveReportOld"),
            (10101, Some(PlayReport::save_report_with_user_old_handler), "SaveReportWithUserOld"),
            (10102, Some(PlayReport::save_report_old2_handler), "SaveReportOld2"),
            (10103, Some(PlayReport::save_report_with_user_old2_handler), "SaveReportWithUserOld2"),
            (10104, Some(PlayReport::save_report_new_handler), "SaveReport"),
            (10105, Some(PlayReport::save_report_with_user_new_handler), "SaveReportWithUser"),
            (10200, Some(PlayReport::request_immediate_transmission_handler), "RequestImmediateTransmission"),
            (10300, Some(PlayReport::get_transmission_status_handler), "GetTransmissionStatus"),
            (10400, Some(PlayReport::get_system_session_id_handler), "GetSystemSessionId"),
            (20100, Some(PlayReport::save_system_report_handler), "SaveSystemReport"),
            (20101, Some(PlayReport::save_system_report_with_user_handler), "SaveSystemReportWithUser"),
            (20200, Some(PlayReport::stub_handler), "SetOperationMode"),
            (30100, Some(PlayReport::stub_handler), "ClearStorage"),
            (30200, Some(PlayReport::stub_handler), "ClearStatistics"),
            (30300, Some(PlayReport::stub_handler), "GetStorageUsage"),
            (30400, Some(PlayReport::stub_handler), "GetStatistics"),
            (30401, Some(PlayReport::stub_handler), "GetThroughputHistory"),
            (30500, Some(PlayReport::stub_handler), "GetLastUploadError"),
            (30600, Some(PlayReport::stub_handler), "GetApplicationUploadSummary"),
            (40100, Some(PlayReport::stub_handler), "IsUserAgreementCheckEnabled"),
            (40101, Some(PlayReport::stub_handler), "SetUserAgreementCheckEnabled"),
            (50100, Some(PlayReport::stub_handler), "ReadAllApplicationReportFiles"),
            (90100, Some(PlayReport::stub_handler), "ReadAllReportFiles"),
            (90101, Some(PlayReport::stub_handler), "Unknown90101"),
            (90102, Some(PlayReport::stub_handler), "Unknown90102"),
            (90200, Some(PlayReport::stub_handler), "GetStatistics"),
            (90201, Some(PlayReport::stub_handler), "GetThroughputHistory"),
            (90300, Some(PlayReport::stub_handler), "GetLastUploadError"),
        ]);

        Self {
            name: name.to_string(),
            reporter,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// SaveReport -- saves a play report of the given type.
    ///
    /// Corresponds to `PlayReport::SaveReport<Type>` in upstream prepo.cpp.
    pub fn save_report(
        &self,
        report_type: PlayReportType,
        title_id: u64,
        process_id: u64,
        data1: &[u8],
        data2: &[u8],
    ) {
        log::debug!(
            "PlayReport({})::save_report called, type={:02X}, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            self.name,
            report_type as u8,
            process_id,
            data1.len(),
            data2.len()
        );
        let reporter_type = match report_type {
            PlayReportType::Old => reporter::PlayReportType::Old,
            PlayReportType::Old2 => reporter::PlayReportType::Old2,
            PlayReportType::New => reporter::PlayReportType::New,
            PlayReportType::System => reporter::PlayReportType::System,
        };
        let data: Vec<&[u8]> = if data2.is_empty() {
            vec![data1]
        } else {
            vec![data1, data2]
        };
        self.reporter.save_play_report(reporter_type, title_id, &data, Some(process_id), None);
    }

    /// SaveReportWithUser -- saves a play report with a user ID.
    ///
    /// Corresponds to `PlayReport::SaveReportWithUser<Type>` in upstream prepo.cpp.
    pub fn save_report_with_user(
        &self,
        report_type: PlayReportType,
        title_id: u64,
        user_id: u128,
        process_id: u64,
        data1: &[u8],
        data2: &[u8],
    ) {
        log::debug!(
            "PlayReport({})::save_report_with_user called, type={:02X}, user_id={:032X}, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            self.name,
            report_type as u8,
            user_id,
            process_id,
            data1.len(),
            data2.len()
        );
        let reporter_type = match report_type {
            PlayReportType::Old => reporter::PlayReportType::Old,
            PlayReportType::Old2 => reporter::PlayReportType::Old2,
            PlayReportType::New => reporter::PlayReportType::New,
            PlayReportType::System => reporter::PlayReportType::System,
        };
        let data: Vec<&[u8]> = if data2.is_empty() {
            vec![data1]
        } else {
            vec![data1, data2]
        };
        self.reporter.save_play_report(reporter_type, title_id, &data, Some(process_id), Some(user_id));
    }

    /// RequestImmediateTransmission (cmd 10200).
    ///
    /// Corresponds to `PlayReport::RequestImmediateTransmission` in upstream prepo.cpp.
    pub fn request_immediate_transmission(&self) {
        log::warn!("(STUBBED) PlayReport::request_immediate_transmission called");
    }

    /// GetTransmissionStatus (cmd 10300).
    ///
    /// Corresponds to `PlayReport::GetTransmissionStatus` in upstream prepo.cpp.
    pub fn get_transmission_status(&self) -> i32 {
        log::warn!("(STUBBED) PlayReport::get_transmission_status called");
        0
    }

    /// GetSystemSessionId (cmd 10400).
    ///
    /// Corresponds to `PlayReport::GetSystemSessionId` in upstream prepo.cpp.
    pub fn get_system_session_id(&self) -> u64 {
        log::warn!("(STUBBED) PlayReport::get_system_session_id called");
        0
    }

    /// SaveSystemReport (cmd 20100).
    ///
    /// Corresponds to `PlayReport::SaveSystemReport` in upstream prepo.cpp.
    pub fn save_system_report(&self, title_id: u64, data1: &[u8], data2: &[u8]) {
        log::debug!(
            "PlayReport({})::save_system_report called, title_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            self.name,
            title_id,
            data1.len(),
            data2.len()
        );
        let data: Vec<&[u8]> = if data2.is_empty() {
            vec![data1]
        } else {
            vec![data1, data2]
        };
        self.reporter.save_play_report(reporter::PlayReportType::System, title_id, &data, None, None);
    }

    /// SaveSystemReportWithUser (cmd 20101).
    ///
    /// Corresponds to `PlayReport::SaveSystemReportWithUser` in upstream prepo.cpp.
    pub fn save_system_report_with_user(
        &self,
        user_id: u128,
        title_id: u64,
        data1: &[u8],
        data2: &[u8],
    ) {
        log::debug!(
            "PlayReport({})::save_system_report_with_user called, user_id={:032X}, title_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            self.name,
            user_id,
            title_id,
            data1.len(),
            data2.len()
        );
        let data: Vec<&[u8]> = if data2.is_empty() {
            vec![data1]
        } else {
            vec![data1, data2]
        };
        self.reporter.save_play_report(reporter::PlayReportType::System, title_id, &data, None, Some(user_id));
    }

    // --- Handler bridge functions ---

    /// Stub handler for nullptr entries -- logs STUBBED and returns success.
    fn stub_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::warn!("(STUBBED) PlayReport command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn save_report_old_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let mut rp = RequestParser::new(ctx);
        let process_id = rp.pop_u64();

        let data1 = ctx.read_buffer_a(0);
        let data2 = ctx.read_buffer_x(0);

        log::debug!(
            "PlayReport({})::SaveReportOld called, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            service.name, process_id, data1.len(), data2.len()
        );

        service.save_report(PlayReportType::Old, 0, process_id, &data1, &data2);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn save_report_with_user_old_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let mut rp = RequestParser::new(ctx);
        let user_id = rp.pop_raw::<u128>();
        let process_id = rp.pop_u64();

        let data1 = ctx.read_buffer_a(0);
        let data2 = ctx.read_buffer_x(0);

        log::debug!(
            "PlayReport({})::SaveReportWithUserOld called, user_id={:032X}, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            service.name, user_id, process_id, data1.len(), data2.len()
        );

        service.save_report_with_user(PlayReportType::Old, 0, user_id, process_id, &data1, &data2);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn save_report_old2_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let mut rp = RequestParser::new(ctx);
        let process_id = rp.pop_u64();

        let data1 = ctx.read_buffer_a(0);
        let data2 = ctx.read_buffer_x(0);

        log::debug!(
            "PlayReport({})::SaveReportOld2 called, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            service.name, process_id, data1.len(), data2.len()
        );

        service.save_report(PlayReportType::Old2, 0, process_id, &data1, &data2);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn save_report_with_user_old2_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let mut rp = RequestParser::new(ctx);
        let user_id = rp.pop_raw::<u128>();
        let process_id = rp.pop_u64();

        let data1 = ctx.read_buffer_a(0);
        let data2 = ctx.read_buffer_x(0);

        log::debug!(
            "PlayReport({})::SaveReportWithUserOld2 called, user_id={:032X}, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            service.name, user_id, process_id, data1.len(), data2.len()
        );

        service.save_report_with_user(PlayReportType::Old2, 0, user_id, process_id, &data1, &data2);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn save_report_new_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let mut rp = RequestParser::new(ctx);
        let process_id = rp.pop_u64();

        let data1 = ctx.read_buffer_a(0);
        let data2 = ctx.read_buffer_x(0);

        log::debug!(
            "PlayReport({})::SaveReport called, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            service.name, process_id, data1.len(), data2.len()
        );

        service.save_report(PlayReportType::New, 0, process_id, &data1, &data2);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn save_report_with_user_new_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let mut rp = RequestParser::new(ctx);
        let user_id = rp.pop_raw::<u128>();
        let process_id = rp.pop_u64();

        let data1 = ctx.read_buffer_a(0);
        let data2 = ctx.read_buffer_x(0);

        log::debug!(
            "PlayReport({})::SaveReportWithUser called, user_id={:032X}, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            service.name, user_id, process_id, data1.len(), data2.len()
        );

        service.save_report_with_user(PlayReportType::New, 0, user_id, process_id, &data1, &data2);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn save_system_report_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let mut rp = RequestParser::new(ctx);
        let title_id = rp.pop_u64();

        let data1 = ctx.read_buffer_a(0);
        let data2 = ctx.read_buffer_x(0);

        log::debug!(
            "PlayReport({})::SaveSystemReport called, title_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            service.name, title_id, data1.len(), data2.len()
        );

        service.save_system_report(title_id, &data1, &data2);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn save_system_report_with_user_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let mut rp = RequestParser::new(ctx);
        let user_id = rp.pop_raw::<u128>();
        let title_id = rp.pop_u64();

        let data1 = ctx.read_buffer_a(0);
        let data2 = ctx.read_buffer_x(0);

        log::debug!(
            "PlayReport({})::SaveSystemReportWithUser called, user_id={:032X}, title_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            service.name, user_id, title_id, data1.len(), data2.len()
        );

        service.save_system_report_with_user(user_id, title_id, &data1, &data2);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn request_immediate_transmission_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        service.request_immediate_transmission();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_transmission_status_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let status = service.get_transmission_status();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i32(status);
    }

    fn get_system_session_id_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const PlayReport) };
        let id = service.get_system_session_id();
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(id);
    }
}

impl SessionRequestHandler for PlayReport {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { &self.name }
}

impl ServiceFramework for PlayReport {
    fn get_service_name(&self) -> &str { &self.name }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

/// Registers "prepo:a", "prepo:a2", "prepo:m", "prepo:s", "prepo:u" services.
///
/// Corresponds to `LoopProcess` in upstream prepo.cpp.
pub fn loop_process() {
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    log::debug!("PlayReport::LoopProcess called");

    let reporter = Arc::new(crate::reporter::Reporter::new());
    let mut server_manager = ServerManager::new(crate::core::SystemRef::null());

    for &name in &["prepo:a", "prepo:a2", "prepo:m", "prepo:s", "prepo:u"] {
        let r = reporter.clone();
        let n = name.to_string();
        server_manager.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(PlayReport::new(&n, r.clone()))
            }),
            64,
        );
    }

    ServerManager::run_server(server_manager);
}

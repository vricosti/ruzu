// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/services.h and services.cpp
//!
//! Upstream `Services::Services(sm, system, token)` launches each service as
//! a separate kernel process via `kernel.RunOnHostCoreProcess()` (returns
//! `std::jthread`, detached) or `kernel.RunOnGuestCoreProcess()` (blocking).
//!
//! Each service module implements `LoopProcess(Core::System& system)` which:
//! 1. Creates its own `ServerManager(system)`
//! 2. Calls `server_manager->RegisterNamedService(name, handler)` for each
//!    service it provides (this registers with the global ServiceManager)
//! 3. Calls `ServerManager::RunServer()` to enter the event loop (blocking)
//!
//! Since we don't yet have kernel process spawning (`RunOnGuestCoreProcess`),
//! `ServerManager::RunServer()`, or the per-process IPC event loop, we call
//! each service's `loop_process()` directly. The `ServerManager::RunServer()`
//! stub returns immediately after registration. The net effect is the same:
//! services end up registered in the global ServiceManager.

use std::sync::{Arc, Mutex};

use crate::hle::service::server_manager::ServerManager;
use crate::hle::service::sm::sm::ServiceManager;

use crate::hle::service::hle_ipc::{
    SessionRequestHandlerFactory, SessionRequestHandlerPtr,
};

/// Generic stub service that accepts any IPC command and returns success.
///
/// Used for services that aren't fully implemented yet but need to exist
/// so that the game's SDK init doesn't abort. This does not exist in upstream
/// (every upstream service has at least a minimal implementation).
pub struct GenericStubService {
    name: String,
    handlers: std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo>,
    handlers_tipc: std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo>,
}

impl GenericStubService {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            handlers: std::collections::BTreeMap::new(),
            handlers_tipc: std::collections::BTreeMap::new(),
        }
    }
}

impl crate::hle::service::hle_ipc::SessionRequestHandler for GenericStubService {
    fn handle_sync_request(
        &self,
        ctx: &mut crate::hle::service::hle_ipc::HLERequestContext,
    ) -> crate::hle::result::ResultCode {
        log::warn!(
            "GenericStubService({}): unhandled command, returning success",
            self.name
        );
        let mut rb = crate::hle::service::ipc_helpers::ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
        crate::hle::result::RESULT_SUCCESS
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl crate::hle::service::service::ServiceFramework for GenericStubService {
    fn get_service_name(&self) -> &str {
        &self.name
    }

    fn handlers(
        &self,
    ) -> &std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(
        &self,
    ) -> &std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo> {
        &self.handlers_tipc
    }
}

/// The purpose of this struct is to own any objects that need to be shared
/// across service implementations. Torn down on system shutdown.
///
/// Upstream: `Service::Services`.
pub struct Services {}

impl Services {
    /// Launches all HLE system services.
    ///
    /// Matches upstream `Services::Services(std::shared_ptr<SM::ServiceManager>& sm,
    ///     Core::System& system, std::stop_token token)`.
    ///
    /// Upstream calls `kernel.RunOnHostCoreProcess()` / `kernel.RunOnGuestCoreProcess()`
    /// for each service. Since we don't have kernel process threads yet, we call
    /// each service's `loop_process()` directly.
    ///
    /// `device_memory` and `memory_manager` are passed as raw pointers (cast to
    /// usize for Send+Sync safety in closures) and forwarded to services that
    /// need DeviceMemory backing (e.g. time services).
    pub fn new(
        service_manager: &Arc<Mutex<ServiceManager>>,
        system: crate::core::SystemRef,
        device_memory: *const crate::device_memory::DeviceMemory,
        memory_manager: *mut crate::hle::kernel::k_memory_manager::KMemoryManager,
        filesystem_controller: Arc<Mutex<crate::hle::service::filesystem::filesystem::FileSystemController>>,
    ) -> Self {
        let dm_addr = device_memory as usize;
        let mm_addr = memory_manager as usize;

        // Upstream: system.GetFileSystemController().CreateFactories(*system.GetFilesystem(), false);
        {
            let mut fsc = filesystem_controller.lock().unwrap();
            fsc.create_factories();
        }

        // ── Host core processes (upstream: .detach()) ──
        // kernel.RunOnHostCoreProcess("audio",      [&] { Audio::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("FS",         [&] { FileSystem::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("jit",        [&] { JIT::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("ldn",        [&] { LDN::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("Loader",     [&] { LDR::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("nvservices", [&] { Nvidia::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("bsdsocket",  [&] { Sockets::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("vi",         [&, token] { VI::LoopProcess(system, token); }).detach();

        Self::loop_process_audio(service_manager, system);
        Self::loop_process_filesystem(service_manager, system, filesystem_controller);
        Self::loop_process_jit(service_manager, system);
        Self::loop_process_ldn(service_manager, system);
        Self::loop_process_loader(service_manager, system);
        Self::loop_process_nvservices(service_manager, system);
        Self::loop_process_bsdsocket(service_manager, system);
        // Upstream detaches host/guest service processes, so VI can block waiting for
        // "dispdrv" while nvnflinger registers it on another service thread. Our current
        // sequential fallback has no detached service threads yet, so we must register the
        // shared dispdrv owner before entering VI.
        Self::loop_process_nvnflinger(service_manager, system);
        Self::loop_process_vi(service_manager, system);

        // ── Guest core processes (upstream: blocking) ──
        // kernel.RunOnGuestCoreProcess("sm", ...) is called FIRST.
        // SM::LoopProcess registers the "sm:" port via ManageNamedPort.
        crate::hle::service::sm::sm::loop_process(service_manager, system);

        // kernel.RunOnGuestCoreProcess("account",    [&] { Account::LoopProcess(system); });
        Self::loop_process_account(service_manager, system);
        // kernel.RunOnGuestCoreProcess("am",         [&] { AM::LoopProcess(system); });
        crate::hle::service::am::am::loop_process(service_manager, system);
        // kernel.RunOnGuestCoreProcess("aoc",        [&] { AOC::LoopProcess(system); });
        crate::hle::service::aoc::addon_content_manager::loop_process(service_manager, system);
        // kernel.RunOnGuestCoreProcess("apm",        [&] { APM::LoopProcess(system); });
        crate::hle::service::apm::apm::loop_process(service_manager, system);
        // kernel.RunOnGuestCoreProcess("bcat",       [&] { BCAT::LoopProcess(system); });
        Self::loop_process_bcat(service_manager, system);
        // kernel.RunOnGuestCoreProcess("bpc",        [&] { BPC::LoopProcess(system); });
        Self::loop_process_bpc(service_manager, system);
        // kernel.RunOnGuestCoreProcess("btdrv",      [&] { BtDrv::LoopProcess(system); });
        Self::loop_process_btdrv(service_manager, system);
        // kernel.RunOnGuestCoreProcess("btm",        [&] { BTM::LoopProcess(system); });
        Self::loop_process_btm(service_manager, system);
        // kernel.RunOnGuestCoreProcess("capsrv",     [&] { Capture::LoopProcess(system); });
        Self::loop_process_capsrv(service_manager, system);
        // kernel.RunOnGuestCoreProcess("erpt",       [&] { ERPT::LoopProcess(system); });
        Self::loop_process_erpt(service_manager, system);
        // kernel.RunOnGuestCoreProcess("es",         [&] { ES::LoopProcess(system); });
        Self::loop_process_es(service_manager, system);
        // kernel.RunOnGuestCoreProcess("eupld",      [&] { EUPLD::LoopProcess(system); });
        Self::loop_process_eupld(service_manager, system);
        // kernel.RunOnGuestCoreProcess("fatal",      [&] { Fatal::LoopProcess(system); });
        Self::loop_process_fatal(service_manager, system);
        // kernel.RunOnGuestCoreProcess("fgm",        [&] { FGM::LoopProcess(system); });
        Self::loop_process_fgm(service_manager, system);
        // kernel.RunOnGuestCoreProcess("friends",    [&] { Friend::LoopProcess(system); });
        Self::loop_process_friends(service_manager, system);
        // kernel.RunOnGuestCoreProcess("settings",   [&] { Set::LoopProcess(system); });
        Self::loop_process_settings(service_manager, system);
        // kernel.RunOnGuestCoreProcess("psc",        [&] { PSC::LoopProcess(system); });
        Self::loop_process_psc(service_manager, system);
        // kernel.RunOnGuestCoreProcess("glue",       [&] { Glue::LoopProcess(system); });
        crate::hle::service::glue::glue::loop_process(service_manager, system, dm_addr, mm_addr);
        // kernel.RunOnGuestCoreProcess("grc",        [&] { GRC::LoopProcess(system); });
        Self::loop_process_grc(service_manager, system);
        // kernel.RunOnGuestCoreProcess("hid",        [&] { HID::LoopProcess(system); });
        Self::loop_process_hid(service_manager, system);
        // kernel.RunOnGuestCoreProcess("lbl",        [&] { LBL::LoopProcess(system); });
        Self::loop_process_lbl(service_manager, system);
        // kernel.RunOnGuestCoreProcess("LogManager.Prod", [&] { LM::LoopProcess(system); });
        Self::loop_process_lm(service_manager, system);
        // kernel.RunOnGuestCoreProcess("mig",        [&] { Migration::LoopProcess(system); });
        Self::loop_process_mig(service_manager, system);
        // kernel.RunOnGuestCoreProcess("mii",        [&] { Mii::LoopProcess(system); });
        Self::loop_process_mii(service_manager, system);
        // kernel.RunOnGuestCoreProcess("mm",         [&] { MM::LoopProcess(system); });
        Self::loop_process_mm(service_manager, system);
        // kernel.RunOnGuestCoreProcess("mnpp",       [&] { MNPP::LoopProcess(system); });
        Self::loop_process_mnpp(service_manager, system);
        // kernel.RunOnGuestCoreProcess("nvnflinger", [&] { Nvnflinger::LoopProcess(system); });
        // Already launched above in the sequential fallback to preserve the upstream
        // VI -> dispdrv dependency without detached service threads.
        // kernel.RunOnGuestCoreProcess("NCM",        [&] { NCM::LoopProcess(system); });
        Self::loop_process_ncm(service_manager, system);
        // kernel.RunOnGuestCoreProcess("nfc",        [&] { NFC::LoopProcess(system); });
        Self::loop_process_nfc(service_manager, system);
        // kernel.RunOnGuestCoreProcess("nfp",        [&] { NFP::LoopProcess(system); });
        Self::loop_process_nfp(service_manager, system);
        // kernel.RunOnGuestCoreProcess("ngc",        [&] { NGC::LoopProcess(system); });
        Self::loop_process_ngc(service_manager, system);
        // kernel.RunOnGuestCoreProcess("nifm",       [&] { NIFM::LoopProcess(system); });
        Self::loop_process_nifm(service_manager, system);
        // kernel.RunOnGuestCoreProcess("nim",        [&] { NIM::LoopProcess(system); });
        Self::loop_process_nim(service_manager, system);
        // kernel.RunOnGuestCoreProcess("npns",       [&] { NPNS::LoopProcess(system); });
        Self::loop_process_npns(service_manager, system);
        // kernel.RunOnGuestCoreProcess("ns",         [&] { NS::LoopProcess(system); });
        Self::loop_process_ns(service_manager, system);
        // kernel.RunOnGuestCoreProcess("olsc",       [&] { OLSC::LoopProcess(system); });
        Self::loop_process_olsc(service_manager, system);
        // kernel.RunOnGuestCoreProcess("omm",        [&] { OMM::LoopProcess(system); });
        Self::loop_process_omm(service_manager, system);
        // kernel.RunOnGuestCoreProcess("pcie",       [&] { PCIe::LoopProcess(system); });
        Self::loop_process_pcie(service_manager, system);
        // kernel.RunOnGuestCoreProcess("pctl",       [&] { PCTL::LoopProcess(system); });
        crate::hle::service::pctl::pctl::loop_process(service_manager, system);
        // kernel.RunOnGuestCoreProcess("pcv",        [&] { PCV::LoopProcess(system); });
        Self::loop_process_pcv(service_manager, system);
        // kernel.RunOnGuestCoreProcess("prepo",      [&] { PlayReport::LoopProcess(system); });
        Self::loop_process_prepo(service_manager, system);
        // kernel.RunOnGuestCoreProcess("ProcessManager", [&] { PM::LoopProcess(system); });
        Self::loop_process_pm(service_manager, system);
        // kernel.RunOnGuestCoreProcess("ptm",        [&] { PTM::LoopProcess(system); });
        Self::loop_process_ptm(service_manager, system);
        // kernel.RunOnGuestCoreProcess("ro",         [&] { RO::LoopProcess(system); });
        Self::loop_process_ro(service_manager, system);
        // kernel.RunOnGuestCoreProcess("spl",        [&] { SPL::LoopProcess(system); });
        Self::loop_process_spl(service_manager, system);
        // kernel.RunOnGuestCoreProcess("ssl",        [&] { SSL::LoopProcess(system); });
        Self::loop_process_ssl(service_manager, system);
        // kernel.RunOnGuestCoreProcess("usb",        [&] { USB::LoopProcess(system); });
        Self::loop_process_usb(service_manager, system);

        log::info!("Services: all service processes launched");
        Self {}
    }

    // ── Stub LoopProcess wrappers ──
    //
    // Each of these matches an upstream `LoopProcess(Core::System&)` that
    // creates a ServerManager, registers named services, and runs the server.
    // Services that have real Rust implementations call their own `loop_process`.
    // Services that are still stubs use `register_stub_services` below.

    fn loop_process_audio(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::audio::audio::loop_process(system);
    }

    fn loop_process_filesystem(
        sm: &Arc<Mutex<ServiceManager>>,
        system: crate::core::SystemRef,
        fsc: Arc<Mutex<crate::hle::service::filesystem::filesystem::FileSystemController>>,
    ) {
        crate::hle::service::filesystem::filesystem::loop_process(sm, system, fsc);
    }

    fn loop_process_jit(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["jit:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_ldn(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["ldn:m", "ldn:s", "ldn:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_loader(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["ldr:pm", "ldr:shel", "ldr:dmnt"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nvservices(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::nvdrv::loop_process(system);
    }

    fn loop_process_bsdsocket(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::sockets::sockets::loop_process(system);
    }

    fn loop_process_vi(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::vi::vi::loop_process(system);
    }

    fn loop_process_account(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::acc::acc::loop_process(system);
    }

    fn loop_process_bcat(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::bcat::bcat::loop_process(system);
    }

    fn loop_process_bpc(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::bpc::bpc::loop_process(system);
    }

    fn loop_process_btdrv(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::btdrv::btdrv::loop_process(system);
    }

    fn loop_process_btm(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::btm::btm::loop_process(system);
    }

    fn loop_process_capsrv(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::caps::caps::loop_process(system);
    }

    fn loop_process_erpt(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::erpt::erpt::loop_process(system);
    }

    fn loop_process_es(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::es::es::loop_process(system);
    }

    fn loop_process_eupld(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::eupld::eupld::loop_process(system);
    }

    fn loop_process_fatal(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["fatal:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_fgm(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::fgm::fgm::loop_process(system);
    }

    fn loop_process_friends(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::friend::friend_interface::loop_process(system);
    }

    fn loop_process_settings(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::set::settings::loop_process(system);
    }

    fn loop_process_psc(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        // psc:c, psc:m as stubs
        register_stub_services(&mut server_manager, &["psc:c", "psc:m"]);
        // time:m — real PSC::Time::ServiceManager
        server_manager.register_named_service(
            "time:m",
            Box::new(|| -> Arc<dyn crate::hle::service::hle_ipc::SessionRequestHandler> {
                Arc::new(super::psc::time::service_manager::TimeServiceManager::new())
            }),
            64,
        );
        // time:su, time:al as stubs for now
        register_stub_services(&mut server_manager, &["time:su", "time:al"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_grc(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::grc::grc::loop_process(system);
    }

    fn loop_process_hid(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::hid::hid::loop_process(system);
    }

    fn loop_process_lbl(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::lbl::lbl::loop_process(system);
    }

    fn loop_process_lm(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        // LM has a real implementation.
        let mut server_manager = ServerManager::new(system);
        let factory: SessionRequestHandlerFactory = Box::new(|| {
            Arc::new(crate::hle::service::lm::lm::LM::new())
        });
        server_manager.register_named_service("lm", factory, 64);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_mig(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["mig:usr"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_mii(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["mii:u", "mii:e"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_mm(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["mm:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_mnpp(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["mnpp:app"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nvnflinger(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::nvnflinger::nvnflinger::loop_process(system);
    }

    fn loop_process_ncm(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["lr", "ncm"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nfc(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::nfc::nfc::loop_process(system);
    }

    fn loop_process_nfp(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::nfp::nfp::loop_process(system);
    }

    fn loop_process_ngc(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["ngc:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nifm(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::nifm::nifm::loop_process(system);
    }

    fn loop_process_nim(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["nim:shp"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_npns(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["npns:s", "npns:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_ns(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::ns::ns::loop_process(system);
    }

    fn loop_process_olsc(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::olsc::olsc::loop_process(system);
    }

    fn loop_process_omm(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["omm"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_pcie(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::pcie::pcie::loop_process(system);
    }

    fn loop_process_pcv(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::pcv::pcv::loop_process(system);
    }

    fn loop_process_prepo(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::prepo::prepo::loop_process(system);
    }

    fn loop_process_pm(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::pm::pm::loop_process(system);
    }

    fn loop_process_ptm(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::ptm::ptm::loop_process(system);
    }

    fn loop_process_ro(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::ro::ro::loop_process(system);
    }

    fn loop_process_spl(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::spl::spl::loop_process(system);
    }

    fn loop_process_ssl(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        let mut server_manager = ServerManager::new(system);
        register_stub_services(&mut server_manager, &["ssl"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_usb(_sm: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
        crate::hle::service::usb::usb::loop_process(system);
    }
}

impl Drop for Services {
    /// Matches upstream `Services::~Services() = default`.
    fn drop(&mut self) {}
}

/// Registers stub services on a ServerManager.
///
/// Each service name gets a `GenericStubService` factory that returns
/// success for any IPC command. This is a temporary pattern for services
/// that aren't ported yet.
pub fn register_stub_services(server_manager: &mut ServerManager, names: &[&str]) {
    for &name in names {
        let svc_name = name.to_string();
        let factory: SessionRequestHandlerFactory = Box::new(move || {
            Arc::new(GenericStubService::new(&svc_name))
        });
        server_manager.register_named_service(name, factory, 64);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_services_creation() {
        // Verify Services can be constructed with a minimal ServiceManager.
        // We don't pass real device_memory/memory_manager in tests.
        let sm = Arc::new(Mutex::new(ServiceManager::new()));
        let fsc = Arc::new(Mutex::new(
            crate::hle::service::filesystem::filesystem::FileSystemController::new(),
        ));
        let _services = Services::new(&sm, crate::core::SystemRef::null(), std::ptr::null(), std::ptr::null_mut(), fsc);

        // Verify some services are registered.
        let sm_lock = sm.lock().unwrap();
        assert!(sm_lock.get_service_port("lm").is_ok());
        assert!(sm_lock.get_service_port("apm").is_ok());
        assert!(sm_lock.get_service_port("hid").is_ok());
    }
}

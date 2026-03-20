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
        device_memory: *const crate::device_memory::DeviceMemory,
        memory_manager: *mut crate::hle::kernel::k_memory_manager::KMemoryManager,
        filesystem_controller: Arc<Mutex<crate::hle::service::filesystem::filesystem::FileSystemController>>,
    ) -> Self {
        let dm_addr = device_memory as usize;
        let mm_addr = memory_manager as usize;

        // Upstream: system.GetFileSystemController().CreateFactories(...);
        // TODO: wire filesystem controller

        // ── Host core processes (upstream: .detach()) ──
        // kernel.RunOnHostCoreProcess("audio",      [&] { Audio::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("FS",         [&] { FileSystem::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("jit",        [&] { JIT::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("ldn",        [&] { LDN::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("Loader",     [&] { LDR::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("nvservices", [&] { Nvidia::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("bsdsocket",  [&] { Sockets::LoopProcess(system); }).detach();
        // kernel.RunOnHostCoreProcess("vi",         [&, token] { VI::LoopProcess(system, token); }).detach();

        Self::loop_process_audio(service_manager);
        Self::loop_process_filesystem(service_manager, filesystem_controller);
        Self::loop_process_jit(service_manager);
        Self::loop_process_ldn(service_manager);
        Self::loop_process_loader(service_manager);
        Self::loop_process_nvservices(service_manager);
        Self::loop_process_bsdsocket(service_manager);
        Self::loop_process_vi(service_manager);

        // ── Guest core processes (upstream: blocking) ──
        // kernel.RunOnGuestCoreProcess("sm", ...) is called FIRST.
        // SM::LoopProcess registers the "sm:" port via ManageNamedPort.
        crate::hle::service::sm::sm::loop_process(service_manager);

        // kernel.RunOnGuestCoreProcess("account",    [&] { Account::LoopProcess(system); });
        Self::loop_process_account(service_manager);
        // kernel.RunOnGuestCoreProcess("am",         [&] { AM::LoopProcess(system); });
        crate::hle::service::am::am::loop_process(service_manager);
        // kernel.RunOnGuestCoreProcess("aoc",        [&] { AOC::LoopProcess(system); });
        crate::hle::service::aoc::addon_content_manager::loop_process(service_manager);
        // kernel.RunOnGuestCoreProcess("apm",        [&] { APM::LoopProcess(system); });
        crate::hle::service::apm::apm::loop_process(service_manager);
        // kernel.RunOnGuestCoreProcess("bcat",       [&] { BCAT::LoopProcess(system); });
        Self::loop_process_bcat(service_manager);
        // kernel.RunOnGuestCoreProcess("bpc",        [&] { BPC::LoopProcess(system); });
        Self::loop_process_bpc(service_manager);
        // kernel.RunOnGuestCoreProcess("btdrv",      [&] { BtDrv::LoopProcess(system); });
        Self::loop_process_btdrv(service_manager);
        // kernel.RunOnGuestCoreProcess("btm",        [&] { BTM::LoopProcess(system); });
        Self::loop_process_btm(service_manager);
        // kernel.RunOnGuestCoreProcess("capsrv",     [&] { Capture::LoopProcess(system); });
        Self::loop_process_capsrv(service_manager);
        // kernel.RunOnGuestCoreProcess("erpt",       [&] { ERPT::LoopProcess(system); });
        Self::loop_process_erpt(service_manager);
        // kernel.RunOnGuestCoreProcess("es",         [&] { ES::LoopProcess(system); });
        Self::loop_process_es(service_manager);
        // kernel.RunOnGuestCoreProcess("eupld",      [&] { EUPLD::LoopProcess(system); });
        Self::loop_process_eupld(service_manager);
        // kernel.RunOnGuestCoreProcess("fatal",      [&] { Fatal::LoopProcess(system); });
        Self::loop_process_fatal(service_manager);
        // kernel.RunOnGuestCoreProcess("fgm",        [&] { FGM::LoopProcess(system); });
        Self::loop_process_fgm(service_manager);
        // kernel.RunOnGuestCoreProcess("friends",    [&] { Friend::LoopProcess(system); });
        Self::loop_process_friends(service_manager);
        // kernel.RunOnGuestCoreProcess("settings",   [&] { Set::LoopProcess(system); });
        Self::loop_process_settings(service_manager);
        // kernel.RunOnGuestCoreProcess("psc",        [&] { PSC::LoopProcess(system); });
        Self::loop_process_psc(service_manager);
        // kernel.RunOnGuestCoreProcess("glue",       [&] { Glue::LoopProcess(system); });
        crate::hle::service::glue::glue::loop_process(service_manager, dm_addr, mm_addr);
        // kernel.RunOnGuestCoreProcess("grc",        [&] { GRC::LoopProcess(system); });
        Self::loop_process_grc(service_manager);
        // kernel.RunOnGuestCoreProcess("hid",        [&] { HID::LoopProcess(system); });
        Self::loop_process_hid(service_manager);
        // kernel.RunOnGuestCoreProcess("lbl",        [&] { LBL::LoopProcess(system); });
        Self::loop_process_lbl(service_manager);
        // kernel.RunOnGuestCoreProcess("LogManager.Prod", [&] { LM::LoopProcess(system); });
        Self::loop_process_lm(service_manager);
        // kernel.RunOnGuestCoreProcess("mig",        [&] { Migration::LoopProcess(system); });
        Self::loop_process_mig(service_manager);
        // kernel.RunOnGuestCoreProcess("mii",        [&] { Mii::LoopProcess(system); });
        Self::loop_process_mii(service_manager);
        // kernel.RunOnGuestCoreProcess("mm",         [&] { MM::LoopProcess(system); });
        Self::loop_process_mm(service_manager);
        // kernel.RunOnGuestCoreProcess("mnpp",       [&] { MNPP::LoopProcess(system); });
        Self::loop_process_mnpp(service_manager);
        // kernel.RunOnGuestCoreProcess("nvnflinger", [&] { Nvnflinger::LoopProcess(system); });
        Self::loop_process_nvnflinger(service_manager);
        // kernel.RunOnGuestCoreProcess("NCM",        [&] { NCM::LoopProcess(system); });
        Self::loop_process_ncm(service_manager);
        // kernel.RunOnGuestCoreProcess("nfc",        [&] { NFC::LoopProcess(system); });
        Self::loop_process_nfc(service_manager);
        // kernel.RunOnGuestCoreProcess("nfp",        [&] { NFP::LoopProcess(system); });
        Self::loop_process_nfp(service_manager);
        // kernel.RunOnGuestCoreProcess("ngc",        [&] { NGC::LoopProcess(system); });
        Self::loop_process_ngc(service_manager);
        // kernel.RunOnGuestCoreProcess("nifm",       [&] { NIFM::LoopProcess(system); });
        Self::loop_process_nifm(service_manager);
        // kernel.RunOnGuestCoreProcess("nim",        [&] { NIM::LoopProcess(system); });
        Self::loop_process_nim(service_manager);
        // kernel.RunOnGuestCoreProcess("npns",       [&] { NPNS::LoopProcess(system); });
        Self::loop_process_npns(service_manager);
        // kernel.RunOnGuestCoreProcess("ns",         [&] { NS::LoopProcess(system); });
        Self::loop_process_ns(service_manager);
        // kernel.RunOnGuestCoreProcess("olsc",       [&] { OLSC::LoopProcess(system); });
        Self::loop_process_olsc(service_manager);
        // kernel.RunOnGuestCoreProcess("omm",        [&] { OMM::LoopProcess(system); });
        Self::loop_process_omm(service_manager);
        // kernel.RunOnGuestCoreProcess("pcie",       [&] { PCIe::LoopProcess(system); });
        Self::loop_process_pcie(service_manager);
        // kernel.RunOnGuestCoreProcess("pctl",       [&] { PCTL::LoopProcess(system); });
        crate::hle::service::pctl::pctl::loop_process(service_manager);
        // kernel.RunOnGuestCoreProcess("pcv",        [&] { PCV::LoopProcess(system); });
        Self::loop_process_pcv(service_manager);
        // kernel.RunOnGuestCoreProcess("prepo",      [&] { PlayReport::LoopProcess(system); });
        Self::loop_process_prepo(service_manager);
        // kernel.RunOnGuestCoreProcess("ProcessManager", [&] { PM::LoopProcess(system); });
        Self::loop_process_pm(service_manager);
        // kernel.RunOnGuestCoreProcess("ptm",        [&] { PTM::LoopProcess(system); });
        Self::loop_process_ptm(service_manager);
        // kernel.RunOnGuestCoreProcess("ro",         [&] { RO::LoopProcess(system); });
        Self::loop_process_ro(service_manager);
        // kernel.RunOnGuestCoreProcess("spl",        [&] { SPL::LoopProcess(system); });
        Self::loop_process_spl(service_manager);
        // kernel.RunOnGuestCoreProcess("ssl",        [&] { SSL::LoopProcess(system); });
        Self::loop_process_ssl(service_manager);
        // kernel.RunOnGuestCoreProcess("usb",        [&] { USB::LoopProcess(system); });
        Self::loop_process_usb(service_manager);

        log::info!("Services: all service processes launched");
        Self {}
    }

    // ── Stub LoopProcess wrappers ──
    //
    // Each of these matches an upstream `LoopProcess(Core::System&)` that
    // creates a ServerManager, registers named services, and runs the server.
    // Services that have real Rust implementations call their own `loop_process`.
    // Services that are still stubs use `register_stub_services` below.

    fn loop_process_audio(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &[
            "audout:u", "audin:u", "audren:u", "audctl", "hwopus",
        ]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_filesystem(
        sm: &Arc<Mutex<ServiceManager>>,
        fsc: Arc<Mutex<crate::hle::service::filesystem::filesystem::FileSystemController>>,
    ) {
        crate::hle::service::filesystem::filesystem::loop_process(sm, fsc);
    }

    fn loop_process_jit(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["jit:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_ldn(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["ldn:m", "ldn:s", "ldn:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_loader(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["ldr:pm", "ldr:shel", "ldr:dmnt"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nvservices(sm: &Arc<Mutex<ServiceManager>>) {
        crate::hle::service::nvdrv::loop_process(sm);
    }

    fn loop_process_bsdsocket(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &[
            "bsd:u", "bsd:s", "bsdcfg", "nsd:u", "nsd:a", "sfdnsres",
        ]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_vi(sm: &Arc<Mutex<ServiceManager>>) {
        crate::hle::service::vi::vi::loop_process(sm);
    }

    fn loop_process_account(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["acc:u0", "acc:u1"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_bcat(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["bcat:a", "bcat:u", "bcat:m", "bcat:s"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_bpc(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["bpc:r", "bpc:sf"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_btdrv(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["btdrv"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_btm(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["btm", "btm:dbg", "btm:sys", "btm:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_capsrv(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &[
            "caps:a", "caps:c", "caps:u", "caps:ss", "caps:sc", "caps:su",
        ]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_erpt(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["erpt:c", "erpt:r"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_es(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["es"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_eupld(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["eupld:c", "eupld:r"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_fatal(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["fatal:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_fgm(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["fgm", "fgm:0", "fgm:9"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_friends(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &[
            "friend:u", "friend:v", "friend:m", "friend:s", "friend:a",
        ]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_settings(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        // Register set, set:cal, set:fd as stubs
        register_stub_services(&mut server_manager, &["set", "set:cal", "set:fd"]);
        // Register set:sys with real ISystemSettingsServer implementation
        server_manager.register_named_service(
            "set:sys",
            Box::new(|| -> Arc<dyn crate::hle::service::hle_ipc::SessionRequestHandler> {
                Arc::new(super::set::system_settings_server::SystemSettingsService::new())
            }),
            64,
        );
        ServerManager::run_server(server_manager);
    }

    fn loop_process_psc(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["psc:c", "psc:m"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_grc(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["grc:c", "grc:d"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_hid(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &[
            "hid", "hid:dbg", "hid:sys", "hidbus", "irs", "irs:sys", "xcd:sys",
        ]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_lbl(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["lbl"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_lm(sm: &Arc<Mutex<ServiceManager>>) {
        // LM has a real implementation.
        let mut server_manager = ServerManager::new(sm.clone());
        let factory: SessionRequestHandlerFactory = Box::new(|| {
            Arc::new(crate::hle::service::lm::lm::LM::new())
        });
        server_manager.register_named_service("lm", factory, 64);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_mig(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["mig:usr"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_mii(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["mii:u", "mii:e"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_mm(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["mm:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_mnpp(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["mnpp:app"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nvnflinger(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["dispdrv"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_ncm(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["lr", "ncm"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nfc(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["nfc:user", "nfc:sys"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nfp(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["nfp:user", "nfp:sys"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_ngc(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["ngc:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nifm(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["nifm:u", "nifm:a", "nifm:s"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_nim(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["nim:shp"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_npns(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["npns:s", "npns:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_ns(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &[
            "ns:su", "ns:am2", "ns:ec", "ns:rid", "ns:rt", "ns:web", "ns:ro",
        ]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_olsc(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["olsc:u"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_omm(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["omm"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_pcie(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["pcie"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_pcv(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["pcv"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_prepo(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &[
            "prepo:u", "prepo:s", "prepo:m", "prepo:a",
        ]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_pm(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["pm:shell", "pm:dmnt", "pm:info"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_ptm(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["ptm"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_ro(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["ro:1"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_spl(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &[
            "csrng",
            "spl:", "spl:mig", "spl:fs", "spl:ssl", "spl:es", "spl:manu",
        ]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_ssl(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["ssl"]);
        ServerManager::run_server(server_manager);
    }

    fn loop_process_usb(sm: &Arc<Mutex<ServiceManager>>) {
        let mut server_manager = ServerManager::new(sm.clone());
        register_stub_services(&mut server_manager, &["usb:ds", "usb:hs", "usb:pm"]);
        ServerManager::run_server(server_manager);
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
fn register_stub_services(server_manager: &mut ServerManager, names: &[&str]) {
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
        let _services = Services::new(&sm, std::ptr::null(), std::ptr::null_mut(), fsc);

        // Verify some services are registered.
        let sm_lock = sm.lock().unwrap();
        assert!(sm_lock.get_service_port("lm").is_ok());
        assert!(sm_lock.get_service_port("apm").is_ok());
        assert!(sm_lock.get_service_port("hid").is_ok());
    }
}

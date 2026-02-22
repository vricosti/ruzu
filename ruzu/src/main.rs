// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

mod config;
mod game_list_ui;
mod window;

use anyhow::{Context, Result};
use clap::Parser;
use log::info;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use parking_lot::RwLock;

use ruzu_crypto::KeyManager;
use ruzu_kernel::kernel::{IpcHandler, IpcHandlerResult};
use ruzu_kernel::objects::{KSharedMemory, KernelObject};
use ruzu_kernel::KernelCore;
use ruzu_loader::game_info::{self, GameFormat};
use ruzu_loader::nro;
use ruzu_loader::nsp;
use ruzu_loader::vfs::RealFile;
use ruzu_loader::xci;
use ruzu_service::buffer_queue::BufferQueue;
use ruzu_service::framework::ServiceManager;
use ruzu_service::hid_shared_memory::HidSharedMemory;
use ruzu_service::ipc::{self as svc_ipc, IpcResponse};

/// ruzu - Nintendo Switch Emulator
#[derive(Parser, Debug)]
#[command(name = "ruzu", version, about = "Nintendo Switch Emulator written in Rust")]
struct Args {
    /// Path to the game file (NRO/NSP/XCI). If omitted, shows the game library.
    #[arg(short, long)]
    game: Option<PathBuf>,

    /// Path to the games directory for the library UI
    #[arg(long)]
    games_dir: Option<PathBuf>,

    /// Path to the keys directory (containing prod.keys / title.keys)
    #[arg(long)]
    keys_dir: Option<PathBuf>,

    /// Path to config file (default: auto-detect yuzu config)
    #[arg(short, long)]
    config: Option<PathBuf>,

    /// Enable verbose logging
    #[arg(short, long)]
    verbose: bool,

    /// Run in headless mode (no window)
    #[arg(long)]
    headless: bool,
}

// ── IPC Bridge ──────────────────────────────────────────────────────────────

/// Bridges the kernel's SVC layer to the service framework.
struct ServiceBridge {
    manager: Arc<RwLock<ServiceManager>>,
}

impl ServiceBridge {
    fn new(manager: Arc<RwLock<ServiceManager>>) -> Self {
        Self { manager }
    }
}

impl IpcHandler for ServiceBridge {
    fn handle_ipc(&self, service_name: &str, tls_data: &[u8]) -> IpcHandlerResult {
        // Parse the IPC command from TLS data.
        let cmd = svc_ipc::parse_ipc_command(tls_data).ok();

        let cmd_id = cmd.as_ref().map(|c| c.command_id).unwrap_or(0);

        // Dispatch to the service.
        let response = cmd.as_ref().and_then(|c| {
            self.manager.write().handle_request(service_name, c.command_id, c)
        });
        let response = response.unwrap_or_else(IpcResponse::success);

        // Build the 0x100-byte response (without handles — those are handled separately).
        let response_bytes = svc_ipc::build_ipc_response_full(
            response.result,
            &response.data,
            &[], // Handles are passed out-of-band via IpcHandlerResult.
            &[],
        );

        // Detect sm:GetService (service_name=="sm:", cmd_id==1) and extract
        // the target service name so the bridge can create a session.
        let create_session_for = if service_name == "sm:" && cmd_id == 1 {
            // Read the resolved name from SmService.
            let mgr = self.manager.read();
            if let Some(sm_lock) = mgr.get_service("sm:") {
                let sm_guard = sm_lock.read();
                // Downcast to SmService to read last_get_service_name.
                // Since we can't easily downcast through Box<dyn ServiceHandler>,
                // we extract the name from the raw_data directly.
                drop(sm_guard);
                drop(mgr);

                // Parse service name from the command's raw_data.
                cmd.as_ref().and_then(|c| {
                    if c.raw_data.len() >= 2 {
                        let name = ruzu_service::sm::read_service_name(&c.raw_data);
                        if !name.is_empty() {
                            Some(name)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        } else {
            None
        };

        // Detect sub-interface creation: services that return move handles.
        // When a service returns a move handle of 0 (placeholder), the bridge
        // creates a session to the appropriate sub-interface.
        let create_sub_session = if !response.handles_to_move.is_empty()
            && response.handles_to_move.iter().any(|&h| h == 0)
        {
            get_sub_interface(service_name, cmd_id).map(|s| s.to_string())
        } else {
            None
        };

        // The session to create: prefer create_session_for (sm:GetService),
        // then create_sub_session (sub-interface), then None.
        let final_session = create_session_for.or(create_sub_session);

        IpcHandlerResult {
            response_bytes,
            create_session_for: final_session,
            copy_handles: response.handles_to_copy.clone(),
            move_handles: response
                .handles_to_move
                .iter()
                .filter(|&&h| h != 0) // Filter out placeholder handles.
                .copied()
                .collect(),
        }
    }
}

// ── Sub-interface routing table ──────────────────────────────────────────────

/// Map (parent_service, cmd_id) → child sub-interface service name.
///
/// Returns `None` if the command does not create a sub-interface.
fn get_sub_interface(service_name: &str, cmd_id: u32) -> Option<&'static str> {
    match (service_name, cmd_id) {
        // AM chain
        ("appletOE", 0) => Some("IApplicationProxy"),
        ("IApplicationProxy", 0) => Some("am:ICommonStateGetter"),
        ("IApplicationProxy", 1) => Some("am:ISelfController"),
        ("IApplicationProxy", 2) => Some("am:IWindowController"),
        ("IApplicationProxy", 3) => Some("am:IAudioController"),
        ("IApplicationProxy", 4) => Some("am:IDisplayController"),
        ("IApplicationProxy", 11) => Some("am:ILibraryAppletCreator"),
        ("IApplicationProxy", 20) => Some("am:IApplicationFunctions"),
        // VI chain
        ("vi:m", 2) => Some("vi:IApplicationDisplayService"),
        ("vi:IApplicationDisplayService", 100) => Some("vi:IHOSBinderDriver"),
        ("vi:IApplicationDisplayService", 101..=103) => {
            Some("vi:IApplicationDisplayService")
        }
        // Audio
        ("audren:u", 0) => Some("audren:IAudioRenderer"),
        ("audren:u", 2) => Some("audren:IAudioDevice"),
        ("audout:u", 1) => Some("audout:IAudioOut"),
        // Filesystem
        ("fsp-srv", 18) => Some("fsp:IFileSystem"),
        ("fsp-srv", 200) => Some("fsp:IStorage"),
        // Account
        ("acc:u0", 5) => Some("acc:IProfile"),
        ("acc:u0", 101) => Some("acc:IBaas"),
        // Time
        ("time:u", 0 | 1 | 4) => Some("time:ISystemClock"),
        ("time:u", 2) => Some("time:ISteadyClock"),
        ("time:u", 3) => Some("time:ITimeZoneService"),
        // Network / parental
        ("pctl:s", 0) => Some("pctl:IParentalControlService"),
        ("nifm:u", 4) => Some("nifm:IGeneralService"),
        ("friend:u", 0) => Some("friend:IFriendService"),
        // Logger
        ("lm", 0) => Some("ILogger"),
        _ => None,
    }
}

// ── Main ────────────────────────────────────────────────────────────────────

fn main() -> Result<()> {
    let args = Args::parse();

    // Initialize logging
    let log_level = if args.verbose { "debug" } else { "info" };
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or(log_level))
        .format_timestamp_millis()
        .init();

    info!("ruzu - Nintendo Switch Emulator");
    info!("================================");

    // Load config
    let settings = config::load_config(args.config.as_ref());
    info!("Renderer backend: {:?}", settings.renderer_backend);

    // Initialize key manager
    let mut keys = KeyManager::new();
    if let Some(keys_dir) = config::find_keys_dir(args.keys_dir.as_ref()) {
        info!("Loading keys from: {}", keys_dir.display());
        if let Err(e) = keys.load_from_directory(&keys_dir) {
            log::warn!("Failed to load keys: {}", e);
        }
    } else {
        info!("No keys directory found (NCA decryption will not be available)");
    }

    // Determine game path: either direct or via game library
    let game_path = match args.game {
        Some(path) => path,
        None => {
            // Show game library UI
            let games_dir = config::find_games_dir(args.games_dir.as_ref());
            match games_dir {
                Some(dir) => {
                    info!("Scanning games directory: {}", dir.display());
                    let games = game_info::scan_directory(&dir, &mut keys);
                    info!("Found {} games", games.len());

                    match game_list_ui::show_game_list(&games)? {
                        Some(idx) => games[idx].path.clone(),
                        None => {
                            info!("No game selected, exiting");
                            return Ok(());
                        }
                    }
                }
                None => {
                    anyhow::bail!(
                        "No game specified and no games directory found.\n\
                         Use --game <path> to load a game directly, or\n\
                         --games-dir <path> to specify a games directory."
                    );
                }
            }
        }
    };

    // Detect format and load
    let format = GameFormat::from_extension(&game_path)
        .with_context(|| format!("Unsupported file format: {}", game_path.display()))?;

    info!(
        "Loading game: {} (format: {})",
        game_path.display(),
        format.label()
    );

    let (code_set, title_id, rom_data) = match format {
        GameFormat::Nro => {
            let raw = std::fs::read(&game_path).context("Failed to read NRO file")?;
            let (cs, tid) = load_nro_game_from_data(&raw)?;
            (cs, tid, Some(raw))
        }
        GameFormat::Nsp => {
            let (cs, tid) = load_nsp_game(&game_path, &mut keys)?;
            (cs, tid, None)
        }
        GameFormat::Xci => {
            let (cs, tid) = load_xci_game(&game_path, &mut keys)?;
            (cs, tid, None)
        }
    };

    info!(
        "Code loaded: text=0x{:X}+0x{:X}, rodata=0x{:X}+0x{:X}, data=0x{:X}+0x{:X}, bss=0x{:X}",
        code_set.segments[0].offset,
        code_set.segments[0].size,
        code_set.segments[1].offset,
        code_set.segments[1].size,
        code_set.segments[2].offset,
        code_set.segments[2].size,
        code_set.bss_size,
    );

    // Create kernel and load code
    let mut kernel = KernelCore::new();
    kernel.create_process("main")?;

    let text_seg = &code_set.segments[0];
    let rodata_seg = &code_set.segments[1];
    let data_seg = &code_set.segments[2];

    let text_data = &code_set.memory
        [text_seg.offset as usize..(text_seg.offset + text_seg.size) as usize];
    let rodata_data = &code_set.memory
        [rodata_seg.offset as usize..(rodata_seg.offset + rodata_seg.size) as usize];
    let data_data =
        &code_set.memory[data_seg.offset as usize..(data_seg.offset + data_seg.size) as usize];

    let entry_point = kernel.load_nro(
        text_data,
        rodata_data,
        data_data,
        code_set.bss_size as usize,
        text_seg.offset,
        rodata_seg.offset,
        data_seg.offset,
    )?;

    info!("Code loaded at base 0x{:X}", entry_point);

    // Create main thread
    let thread_handle = kernel.create_main_thread(entry_point)?;
    info!("Main thread created (handle={})", thread_handle);

    // Set title ID
    if let Some(process) = kernel.process_mut() {
        process.title_id = if title_id != 0 {
            title_id
        } else {
            settings.title_id
        };
    }

    // ── Extract romfs data from NRO asset header ──────────────────────────
    let romfs_data: Option<Arc<Vec<u8>>> = code_set.asset_header.as_ref().and_then(|asset| {
        if asset.romfs.size == 0 {
            info!("No romfs section in NRO asset header");
            return None;
        }
        // romfs offset is relative to the asset header start (= NRO file_size).
        // We need the raw NRO data to extract romfs bytes.
        let raw = rom_data.as_ref()?;
        // Find the NRO file_size from the header (at offset 0x18).
        let nro_file_size = if raw.len() >= 0x1C {
            u32::from_le_bytes([raw[0x18], raw[0x19], raw[0x1A], raw[0x1B]]) as usize
        } else {
            return None;
        };
        let romfs_start = nro_file_size + asset.romfs.offset as usize;
        let romfs_end = romfs_start + asset.romfs.size as usize;
        if romfs_end <= raw.len() {
            let data = raw[romfs_start..romfs_end].to_vec();
            info!("Extracted romfs: {} bytes", data.len());
            Some(Arc::new(data))
        } else {
            log::warn!(
                "romfs extends past file end: offset=0x{:X}, size=0x{:X}, file_len=0x{:X}",
                romfs_start, asset.romfs.size, raw.len()
            );
            None
        }
    });

    // ── Create shared state and wire services ────────────────────────────

    let hid_shared_mem = Arc::new(RwLock::new(HidSharedMemory::new()));
    let buffer_queue = Arc::new(RwLock::new(BufferQueue::new()));

    // Create HID shared memory kernel object.
    let hid_shm_handle = {
        let process = kernel.process_mut().unwrap();
        let shm = KSharedMemory::new(ruzu_service::hid_shared_memory::HID_SHARED_MEMORY_SIZE);
        process
            .handle_table
            .add(KernelObject::SharedMemory(shm))
            .expect("Failed to create HID shared memory handle")
    };

    // Create and register services.
    let mut manager = ServiceManager::new();
    manager.register_service("sm:", Box::new(ruzu_service::sm::SmService::new()));
    manager.register_service(
        "hid",
        Box::new(ruzu_service::hid::HidService::new_with_shm_handle(hid_shm_handle)),
    );
    manager.register_service("nvdrv", Box::new(ruzu_service::nvdrv::NvdrvService::new()));
    manager.register_service("nvdrv:a", Box::new(ruzu_service::nvdrv::NvdrvService::new()));
    manager.register_service("nvdrv:s", Box::new(ruzu_service::nvdrv::NvdrvService::new()));
    manager.register_service("nvdrv:t", Box::new(ruzu_service::nvdrv::NvdrvService::new()));
    manager.register_service("vi:m", Box::new(ruzu_service::vi::ViManagerService::new()));
    manager.register_service(
        "vi:IApplicationDisplayService",
        Box::new(ruzu_service::vi::ViDisplayService::new(buffer_queue.clone())),
    );
    manager.register_service(
        "vi:IHOSBinderDriver",
        Box::new(ruzu_service::vi::ViBinderService::new(buffer_queue.clone())),
    );
    // AM service chain
    manager.register_service("appletOE", Box::new(ruzu_service::am::AmService::new()));
    manager.register_service(
        "IApplicationProxy",
        Box::new(ruzu_service::am::ApplicationProxyService::new()),
    );
    manager.register_service(
        "am:ICommonStateGetter",
        Box::new(ruzu_service::am::CommonStateGetterService::new()),
    );
    manager.register_service(
        "am:ISelfController",
        Box::new(ruzu_service::am::SelfControllerService::new()),
    );
    manager.register_service(
        "am:IWindowController",
        Box::new(ruzu_service::am::WindowControllerService::new()),
    );
    manager.register_service(
        "am:IApplicationFunctions",
        Box::new(ruzu_service::am::ApplicationFunctionsService::new()),
    );
    manager.register_service(
        "am:IAudioController",
        Box::new(ruzu_service::am::AudioControllerService::new()),
    );
    manager.register_service(
        "am:IDisplayController",
        Box::new(ruzu_service::am::DisplayControllerService::new()),
    );
    manager.register_service(
        "am:ILibraryAppletCreator",
        Box::new(ruzu_service::am::LibraryAppletCreatorService::new()),
    );

    // Filesystem
    manager.register_service(
        "fsp-srv",
        Box::new(ruzu_service::fs::FspSrvService::new_with_romfs(romfs_data.clone())),
    );
    if let Some(ref romfs) = romfs_data {
        manager.register_service(
            "fsp:IStorage",
            Box::new(ruzu_service::fs::StorageService::new(romfs.clone())),
        );
    } else {
        manager.register_service(
            "fsp:IStorage",
            Box::new(ruzu_service::fs::StorageService::new_empty()),
        );
    }
    manager.register_service(
        "fsp:IFileSystem",
        Box::new(ruzu_service::fs::FileSystemService::new()),
    );

    // Audio
    manager.register_service("audren:u", Box::new(ruzu_service::audio::AudRendererService::new()));
    manager.register_service(
        "audren:IAudioRenderer",
        Box::new(ruzu_service::audio::AudioRendererService::new()),
    );
    manager.register_service(
        "audren:IAudioDevice",
        Box::new(ruzu_service::audio::AudioDeviceService::new()),
    );
    manager.register_service("audout:u", Box::new(ruzu_service::audio::AudOutService::new()));
    manager.register_service(
        "audout:IAudioOut",
        Box::new(ruzu_service::audio::AudioOutService::new()),
    );

    // Account
    manager.register_service("acc:u0", Box::new(ruzu_service::account::AccountService::new()));
    manager.register_service(
        "acc:IProfile",
        Box::new(ruzu_service::account::ProfileService::new()),
    );
    manager.register_service("acc:IBaas", Box::new(ruzu_service::account::BaasService::new()));

    // Time
    manager.register_service("time:u", Box::new(ruzu_service::time::TimeService::new()));
    manager.register_service(
        "time:ISystemClock",
        Box::new(ruzu_service::time::SystemClockService::new()),
    );
    manager.register_service(
        "time:ISteadyClock",
        Box::new(ruzu_service::time::SteadyClockService::new()),
    );
    manager.register_service(
        "time:ITimeZoneService",
        Box::new(ruzu_service::time::TimeZoneService::new()),
    );

    // Network / misc
    manager.register_service("pctl:s", Box::new(ruzu_service::pctl::PctlService::new()));
    manager.register_service(
        "pctl:IParentalControlService",
        Box::new(ruzu_service::pctl::ParentalControlService::new()),
    );
    manager.register_service("nifm:u", Box::new(ruzu_service::nifm::NifmService::new()));
    manager.register_service(
        "nifm:IGeneralService",
        Box::new(ruzu_service::nifm::GeneralNetworkService::new()),
    );
    manager.register_service("ssl", Box::new(ruzu_service::ssl::SslService::new()));
    manager.register_service("nsd:u", Box::new(ruzu_service::nsd::NsdService::new()));
    manager.register_service("friend:u", Box::new(ruzu_service::friends::FriendsService::new()));
    manager.register_service(
        "friend:IFriendService",
        Box::new(ruzu_service::friends::FriendInterfaceService::new()),
    );

    // Other
    manager.register_service("set:sys", Box::new(ruzu_service::set::SetSysService::new()));
    manager.register_service("lm", Box::new(ruzu_service::lm::LmService::new()));
    manager.register_service("ILogger", Box::new(ruzu_service::lm::LoggerService::new()));

    // Wire IPC bridge.
    let manager_arc = Arc::new(RwLock::new(manager));
    let bridge = Arc::new(ServiceBridge::new(manager_arc));
    kernel.ipc_handler = Some(bridge);

    // Track where the game maps HID shared memory (set by MapSharedMemory SVC).
    let hid_guest_addr: Arc<AtomicU64> = Arc::new(AtomicU64::new(0));

    if args.headless {
        info!("Running in headless mode");
        run_headless(&mut kernel)?;
    } else {
        run_with_window(
            &mut kernel,
            hid_shared_mem,
            buffer_queue,
            hid_shm_handle,
            hid_guest_addr,
        )?;
    }

    info!("Emulation finished");
    Ok(())
}

/// Load an NRO game from raw bytes (already read from disk).
fn load_nro_game_from_data(rom_data: &[u8]) -> Result<(nro::CodeSet, u64)> {
    let code_set = nro::load_nro(rom_data).context("Failed to parse NRO file")?;

    if code_set.is_homebrew {
        info!("Detected homebrew NRO");
    }

    Ok((code_set, 0))
}

/// Load an NSP game file.
fn load_nsp_game(path: &PathBuf, keys: &mut KeyManager) -> Result<(nro::CodeSet, u64)> {
    let file: Arc<dyn ruzu_loader::vfs::VfsFile> =
        Arc::new(RealFile::open(path).context("Failed to open NSP file")?);

    let result = nsp::load_nsp(file, keys).context("Failed to load NSP")?;

    if let Some(ref title) = result.title {
        info!("Title: {}", title);
    }
    if let Some(ref developer) = result.developer {
        info!("Developer: {}", developer);
    }
    if let Some(ref version) = result.version {
        info!("Version: {}", version);
    }
    info!("Title ID: 0x{:016X}", result.title_id);

    Ok((result.code_set, result.title_id))
}

/// Load an XCI game file.
fn load_xci_game(path: &PathBuf, keys: &mut KeyManager) -> Result<(nro::CodeSet, u64)> {
    let file: Arc<dyn ruzu_loader::vfs::VfsFile> =
        Arc::new(RealFile::open(path).context("Failed to open XCI file")?);

    let result = xci::load_xci(file, keys)
        .map_err(|e| anyhow::anyhow!("XCI load error: {}", e))?;

    if let Some(ref title) = result.title {
        info!("Title: {}", title);
    }
    if let Some(ref developer) = result.developer {
        info!("Developer: {}", developer);
    }
    info!("Title ID: 0x{:016X}", result.title_id);

    Ok((result.code_set, result.title_id))
}

/// Run in headless mode (no window, CPU only).
fn run_headless(kernel: &mut KernelCore) -> Result<()> {
    info!("Starting CPU execution (headless)...");

    run_cpu_loop(kernel)
}

/// Instructions per time slice for each thread.
const INSTRUCTIONS_PER_SLICE: u64 = 50_000;

/// Core CPU execution loop with multi-thread scheduling.
fn run_cpu_loop(kernel: &mut KernelCore) -> Result<()> {
    use ruzu_cpu::interpreter::Interpreter;
    use ruzu_cpu::state::{CpuExecutor, HaltReason};
    use ruzu_kernel::scheduler::Scheduler;
    use ruzu_kernel::svc::dispatch_svc;
    use ruzu_kernel::thread::ThreadState;

    let mut interp = Interpreter::new();

    loop {
        if kernel.should_stop {
            break;
        }

        // Check timeouts on waiting threads.
        {
            let tick = kernel.tick_counter;
            if let Some(process) = kernel.process_mut() {
                Scheduler::check_timeouts(&mut process.threads, tick);
            }
        }

        // Schedule next thread (priority-aware round-robin).
        let idx = {
            let process = match kernel.process() {
                Some(p) => p,
                None => break,
            };
            let thread_states: Vec<_> = process
                .threads
                .iter()
                .map(|t| (t.handle, t.state, t.priority))
                .collect();
            kernel.scheduler.schedule_next(&thread_states)
        };

        let idx = match idx {
            Some(i) => i,
            None => {
                // No runnable threads — check if any are still waiting.
                let has_waiting = kernel
                    .process()
                    .map(|p| {
                        p.threads
                            .iter()
                            .any(|t| t.state == ThreadState::Waiting)
                    })
                    .unwrap_or(false);

                if has_waiting {
                    // Advance time and retry.
                    kernel.tick_counter += INSTRUCTIONS_PER_SLICE;
                    continue;
                }
                info!("No runnable threads, stopping");
                break;
            }
        };

        // Store current thread index for SVC dispatch.
        kernel.current_thread_idx = Some(idx);

        // Run interpreter with budget.
        interp.set_budget(INSTRUCTIONS_PER_SLICE);
        let process = kernel.process_mut().unwrap();
        let reason = interp.run(
            &mut process.threads[idx].cpu_state,
            &mut process.memory,
        );
        kernel.tick_counter += INSTRUCTIONS_PER_SLICE;

        match reason {
            HaltReason::Svc(n) => {
                // Clone CPU state for SVC dispatch, then write back.
                let mut cpu = kernel.process().unwrap().threads[idx].cpu_state.clone();
                dispatch_svc(kernel, &mut cpu, n);
                if let Some(process) = kernel.process_mut() {
                    // Only write back if thread is still runnable (not blocked by the SVC).
                    if process.threads[idx].state != ThreadState::Waiting {
                        process.threads[idx].cpu_state = cpu;
                    }
                }
            }
            HaltReason::BudgetExhausted => {
                // Time slice done, loop to reschedule.
            }
            HaltReason::ExternalHalt => {
                info!("CPU externally halted");
                break;
            }
            HaltReason::DataAbort { addr } => {
                let pc = kernel
                    .process()
                    .map(|p| p.threads[idx].cpu_state.pc)
                    .unwrap_or(0);
                log::error!(
                    "Data abort at 0x{:016X} (PC=0x{:016X})",
                    addr, pc
                );
                break;
            }
            HaltReason::InstructionAbort { addr } => {
                log::error!("Instruction abort at 0x{:016X}", addr);
                break;
            }
            HaltReason::Breakpoint => {
                info!("Breakpoint hit");
                break;
            }
            HaltReason::Step => {
                // Single step complete, continue.
            }
        }
    }

    kernel.current_thread_idx = None;
    Ok(())
}

/// Run with SDL2 window, input polling, and framebuffer presentation.
fn run_with_window(
    kernel: &mut KernelCore,
    hid_shared_mem: Arc<RwLock<HidSharedMemory>>,
    buffer_queue: Arc<RwLock<BufferQueue>>,
    hid_shm_handle: u32,
    hid_guest_addr: Arc<AtomicU64>,
) -> Result<()> {
    use ruzu_cpu::interpreter::Interpreter;
    use ruzu_cpu::state::{CpuExecutor, HaltReason};
    use ruzu_kernel::scheduler::Scheduler;
    use ruzu_kernel::svc::dispatch_svc;
    use ruzu_kernel::thread::ThreadState;
    use window::{EmulatorWindow, DEFAULT_HEIGHT, DEFAULT_WIDTH};

    let mut emu_window =
        EmulatorWindow::new("ruzu - Nintendo Switch Emulator", DEFAULT_WIDTH, DEFAULT_HEIGHT)?;

    info!("Window created, entering main loop");
    info!("Press ESC or close window to exit");
    info!("Controls: Z=A, X=B, C=X, V=Y, Arrows=D-Pad, Enter=+, RShift=-, Q/E=L/R, A/D=ZL/ZR, IJKL=LStick");

    let mut interp = Interpreter::new();

    // Number of time slices to run per frame (~60 FPS ~ 16ms).
    const SLICES_PER_FRAME: usize = 10;

    loop {
        // ── Poll SDL2 events and read input ─────────────────────────────
        let input = match emu_window.poll_events_with_input() {
            Some(input) => input,
            None => {
                info!("Window close requested");
                break;
            }
        };

        if kernel.should_stop {
            break;
        }

        // ── Update HID shared memory ────────────────────────────────────
        {
            let mut hid = hid_shared_mem.write();
            hid.update_input(input.buttons, input.l_stick, input.r_stick);

            // Sync HID data to guest memory if the game has mapped it.
            let guest_addr = hid_guest_addr.load(Ordering::Relaxed);
            if guest_addr != 0 {
                if let Some(process) = kernel.process_mut() {
                    let _ = process.memory.write_bytes(guest_addr, &hid.data);
                }
            }
        }

        // ── Try to detect HID mapping address ──────────────────────────
        // Check if the game has mapped our HID shared memory handle.
        // We look for a SharedMemory mapping by checking if the pool offset
        // matches the HID handle. This is a simplified detection approach.
        if hid_guest_addr.load(Ordering::Relaxed) == 0 {
            if let Some(process) = kernel.process() {
                if let Ok(KernelObject::SharedMemory(shm)) =
                    process.handle_table.get(hid_shm_handle)
                {
                    if shm.backing_offset.is_some() {
                        // Look for the mapping address by scanning memory regions.
                        // For now, use a heuristic: HID is typically mapped at a
                        // specific address in homebrew.
                        // A more robust approach would track MapSharedMemory calls.
                    }
                }
            }
        }

        // ── Run CPU slices ──────────────────────────────────────────────
        for _ in 0..SLICES_PER_FRAME {
            if kernel.should_stop {
                break;
            }

            // Check timeouts on waiting threads.
            {
                let tick = kernel.tick_counter;
                if let Some(process) = kernel.process_mut() {
                    Scheduler::check_timeouts(&mut process.threads, tick);
                    for thread in process.threads.iter_mut() {
                        if thread.state == ThreadState::Runnable && thread.wait_result != 0 {
                            thread.cpu_state.x[0] = thread.wait_result as u64;
                            thread.cpu_state.x[1] = u64::MAX;
                        }
                    }
                }
            }

            // Schedule next thread.
            let idx = {
                let process = match kernel.process() {
                    Some(p) => p,
                    None => break,
                };
                let thread_states: Vec<_> = process
                    .threads
                    .iter()
                    .map(|t| (t.handle, t.state, t.priority))
                    .collect();
                kernel.scheduler.schedule_next(&thread_states)
            };

            let idx = match idx {
                Some(i) => i,
                None => {
                    let has_waiting = kernel
                        .process()
                        .map(|p| {
                            p.threads
                                .iter()
                                .any(|t| t.state == ThreadState::Waiting)
                        })
                        .unwrap_or(false);

                    if has_waiting {
                        kernel.tick_counter += INSTRUCTIONS_PER_SLICE;
                        continue;
                    }
                    break;
                }
            };

            kernel.current_thread_idx = Some(idx);

            interp.set_budget(INSTRUCTIONS_PER_SLICE);
            let process = kernel.process_mut().unwrap();
            let reason = interp.run(
                &mut process.threads[idx].cpu_state,
                &mut process.memory,
            );
            kernel.tick_counter += INSTRUCTIONS_PER_SLICE;

            match reason {
                HaltReason::Svc(n) => {
                    let mut cpu =
                        kernel.process().unwrap().threads[idx].cpu_state.clone();
                    dispatch_svc(kernel, &mut cpu, n);

                    // Track MapSharedMemory calls to detect HID mapping.
                    if n == 0x13 {
                        // SVC 0x13 = MapSharedMemory
                        let map_handle = cpu.x[0] as u32;
                        let map_addr = cpu.x[1];
                        if map_handle == hid_shm_handle && map_addr != 0 {
                            log::info!(
                                "HID shared memory mapped at guest addr 0x{:X}",
                                map_addr
                            );
                            hid_guest_addr.store(map_addr, Ordering::Relaxed);
                        }
                    }

                    if let Some(process) = kernel.process_mut() {
                        if process.threads[idx].state != ThreadState::Waiting {
                            process.threads[idx].cpu_state = cpu;
                        }
                    }
                }
                HaltReason::BudgetExhausted => {
                    // Time slice done, continue to next slice.
                }
                HaltReason::ExternalHalt => {
                    kernel.should_stop = true;
                    break;
                }
                HaltReason::DataAbort { addr } => {
                    log::error!("Data abort at 0x{:016X}", addr);
                    kernel.should_stop = true;
                    break;
                }
                HaltReason::InstructionAbort { addr } => {
                    log::error!("Instruction abort at 0x{:016X}", addr);
                    kernel.should_stop = true;
                    break;
                }
                _ => {}
            }
        }

        kernel.current_thread_idx = None;

        // ── Present framebuffer ─────────────────────────────────────────
        {
            let mut bq = buffer_queue.write();
            if let Some((slot, buffer)) = bq.acquire() {
                let width = buffer.width;
                let height = buffer.height;
                let offset = buffer.offset;
                let size = (width * height * 4) as usize;

                if offset != 0 && size > 0 {
                    // Read pixel data from guest memory.
                    let mut pixels = vec![0u8; size];
                    if let Some(process) = kernel.process() {
                        for (i, byte) in pixels.iter_mut().enumerate() {
                            *byte = process
                                .memory
                                .read_u8(offset + i as u64)
                                .unwrap_or(0);
                        }
                    }
                    emu_window.present_framebuffer(&pixels, width, height);
                }

                bq.release(slot);
            }
        }
    }

    Ok(())
}

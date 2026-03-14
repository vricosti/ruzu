// SPDX-FileCopyrightText: 2014 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! yuzu-cmd: SDL2 command-line emulator frontend.
//!
//! Port of `yuzu_cmd/yuzu.cpp`.
//!
//! This binary is the SDL2-based command-line frontend for the yuzu Nintendo
//! Switch emulator. It parses command-line arguments, loads an SDL2
//! configuration, picks the correct renderer backend (OpenGL, Vulkan, or
//! Null), initialises the emulator core, and runs the main event loop.
//!
//! # Argument mapping
//!
//! | C++ long option      | Rust (`clap`) field      |
//! |----------------------|--------------------------|
//! | `-c` / `--config`    | `config`                 |
//! | `-f` / `--fullscreen`| `fullscreen`             |
//! | `-g` / `--game`      | `game`                   |
//! | `-m` / `--multiplayer`| `multiplayer`           |
//! | `-p` / `--program`   | `program`                |
//! | `-u` / `--user`      | `user`                   |
//! | `-v` / `--version`   | handled by clap          |

use clap::Parser;
use rdynarmic::frontend::a32::decoder::decode_arm;

pub mod emu_window;
pub mod sdl_config;

use emu_window::{
    emu_window_sdl2_gl::EmuWindowSdl2Gl, emu_window_sdl2_null::EmuWindowSdl2Null,
    emu_window_sdl2_vk::EmuWindowSdl2Vk,
};
use sdl_config::SdlConfig;

// ---------------------------------------------------------------------------
// CLI argument definitions
// ---------------------------------------------------------------------------

/// yuzu-cmd — SDL2 command-line Nintendo Switch emulator frontend.
///
/// Maps to the `getopt_long` option table in C++ `main()`.
#[derive(Parser, Debug)]
#[command(name = "yuzu-cmd")]
#[command(version)]
#[command(about = "yuzu Nintendo Switch emulator (SDL2 CLI frontend)")]
struct Args {
    /// Load the specified configuration file.
    ///
    /// Maps to C++ `-c` / `--config`.
    #[arg(short = 'c', long = "config", value_name = "PATH")]
    config: Option<String>,

    /// Start in fullscreen mode.
    ///
    /// Maps to C++ `-f` / `--fullscreen`.
    #[arg(short = 'f', long = "fullscreen")]
    fullscreen: bool,

    /// File path of the game to load.
    ///
    /// Maps to C++ `-g` / `--game`.
    #[arg(short = 'g', long = "game", value_name = "PATH")]
    game: Option<String>,

    /// Multiplayer connection string: `nick[:password]@address[:port]`.
    ///
    /// Maps to C++ `-m` / `--multiplayer`.
    #[arg(
        short = 'm',
        long = "multiplayer",
        value_name = "nick:password@address:port"
    )]
    multiplayer: Option<String>,

    /// Arguments to pass to the guest executable.
    ///
    /// Maps to C++ `-p` / `--program`.
    #[arg(short = 'p', long = "program", value_name = "ARGS")]
    program: Option<String>,

    /// Select a specific user profile (0–7).
    ///
    /// Maps to C++ `-u` / `--user`.
    #[arg(short = 'u', long = "user", value_name = "INDEX")]
    user: Option<u8>,

    /// Renderer backend: opengl, vulkan, or null.
    ///
    /// Not in upstream CLI — added for convenience while Settings is not fully ported.
    #[arg(short = 'r', long = "renderer", value_name = "BACKEND", default_value = "opengl")]
    renderer: String,

    /// Positional game path (alternative to --game).
    ///
    /// Maps to C++ bare positional argument handling in the `getopt_long` loop.
    #[arg(value_name = "FILENAME")]
    filename: Option<String>,
}

// ---------------------------------------------------------------------------
// Network callback stubs
//
// Maps to the static helpers `OnStateChanged`, `OnNetworkError`,
// `OnMessageReceived`, `OnStatusMessageReceived` in `yuzu.cpp`.
// ---------------------------------------------------------------------------

/// Called when the room-member connection state changes.
///
/// Maps to C++ `static void OnStateChanged(const Network::RoomMember::State&)`.
fn on_state_changed(state: u32) {
    // Upstream: switch (state) { Idle, Joining, Joined, Moderator }
    match state {
        0 => log::debug!("Network: Network is idle"),
        1 => log::debug!("Network: Connection sequence to room started"),
        2 => log::debug!("Network: Successfully joined to the room"),
        3 => log::debug!("Network: Successfully joined the room as a moderator"),
        _ => {}
    }
}

/// Called when a network error occurs. Logs the error and may exit.
///
/// Maps to C++ `static void OnNetworkError(const Network::RoomMember::Error&)`.
fn on_network_error(error: u32) {
    // Upstream: switch (error) — fatal errors call exit(1)
    match error {
        0 => log::debug!("Network: Lost connection to the room"),
        1 => {
            log::error!("Network: Error: Could not connect");
            std::process::exit(1);
        }
        2 => {
            log::error!(
                "Network: You tried to use the same nickname as another user \
                 that is connected to the Room"
            );
            std::process::exit(1);
        }
        3 => {
            log::error!(
                "Network: You tried to use the same fake IP-Address as another user \
                 that is connected to the Room"
            );
            std::process::exit(1);
        }
        4 => {
            log::error!("Network: Room replied with: Wrong password");
            std::process::exit(1);
        }
        5 => {
            log::error!(
                "Network: You are using a different version than the room \
                 you are trying to connect to"
            );
            std::process::exit(1);
        }
        6 => {
            log::error!("Network: The room is full");
            std::process::exit(1);
        }
        7 => log::error!("Network: You have been kicked by the host"),
        8 => log::error!("Network: You have been banned by the host"),
        9 => log::error!("Network: UnknownError"),
        10 => log::error!("Network: PermissionDenied"),
        11 => log::error!("Network: NoSuchUser"),
        _ => log::error!("Network: Unknown error code {}", error),
    }
}

/// Called when a chat message is received from another room member.
///
/// Maps to C++ `static void OnMessageReceived(const Network::ChatEntry&)`.
fn on_message_received(nickname: &str, message: &str) {
    println!("\n{}: {}\n", nickname, message);
}

/// Called when a status message (join/leave/kick/ban) is received.
///
/// Maps to C++ `static void OnStatusMessageReceived(const Network::StatusMessageEntry&)`.
fn on_status_message_received(msg_type: u32, nickname: &str) {
    // Upstream: Network::IdMemberJoin=1, IdMemberLeave=2, IdMemberKicked=3,
    //           IdMemberBanned=4, IdAddressUnbanned=5
    let message = match msg_type {
        1 => Some(format!("{} has joined", nickname)),
        2 => Some(format!("{} has left", nickname)),
        3 => Some(format!("{} has been kicked", nickname)),
        4 => Some(format!("{} has been banned", nickname)),
        5 => Some(format!("{} has been unbanned", nickname)),
        _ => None,
    };
    if let Some(msg) = message {
        println!("\n* {}\n", msg);
    }
}

fn restore_thread_to_jit(
    jit: &mut dyn ruzu_core::arm::arm_interface::ArmInterface,
    ctx: &mut ruzu_core::arm::arm_interface::ThreadContext,
    thread: &std::sync::Arc<std::sync::Mutex<ruzu_core::hle::kernel::k_thread::KThread>>,
) {
    let thread = thread.lock().unwrap();
    thread.restore_guest_context(ctx);
    jit.set_context(ctx);
    jit.set_tpidrro_el0(thread.get_tls_address().get());
}

struct GuestRuntimeBridge {
    active_thread: std::sync::Arc<std::sync::Mutex<ruzu_core::hle::kernel::k_thread::KThread>>,
}

impl GuestRuntimeBridge {
    fn new(
        main_thread: std::sync::Arc<std::sync::Mutex<ruzu_core::hle::kernel::k_thread::KThread>>,
    ) -> Self {
        Self {
            active_thread: main_thread,
        }
    }

    fn initialize_jit(
        &self,
        jit: &mut dyn ruzu_core::arm::arm_interface::ArmInterface,
        ctx: &mut ruzu_core::arm::arm_interface::ThreadContext,
    ) {
        restore_thread_to_jit(jit, ctx, &self.active_thread);
    }

    fn handoff_after_svc(
        &mut self,
        jit: &mut dyn ruzu_core::arm::arm_interface::ArmInterface,
        ctx: &mut ruzu_core::arm::arm_interface::ThreadContext,
        scheduler: &std::sync::Arc<std::sync::Mutex<ruzu_core::hle::kernel::k_scheduler::KScheduler>>,
        process: &std::sync::Arc<std::sync::Mutex<ruzu_core::hle::kernel::k_process::KProcess>>,
    ) {
        jit.get_context(ctx);
        self.active_thread.lock().unwrap().capture_guest_context(ctx);

        let current_thread_id = self.active_thread.lock().unwrap().get_thread_id();
        let next_thread = scheduler
            .lock()
            .unwrap()
            .wait_for_next_thread(process, current_thread_id);
        let Some(next_thread) = next_thread else {
            return;
        };

        let switch_needed =
            next_thread.lock().unwrap().get_thread_id() != self.active_thread.lock().unwrap().get_thread_id();
        if !switch_needed {
            return;
        }

        restore_thread_to_jit(jit, ctx, &next_thread);
        self.active_thread = next_thread;
    }
}


// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

/// Application entry point.
///
/// Maps to C++ `int main(int argc, char** argv)` in `yuzu.cpp`.
fn main() {
    // Initialise logging backend (upstream: Common::Log::Initialize /
    // SetColorConsoleBackendEnabled / Start).
    env_logger::init();

    // Parse CLI arguments (upstream: getopt_long loop).
    let args = Args::parse();

    // Resolve game path from --game flag or positional argument.
    // Maps to C++ `filepath` variable.
    let filepath = args.game.or(args.filename).unwrap_or_default();

    if filepath.is_empty() {
        log::error!("Failed to load ROM: No ROM specified");
        std::process::exit(-1);
    }

    // Load SDL configuration.
    // Maps to C++ `SdlConfig config{config_path}`.
    let _config = SdlConfig::new(args.config);

    // Apply optional per-run overrides.
    // Maps to C++ `Settings::values.program_args = program_args` and
    // `Settings::values.current_user = clamp(*selected_user, 0, 7)`.
    //
    // Settings::values not yet ported — log and continue.
    if let Some(ref program_args) = args.program {
        // Upstream: Settings::values.program_args = program_args
        log::warn!(
            "program_args '{}' provided but Settings::values not yet ported; ignoring",
            program_args
        );
    }
    if let Some(user_index) = args.user {
        // Upstream: Settings::values.current_user = std::clamp(*selected_user, 0, 7)
        let clamped = user_index.min(7);
        log::warn!(
            "user_index {} (clamped to {}) provided but Settings::values not yet ported; ignoring",
            user_index,
            clamped
        );
    }

    // Initialise core system.
    // Maps to C++ `Core::System system{}; system.Initialize(); system.ApplySettings()`.
    let mut system = ruzu_core::core::System::new();
    system.initialize();

    // Determine renderer backend.
    // Maps to C++ switch on `Settings::values.renderer_backend.GetValue()`.
    // Once Settings is fully ported, this should read from config instead of CLI.
    let renderer_backend = match args.renderer.to_lowercase().as_str() {
        "opengl" | "gl" | "0" => "opengl",
        "vulkan" | "vk" | "1" => "vulkan",
        "null" | "2" => "null",
        other => {
            log::warn!("Unknown renderer '{}', defaulting to opengl", other);
            "opengl"
        }
    };
    log::info!("Renderer backend: {}", renderer_backend);

    // Multiplayer callbacks are wired here when Network::RoomMember is ported.
    // For now, log if multiplayer was requested.
    if let Some(ref mp) = args.multiplayer {
        log::warn!(
            "Multiplayer '{}' requested but Network::RoomMember not yet ported; ignoring",
            mp
        );
    }

    // Load the game ROM.
    // Maps to C++ `system.Load(...)`.
    let load_result = system.load(&filepath);
    if load_result != ruzu_core::core::SystemResultStatus::Success {
        log::error!(
            "Failed to load ROM '{}': {:?}",
            filepath,
            load_result,
        );
        std::process::exit(-1);
    }

    // Start emulation.
    // Maps to C++ `system.Run()`.
    system.run();

    // -----------------------------------------------------------------------
    // Create ARM JIT and run with SVC dispatch loop.
    // -----------------------------------------------------------------------
    // In the full implementation, KProcess::Run() creates the main thread,
    // which creates the ArmInterface and runs on PhysicalCore. For now, we
    // still run the JIT directly, but we keep process/thread ownership in the
    // kernel objects so SVCs operate on real KProcess/KThread state.
    if let Some(process) = system.current_process_mut() {
        let mut process = std::mem::take(process);
        let shared_memory = process.get_shared_memory();
        let is_64bit = process.is_64bit();

        let (code_base, code_size) = {
            let mem = shared_memory.read().unwrap();
            (mem.base, mem.data.len())
        };

        log::info!(
            "Process memory: base={:#x}, size={:#x} ({:.1} MiB), {}",
            code_base, code_size, code_size as f64 / (1024.0 * 1024.0),
            if is_64bit { "AArch64" } else { "AArch32" }
        );

        // Pre-execution: verify module data by reading .dynstr for sdk module.
        // sdk is the main symbol exporter — if its dynstr is valid, decompression worked.
        {
            let mem = shared_memory.read().unwrap();
            // Helper to read module .dynamic and dump first symbols.
            let dump_module = |name: &str, base: u64| {
                let mod0_ptr = mem.read_32(base + 4) as u64;
                let mod0_addr = base + mod0_ptr;
                let magic = mem.read_32(mod0_addr);
                if magic != 0x30444F4D {
                    log::warn!("{} @ {:#x}: bad MOD0 magic {:#x}", name, base, magic);
                    return;
                }
                let dyn_off = mem.read_32(mod0_addr + 4) as i32;
                let dyn_addr = (mod0_addr as i64 + dyn_off as i64) as u64;
                let mut strtab = 0u64;
                let mut symtab = 0u64;
                let mut strsz = 0u64;
                let mut dt_hash = 0u64;
                let mut dt_gnu_hash = 0u64;
                for i in 0..80u64 {
                    let tag = mem.read_32(dyn_addr + i * 8);
                    let val = mem.read_32(dyn_addr + i * 8 + 4);
                    match tag {
                        4 => dt_hash = val as u64,
                        5 => strtab = val as u64,
                        6 => symtab = val as u64,
                        10 => strsz = val as u64,
                        _ => {
                            // DT_GNU_HASH = 0x6ffffef5
                            if tag == 0x6ffffef5u32 { dt_gnu_hash = val as u64; }
                        }
                    }
                    if tag == 0 { break; }
                }
                // DT_STRTAB/DT_SYMTAB are un-relocated — add base to read correct addresses.
                let strtab_addr = base + strtab;
                let symtab_addr = base + symtab;
                log::info!("{} @ {:#x}: DT_STRTAB={:#x}(abs={:#x}) DT_SYMTAB={:#x}(abs={:#x}) STRSZ={:#x} DT_HASH={:#x} DT_GNU_HASH={:#x}",
                    name, base, strtab, strtab_addr, symtab, symtab_addr, strsz, dt_hash, dt_gnu_hash);
                // Dump first 5 symbols.
                for si in 0..5u64 {
                    let sa = symtab_addr + si * 16;
                    if !mem.is_valid_range(sa, 16) { break; }
                    let st_name = mem.read_32(sa);
                    let st_value = mem.read_32(sa + 4);
                    let st_info = mem.read_8(sa + 12);
                    let na = strtab_addr + st_name as u64;
                    let mut sn = String::new();
                    if mem.is_valid_range(na, 1) {
                        for j in 0..80u64 {
                            let c = mem.read_8(na + j);
                            if c == 0 { break; }
                            sn.push(c as char);
                        }
                    }
                    log::info!("  sym[{}]: st_name={:#x} val={:#x} info={:#x} '{}'", si, st_name, st_value, st_info, sn);
                }
                // Search for __nnDetailInitLibc0 in this module's strtab.
                let target = b"__nnDetailInitLibc0";
                let target_len = target.len() as u64;
                if strsz > target_len && mem.is_valid_range(strtab_addr, strsz as usize) {
                    let mut sym_found = false;
                    for offset in 0..(strsz - target_len) {
                        let mut m = true;
                        for (k, &b) in target.iter().enumerate() {
                            if mem.read_8(strtab_addr + offset + k as u64) != b {
                                m = false;
                                break;
                            }
                        }
                        if m {
                            // Check it's a real string boundary (preceded by NUL or at offset 0)
                            let preceded_by_nul = offset == 0 ||
                                mem.read_8(strtab_addr + offset - 1) == 0;
                            if preceded_by_nul {
                                log::info!("  FOUND '__nnDetailInitLibc0' at strtab+{:#x} (abs={:#x})",
                                    offset, strtab_addr + offset);
                                // Now find which symbol entry references this strtab offset
                                // by scanning the symtab for st_name == offset
                                let sym_entry_size = 16u64; // ELF32 Sym size
                                let n_syms = (strtab_addr - symtab_addr) / sym_entry_size;
                                for si in 0..n_syms {
                                    let sa = symtab_addr + si * sym_entry_size;
                                    if !mem.is_valid_range(sa, sym_entry_size as usize) { break; }
                                    let st_name_val = mem.read_32(sa);
                                    if st_name_val as u64 == offset {
                                        let st_value = mem.read_32(sa + 4);
                                        let st_size = mem.read_32(sa + 8);
                                        let st_info = mem.read_8(sa + 12);
                                        let st_other = mem.read_8(sa + 13);
                                        let st_shndx = mem.read_16(sa + 14);
                                        let bind = st_info >> 4;
                                        let stype = st_info & 0xf;
                                        log::info!("    sym[{}]: val={:#x} sz={:#x} bind={} type={} other={} shndx={}",
                                            si, st_value, st_size, bind, stype, st_other, st_shndx);
                                    }
                                }
                                sym_found = true;
                                break;
                            }
                        }
                    }
                    if !sym_found {
                        log::info!("  '__nnDetailInitLibc0' NOT in strtab");
                    }
                }
            };
            dump_module("rtld", code_base);
            dump_module("main", 0x206000);
            dump_module("subsdk0", 0x1512000);
            dump_module("subsdk1", 0x16AB000);
            dump_module("subsdk2", 0x16D3000);
            dump_module("subsdk3", 0x16E6000);
            dump_module("subsdk4", 0x1723000);
            dump_module("sdk", 0x1C9C000);

            // Dump first 32 bytes of rtld's data segment (GOT is typically here).
            // After self-relocation, GOT entries should contain base-adjusted addresses.
            log::info!("rtld data segment (0x205000) PRE-execution:");
            for i in 0..8u64 {
                let addr = 0x205000 + i * 4;
                let val = mem.read_32(addr);
                log::info!("  [{:#010x}] = {:#010x}", addr, val);
            }

            if mem.is_valid_range(code_base, 0x6000) {
                let _ = std::fs::write("/tmp/rtld.bin", mem.read_block(code_base, 0x6000));
            }
        }

        // Allocate TLS (Thread Local Storage) region.
        // Upstream: KThread::Initialize() calls owner->CreateThreadLocalRegion()
        // which allocates a page and zeros it. The CP15 TPIDRURO register is set
        // to this address before running the thread (physical_core.cpp:158).
        // rtld reads TPIDRURO to find thread-local data for symbol resolution.
        // ThreadLocalRegionSize = 0x200 upstream, but a full page is allocated.
        let tls_page_size: u64 = 0x1000;
        // Leave a small gap after modules (upstream shows FREE gap before TLS).
        let tls_base = {
            let modules_end = code_base + code_size as u64;
            // Align to page boundary (should already be aligned).
            let gap = 0x4000u64; // Upstream shows ~0x4000 gap before TLS
            let base = modules_end + gap;
            // Page-align
            (base + 0xFFF) & !0xFFF
        };
        {
            let mut mem = shared_memory.write().unwrap();
            let tls_end = tls_base + tls_page_size;
            let new_total = (tls_end - mem.base) as usize;
            if new_total > mem.data.len() {
                mem.data.resize(new_total, 0);
            }
            // Zero the TLS region (upstream: Memory::ZeroBlock(m_tls_address, ThreadLocalRegionSize))
            let tls_offset = (tls_base - mem.base) as usize;
            for b in &mut mem.data[tls_offset..tls_offset + tls_page_size as usize] {
                *b = 0;
            }
            // Register TLS in the block manager for QueryMemory.
            use ruzu_core::hle::kernel::k_memory_block::{KMemoryPermission, KMemoryState};
            mem.update_region(
                tls_base,
                tls_page_size,
                KMemoryState::THREAD_LOCAL,
                KMemoryPermission::USER_READ_WRITE,
            );
        }
        log::info!("TLS: base={:#x}, size={:#x}", tls_base, tls_page_size);

        // Allocate stack memory (1 MiB) after the TLS region.
        // Upstream: KProcess::Run() allocates stack via page table.
        let stack_size: u64 = 1024 * 1024;
        // Leave a gap after TLS (upstream shows FREE gap between TLS and stack).
        let stack_base = tls_base + tls_page_size + 0x4000; // ~0x4000 gap like upstream
        let stack_top = stack_base + stack_size;
        {
            let mut mem = shared_memory.write().unwrap();
            let new_total = (stack_top - mem.base) as usize;
            if new_total > mem.data.len() {
                mem.data.resize(new_total, 0);
            }
            // Register stack in the block manager for QueryMemory.
            use ruzu_core::hle::kernel::k_memory_block::{KMemoryPermission, KMemoryState};
            mem.update_region(
                stack_base,
                stack_size,
                KMemoryState::STACK,
                KMemoryPermission::USER_READ_WRITE,
            );
        }
        log::info!("Stack: base={:#x}, top={:#x} ({} KiB)", stack_base, stack_top, stack_size / 1024);

        // Log tracked memory regions for debugging.
        {
            let mem = shared_memory.read().unwrap();
            let mut count = 0;
            for block in mem.block_manager.iter() {
                use ruzu_core::hle::kernel::k_memory_block::KMemoryState;
                if block.get_state() != KMemoryState::FREE {
                    let perm = block.get_permission();
                    log::info!("  [{:#010x}..{:#010x}) size={:#x} state={:?} perm={:?}",
                        block.get_address(),
                        block.get_end_address(),
                        block.get_size(),
                        block.get_state(),
                        perm);
                    count += 1;
                }
            }
            log::info!("Memory regions: {} non-free blocks tracked", count);
        }

        // The loader path does not yet call KProcess::LoadFromMetadata(), so bootstrap the
        // process-owned runtime pieces that upstream would normally initialize there.
        process.initialize_handle_table();
        process.initialize_thread_local_region_base(tls_base + tls_page_size);

        let process = std::sync::Arc::new(std::sync::Mutex::new(process));
        let next_thread_id = std::sync::Arc::new(std::sync::atomic::AtomicU64::new(2));
        let next_object_id = std::sync::Arc::new(std::sync::atomic::AtomicU32::new(2));
        let scheduler = std::sync::Arc::new(std::sync::Mutex::new(
            ruzu_core::hle::kernel::k_scheduler::KScheduler::new(0),
        ));
        process.lock().unwrap().attach_scheduler(&scheduler);
        let main_thread = std::sync::Arc::new(std::sync::Mutex::new(
            ruzu_core::hle::kernel::k_thread::KThread::new(),
        ));
        {
            let mut thread = main_thread.lock().unwrap();
            thread.initialize_main_thread(
                code_base,
                stack_top,
                0,
                tls_base,
                &process,
                1,
                1,
                is_64bit,
            );
        }
        {
            let mut process_guard = process.lock().unwrap();
            process_guard.register_thread_object(main_thread.clone());
            let _ = process_guard.handle_table.add(1);
        }
        scheduler.lock().unwrap().initialize(1, 0, 0);

        use ruzu_core::arm::arm_interface::{
            ArmInterface, HaltReason, KProcess as OpaqueKProcess, KThread as OpaqueKThread,
        };
        use ruzu_core::hle::kernel::svc_dispatch::{self, SvcContext};

        let dummy_system: u32 = 0;
        let dummy_exclusive: u32 = 0;
        let dummy_process = unsafe {
            &*(&dummy_system as *const u32 as *const OpaqueKProcess)
        };

        // Run the CPU with SVC dispatch loop.
        // Supports both AArch32 and AArch64 via trait object.
        let mut jit: Box<dyn ArmInterface> = if is_64bit {
            use ruzu_core::arm::dynarmic::arm_dynarmic_64::ArmDynarmic64;
            Box::new(ArmDynarmic64::new(
                &dummy_system as &dyn std::any::Any,
                true, dummy_process,
                &dummy_exclusive as &dyn std::any::Any,
                0, shared_memory.clone(),
            ))
        } else {
            use ruzu_core::arm::dynarmic::arm_dynarmic_32::ArmDynarmic32;
            Box::new(ArmDynarmic32::new(
                &dummy_system as &dyn std::any::Any,
                true, dummy_process,
                &dummy_exclusive as &dyn std::any::Any,
                0, shared_memory.clone(),
            ))
        };

        // Arc identity check: verify JIT and host share the same memory instance
        log::info!("Arc identity: shared_memory ptr = {:?}", std::sync::Arc::as_ptr(&shared_memory));

        // Create SVC context for the kernel dispatch module.
        let svc_ctx = SvcContext {
            shared_memory: shared_memory.clone(),
            code_base,
            code_size: code_size as u64,
            stack_base,
            stack_size,
            program_id: 0x0100152000022000, // MK8D title ID
            tls_base,
            current_process: process.clone(),
            scheduler: scheduler.clone(),
            next_thread_id: next_thread_id.clone(),
            next_object_id: next_object_id.clone(),
            is_64bit,
        };

        // Set initial context.
        // Maps to upstream ResetThreadContext32/64 in k_thread.cpp via KThread::Initialize.
        let mut ctx = ruzu_core::arm::arm_interface::ThreadContext::default();
        let mut guest_runtime = GuestRuntimeBridge::new(main_thread.clone());
        guest_runtime.initialize_jit(&mut *jit, &mut ctx);

        // Upstream: physical_core.cpp:158 — SetTpidrroEl0(GetInteger(thread->GetTlsAddress()))
        log::info!("TLS: set TPIDRURO/TPIDRRO_EL0 = {:#x}", tls_base);

        log::info!(
            "CPU: starting {} JIT, PC={:#x}, SP={:#x}",
            if is_64bit { "AArch64" } else { "AArch32" },
            code_base, stack_top
        );

        // SVC dispatch loop.
        // Maps to upstream KProcess::Run() → PhysicalCore event loop.
        let dummy_thread = unsafe { &mut *(&mut 0u32 as *mut u32 as *mut OpaqueKThread) };

        let mut svc_count = 0u32;
        let mut iteration = 0u32;
        let mut query_memory_count = 0u32;
        let mut dumped_module_objects = false;
        loop {
            let halt_reason = jit.run_thread(dummy_thread);
            iteration += 1;

            if halt_reason.contains(HaltReason::SUPERVISOR_CALL) {
                let svc_num = jit.get_svc_number();
                svc_count += 1;

                let mut svc_args = [0u64; 8];
                jit.get_svc_arguments(&mut svc_args);

                let svc_name = svc_dispatch::SvcId::from_u32(svc_num)
                    .map(|id| format!("{:?}", id))
                    .unwrap_or_else(|| format!("Unknown(0x{:02X})", svc_num));

                jit.get_context(&mut ctx);
                log::info!(
                    "SVC #{}: {} (0x{:02X}) args=[{:#x}, {:#x}, {:#x}, {:#x}] PC={:#x} LR={:#x}",
                    svc_count, svc_name, svc_num,
                    svc_args[0], svc_args[1], svc_args[2], svc_args[3],
                    ctx.pc, ctx.lr
                );

                if svc_num == 0x06 {
                    query_memory_count += 1;
                } else if query_memory_count >= 25 && !dumped_module_objects {
                    dumped_module_objects = true;
                    let mem = shared_memory.read().unwrap();
                    let dump_module_object = |name: &str, base: u64| {
                        if !mem.is_valid_range(base + 4, 4) {
                            log::info!("{} @ {:#x}: module header unavailable", name, base);
                            return;
                        }

                        let mod_offset = mem.read_32(base + 4) as u64;
                        let mod_addr = base + mod_offset;
                        if !mem.is_valid_range(mod_addr, 0x1c) {
                            log::info!(
                                "{} @ {:#x}: MOD0 out of range (offset={:#x})",
                                name,
                                base,
                                mod_offset
                            );
                            return;
                        }

                        let magic = mem.read_32(mod_addr);
                        let dynamic_offset = mem.read_32(mod_addr + 4) as u64;
                        let bss_start_offset = mem.read_32(mod_addr + 8) as u64;
                        let bss_end_offset = mem.read_32(mod_addr + 12) as u64;
                        let module_offset = mem.read_32(mod_addr + 24) as u64;
                        let module_object = base + module_offset;

                        log::info!(
                            "{} @ {:#x}: MOD0={:#x} mod={:#x} dyn={:#x} bss=[{:#x}..{:#x}) module_obj={:#x}",
                            name,
                            base,
                            magic,
                            mod_addr,
                            mod_addr + dynamic_offset,
                            base + bss_start_offset,
                            base + bss_end_offset,
                            module_object
                        );

                        if !mem.is_valid_range(module_object, 0x40) {
                            log::info!(
                                "  {} module object @ {:#x} is out of range",
                                name,
                                module_object
                            );
                            return;
                        }

                        for i in 0..16u64 {
                            let addr = module_object + i * 4;
                            let val = mem.read_32(addr);
                            log::info!("  [{} + {:#04x}] = {:#010x}", name, i * 4, val);
                        }
                    };

                    log::info!(
                        "=== RTLD MODULE OBJECT DUMP after {} QueryMemory calls, first non-QM SVC=0x{:02X} ===",
                        query_memory_count,
                        svc_num
                    );
                    dump_module_object("rtld", 0x200000);
                    dump_module_object("main", 0x206000);
                    dump_module_object("subsdk0", 0x1512000);
                    dump_module_object("subsdk1", 0x16AB000);
                    dump_module_object("subsdk2", 0x16D3000);
                    dump_module_object("subsdk3", 0x16E6000);
                    dump_module_object("subsdk4", 0x1723000);
                    dump_module_object("sdk", 0x1C9C000);
                    log::info!("=== RTLD LIST REGION DUMP 0x2051d0..0x205240 ===");
                    for addr in (0x2051d0u64..0x205240u64).step_by(4) {
                        log::info!("  [{:#010x}] = {:#010x}", addr, mem.read_32(addr));
                    }
                }

                if svc_num == 0x27 {
                    // OutputDebugString — read the guest string
                    let mem = shared_memory.read().unwrap();
                    let str_addr = svc_args[0];
                    let str_len = svc_args[1] as usize;
                    if mem.is_valid_range(str_addr, str_len.max(1)) {
                        let mut s = String::new();
                        for j in 0..str_len {
                            let c = mem.read_8(str_addr + j as u64);
                            if c == 0 { break; }
                            s.push(c as char);
                        }
                        log::info!("  [guest] {}", s);
                    }
                }

                svc_dispatch::call(svc_num, is_64bit, &mut svc_args, &svc_ctx);
                jit.set_svc_arguments(&svc_args);

                guest_runtime.handoff_after_svc(
                    &mut *jit,
                    &mut ctx,
                    &scheduler,
                    &process,
                );

            } else if halt_reason.contains(HaltReason::PREFETCH_ABORT) {
                jit.get_context(&mut ctx);
                log::error!("PREFETCH_ABORT at PC={:#x}, SP={:#x}, LR={:#x}", ctx.pc, ctx.sp, ctx.lr);
                let mem = shared_memory.read().unwrap();
                let crash_start = ctx.pc.saturating_sub(0x10);
                for addr in (crash_start..=ctx.pc.saturating_add(0x10)).step_by(4) {
                    if !mem.is_valid_range(addr, 4) {
                        log::error!("  [{:#010x}] <unmapped>", addr);
                        continue;
                    }
                    let insn = mem.read_32(addr);
                    let decoded = decode_arm(insn);
                    let marker = if addr == ctx.pc { " <PC>" } else { "" };
                    log::error!(
                        "  [{:#010x}] {:#010x} {:?}{}",
                        addr,
                        insn,
                        decoded.id,
                        marker
                    );
                }
                break;
            } else if halt_reason.contains(HaltReason::BREAK_LOOP) {
                log::info!("BREAK_LOOP after {} iterations, {} SVCs", iteration, svc_count);
                break;
            } else {
                jit.get_context(&mut ctx);
                log::warn!(
                    "JIT halted: {:?} at PC={:#x}, SP={:#x} (iter={}, svcs={})",
                    halt_reason, ctx.pc, ctx.sp, iteration, svc_count
                );
                break;
            }
        }

        // Final context dump.
        jit.get_context(&mut ctx);
        log::info!("Final: PC={:#x}, SP={:#x}, R0={:#x}, iterations={}, SVCs={}",
            ctx.pc, ctx.sp, ctx.r[0], iteration, svc_count);
    } else {
        log::warn!("No process loaded, skipping CPU execution");
    }

    // Create the renderer window and run the event loop.
    // Maps to C++: switch on renderer_backend, then emu_window->IsOpen() / WaitEvent loop.
    match renderer_backend {
        "opengl" => {
            let mut emu_window = EmuWindowSdl2Gl::new(args.fullscreen);

            // Initialize the RendererOpenGL for frame presentation.
            // In the full implementation, this is done inside GPU::BindRenderer().
            let syncpoints = std::sync::Arc::new(
                video_core::syncpoint::SyncpointManager::new(),
            );
            let renderer = video_core::renderer_opengl::RendererOpenGL::new(
                |s| {
                    let cs = std::ffi::CString::new(s).unwrap();
                    unsafe {
                        sdl2::sys::SDL_GL_GetProcAddress(cs.as_ptr()) as *const _
                    }
                },
                syncpoints,
            );
            let mut renderer = match renderer {
                Ok(r) => r,
                Err(e) => {
                    log::error!("Failed to initialize OpenGL renderer: {}", e);
                    std::process::exit(1);
                }
            };

            // Generate a test framebuffer: 1280x720 gradient pattern.
            let fb_width = 1280u32;
            let fb_height = 720u32;
            let mut test_fb = vec![0u8; (fb_width * fb_height * 4) as usize];
            for y in 0..fb_height {
                for x in 0..fb_width {
                    let idx = ((y * fb_width + x) * 4) as usize;
                    test_fb[idx] = (x * 255 / fb_width) as u8;      // R
                    test_fb[idx + 1] = (y * 255 / fb_height) as u8;  // G
                    test_fb[idx + 2] = 128;                           // B
                    test_fb[idx + 3] = 255;                           // A
                }
            }

            log::info!("Entering OpenGL render loop");
            while emu_window.is_open() {
                emu_window.poll_events();

                let (vp_w, vp_h) = emu_window.get_drawable_size();
                renderer.composite(&test_fb, fb_width, fb_height, vp_w as u32, vp_h as u32);
                emu_window.swap_buffers();

                // Limit to ~60 FPS to avoid burning CPU.
                std::thread::sleep(std::time::Duration::from_millis(16));
            }
        }
        "vulkan" => {
            let mut emu_window = EmuWindowSdl2Vk::new(args.fullscreen);
            while emu_window.is_open() {
                emu_window.wait_event();
            }
        }
        _ => {
            let mut emu_window = EmuWindowSdl2Null::new(args.fullscreen);
            while emu_window.is_open() {
                emu_window.wait_event();
            }
        }
    }
}

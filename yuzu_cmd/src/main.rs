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

// ---------------------------------------------------------------------------
// Minimal SVC handler for rtld bootstrap
// ---------------------------------------------------------------------------

/// Handle a supervisor call during early game startup.
///
/// This is a temporary shim that provides just enough SVC responses for
/// the rtld dynamic linker to proceed. In the full implementation, SVCs
/// are dispatched through the kernel's svc_dispatch module.
///
/// Return convention: R0 = result code (0 = success).
fn handle_svc(
    svc_num: u32,
    args: &mut [u64; 8],
    is_64bit: bool,
    code_base: u64,
    code_size: u64,
    stack_base: u64,
    stack_size: u64,
    shared_memory: &ruzu_core::hle::kernel::k_process::SharedProcessMemory,
) {
    use ruzu_core::hle::kernel::svc_dispatch::SvcId;

    match SvcId::from_u32(svc_num) {
        Some(SvcId::SetHeapSize) => {
            // SVC 0x01: SetHeapSize(size) -> (result, address)
            // Upstream allocates heap via page table.
            // For now, return a fixed heap address after the stack.
            let heap_size = args[1];
            let heap_base = stack_base + stack_size;
            log::info!("  SetHeapSize({:#x}) -> heap at {:#x}", heap_size, heap_base);
            args[0] = 0; // Success
            args[1] = heap_base;
        }

        Some(SvcId::SetMemoryAttribute) => {
            // SVC 0x03: SetMemoryAttribute(addr, size, mask, attr) -> result
            // No-op for now, return success.
            args[0] = 0;
        }

        Some(SvcId::SetMemoryPermission) => {
            // SVC 0x02: SetMemoryPermission(addr, size, perm) -> result
            args[0] = 0;
        }

        Some(SvcId::MapMemory) => {
            // SVC 0x04: MapMemory(dst, src, size) -> result
            args[0] = 0;
        }

        Some(SvcId::UnmapMemory) => {
            // SVC 0x05: UnmapMemory(dst, src, size) -> result
            args[0] = 0;
        }

        Some(SvcId::QueryMemory) => {
            // SVC 0x06: QueryMemory(mem_info_ptr, page_info_ptr, addr) -> result
            // For ARM32: R0=mem_info_ptr, R1=page_info_ptr, R2=addr
            // Upstream writes a MemoryInfo struct (40 bytes) to mem_info_ptr.
            // MemoryInfo layout: { base_addr: u64, size: u64, state: u32, attr: u32,
            //                      perm: u32, ipc_count: u32, device_count: u32, padding: u32 }
            let mem_info_ptr = args[0];
            let query_addr = args[2];
            log::info!("  QueryMemory(info_ptr={:#x}, addr={:#x})", mem_info_ptr, query_addr);

            // Determine which region the query address falls in.
            let heap_base = stack_base + stack_size;
            let (base, size, state, perm) = if query_addr >= code_base && query_addr < code_base + code_size {
                // Code region: RX
                (code_base, code_size, 6u32 /* CodeStatic */, 5u32 /* RX */)
            } else if query_addr >= stack_base && query_addr < stack_base + stack_size {
                // Stack region: RW
                (stack_base, stack_size, 5u32 /* Stack */, 3u32 /* RW */)
            } else if query_addr >= heap_base {
                // Heap region: RW
                (heap_base, 0x1000_0000u64, 4u32 /* Normal */, 3u32 /* RW */)
            } else {
                // Unmapped
                (0u64, query_addr, 0u32 /* Free */, 0u32 /* None */)
            };

            // Write MemoryInfo struct to guest memory.
            {
                let mut mem = shared_memory.write().unwrap();
                if mem.is_valid_range(mem_info_ptr, 40) {
                    mem.write_64(mem_info_ptr, base);           // base_address
                    mem.write_64(mem_info_ptr + 8, size);       // size
                    mem.write_32(mem_info_ptr + 16, state);     // state
                    mem.write_32(mem_info_ptr + 20, 0);         // attribute
                    mem.write_32(mem_info_ptr + 24, perm);      // permission
                    mem.write_32(mem_info_ptr + 28, 0);         // ipc_count
                    mem.write_32(mem_info_ptr + 32, 0);         // device_count
                    mem.write_32(mem_info_ptr + 36, 0);         // padding
                }
            }

            args[0] = 0; // Success
            // page_info is returned in R1 for 32-bit
            args[1] = 0;
        }

        Some(SvcId::ExitProcess) => {
            // SVC 0x07: ExitProcess
            log::info!("  ExitProcess called — stopping execution");
            args[0] = 0;
        }

        Some(SvcId::CreateThread) => {
            // SVC 0x08: CreateThread(entry, arg, stack_top, priority, core_id) -> (result, handle)
            log::warn!("  CreateThread: not implemented");
            args[0] = 0xF601; // ResultOutOfResource (stub)
        }

        Some(SvcId::SleepThread) => {
            // SVC 0x0B: SleepThread(nanoseconds)
            // No-op.
            args[0] = 0;
        }

        Some(SvcId::GetCurrentProcessorNumber) => {
            // SVC 0x10: -> core_id
            args[0] = 0; // Always core 0
        }

        Some(SvcId::CloseHandle) => {
            // SVC 0x16: CloseHandle(handle) -> result
            args[0] = 0;
        }

        Some(SvcId::GetSystemTick) => {
            // SVC 0x1E: -> tick
            // Return a monotonically increasing tick.
            static TICK: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
            let t = TICK.fetch_add(1000, std::sync::atomic::Ordering::Relaxed);
            args[0] = t;
            args[1] = t; // For 32-bit: R0=lo, R1=hi
        }

        Some(SvcId::ConnectToNamedPort) => {
            // SVC 0x1F: ConnectToNamedPort(name_ptr) -> (result, handle)
            log::warn!("  ConnectToNamedPort: returning dummy handle");
            args[0] = 0;
            args[1] = 0x1000; // Dummy handle
        }

        Some(SvcId::SendSyncRequest) => {
            // SVC 0x21: SendSyncRequest(handle) -> result
            log::warn!("  SendSyncRequest(handle={:#x}): stub", args[0]);
            args[0] = 0;
        }

        Some(SvcId::GetProcessId) => {
            // SVC 0x24: GetProcessId(handle) -> (result, pid)
            args[0] = 0;
            args[1] = 1; // PID = 1
        }

        Some(SvcId::GetThreadId) => {
            // SVC 0x25: GetThreadId(handle) -> (result, tid)
            args[0] = 0;
            args[1] = 1; // TID = 1
        }

        Some(SvcId::Break) => {
            // SVC 0x26: Break(reason, info1, info2)
            log::error!("  Break(reason={:#x}, info1={:#x}, info2={:#x})",
                args[0], args[1], args[2]);
            args[0] = 0;
        }

        Some(SvcId::OutputDebugString) => {
            // SVC 0x27: OutputDebugString(str_ptr, size) -> result
            let str_ptr = args[0];
            let str_size = args[1] as usize;
            let msg = {
                let mem = shared_memory.read().unwrap();
                let bytes = if mem.is_valid_range(str_ptr, str_size) {
                    mem.read_block(str_ptr, str_size).to_vec()
                } else {
                    vec![]
                };
                String::from_utf8_lossy(&bytes).to_string()
            };
            log::info!("  OutputDebugString: \"{}\"", msg);
            args[0] = 0;
        }

        Some(SvcId::GetInfo) => {
            // SVC 0x29: GetInfo(info_type, handle, sub_id) -> (result, value)
            // This is the most critical SVC for rtld.
            let info_type = args[1] as u32;
            let handle = args[2] as u32;
            let sub_id = args[3];

            let (result, value) = handle_get_info(
                info_type, handle, sub_id,
                code_base, code_size, stack_base, stack_size,
            );

            args[0] = result;
            args[1] = value;
        }

        Some(SvcId::MapPhysicalMemory) => {
            // SVC 0x2C: MapPhysicalMemory(addr, size) -> result
            args[0] = 0;
        }

        Some(SvcId::GetResourceLimitLimitValue) => {
            // SVC 0x30: GetResourceLimitLimitValue(handle, which) -> (result, value)
            args[0] = 0;
            args[1] = 0x1_0000_0000; // 4 GiB
        }

        Some(SvcId::GetResourceLimitCurrentValue) => {
            // SVC 0x31: GetResourceLimitCurrentValue(handle, which) -> (result, value)
            args[0] = 0;
            args[1] = 0;
        }

        Some(other) => {
            log::warn!("  Unhandled SVC: {:?} (0x{:02X})", other, svc_num);
            args[0] = 0; // Return success to keep going
        }

        None => {
            log::error!("  Unknown SVC 0x{:02X}", svc_num);
            args[0] = 0xF001; // Generic error
        }
    }
}

/// Handle SVC 0x29 (GetInfo).
///
/// Returns (result_code, value). This is the most important SVC for rtld
/// as it queries memory layout information needed for relocation.
fn handle_get_info(
    info_type: u32,
    _handle: u32,
    sub_id: u64,
    code_base: u64,
    code_size: u64,
    stack_base: u64,
    stack_size: u64,
) -> (u64, u64) {
    // InfoType values from svc_types.rs
    const ALIAS_REGION_ADDRESS: u32 = 2;
    const ALIAS_REGION_SIZE: u32 = 3;
    const HEAP_REGION_ADDRESS: u32 = 4;
    const HEAP_REGION_SIZE: u32 = 5;
    const TOTAL_MEMORY_SIZE: u32 = 6;
    const USED_MEMORY_SIZE: u32 = 7;
    const DEBUGGER_ATTACHED: u32 = 8;
    const ASLR_REGION_ADDRESS: u32 = 12;
    const ASLR_REGION_SIZE: u32 = 13;
    const STACK_REGION_ADDRESS: u32 = 14;
    const STACK_REGION_SIZE: u32 = 15;
    const SYSTEM_RESOURCE_SIZE_TOTAL: u32 = 16;
    const SYSTEM_RESOURCE_SIZE_USED: u32 = 17;
    const PROGRAM_ID: u32 = 18;
    const RANDOM_ENTROPY: u32 = 20;
    const USER_EXCEPTION_CONTEXT_ADDR: u32 = 21;
    const TOTAL_NON_SYSTEM_MEMORY_SIZE: u32 = 22;
    const USED_NON_SYSTEM_MEMORY_SIZE: u32 = 23;
    const IS_APPLICATION: u32 = 24;

    let heap_base = stack_base + stack_size;

    let value = match info_type {
        ALIAS_REGION_ADDRESS => {
            // For 32-bit: alias region at 1 GiB
            0x4000_0000u64
        }
        ALIAS_REGION_SIZE => {
            // 1 GiB
            0x4000_0000u64
        }
        HEAP_REGION_ADDRESS => heap_base,
        HEAP_REGION_SIZE => 0x1000_0000, // 256 MiB
        TOTAL_MEMORY_SIZE => 0x1000_0000, // 256 MiB
        USED_MEMORY_SIZE => code_size + stack_size,
        DEBUGGER_ATTACHED => 0, // Not attached
        ASLR_REGION_ADDRESS => code_base,
        ASLR_REGION_SIZE => 0x8000_0000, // 2 GiB for 32-bit
        STACK_REGION_ADDRESS => stack_base,
        STACK_REGION_SIZE => stack_size,
        SYSTEM_RESOURCE_SIZE_TOTAL => 0,
        SYSTEM_RESOURCE_SIZE_USED => 0,
        PROGRAM_ID => 0x0100152000022000, // MK8D title ID
        RANDOM_ENTROPY => {
            // Return random entropy for the given sub_id (0-3).
            // Upstream uses KProcess::GetRandomEntropy(sub_id).
            0xDEAD_BEEF_0000_0000u64 | sub_id
        }
        USER_EXCEPTION_CONTEXT_ADDR => 0, // Not yet set up
        TOTAL_NON_SYSTEM_MEMORY_SIZE => 0x1000_0000,
        USED_NON_SYSTEM_MEMORY_SIZE => code_size + stack_size,
        IS_APPLICATION => 1, // Yes, this is an application
        _ => {
            log::warn!("  GetInfo: unknown info_type={}, sub_id={}", info_type, sub_id);
            return (0, 0); // Return success with 0
        }
    };

    log::debug!("  GetInfo(type={}, sub_id={}) -> {:#x}", info_type, sub_id, value);
    (0, value) // Success
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
    // create the JIT directly and run with a minimal SVC handler.
    if let Some(process) = system.current_process() {
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

        // Dump first 32 words of rtld for debugging.
        {
            let mem = shared_memory.read().unwrap();
            log::info!("rtld header dump:");
            for i in 0..16 {
                let addr = code_base + i * 4;
                let word = mem.read_32(addr);
                log::info!("  [{:#010x}] = {:#010x}", addr, word);
            }
        }

        // Allocate stack memory (1 MiB) after the code region.
        // Upstream: KProcess::Run() allocates stack via page table.
        let stack_size: u64 = 1024 * 1024;
        let stack_base = code_base + code_size as u64;
        let stack_top = stack_base + stack_size;
        {
            let mut mem = shared_memory.write().unwrap();
            let new_total = (stack_top - mem.base) as usize;
            if new_total > mem.data.len() {
                mem.data.resize(new_total, 0);
            }
        }
        log::info!("Stack: base={:#x}, top={:#x} ({} KiB)", stack_base, stack_top, stack_size / 1024);

        use ruzu_core::arm::arm_interface::{
            ArmInterface, HaltReason, KProcess as OpaqueKProcess, KThread as OpaqueKThread,
        };
        use ruzu_core::hle::kernel::svc_dispatch;

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

        // Set initial context.
        // Maps to upstream ResetThreadContext32/64 in k_thread.cpp.
        let mut ctx = ruzu_core::arm::arm_interface::ThreadContext::default();
        ctx.pc = code_base;
        ctx.sp = stack_top;
        ctx.r[0] = 0; // Main thread argument = 0
        if !is_64bit {
            // Upstream ResetThreadContext32 sets r[13]=stack_top, r[15]=entry_point.
            // set_context reads R13 from ctx.r[13], not ctx.sp.
            ctx.r[13] = stack_top;
            ctx.r[15] = code_base;
            // Upstream zeros CPSR via ctx={}, so leave pstate=0.
            // The JIT handles mode setup internally.
        }
        jit.set_context(&ctx);

        log::info!(
            "CPU: starting {} JIT, PC={:#x}, SP={:#x}",
            if is_64bit { "AArch64" } else { "AArch32" },
            code_base, stack_top
        );

        // SVC dispatch loop.
        let dummy_thread = unsafe {
            &mut *(&mut 0u32 as *mut u32 as *mut OpaqueKThread)
        };

        // Step through the first N instructions to diagnose where the code goes.
        let step_count = 200;
        log::info!("Stepping through first {} instructions...", step_count);
        for i in 0..step_count {
            jit.get_context(&mut ctx);
            let pc = ctx.pc;

            // Read the instruction at PC
            let insn = {
                let mem = shared_memory.read().unwrap();
                if mem.is_valid_range(pc, 4) { mem.read_32(pc) } else { 0xDEADDEAD }
            };

            if i < 50 || pc == 0 || i % 50 == 0 {
                log::info!(
                    "  step {}: PC={:#010x} insn={:#010x} SP={:#x} R0={:#x} R1={:#x} LR={:#x}",
                    i, pc, insn, ctx.sp, ctx.r[0], ctx.r[1], ctx.lr
                );
            }

            if pc == 0 {
                log::error!("Reached PC=0 at step {}", i);
                break;
            }

            let halt_reason = jit.step_thread(dummy_thread);

            if halt_reason.contains(HaltReason::SUPERVISOR_CALL) {
                let svc_num = jit.get_svc_number();
                log::info!("  SVC at step {}: {:#x}", i, svc_num);
                // Handle SVC and continue
                let mut svc_args = [0u64; 8];
                jit.get_svc_arguments(&mut svc_args);
                handle_svc(svc_num, &mut svc_args, is_64bit, code_base, code_size as u64, stack_base, stack_size, &shared_memory);
                jit.set_svc_arguments(&svc_args);
            } else if halt_reason.contains(HaltReason::PREFETCH_ABORT) {
                jit.get_context(&mut ctx);
                log::error!("PREFETCH_ABORT at step {} PC={:#x}", i, ctx.pc);
                break;
            }
        }

        jit.get_context(&mut ctx);
        log::info!("After stepping: PC={:#x}, SP={:#x}, R0={:#x}", ctx.pc, ctx.sp, ctx.r[0]);

        let max_iterations = 10000;
        let mut svc_count = 0u32;
        let mut iteration = 0u32;

        loop {
            let halt_reason = jit.run_thread(dummy_thread);
            iteration += 1;

            if halt_reason.contains(HaltReason::SUPERVISOR_CALL) {
                let svc_num = jit.get_svc_number();
                svc_count += 1;

                // Get SVC arguments from registers.
                let mut svc_args = [0u64; 8];
                jit.get_svc_arguments(&mut svc_args);

                let svc_name = svc_dispatch::SvcId::from_u32(svc_num)
                    .map(|id| format!("{:?}", id))
                    .unwrap_or_else(|| format!("Unknown(0x{:02X})", svc_num));

                log::info!(
                    "SVC #{}: {} (0x{:02X}) args=[{:#x}, {:#x}, {:#x}, {:#x}]",
                    svc_count, svc_name, svc_num,
                    svc_args[0], svc_args[1], svc_args[2], svc_args[3]
                );

                // Minimal SVC handling for rtld bootstrap.
                // Return success (R0=0) for most SVCs so rtld can proceed.
                handle_svc(svc_num, &mut svc_args, is_64bit, code_base, code_size as u64, stack_base, stack_size, &shared_memory);

                // Write back SVC results.
                jit.set_svc_arguments(&svc_args);

                if svc_count > 500 {
                    log::warn!("SVC limit reached ({}), stopping", svc_count);
                    break;
                }
            } else if halt_reason.contains(HaltReason::PREFETCH_ABORT) {
                jit.get_context(&mut ctx);
                log::error!("PREFETCH_ABORT at PC={:#x}", ctx.pc);
                break;
            } else if halt_reason.contains(HaltReason::BREAK_LOOP) {
                log::info!("BREAK_LOOP after {} iterations, {} SVCs", iteration, svc_count);
                break;
            } else {
                jit.get_context(&mut ctx);
                log::info!(
                    "JIT halted: {:?} at PC={:#x}, SP={:#x} (iter={}, svcs={})",
                    halt_reason, ctx.pc, ctx.sp, iteration, svc_count
                );
                break;
            }

            if iteration >= max_iterations {
                log::warn!("Max iterations ({}) reached, stopping", max_iterations);
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

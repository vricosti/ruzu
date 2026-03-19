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
#[cfg(feature = "debug-logs")]
pub mod main_log;
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
            let code_base = process.page_table.get_code_region_start().get();
            let code_size = process.page_table.get_code_region_size();
            (code_base, code_size)
        };

        log::info!(
            "Process memory: base={:#x}, size={:#x} ({:.1} MiB), {}",
            code_base, code_size, code_size as f64 / (1024.0 * 1024.0),
            if is_64bit { "AArch64" } else { "AArch32" }
        );

        // Pre-execution: verify module data by reading .dynstr for sdk module.
        #[cfg(feature = "debug-logs")]
        main_log::dump_all_modules(&shared_memory, code_base);

        // TLS page allocation and PLR are now set up by the loader
        // (deconstructed_rom_directory.rs) calling load_from_metadata() ->
        // initialize_for_user() -> initialize(), matching upstream flow.
        let tls_page_size: u64 = 0x1000;

        // Log tracked memory regions for debugging.
        // Block manager now lives in the page table (KPageTableBase).
        {
            use ruzu_core::hle::kernel::k_memory_block::KMemoryState;
            let bm = process.page_table.get_base().get_memory_block_manager();
            let mut count = 0;
            for block in bm.iter() {
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

        let load_parameters = system
            .load_parameters()
            .cloned()
            .expect("loader must provide process launch parameters");

        let process = std::sync::Arc::new(std::sync::Mutex::new(process));
        process.lock().unwrap().bind_self_reference(&process);
        let scheduler = std::sync::Arc::new(std::sync::Mutex::new(
            ruzu_core::hle::kernel::k_scheduler::KScheduler::new(0),
        ));
        process.lock().unwrap().attach_scheduler(&scheduler);
        let (main_thread, _main_thread_handle, stack_base, stack_top) =
            process
                .lock()
                .unwrap()
                .run(
                    load_parameters.main_thread_priority,
                    load_parameters.main_thread_stack_size as usize,
                    1,
                    1,
                    is_64bit,
                )
                .expect("process runtime bootstrap must succeed");
        let stack_size = load_parameters.main_thread_stack_size as usize;
        let tls_base = main_thread.lock().unwrap().get_tls_address().get();
        log::info!("TLS: base={:#x}, size={:#x}", tls_base, tls_page_size);
        log::info!(
            "Stack: base={:#x}, top={:#x} ({} KiB)",
            stack_base,
            stack_top,
            stack_size / 1024
        );

        use ruzu_core::arm::arm_interface::{
            ArmInterface, HaltReason, KProcess as OpaqueKProcess, KThread as OpaqueKThread,
        };
        use ruzu_core::hle::kernel::physical_core::PhysicalCoreExecutionControl;
        use ruzu_core::hle::kernel::svc_dispatch;

        let dummy_system: u32 = 0;
        let dummy_exclusive: u32 = 0;
        let dummy_process = unsafe {
            &*(&dummy_system as *const u32 as *const OpaqueKProcess)
        };

        // Run the CPU with SVC dispatch loop.
        // Supports both AArch32 and AArch64 via trait object.
        let core_memory = system.memory_shared();
        let mut jit: Box<dyn ArmInterface> = if is_64bit {
            use ruzu_core::arm::dynarmic::arm_dynarmic_64::ArmDynarmic64;
            Box::new(ArmDynarmic64::new(
                &dummy_system as &dyn std::any::Any,
                true, dummy_process,
                &dummy_exclusive as &dyn std::any::Any,
                0, shared_memory.clone(),
                system.core_timing_shared(),
                core_memory.clone(),
            ))
        } else {
            use ruzu_core::arm::dynarmic::arm_dynarmic_32::ArmDynarmic32;
            Box::new(ArmDynarmic32::new(
                &dummy_system as &dyn std::any::Any,
                true, dummy_process, // TODO: use false for single-core scheduling once rdynarmic cycle counting perf is fixed
                &dummy_exclusive as &dyn std::any::Any,
                0, shared_memory.clone(), system.core_timing_shared(),
                core_memory,
            ))
        };

        // Arc identity check: verify JIT and host share the same memory instance
        log::info!("Arc identity: shared_memory ptr = {:?}", std::sync::Arc::as_ptr(&shared_memory));

        // Wire the runtime SVC state on System so that SVC handlers
        // can access process, scheduler, memory, etc. via &System.
        system.set_current_process_arc(process.clone());
        system.set_scheduler_arc(scheduler.clone());
        system.set_shared_process_memory(shared_memory.clone());
        system.set_runtime_program_id(0x0100152000022000); // MK8D title ID
        system.set_runtime_64bit(is_64bit);

        // Set initial context.
        // Maps to upstream ResetThreadContext32/64 in k_thread.cpp via KThread::Initialize.
        let mut ctx = ruzu_core::arm::arm_interface::ThreadContext::default();
        let physical_core: *const ruzu_core::hle::kernel::physical_core::PhysicalCore = system
            .kernel()
            .and_then(|kernel| kernel.physical_core(0))
            .map(|core| core as *const _)
            .expect("kernel physical core 0 must exist after System::initialize");
        unsafe { &*physical_core }.initialize_guest_runtime(main_thread.clone(), &mut *jit, &mut ctx);

        // Upstream: physical_core.cpp:158 — SetTpidrroEl0(GetInteger(thread->GetTlsAddress()))
        log::info!("TLS: set TPIDRURO/TPIDRRO_EL0 = {:#x}", tls_base);
        jit.set_tpidrro_el0(tls_base);

        log::info!(
            "CPU: starting {} JIT, PC={:#x}, SP={:#x}",
            if is_64bit { "AArch64" } else { "AArch32" },
            code_base, stack_top
        );

        // Dump __nnDetailInitLibc0 function code and TLS area.
        #[cfg(feature = "debug-logs")]
        main_log::dump_initlibc0_and_tls(&shared_memory, tls_base);

        // SVC dispatch loop.
        // Maps to upstream KProcess::Run() → PhysicalCore event loop.
        let dummy_thread = unsafe { &mut *(&mut 0u32 as *mut u32 as *mut OpaqueKThread) };

        let mut query_memory_count = 0u32;
        let mut dumped_module_objects = false;
        let mut step_halt_count = 0u64;
        let mut last_step_pc = 0u64;
        let mut repeated_step_pc_count = 0u64;
        let mut dumped_repeated_step_window = false;
        let repeated_step_window_threshold = std::env::var("RUZU_REPEAT_STEP_WINDOW")
            .ok()
            .and_then(|value| value.parse::<u64>().ok())
            .unwrap_or(10_000);
        let mut total_iteration = 0u32;
        let mut total_svc_count = 0u32;
        loop {
            let physical_core_ref = unsafe { &*physical_core };
            let (iteration, svc_count, exec_control) = physical_core_ref.run_loop(
                &mut *jit,
                dummy_thread,
                &mut ctx,
                &scheduler,
                &process,
                is_64bit,
                &system,
                |svc_num, svc_args, ctx, svc_count, _iteration| {
                let svc_name = svc_dispatch::SvcId::from_u32(svc_num)
                    .map(|id| format!("{:?}", id))
                    .unwrap_or_else(|| format!("Unknown(0x{:02X})", svc_num));
                log::info!(
                    "SVC #{}: {} (0x{:02X}) args=[{:#x}, {:#x}, {:#x}, {:#x}] PC={:#x} LR={:#x}",
                    svc_count, svc_name, svc_num,
                    svc_args[0], svc_args[1], svc_args[2], svc_args[3],
                    ctx.pc, ctx.lr
                );

                // Dump full register state for all SVCs to trace init sequence.
                // Show args + LR for every SVC to trace the call chain.
                if svc_count >= 85 {
                    log::warn!("  [SVC #{}] r0-r15: [{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
                        svc_count,
                        ctx.r[0], ctx.r[1], ctx.r[2], ctx.r[3],
                        ctx.r[4], ctx.r[5], ctx.r[6], ctx.r[7],
                        ctx.r[8], ctx.r[9], ctx.r[10], ctx.r[11],
                        ctx.r[12], ctx.sp, ctx.lr, ctx.pc);
                }

                // Abort context dumps
                #[cfg(feature = "debug-logs")]
                {
                    // Detect abort handler entry: SetThreadPriority(0x0D) with
                    // priority=44 (0x2c) is the abort handler thread setup.
                    if svc_num == 0x0D && svc_args[1] == 0x2c {
                        main_log::dump_abort_context(&shared_memory, ctx);
                    }
                    // GetThreadId(0x25) right after SetThreadPriority is the
                    // abort handler continuing.
                    if svc_num == 0x25 && ctx.r[4] > 0x200000 {
                        main_log::dump_abort_backtrace(&shared_memory, ctx);
                    }
                }

                // ArbitrateLock debug
                #[cfg(feature = "debug-logs")]
                {
                    static DUMPED_ARBITRATE: std::sync::atomic::AtomicBool =
                        std::sync::atomic::AtomicBool::new(false);
                    if svc_num == 0x1A
                        && !DUMPED_ARBITRATE.swap(true, std::sync::atomic::Ordering::Relaxed)
                    {
                        main_log::dump_arbitrate_lock(&shared_memory, ctx, svc_args);
                    }
                }

                // Module object dump after rtld relocation
                if svc_num == 0x06 {
                    query_memory_count += 1;
                }
                #[cfg(feature = "debug-logs")]
                if query_memory_count >= 25 && !dumped_module_objects && svc_num != 0x06 {
                    dumped_module_objects = true;
                    main_log::dump_module_objects(&shared_memory, query_memory_count, svc_num);
                }

                    PhysicalCoreExecutionControl::Continue
                },
                |halt_reason: HaltReason, exception_address, ctx, svc_count, iteration| {
                if halt_reason.contains(HaltReason::STEP_THREAD) {
                    step_halt_count += 1;
                    if ctx.pc == last_step_pc {
                        repeated_step_pc_count += 1;
                    } else {
                        last_step_pc = ctx.pc;
                        repeated_step_pc_count = 1;
                        dumped_repeated_step_window = false;
                    }
                    #[cfg(feature = "debug-logs")]
                    {
                        if step_halt_count == 1 {
                            main_log::dump_instruction_window(&shared_memory, "First STEP halt", ctx.pc, Some(ctx.pc));
                        }
                        if repeated_step_pc_count >= repeated_step_window_threshold
                            && !dumped_repeated_step_window
                        {
                            dumped_repeated_step_window = true;
                            main_log::dump_instruction_window(
                                &shared_memory,
                                &format!("Repeated STEP PC after {} repeats", repeated_step_pc_count),
                                ctx.pc,
                                Some(ctx.pc),
                            );
                        }
                    }
                    if step_halt_count == 1 || step_halt_count % 10_000 == 0 {
                        log::info!(
                            "STEP halt #{} at PC={:#x}, SP={:#x}, LR={:#x} (iter={}, svcs={})",
                            step_halt_count, ctx.pc, ctx.sp, ctx.lr, iteration, svc_count
                        );
                    }
                    return PhysicalCoreExecutionControl::Continue;
                } else if halt_reason.contains(HaltReason::PREFETCH_ABORT) {
                    log::error!("PREFETCH_ABORT at PC={:#x}, SP={:#x}, LR={:#x}", ctx.pc, ctx.sp, ctx.lr);
                    log::error!("Reported exception address: {:?}", exception_address);
                    #[cfg(feature = "debug-logs")]
                    {
                        main_log::dump_instruction_window(&shared_memory, "Crash window", ctx.pc, Some(ctx.pc));
                        if let Some(exception_address) = exception_address {
                            main_log::dump_instruction_window(&shared_memory, "Exception window", exception_address, Some(exception_address));
                        }
                    }
                    PhysicalCoreExecutionControl::Break
                } else if halt_reason.contains(HaltReason::BREAK_LOOP) {
                    PhysicalCoreExecutionControl::Yield
                } else {
                    log::warn!(
                        "JIT halted: {:?} at PC={:#x}, SP={:#x} (iter={}, svcs={})",
                        halt_reason, ctx.pc, ctx.sp, iteration, svc_count
                    );
                    PhysicalCoreExecutionControl::Break
                    }
                },
            );
            total_iteration += iteration;
            total_svc_count += svc_count;

            match exec_control {
                PhysicalCoreExecutionControl::Continue => continue,
                PhysicalCoreExecutionControl::Yield => {
                    let core_timing = system.core_timing_shared();
                    system
                        .get_cpu_manager_mut()
                        .preempt_single_core(&core_timing, true);
                    continue;
                }
                PhysicalCoreExecutionControl::Break => break,
            }
        }

        // Final context dump.
        jit.get_context(&mut ctx);
        log::info!("Final: PC={:#x}, SP={:#x}, R0={:#x}, iterations={}, SVCs={}",
            ctx.pc, ctx.sp, ctx.r[0], total_iteration, total_svc_count);
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

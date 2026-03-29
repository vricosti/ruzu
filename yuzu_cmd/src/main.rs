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
    #[arg(
        short = 'r',
        long = "renderer",
        value_name = "BACKEND",
        default_value = "opengl"
    )]
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
///
/// Upstream order of operations:
/// 1. Parse args, create config
/// 2. system.Initialize()
/// 3. Create emu_window based on renderer backend
/// 4. system.SetContentProvider/SetFilesystem/CreateFactories
/// 5. system.Load(*emu_window, filepath, load_parameters)
/// 6. system.GPU().Start()
/// 7. system.GetCpuManager().OnGpuReady()
/// 8. system.Run()  — starts CPU threads in background
/// 9. while (emu_window->IsOpen()) { emu_window->WaitEvent(); }
/// 10. system.Pause(), system.ShutdownMainProcess()
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
    if let Some(ref program_args) = args.program {
        common::settings::values_mut()
            .program_args
            .set_value(program_args.clone());
    }
    if let Some(user_index) = args.user {
        let clamped = (user_index as i32).clamp(0, 7);
        common::settings::values_mut()
            .current_user
            .set_value(clamped);
    }

    // Log configuration settings.
    // Matches upstream: Settings::LogSettings() called in EmuWindow constructor.
    common::settings::log_settings(&common::settings::values());

    // Initialise core system.
    // Maps to C++ `Core::System system{}; system.Initialize();`.
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

    // -----------------------------------------------------------------------
    // Step 3 (upstream): Create emu_window BEFORE loading.
    // Maps to C++:
    //   switch (Settings::values.renderer_backend.GetValue()) {
    //     case OpenGL:  emu_window = make_unique<EmuWindow_SDL2_GL>(...);
    //     case Vulkan:  emu_window = make_unique<EmuWindow_SDL2_VK>(...);
    //     case Null:    emu_window = make_unique<EmuWindow_SDL2_Null>(...);
    //   }
    // -----------------------------------------------------------------------
    enum EmuWindow {
        Gl(EmuWindowSdl2Gl),
        Vk(EmuWindowSdl2Vk),
        Null(EmuWindowSdl2Null),
    }

    let mut emu_window = match renderer_backend {
        "opengl" => EmuWindow::Gl(EmuWindowSdl2Gl::new(args.fullscreen)),
        "vulkan" => EmuWindow::Vk(EmuWindowSdl2Vk::new(args.fullscreen)),
        _ => EmuWindow::Null(EmuWindowSdl2Null::new(args.fullscreen)),
    };

    // -----------------------------------------------------------------------
    // Step 4 (upstream lines 367-370):
    //   system.SetContentProvider(make_unique<FileSys::ContentProviderUnion>());
    //   system.SetFilesystem(make_shared<FileSys::RealVfsFilesystem>());
    //   system.GetFileSystemController().CreateFactories(*system.GetFilesystem());
    //   system.GetUserChannel().clear();
    // -----------------------------------------------------------------------
    {
        use ruzu_core::file_sys::registered_cache::ContentProviderUnion;
        system.set_content_provider(std::sync::Arc::new(std::sync::Mutex::new(
            ContentProviderUnion::new(),
        )));
        // VFS is already created in system.initialize(); ensure it's set.
        if system.get_filesystem().is_none() {
            system.set_filesystem(ruzu_core::file_sys::vfs::vfs_real::RealVfsFilesystem::new());
        }
        system
            .get_filesystem_controller()
            .lock()
            .unwrap()
            .create_factories();
        // Note: get_filesystem_controller() returns Arc clone, lock it.
        system.clear_user_channel();
    }

    // -----------------------------------------------------------------------
    // Step 5: Register subsystem factory.
    // Upstream creates Host1x, GPU, AudioCore inside SetupForApplicationProcess()
    // (core.cpp:277-283). In Rust, video_core/audio_core crates can't be created
    // from core due to circular dependencies, so we provide a factory callback.
    // -----------------------------------------------------------------------

    // Capture the raw SDL window pointer for creating GL contexts inside the factory.
    // The window pointer is valid for the lifetime of emu_window (which outlives System).
    // Store as usize to avoid Send issues with raw pointers.
    let sdl_window_ptr_usize: usize = match &emu_window {
        EmuWindow::Gl(w) => w.raw_window() as usize,
        EmuWindow::Vk(w) => w.raw_window() as usize,
        EmuWindow::Null(w) => w.raw_window() as usize,
    };
    let renderer_backend_str = renderer_backend.to_string();

    system.set_subsystem_factory(Box::new(move |system| {
        use std::sync::Arc;

        // Host1x (upstream core.cpp:277): host1x_core = make_unique<Host1x>(system)
        let host1x = video_core::host1x::host1x::Host1x::new();
        let syncpoints = host1x.syncpoint_manager().clone();
        system.set_host1x_core(Box::new(host1x));

        // GPU (upstream core.cpp:278): gpu_core = VideoCore::CreateGPU(emu_window, system)
        //
        // Upstream flow:
        //   auto context = emu_window.CreateSharedContext();
        //   auto scope = context->Acquire();
        //   auto renderer = CreateRenderer(system, emu_window, *gpu, context);
        //   gpu->BindRenderer(renderer);
        let renderer: Box<dyn video_core::renderer_base::RendererBase> =
            match renderer_backend_str.as_str() {
                "opengl" => {
                    // Create a shared GL context for the renderer/GPU thread.
                    // Safety: sdl_window_ptr_usize was cast from a valid *mut SDL_Window
                    // that is alive for the duration of this closure.
                    let window_ptr = sdl_window_ptr_usize as *mut sdl2::sys::SDL_Window;
                    let context = Box::new(emu_window::emu_window_sdl2_gl::SdlGlContext::new(
                        window_ptr,
                    ));
                    let renderer = video_core::renderer_opengl::RendererOpenGL::new(
                        |s| {
                            let cs = std::ffi::CString::new(s).unwrap();
                            unsafe {
                                sdl2::sys::SDL_GL_GetProcAddress(cs.as_ptr())
                                    as *const std::os::raw::c_void
                            }
                        },
                        syncpoints.clone(),
                        context,
                    )
                    .unwrap_or_else(|e| {
                        log::error!("Failed to create OpenGL renderer: {}", e);
                        std::process::exit(1);
                    });
                    Box::new(renderer)
                }
                "vulkan" => {
                    log::warn!("Vulkan renderer not yet implemented, falling back to null");
                    Box::new(video_core::renderer_null::renderer_null::RendererNull::new(syncpoints.clone()))
                }
                _ => Box::new(video_core::renderer_null::renderer_null::RendererNull::new(syncpoints.clone())),
            };

        let gpu = video_core::video_core::create_gpu(false, true, renderer);
        let system_ref = ruzu_core::core::SystemRef::from_ref(&system);
        gpu.set_guest_memory_reader(Arc::new(move |addr, output: &mut [u8]| {
            if let Some(process) = system_ref.get().current_process() {
                let bytes = process.read_memory_vec(addr, output.len());
                output.copy_from_slice(&bytes);
            }
        }));
        system.set_gpu_core(Box::new(gpu));

        // AudioCore (upstream core.cpp:283): audio_core = make_unique<AudioCore>(system)
        let shared_system: audio_core::SharedSystem =
            Arc::new(parking_lot::Mutex::new(ruzu_core::core::System::new()));
        let ac = audio_core::AudioCore::new(shared_system);
        system.set_audio_core(Box::new(ac));
    }));

    // -----------------------------------------------------------------------
    // Step 6 (upstream): system.Load(*emu_window, filepath, load_parameters).
    // Upstream passes the window reference to Load; our Rust Load doesn't
    // accept one yet, so we call with the current signature.
    // The factory callback above will be called during load() ->
    // setup_for_application_process() to create Host1x/GPU/AudioCore.
    // -----------------------------------------------------------------------
    let load_result = system.load(&filepath);
    if load_result != ruzu_core::core::SystemResultStatus::Success {
        log::error!("Failed to load ROM '{}': {:?}", filepath, load_result,);
        std::process::exit(-1);
    }

    // Multiplayer callbacks are wired here when Network::RoomMember is ported.
    if let Some(ref mp) = args.multiplayer {
        log::warn!(
            "Multiplayer '{}' requested but Network::RoomMember not yet ported; ignoring",
            mp
        );
    }

    // -----------------------------------------------------------------------
    // Step 6 (upstream): system.GPU().Start()
    // Core is loaded, start the GPU (makes the GPU contexts current to this thread).
    // -----------------------------------------------------------------------
    if let Some(gpu_any) = system.gpu_core() {
        if let Some(gpu) = gpu_any.as_any().downcast_ref::<video_core::gpu::Gpu>() {
            gpu.start();
        } else {
            log::warn!("GPU core is not a video_core::gpu::Gpu — cannot call start()");
        }
    } else {
        log::warn!("No GPU core set — skipping GPU().Start()");
    }

    // -----------------------------------------------------------------------
    // Step 7 (upstream): system.GetCpuManager().OnGpuReady()
    // -----------------------------------------------------------------------
    system.get_cpu_manager().on_gpu_ready();

    // -----------------------------------------------------------------------
    // Upstream: system.RegisterExitCallback([&] { exit(0); })
    // The SDL frontend exits immediately when the core requests application exit.
    // -----------------------------------------------------------------------
    system.register_exit_callback(Box::new(|| {
        std::process::exit(0);
    }));

    // -----------------------------------------------------------------------
    // Step 8 (upstream): system.Run()
    // Starts CPU threads running in background via CpuManager.
    // -----------------------------------------------------------------------
    system.run();

    // -----------------------------------------------------------------------
    // Step 9 (upstream): while (emu_window->IsOpen()) { emu_window->WaitEvent(); }
    // Main thread stays in the window event loop.
    // -----------------------------------------------------------------------
    log::info!("Entering main event loop");
    match &mut emu_window {
        EmuWindow::Gl(w) => {
            while w.is_open() {
                w.wait_event();
            }
        }
        EmuWindow::Vk(w) => {
            while w.is_open() {
                w.wait_event();
            }
        }
        EmuWindow::Null(w) => {
            while w.is_open() {
                w.wait_event();
            }
        }
    }

    // -----------------------------------------------------------------------
    // Step 10 (upstream): system.Pause(); system.ShutdownMainProcess();
    // Cleanup after window closes.
    // -----------------------------------------------------------------------
    log::info!("Window closed, shutting down");
    system.pause();
    system.shutdown_main_process();
}

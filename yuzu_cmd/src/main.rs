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

    // Run emulation.
    // Maps to upstream: system.Run() triggers CpuManager threads internally.
    let (total_iterations, total_svcs) = system.run_main_loop();
    log::info!("Emulation finished: {} iterations, {} SVCs", total_iterations, total_svcs);

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

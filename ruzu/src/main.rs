// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

mod config;
mod window;

use anyhow::{Context, Result};
use clap::Parser;
use log::info;
use std::path::PathBuf;

use ruzu_kernel::KernelCore;
use ruzu_loader::nro;

/// ruzu - Nintendo Switch Emulator
#[derive(Parser, Debug)]
#[command(name = "ruzu", version, about = "Nintendo Switch Emulator written in Rust")]
struct Args {
    /// Path to the game file (NRO)
    #[arg(short, long)]
    game: PathBuf,

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

    // Load NRO file
    info!("Loading game: {}", args.game.display());
    let rom_data = std::fs::read(&args.game).context("Failed to read game file")?;
    let code_set = nro::load_nro(&rom_data).context("Failed to parse NRO file")?;

    info!(
        "NRO loaded: text=0x{:X}+0x{:X}, rodata=0x{:X}+0x{:X}, data=0x{:X}+0x{:X}, bss=0x{:X}",
        code_set.segments[0].offset,
        code_set.segments[0].size,
        code_set.segments[1].offset,
        code_set.segments[1].size,
        code_set.segments[2].offset,
        code_set.segments[2].size,
        code_set.bss_size,
    );

    if code_set.is_homebrew {
        info!("Detected homebrew NRO");
    }

    // Create kernel
    let mut kernel = KernelCore::new();
    kernel.create_process("main")?;

    // Extract segments for loading
    let text_seg = &code_set.segments[0];
    let rodata_seg = &code_set.segments[1];
    let data_seg = &code_set.segments[2];

    let text_data = &code_set.memory
        [text_seg.offset as usize..(text_seg.offset + text_seg.size) as usize];
    let rodata_data = &code_set.memory
        [rodata_seg.offset as usize..(rodata_seg.offset + rodata_seg.size) as usize];
    let data_data =
        &code_set.memory[data_seg.offset as usize..(data_seg.offset + data_seg.size) as usize];

    // Load into kernel
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
        process.title_id = settings.title_id;
    }

    if args.headless {
        info!("Running in headless mode");
        run_headless(&mut kernel)?;
    } else {
        run_with_window(&mut kernel)?;
    }

    info!("Emulation finished");
    Ok(())
}

/// Run in headless mode (no window, CPU only).
fn run_headless(kernel: &mut KernelCore) -> Result<()> {
    info!("Starting CPU execution (headless)...");

    // In Phase 1 without Dynarmic, we can only do a stub execution loop
    // that demonstrates the SVC handling infrastructure is working
    let max_steps = 1_000_000;
    let step = 0;

    while !kernel.should_stop && step < max_steps {
        // Get current thread's CPU state
        let process = match kernel.process_mut() {
            Some(p) => p,
            None => break,
        };

        let thread = match process.current_thread_mut() {
            Some(t) => t,
            None => {
                info!("No runnable threads, stopping");
                break;
            }
        };

        if thread.is_terminated() {
            info!("Main thread terminated");
            break;
        }

        // In Phase 1, we need a CPU backend (Dynarmic) to actually execute
        // ARM64 instructions. For now, just demonstrate the infrastructure.
        // When Dynarmic FFI is integrated, this loop will:
        // 1. Call jit.run() which executes until SVC/exception
        // 2. Handle the SVC via dispatch_svc()
        // 3. Return control to the JIT

        info!("CPU execution requires Dynarmic FFI backend (not yet linked)");
        info!("Infrastructure is ready: kernel, process, thread, SVCs, memory");
        break;
    }

    Ok(())
}

/// Run with SDL2 window and Vulkan renderer.
fn run_with_window(kernel: &mut KernelCore) -> Result<()> {
    use window::{EmulatorWindow, DEFAULT_HEIGHT, DEFAULT_WIDTH};

    let mut emu_window =
        EmulatorWindow::new("ruzu - Nintendo Switch Emulator", DEFAULT_WIDTH, DEFAULT_HEIGHT)?;

    info!("Window created, entering main loop");
    info!("Press ESC or close window to exit");

    // Main loop
    loop {
        // Poll window events
        if !emu_window.poll_events() {
            info!("Window close requested");
            break;
        }

        // Check if emulation should stop
        if kernel.should_stop {
            break;
        }

        // In Phase 1 without Dynarmic, just keep the window alive
        // When CPU backend is ready, each frame will:
        // 1. Run CPU for N cycles
        // 2. Handle SVCs
        // 3. Present frame via Vulkan

        // Small sleep to avoid busy-waiting
        std::thread::sleep(std::time::Duration::from_millis(16)); // ~60fps
    }

    Ok(())
}

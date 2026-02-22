// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

mod config;
mod game_list_ui;
mod window;

use anyhow::{Context, Result};
use clap::Parser;
use log::info;
use std::path::PathBuf;
use std::sync::Arc;

use ruzu_crypto::KeyManager;
use ruzu_kernel::KernelCore;
use ruzu_loader::game_info::{self, GameFormat};
use ruzu_loader::nro;
use ruzu_loader::nsp;
use ruzu_loader::vfs::RealFile;
use ruzu_loader::xci;

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

    let (code_set, title_id) = match format {
        GameFormat::Nro => load_nro_game(&game_path)?,
        GameFormat::Nsp => load_nsp_game(&game_path, &mut keys)?,
        GameFormat::Xci => load_xci_game(&game_path, &mut keys)?,
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

    if args.headless {
        info!("Running in headless mode");
        run_headless(&mut kernel)?;
    } else {
        run_with_window(&mut kernel)?;
    }

    info!("Emulation finished");
    Ok(())
}

/// Load an NRO game file.
fn load_nro_game(path: &PathBuf) -> Result<(nro::CodeSet, u64)> {
    let rom_data = std::fs::read(path).context("Failed to read NRO file")?;
    let code_set = nro::load_nro(&rom_data).context("Failed to parse NRO file")?;

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
///
/// Uses a priority-aware round-robin scheduler. Each thread gets a fixed
/// instruction budget per time slice. Waiting threads are skipped; timeouts
/// are checked each iteration.
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

/// Run with SDL2 window and Vulkan renderer.
fn run_with_window(kernel: &mut KernelCore) -> Result<()> {
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

    let mut interp = Interpreter::new();

    // Number of time slices to run per frame (~60 FPS ≈ 16ms).
    const SLICES_PER_FRAME: usize = 10;

    loop {
        if !emu_window.poll_events() {
            info!("Window close requested");
            break;
        }

        if kernel.should_stop {
            break;
        }

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
    }

    Ok(())
}

//! Port of `zuyu/src/audio_core/`.
//! Status: EN COURS
//! Derniere synchro: 2026-03-06

pub mod adsp;
pub mod audio_core;
pub mod audio_event;
pub mod audio_in_manager;
pub mod audio_manager;
pub mod audio_out_manager;
pub mod audio_render_manager;
pub mod common;
pub mod device;
pub mod errors;
pub mod r#in;
pub mod opus;
pub mod out;
pub mod renderer;
pub mod sink;

pub use crate::audio_core::AudioCore;
pub use crate::audio_event::Event;
pub use crate::audio_in_manager::Manager as AudioInManager;
pub use crate::audio_manager::AudioManager;
pub use crate::audio_out_manager::Manager as AudioOutManager;
pub use crate::audio_render_manager::Manager as AudioRenderManager;
pub use ::common::ResultCode;
use parking_lot::Mutex;
use std::sync::Arc;

pub type SharedSystem = Arc<Mutex<ruzu_core::core::System>>;
pub type Result = ResultCode;

/// Global guest memory accessor for the audio renderer's decode path.
///
/// Set once at audio system initialization (see
/// `adsp/apps/audio_renderer/command_list_processor.rs::initialize`)
/// and used by `renderer/command/data_source/decode.rs` to translate
/// guest virtual addresses to host bytes via `Memory::read_block`.
///
/// Upstream zuyu plumbs `Core::Memory::Memory&` through the entire
/// decode call chain. Ruzu uses a global to avoid the invasive
/// refactor — since ruzu has exactly one application process at a
/// time, a single global accessor is sufficient.
pub static GUEST_MEMORY_ACCESSOR: std::sync::OnceLock<
    std::sync::Arc<std::sync::Mutex<ruzu_core::memory::memory::Memory>>,
> = std::sync::OnceLock::new();

/// Read a block from guest memory via the global accessor.
/// Returns true on success, false if accessor unset or read failed.
pub fn guest_read_block(guest_addr: u64, dest: &mut [u8]) -> bool {
    static MISS_LOGGED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    if let Some(mem) = GUEST_MEMORY_ACCESSOR.get() {
        if let Ok(guard) = mem.lock() {
            return guard.read_block(guest_addr, dest);
        }
    } else if !MISS_LOGGED.swap(true, std::sync::atomic::Ordering::Relaxed) {
        log::warn!(
            "guest_read_block: GUEST_MEMORY_ACCESSOR not set yet (first miss at 0x{:X})",
            guest_addr
        );
    }
    false
}

/// Write a block to guest memory via the global accessor.
/// Returns true on success, false if accessor unset or write failed.
pub fn guest_write_block(guest_addr: u64, src: &[u8]) -> bool {
    static MISS_LOGGED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    if let Some(mem) = GUEST_MEMORY_ACCESSOR.get() {
        if let Ok(guard) = mem.lock() {
            return guard.write_block(guest_addr, src);
        }
    } else if !MISS_LOGGED.swap(true, std::sync::atomic::Ordering::Relaxed) {
        log::warn!(
            "guest_write_block: GUEST_MEMORY_ACCESSOR not set yet (first miss at 0x{:X})",
            guest_addr
        );
    }
    false
}

/// Allow other audio_core code paths to initialize the global accessor
/// directly (e.g. from system_manager init when CommandListProcessor::initialize
/// has not been called yet).
pub fn init_guest_memory_accessor(
    mem: std::sync::Arc<std::sync::Mutex<ruzu_core::memory::memory::Memory>>,
) {
    if GUEST_MEMORY_ACCESSOR.set(mem).is_ok() {
        log::info!("audio_core: GUEST_MEMORY_ACCESSOR initialized via init_guest_memory_accessor");
    }
}

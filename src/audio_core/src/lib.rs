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
pub mod raw_write_trace;
pub mod renderer;
pub mod sink;

pub use crate::audio_core::AudioCore;
pub use crate::audio_event::Event;
pub use crate::audio_in_manager::Manager as AudioInManager;
pub use crate::audio_manager::AudioManager;
pub use crate::audio_out_manager::Manager as AudioOutManager;
pub use crate::audio_render_manager::Manager as AudioRenderManager;
pub use ::common::ResultCode;
pub type SharedSystem = ruzu_core::core::SystemRef;
pub type Result = ResultCode;

#[cfg(test)]
pub(crate) fn make_test_system() -> SharedSystem {
    let system = Box::leak(Box::new(ruzu_core::core::System::new()));
    ruzu_core::core::SystemRef::from_ref(system)
}

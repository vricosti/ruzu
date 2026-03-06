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

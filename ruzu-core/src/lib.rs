//! ruzu-core: Core system orchestrator for the ruzu emulator.
//!
//! This crate contains the top-level System struct and supporting modules
//! that manage the emulation lifecycle: initialization, execution, timing,
//! performance tracking, and shutdown.
//!
//! Port of zuyu/src/core/ (orchestrator files).
//! Status: EN COURS
//! Derniere synchro: 2026-03-05

pub mod constants;
pub mod core;
pub mod core_timing;
pub mod cpu_manager;
pub mod device_memory;
pub mod hardware_properties;
pub mod perf_stats;

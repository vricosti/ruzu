//! ruzu-core: Core system orchestrator for the ruzu emulator.
//!
//! This crate contains the top-level System struct and supporting modules
//! that manage the emulation lifecycle: initialization, execution, timing,
//! performance tracking, and shutdown.
//!
//! Port of zuyu/src/core/ (orchestrator files).
//! Status: EN COURS
//! Derniere synchro: 2026-03-05

// Orchestrator
pub mod constants;
pub mod core;
pub mod core_timing;
pub mod cpu_manager;
pub mod device_memory;
pub mod device_memory_manager;
pub mod gpu_core;
pub mod gpu_dirty_memory_manager;
pub mod guest_memory;
pub mod hardware_properties;
pub mod host1x_core;
pub mod perf_stats;
pub mod reporter;
pub mod telemetry_session;

// ARM CPU interface
pub mod arm;

// Memory subsystem
pub mod memory;

// HLE (High-Level Emulation)
pub mod hle;

// Filesystem
pub mod file_sys;

// Loader
pub mod loader;

// Crypto
pub mod crypto;

// Debugger
pub mod debugger;

// Frontend
pub mod frontend;

// Internal network
pub mod internal_network;

// Tools
pub mod tools;

//! Port of zuyu/src/core/hle/kernel/board/nintendo/nx/
//! Status: In progress
//! Derniere synchro: 2026-03-11
//!
//! Board-specific kernel modules for the Nintendo NX (Switch).
//! The upstream directory is board/nintendo/nx/; we flatten to board/
//! since only the NX board exists.

pub mod k_memory_layout;
pub mod k_system_control;
pub mod secure_monitor;

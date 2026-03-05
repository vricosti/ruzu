//! Port of zuyu/src/common/logging/ directory
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! The logging subsystem. Provides structured logging with per-class filtering,
//! colored console output, and file logging with a background thread.

pub mod backend;
pub mod filter;
pub mod log_entry;
pub mod text_formatter;
pub mod types;

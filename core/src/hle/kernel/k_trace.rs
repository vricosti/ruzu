//! Port of zuyu/src/core/hle/kernel/k_trace.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! Kernel trace buffer constants.

/// Whether kernel tracing is enabled.
pub const IS_K_TRACE_ENABLED: bool = false;

/// Size of the kernel trace buffer in bytes.
/// 16 MiB when tracing is enabled, 0 otherwise.
pub const K_TRACE_BUFFER_SIZE: usize = if IS_K_TRACE_ENABLED {
    16 * 1024 * 1024
} else {
    0
};

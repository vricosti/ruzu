//! Port of zuyu/src/core/hle/kernel/svc_common.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! Common SVC (Supervisor Call) types and constants used by the kernel.

/// Kernel handle type (u32).
pub type Handle = u32;

/// Maximum number of argument handles.
pub const ARGUMENT_HANDLE_COUNT_MAX: i32 = 0x40;

/// Mask applied to handles in wait operations.
pub const HANDLE_WAIT_MASK: u32 = 1u32 << 30;

/// Sentinel value representing an infinite wait timeout.
pub const WAIT_INFINITE: i64 = -1;

/// Required alignment for heap size operations (2 MiB).
pub const HEAP_SIZE_ALIGNMENT: usize = 2 * 1024 * 1024;

/// The invalid handle value.
pub const INVALID_HANDLE: Handle = 0;

/// Pseudo-handles for the current thread and current process.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PseudoHandle {
    CurrentThread = 0xFFFF8000,
    CurrentProcess = 0xFFFF8001,
}

/// Returns true if the given handle is a pseudo-handle (CurrentThread or CurrentProcess).
pub fn is_pseudo_handle(handle: Handle) -> bool {
    handle == PseudoHandle::CurrentProcess as Handle
        || handle == PseudoHandle::CurrentThread as Handle
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pseudo_handle_values() {
        assert_eq!(PseudoHandle::CurrentThread as u32, 0xFFFF8000);
        assert_eq!(PseudoHandle::CurrentProcess as u32, 0xFFFF8001);
    }

    #[test]
    fn test_is_pseudo_handle() {
        assert!(is_pseudo_handle(0xFFFF8000));
        assert!(is_pseudo_handle(0xFFFF8001));
        assert!(!is_pseudo_handle(0));
        assert!(!is_pseudo_handle(1));
        assert!(!is_pseudo_handle(INVALID_HANDLE));
    }

    #[test]
    fn test_constants() {
        assert_eq!(HANDLE_WAIT_MASK, 0x40000000);
        assert_eq!(WAIT_INFINITE, -1);
        assert_eq!(HEAP_SIZE_ALIGNMENT, 2 * 1024 * 1024);
        assert_eq!(INVALID_HANDLE, 0);
        assert_eq!(ARGUMENT_HANDLE_COUNT_MAX, 0x40);
    }
}

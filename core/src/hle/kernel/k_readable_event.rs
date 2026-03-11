//! Port of zuyu/src/core/hle/kernel/k_readable_event.h / k_readable_event.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KReadableEvent: the readable (waitable) end of a kernel event.

/// The readable event object.
/// Matches upstream `KReadableEvent` class (k_readable_event.h).
pub struct KReadableEvent {
    pub is_signaled: bool,
    pub parent_id: Option<u64>, // KEvent ID
}

impl KReadableEvent {
    pub fn new() -> Self {
        Self {
            is_signaled: false,
            parent_id: None,
        }
    }

    /// Initialize with a parent event.
    pub fn initialize(&mut self, parent_id: u64) {
        self.parent_id = Some(parent_id);
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    /// Signal the readable event.
    /// Matches upstream `KReadableEvent::Signal`.
    pub fn signal(&mut self) -> u32 {
        if !self.is_signaled {
            self.is_signaled = true;
            // TODO: NotifyAvailable()
        }
        0 // ResultSuccess
    }

    /// Clear the readable event.
    /// Matches upstream `KReadableEvent::Clear`.
    pub fn clear(&mut self) -> u32 {
        self.is_signaled = false;
        0 // ResultSuccess
    }

    /// Reset the event (clear and return error if not signaled).
    /// Matches upstream `KReadableEvent::Reset`.
    pub fn reset(&mut self) -> u32 {
        if !self.is_signaled {
            return 1; // ResultInvalidState — TODO: use proper result code
        }
        self.is_signaled = false;
        0
    }

    /// Is the event signaled?
    pub fn is_signaled(&self) -> bool {
        self.is_signaled
    }

    /// Destroy the readable event.
    /// TODO: Port from k_readable_event.cpp.
    pub fn destroy(&mut self) {
        // TODO: Notify parent
    }
}

impl Default for KReadableEvent {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signal_and_clear() {
        let mut event = KReadableEvent::new();
        assert!(!event.is_signaled());
        event.signal();
        assert!(event.is_signaled());
        event.clear();
        assert!(!event.is_signaled());
    }

    #[test]
    fn test_reset_not_signaled() {
        let mut event = KReadableEvent::new();
        assert_ne!(event.reset(), 0); // Should fail — not signaled
    }

    #[test]
    fn test_reset_signaled() {
        let mut event = KReadableEvent::new();
        event.signal();
        assert_eq!(event.reset(), 0);
        assert!(!event.is_signaled());
    }
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Dirty flag state tracker for selective GPU state updates.
//!
//! Ref: zuyu `vk_state_tracker.h` — Touch*() methods atomically read-and-clear
//! flags to avoid redundant Vulkan dynamic state commands.

/// Dirty flag indices matching zuyu's Dirty:: enum.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirtyFlag {
    Viewports = 0,
    Scissors = 1,
    DepthBias = 2,
    BlendConstants = 3,
    DepthBounds = 4,
    StencilProperties = 5,
    LineWidth = 6,
    CullMode = 7,
    DepthTestEnable = 8,
    DepthWriteEnable = 9,
    DepthCompareOp = 10,
    FrontFace = 11,
    StencilTestEnable = 12,
    PrimitiveRestart = 13,
    RasterizerDiscard = 14,
    DepthBiasEnable = 15,
    LogicOp = 16,
    Blending = 17,
    BlendEnable = 18,
    BlendEquations = 19,
    ColorMask = 20,
    VertexInput = 21,
}

/// Total number of dirty flags.
const NUM_FLAGS: u8 = 22;

/// Tracks GPU state changes via dirty flags.
///
/// Ref: zuyu StateTracker — `Touch*()` methods check and clear dirty flags,
/// returning true if the state needs updating. This avoids redundant Vulkan
/// dynamic state commands when state hasn't changed between draw calls.
pub struct StateTracker {
    /// Bitfield of currently dirty flags.
    flags: u64,
    /// Flags to set dirty on command buffer invalidation (all flags).
    invalidation_flags: u64,
}

impl StateTracker {
    /// Create a new state tracker with all flags dirty.
    pub fn new() -> Self {
        let all_flags = (1u64 << NUM_FLAGS) - 1;
        Self {
            flags: all_flags,
            invalidation_flags: all_flags,
        }
    }

    /// Mark all flags as dirty (called on command buffer reset).
    pub fn invalidate_command_buffer_state(&mut self) {
        self.flags = self.invalidation_flags;
    }

    /// Check if a flag is dirty, and clear it. Returns true if it was dirty.
    ///
    /// This is the primary interface — call this before recording a dynamic
    /// state command to skip it if the state hasn't changed.
    pub fn touch(&mut self, flag: DirtyFlag) -> bool {
        let bit = 1u64 << (flag as u8);
        let was_dirty = (self.flags & bit) != 0;
        self.flags &= !bit;
        was_dirty
    }

    /// Mark a specific flag as dirty.
    pub fn set_dirty(&mut self, flag: DirtyFlag) {
        self.flags |= 1u64 << (flag as u8);
    }

    /// Check if a flag is dirty without clearing it.
    pub fn is_dirty(&self, flag: DirtyFlag) -> bool {
        (self.flags & (1u64 << (flag as u8))) != 0
    }

    /// Mark all flags as dirty.
    pub fn set_all_dirty(&mut self) {
        self.flags = self.invalidation_flags;
    }
}

impl Default for StateTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_all_dirty() {
        let tracker = StateTracker::new();
        assert!(tracker.is_dirty(DirtyFlag::Viewports));
        assert!(tracker.is_dirty(DirtyFlag::Scissors));
        assert!(tracker.is_dirty(DirtyFlag::VertexInput));
    }

    #[test]
    fn test_touch_clears_flag() {
        let mut tracker = StateTracker::new();
        assert!(tracker.touch(DirtyFlag::Viewports));
        assert!(!tracker.touch(DirtyFlag::Viewports));
    }

    #[test]
    fn test_set_dirty() {
        let mut tracker = StateTracker::new();
        tracker.touch(DirtyFlag::Scissors);
        assert!(!tracker.is_dirty(DirtyFlag::Scissors));
        tracker.set_dirty(DirtyFlag::Scissors);
        assert!(tracker.is_dirty(DirtyFlag::Scissors));
    }

    #[test]
    fn test_invalidate_resets_all() {
        let mut tracker = StateTracker::new();
        tracker.touch(DirtyFlag::Viewports);
        tracker.touch(DirtyFlag::Scissors);
        tracker.touch(DirtyFlag::DepthBias);
        tracker.invalidate_command_buffer_state();
        assert!(tracker.is_dirty(DirtyFlag::Viewports));
        assert!(tracker.is_dirty(DirtyFlag::Scissors));
        assert!(tracker.is_dirty(DirtyFlag::DepthBias));
    }

    #[test]
    fn test_touch_does_not_affect_other_flags() {
        let mut tracker = StateTracker::new();
        tracker.touch(DirtyFlag::Viewports);
        assert!(!tracker.is_dirty(DirtyFlag::Viewports));
        assert!(tracker.is_dirty(DirtyFlag::Scissors));
        assert!(tracker.is_dirty(DirtyFlag::DepthBias));
    }
}

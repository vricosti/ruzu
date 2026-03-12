// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_state_tracker.h` / `vk_state_tracker.cpp`.
//!
//! Dirty flag state tracker for selective GPU state updates.
//! Touch*() methods atomically read-and-clear flags to avoid
//! redundant Vulkan dynamic state commands.

use crate::engines::maxwell_3d::PrimitiveTopology;

// ---------------------------------------------------------------------------
// Dirty flag indices — port of Vulkan::Dirty enum
// ---------------------------------------------------------------------------

/// Dirty flag indices matching upstream `Vulkan::Dirty::` enum.
///
/// The first few indices (0..LastCommonEntry) are shared with the common
/// dirty flag system in `VideoCommon::Dirty`. Backend-specific flags start
/// after `LastCommonEntry`. We keep the same numeric offsets as upstream
/// to preserve behavioral parity with flag table setup.
pub mod dirty {
    // Common entries (from VideoCommon::Dirty)
    pub const RENDER_TARGETS: u8 = 0;
    pub const COLOR_BUFFER_0: u8 = 1;
    pub const COLOR_BUFFER_7: u8 = 8;
    pub const ZETA_BUFFER: u8 = 9;
    pub const VERTEX_BUFFERS: u8 = 10;
    pub const VERTEX_BUFFER_0: u8 = 11;
    pub const VERTEX_BUFFER_31: u8 = 42;
    pub const RESCALE_VIEWPORTS: u8 = 43;
    pub const RESCALE_SCISSORS: u8 = 44;
    pub const DEPTH_BIAS_GLOBAL: u8 = 45;
    pub const LAST_COMMON_ENTRY: u8 = 46;

    // Vulkan-specific entries
    pub const VERTEX_INPUT: u8 = LAST_COMMON_ENTRY;
    pub const VERTEX_ATTRIBUTE_0: u8 = VERTEX_INPUT + 1;
    pub const VERTEX_ATTRIBUTE_31: u8 = VERTEX_ATTRIBUTE_0 + 31;
    pub const VERTEX_BINDING_0: u8 = VERTEX_ATTRIBUTE_31 + 1;
    pub const VERTEX_BINDING_31: u8 = VERTEX_BINDING_0 + 31;

    pub const VIEWPORTS: u8 = VERTEX_BINDING_31 + 1;
    pub const SCISSORS: u8 = VIEWPORTS + 1;
    pub const DEPTH_BIAS: u8 = SCISSORS + 1;
    pub const BLEND_CONSTANTS: u8 = DEPTH_BIAS + 1;
    pub const DEPTH_BOUNDS: u8 = BLEND_CONSTANTS + 1;
    pub const STENCIL_PROPERTIES: u8 = DEPTH_BOUNDS + 1;
    pub const STENCIL_REFERENCE: u8 = STENCIL_PROPERTIES + 1;
    pub const STENCIL_WRITE_MASK: u8 = STENCIL_REFERENCE + 1;
    pub const STENCIL_COMPARE: u8 = STENCIL_WRITE_MASK + 1;
    pub const LINE_WIDTH: u8 = STENCIL_COMPARE + 1;

    pub const CULL_MODE: u8 = LINE_WIDTH + 1;
    pub const DEPTH_BOUNDS_ENABLE: u8 = CULL_MODE + 1;
    pub const DEPTH_TEST_ENABLE: u8 = DEPTH_BOUNDS_ENABLE + 1;
    pub const DEPTH_WRITE_ENABLE: u8 = DEPTH_TEST_ENABLE + 1;
    pub const DEPTH_COMPARE_OP: u8 = DEPTH_WRITE_ENABLE + 1;
    pub const FRONT_FACE: u8 = DEPTH_COMPARE_OP + 1;
    pub const STENCIL_OP: u8 = FRONT_FACE + 1;
    pub const STENCIL_TEST_ENABLE: u8 = STENCIL_OP + 1;
    pub const PRIMITIVE_RESTART_ENABLE: u8 = STENCIL_TEST_ENABLE + 1;
    pub const RASTERIZER_DISCARD_ENABLE: u8 = PRIMITIVE_RESTART_ENABLE + 1;
    pub const DEPTH_BIAS_ENABLE: u8 = RASTERIZER_DISCARD_ENABLE + 1;
    pub const STATE_ENABLE: u8 = DEPTH_BIAS_ENABLE + 1;
    pub const LOGIC_OP: u8 = STATE_ENABLE + 1;
    pub const LOGIC_OP_ENABLE: u8 = LOGIC_OP + 1;
    pub const DEPTH_CLAMP_ENABLE: u8 = LOGIC_OP_ENABLE + 1;

    pub const BLENDING: u8 = DEPTH_CLAMP_ENABLE + 1;
    pub const BLEND_ENABLE: u8 = BLENDING + 1;
    pub const BLEND_EQUATIONS: u8 = BLEND_ENABLE + 1;
    pub const COLOR_MASK: u8 = BLEND_EQUATIONS + 1;
    pub const VIEWPORT_SWIZZLES: u8 = COLOR_MASK + 1;

    pub const LAST: u8 = VIEWPORT_SWIZZLES + 1;
}

/// Total number of dirty flags.
const NUM_FLAGS: usize = dirty::LAST as usize;

/// Backing store for dirty flags — a simple boolean array.
/// Port of `Tegra::Engines::Maxwell3D::DirtyState::Flags`.
pub type DirtyFlags = [bool; 256];

// ---------------------------------------------------------------------------
// StencilProperties
// ---------------------------------------------------------------------------

/// Tracked stencil state for a single face.
/// Port of `StateTracker::StencilProperties`.
#[derive(Debug, Clone, Copy, Default)]
struct StencilProperties {
    reference: u32,
    write_mask: u32,
    compare_mask: u32,
}

// ---------------------------------------------------------------------------
// StateTracker
// ---------------------------------------------------------------------------

/// Tracks GPU state changes via dirty flags.
///
/// Port of `Vulkan::StateTracker` class.
///
/// The Touch*() methods check and clear dirty flags, returning true if the
/// corresponding Vulkan dynamic state command needs to be recorded.
pub struct StateTracker {
    flags: DirtyFlags,
    invalidation_flags: DirtyFlags,
    current_topology: Option<PrimitiveTopology>,
    two_sided_stencil: bool,
    front: StencilProperties,
    back: StencilProperties,
    stencil_reset: bool,
}

impl StateTracker {
    /// Create a new state tracker with all flags dirty.
    pub fn new() -> Self {
        let mut flags = [false; 256];
        let mut invalidation_flags = [false; 256];

        // Start with all Vulkan-specific flags dirty
        for i in 0..NUM_FLAGS {
            flags[i] = true;
            invalidation_flags[i] = true;
        }

        Self {
            flags,
            invalidation_flags,
            current_topology: None,
            two_sided_stencil: false,
            front: StencilProperties::default(),
            back: StencilProperties::default(),
            stencil_reset: false,
        }
    }

    /// Port of `StateTracker::InvalidateCommandBufferState`.
    pub fn invalidate_command_buffer_state(&mut self) {
        for i in 0..256 {
            if self.invalidation_flags[i] {
                self.flags[i] = true;
            }
        }
        self.current_topology = None;
        self.stencil_reset = true;
    }

    /// Port of `StateTracker::InvalidateState`.
    pub fn invalidate_state(&mut self) {
        for flag in self.flags.iter_mut() {
            *flag = true;
        }
        self.current_topology = None;
        self.stencil_reset = true;
    }

    /// Port of `StateTracker::InvalidateViewports`.
    pub fn invalidate_viewports(&mut self) {
        self.flags[dirty::VIEWPORTS as usize] = true;
    }

    /// Port of `StateTracker::InvalidateScissors`.
    pub fn invalidate_scissors(&mut self) {
        self.flags[dirty::SCISSORS as usize] = true;
    }

    // -- Exchange helper (read-and-clear) --

    fn exchange(&mut self, id: u8) -> bool {
        let is_dirty = self.flags[id as usize];
        self.flags[id as usize] = false;
        is_dirty
    }

    fn exchange_check<T: PartialEq + Copy>(&mut self, old: &mut T, new: T) -> bool {
        let result = *old != new;
        *old = new;
        result
    }

    // -- Touch methods --

    /// Port of `StateTracker::TouchViewports`.
    pub fn touch_viewports(&mut self) -> bool {
        let dirty_viewports = self.exchange(dirty::VIEWPORTS);
        let rescale_viewports = self.exchange(dirty::RESCALE_VIEWPORTS);
        dirty_viewports || rescale_viewports
    }

    /// Port of `StateTracker::TouchScissors`.
    pub fn touch_scissors(&mut self) -> bool {
        let dirty_scissors = self.exchange(dirty::SCISSORS);
        let rescale_scissors = self.exchange(dirty::RESCALE_SCISSORS);
        dirty_scissors || rescale_scissors
    }

    /// Port of `StateTracker::TouchDepthBias`.
    pub fn touch_depth_bias(&mut self) -> bool {
        let a = self.exchange(dirty::DEPTH_BIAS);
        let b = self.exchange(dirty::DEPTH_BIAS_GLOBAL);
        a || b
    }

    /// Port of `StateTracker::TouchBlendConstants`.
    pub fn touch_blend_constants(&mut self) -> bool {
        self.exchange(dirty::BLEND_CONSTANTS)
    }

    /// Port of `StateTracker::TouchDepthBounds`.
    pub fn touch_depth_bounds(&mut self) -> bool {
        self.exchange(dirty::DEPTH_BOUNDS)
    }

    /// Port of `StateTracker::TouchStencilProperties`.
    pub fn touch_stencil_properties(&mut self) -> bool {
        self.exchange(dirty::STENCIL_PROPERTIES)
    }

    /// Port of `StateTracker::TouchStencilReference`.
    pub fn touch_stencil_reference(&mut self) -> bool {
        self.exchange(dirty::STENCIL_REFERENCE)
    }

    /// Port of `StateTracker::TouchStencilWriteMask`.
    pub fn touch_stencil_write_mask(&mut self) -> bool {
        self.exchange(dirty::STENCIL_WRITE_MASK)
    }

    /// Port of `StateTracker::TouchStencilCompare`.
    pub fn touch_stencil_compare(&mut self) -> bool {
        self.exchange(dirty::STENCIL_COMPARE)
    }

    /// Port of `StateTracker::TouchStencilSide`.
    pub fn touch_stencil_side(&mut self, two_sided_stencil_new: bool) -> bool {
        let changed = self.two_sided_stencil != two_sided_stencil_new;
        self.two_sided_stencil = two_sided_stencil_new;
        changed || self.stencil_reset
    }

    /// Port of `StateTracker::CheckStencilReferenceFront`.
    pub fn check_stencil_reference_front(&mut self, new_value: u32) -> bool {
        let changed = self.front.reference != new_value;
        self.front.reference = new_value;
        changed || self.stencil_reset
    }

    /// Port of `StateTracker::CheckStencilReferenceBack`.
    pub fn check_stencil_reference_back(&mut self, new_value: u32) -> bool {
        let changed = self.back.reference != new_value;
        self.back.reference = new_value;
        changed || self.stencil_reset
    }

    /// Port of `StateTracker::CheckStencilWriteMaskFront`.
    pub fn check_stencil_write_mask_front(&mut self, new_value: u32) -> bool {
        let changed = self.front.write_mask != new_value;
        self.front.write_mask = new_value;
        changed || self.stencil_reset
    }

    /// Port of `StateTracker::CheckStencilWriteMaskBack`.
    pub fn check_stencil_write_mask_back(&mut self, new_value: u32) -> bool {
        let changed = self.back.write_mask != new_value;
        self.back.write_mask = new_value;
        changed || self.stencil_reset
    }

    /// Port of `StateTracker::CheckStencilCompareMaskFront`.
    pub fn check_stencil_compare_mask_front(&mut self, new_value: u32) -> bool {
        let changed = self.front.compare_mask != new_value;
        self.front.compare_mask = new_value;
        changed || self.stencil_reset
    }

    /// Port of `StateTracker::CheckStencilCompareMaskBack`.
    pub fn check_stencil_compare_mask_back(&mut self, new_value: u32) -> bool {
        let changed = self.back.compare_mask != new_value;
        self.back.compare_mask = new_value;
        changed || self.stencil_reset
    }

    /// Port of `StateTracker::ClearStencilReset`.
    pub fn clear_stencil_reset(&mut self) {
        self.stencil_reset = false;
    }

    /// Port of `StateTracker::TouchLineWidth`.
    pub fn touch_line_width(&mut self) -> bool {
        self.exchange(dirty::LINE_WIDTH)
    }

    /// Port of `StateTracker::TouchCullMode`.
    pub fn touch_cull_mode(&mut self) -> bool {
        self.exchange(dirty::CULL_MODE)
    }

    /// Port of `StateTracker::TouchStateEnable`.
    pub fn touch_state_enable(&mut self) -> bool {
        self.exchange(dirty::STATE_ENABLE)
    }

    /// Port of `StateTracker::TouchDepthBoundsTestEnable`.
    pub fn touch_depth_bounds_test_enable(&mut self) -> bool {
        self.exchange(dirty::DEPTH_BOUNDS_ENABLE)
    }

    /// Port of `StateTracker::TouchDepthTestEnable`.
    pub fn touch_depth_test_enable(&mut self) -> bool {
        self.exchange(dirty::DEPTH_TEST_ENABLE)
    }

    /// Port of `StateTracker::TouchDepthWriteEnable`.
    pub fn touch_depth_write_enable(&mut self) -> bool {
        self.exchange(dirty::DEPTH_WRITE_ENABLE)
    }

    /// Port of `StateTracker::TouchPrimitiveRestartEnable`.
    pub fn touch_primitive_restart_enable(&mut self) -> bool {
        self.exchange(dirty::PRIMITIVE_RESTART_ENABLE)
    }

    /// Port of `StateTracker::TouchRasterizerDiscardEnable`.
    pub fn touch_rasterizer_discard_enable(&mut self) -> bool {
        self.exchange(dirty::RASTERIZER_DISCARD_ENABLE)
    }

    /// Port of `StateTracker::TouchDepthBiasEnable`.
    pub fn touch_depth_bias_enable(&mut self) -> bool {
        self.exchange(dirty::DEPTH_BIAS_ENABLE)
    }

    /// Port of `StateTracker::TouchLogicOpEnable`.
    pub fn touch_logic_op_enable(&mut self) -> bool {
        self.exchange(dirty::LOGIC_OP_ENABLE)
    }

    /// Port of `StateTracker::TouchDepthClampEnable`.
    pub fn touch_depth_clamp_enable(&mut self) -> bool {
        self.exchange(dirty::DEPTH_CLAMP_ENABLE)
    }

    /// Port of `StateTracker::TouchDepthCompareOp`.
    pub fn touch_depth_compare_op(&mut self) -> bool {
        self.exchange(dirty::DEPTH_COMPARE_OP)
    }

    /// Port of `StateTracker::TouchFrontFace`.
    pub fn touch_front_face(&mut self) -> bool {
        self.exchange(dirty::FRONT_FACE)
    }

    /// Port of `StateTracker::TouchStencilOp`.
    pub fn touch_stencil_op(&mut self) -> bool {
        self.exchange(dirty::STENCIL_OP)
    }

    /// Port of `StateTracker::TouchBlending`.
    pub fn touch_blending(&mut self) -> bool {
        self.exchange(dirty::BLENDING)
    }

    /// Port of `StateTracker::TouchBlendEnable`.
    pub fn touch_blend_enable(&mut self) -> bool {
        self.exchange(dirty::BLEND_ENABLE)
    }

    /// Port of `StateTracker::TouchBlendEquations`.
    pub fn touch_blend_equations(&mut self) -> bool {
        self.exchange(dirty::BLEND_EQUATIONS)
    }

    /// Port of `StateTracker::TouchColorMask`.
    pub fn touch_color_mask(&mut self) -> bool {
        self.exchange(dirty::COLOR_MASK)
    }

    /// Port of `StateTracker::TouchStencilTestEnable`.
    pub fn touch_stencil_test_enable(&mut self) -> bool {
        self.exchange(dirty::STENCIL_TEST_ENABLE)
    }

    /// Port of `StateTracker::TouchLogicOp`.
    pub fn touch_logic_op(&mut self) -> bool {
        self.exchange(dirty::LOGIC_OP)
    }

    /// Port of `StateTracker::ChangePrimitiveTopology`.
    pub fn change_primitive_topology(&mut self, new_topology: PrimitiveTopology) -> bool {
        let has_changed = self.current_topology != Some(new_topology);
        self.current_topology = Some(new_topology);
        has_changed
    }

    /// Access the dirty flags array directly (for table setup).
    pub fn flags_mut(&mut self) -> &mut DirtyFlags {
        &mut self.flags
    }

    /// Read-only access to dirty flags.
    pub fn flags(&self) -> &DirtyFlags {
        &self.flags
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
        assert!(tracker.flags[dirty::VIEWPORTS as usize]);
        assert!(tracker.flags[dirty::SCISSORS as usize]);
        assert!(tracker.flags[dirty::VERTEX_INPUT as usize]);
    }

    #[test]
    fn test_touch_clears_flag() {
        let mut tracker = StateTracker::new();
        assert!(tracker.touch_viewports());
        assert!(!tracker.touch_viewports());
    }

    #[test]
    fn test_invalidate_command_buffer_state() {
        let mut tracker = StateTracker::new();
        tracker.touch_viewports();
        tracker.touch_scissors();
        tracker.touch_depth_bias();
        tracker.invalidate_command_buffer_state();
        assert!(tracker.touch_viewports());
        assert!(tracker.touch_scissors());
        assert!(tracker.touch_depth_bias());
    }

    #[test]
    fn test_touch_does_not_affect_others() {
        let mut tracker = StateTracker::new();
        tracker.touch_viewports();
        assert!(!tracker.flags[dirty::VIEWPORTS as usize]);
        assert!(tracker.flags[dirty::SCISSORS as usize]);
    }

    #[test]
    fn test_change_primitive_topology() {
        let mut tracker = StateTracker::new();
        assert!(tracker.change_primitive_topology(PrimitiveTopology::Triangles));
        assert!(!tracker.change_primitive_topology(PrimitiveTopology::Triangles));
        assert!(tracker.change_primitive_topology(PrimitiveTopology::Lines));
    }

    #[test]
    fn test_stencil_reference_front() {
        let mut tracker = StateTracker::new();
        // First time: stencil_reset is false, but value changes from 0 to 42
        assert!(tracker.check_stencil_reference_front(42));
        // Same value, no stencil_reset
        assert!(!tracker.check_stencil_reference_front(42));
        // Different value
        assert!(tracker.check_stencil_reference_front(100));
    }

    #[test]
    fn test_stencil_reset_forces_update() {
        let mut tracker = StateTracker::new();
        tracker.check_stencil_reference_front(42);
        // Force stencil reset
        tracker.invalidate_command_buffer_state();
        // Even same value returns true due to stencil_reset
        assert!(tracker.check_stencil_reference_front(42));
    }

    #[test]
    fn test_touch_blend_constants() {
        let mut tracker = StateTracker::new();
        assert!(tracker.touch_blend_constants());
        assert!(!tracker.touch_blend_constants());
    }
}

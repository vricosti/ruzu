// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_null/renderer_null.h and renderer_null.cpp
//! Status: COMPLET
//!
//! Null renderer — frame composition is a no-op (no display output).
//! Used for headless testing and benchmarking.

use std::sync::Arc;

use log::trace;

use super::null_rasterizer::RasterizerNull;
use crate::rasterizer_interface::RasterizerInterface;
use crate::syncpoint::SyncpointManager;

/// Tiled capture buffer size (matching zuyu's `VideoCore::Capture::TiledSize`).
///
/// 1280 * 720 * 4 bytes (RGBA8) = 3,686,400 bytes.
const CAPTURE_TILED_SIZE: usize = 1280 * 720 * 4;

/// Null renderer — corresponds to zuyu's `Null::RendererNull`.
///
/// Extends the renderer base concept with no-op frame composition.
/// Owns a [`RasterizerNull`] for draw call handling.
pub struct RendererNull {
    rasterizer: RasterizerNull,
    frame_count: u64,
}

impl RendererNull {
    /// Create a new null renderer.
    pub fn new(syncpoints: Arc<SyncpointManager>) -> Self {
        Self {
            rasterizer: RasterizerNull::new(syncpoints),
            frame_count: 0,
        }
    }

    /// Composite framebuffers — no-op in null renderer.
    ///
    /// Matches zuyu's `RendererNull::Composite()`: increments frame counter
    /// but produces no display output.
    pub fn composite(&mut self, framebuffer_count: usize) {
        if framebuffer_count == 0 {
            return;
        }
        self.frame_count += 1;
        trace!("RendererNull::composite frame={}", self.frame_count);
    }

    /// Get a zeroed applet capture buffer.
    ///
    /// Matches zuyu: returns `TiledSize` bytes of zeros.
    pub fn get_applet_capture_buffer(&self) -> Vec<u8> {
        vec![0u8; CAPTURE_TILED_SIZE]
    }

    /// Access the rasterizer.
    pub fn rasterizer(&self) -> &RasterizerNull {
        &self.rasterizer
    }

    /// Access the rasterizer mutably.
    pub fn rasterizer_mut(&mut self) -> &mut RasterizerNull {
        &mut self.rasterizer
    }

    /// Access the rasterizer as a trait object.
    pub fn read_rasterizer(&mut self) -> &mut dyn RasterizerInterface {
        &mut self.rasterizer
    }

    /// Get the device vendor string.
    pub fn device_vendor(&self) -> &str {
        "NULL"
    }

    /// Get the current frame count.
    pub fn frame_count(&self) -> u64 {
        self.frame_count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_renderer_null_composite() {
        let sp = Arc::new(SyncpointManager::new());
        let mut renderer = RendererNull::new(sp);

        // Empty framebuffer list should be a no-op
        renderer.composite(0);
        assert_eq!(renderer.frame_count(), 0);

        // Non-empty should increment
        renderer.composite(1);
        assert_eq!(renderer.frame_count(), 1);

        renderer.composite(2);
        assert_eq!(renderer.frame_count(), 2);
    }

    #[test]
    fn test_renderer_null_capture_buffer() {
        let sp = Arc::new(SyncpointManager::new());
        let renderer = RendererNull::new(sp);

        let buf = renderer.get_applet_capture_buffer();
        assert_eq!(buf.len(), 1280 * 720 * 4);
        assert!(buf.iter().all(|&b| b == 0));
    }

    #[test]
    fn test_renderer_null_vendor() {
        let sp = Arc::new(SyncpointManager::new());
        let renderer = RendererNull::new(sp);
        assert_eq!(renderer.device_vendor(), "NULL");
    }

    #[test]
    fn test_renderer_null_rasterizer_access() {
        let sp = Arc::new(SyncpointManager::new());
        let mut renderer = RendererNull::new(sp);

        // Should be able to access the rasterizer and call methods
        renderer.rasterizer_mut().draw(false, 1);
        renderer.rasterizer_mut().flush_all();
        assert!(!renderer.rasterizer().must_flush_region(0, 0));
    }
}

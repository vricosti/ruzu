// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Central GPU context.
//!
//! Owns the GPU memory manager, syncpoint manager, command processor, and
//! GPFIFO submission queue. Services push GPFIFO entries via `submit_gpfifo()`,
//! and the main loop drains the queue via `flush()`.

use std::sync::Arc;

use parking_lot::{Mutex, RwLock};

use crate::backend::null_backend::NullBackend;
use crate::backend::GpuBackend;
use crate::command_processor::{CommandProcessor, GpEntry};
use crate::engines::fermi_2d::Fermi2D;
use crate::engines::inline_to_memory::InlineToMemory;
use crate::engines::kepler_compute::KeplerCompute;
use crate::engines::maxwell_3d::Maxwell3D;
use crate::engines::maxwell_dma::MaxwellDMA;
use crate::memory_manager::GpuMemoryManager;
use crate::syncpoint::SyncpointManager;

/// Output from a GPU flush — framebuffer data to write back to guest memory.
pub struct FramebufferOutput {
    /// GPU virtual address of the render target.
    pub gpu_va: u64,
    /// Width in pixels.
    pub width: u32,
    /// Height in pixels.
    pub height: u32,
    /// RGBA8 pixel data, row-major.
    pub pixels: Vec<u8>,
}

/// Central GPU state shared between the service layer and the main loop.
pub struct GpuContext {
    pub memory_manager: RwLock<GpuMemoryManager>,
    pub syncpoints: Arc<SyncpointManager>,
    command_processor: Mutex<CommandProcessor>,
    gpfifo_queue: Mutex<Vec<GpEntry>>,
    #[allow(dead_code)]
    backend: Mutex<Box<dyn GpuBackend>>,
}

impl GpuContext {
    /// Create a new GPU context with default engines and null backend.
    pub fn new() -> Self {
        let engines: Vec<Option<Box<dyn crate::engines::Engine>>> = vec![
            Some(Box::new(Maxwell3D::new())),       // subchannel 0
            Some(Box::new(KeplerCompute::new())),   // subchannel 1
            Some(Box::new(InlineToMemory::new())),  // subchannel 2
            Some(Box::new(Fermi2D::new())),         // subchannel 3
            Some(Box::new(MaxwellDMA::new())),      // subchannel 4
        ];

        Self {
            memory_manager: RwLock::new(GpuMemoryManager::new()),
            syncpoints: Arc::new(SyncpointManager::new()),
            command_processor: Mutex::new(CommandProcessor::new(engines)),
            gpfifo_queue: Mutex::new(Vec::new()),
            backend: Mutex::new(Box::new(NullBackend::new())),
        }
    }

    /// Queue GPFIFO entries for processing (called by NvHostGpu SubmitGpfifo).
    pub fn submit_gpfifo(&self, entries: Vec<GpEntry>) {
        log::debug!("GpuContext: submit_gpfifo {} entries", entries.len());
        let mut queue = self.gpfifo_queue.lock();
        queue.extend(entries);
    }

    /// Process all queued GPFIFO entries (called by the main loop each frame).
    ///
    /// `read_mem` reads bytes from a guest physical address into a buffer.
    /// Returns framebuffer output if the GPU produced rendered pixels (e.g. from
    /// a clear operation).
    pub fn flush(&self, read_mem: &dyn Fn(u64, &mut [u8])) -> Option<FramebufferOutput> {
        let entries: Vec<GpEntry> = {
            let mut queue = self.gpfifo_queue.lock();
            std::mem::take(&mut *queue)
        };

        if entries.is_empty() {
            return None;
        }

        log::debug!("GpuContext: flush {} GPFIFO entries", entries.len());

        let mm = self.memory_manager.read();
        let mut proc = self.command_processor.lock();

        // Create a reader that translates GPU VA → CPU PA, then reads from guest mem.
        let gpu_read = |gpu_va: u64, buf: &mut [u8]| {
            mm.read(gpu_va, buf, read_mem);
        };

        proc.process_entries(&entries, &gpu_read);

        // Collect framebuffer output from engines.
        let framebuffers = proc.take_framebuffers();
        framebuffers.into_iter().next().map(|fb| FramebufferOutput {
            gpu_va: fb.gpu_va,
            width: fb.width,
            height: fb.height,
            pixels: fb.pixels,
        })
    }
}

impl Default for GpuContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_submit_and_flush_empty() {
        let ctx = GpuContext::new();
        // Flush with no entries should be a no-op.
        ctx.flush(&|_addr, _buf| {});
    }

    #[test]
    fn test_submit_gpfifo() {
        let ctx = GpuContext::new();

        ctx.submit_gpfifo(vec![
            GpEntry {
                entry0: 0x1000,
                entry1: 0,
            },
            GpEntry {
                entry0: 0x2000,
                entry1: 0,
            },
        ]);

        // Queue should have 2 entries.
        let queue = ctx.gpfifo_queue.lock();
        assert_eq!(queue.len(), 2);
    }

    #[test]
    fn test_flush_drains_queue() {
        let ctx = GpuContext::new();

        ctx.submit_gpfifo(vec![GpEntry {
            entry0: 0,
            entry1: 0, // length = 0, will be skipped
        }]);

        ctx.flush(&|_addr, _buf| {});

        // Queue should be empty after flush.
        let queue = ctx.gpfifo_queue.lock();
        assert!(queue.is_empty());
    }
}

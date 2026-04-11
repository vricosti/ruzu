// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Central GPU context.
//!
//! Owns the GPU memory manager, syncpoint manager, command processor, and
//! GPFIFO submission queue. Services push GPFIFO entries via `submit_gpfifo()`,
//! and the main loop drains the queue via `flush()`.
//!
//! Supports two rendering backends:
//! - **Software** (default): Uses `SoftwareRasterizer` for CPU-based rendering.
//! - **Vulkan**: Uses `RasterizerVulkan` with the Maxwell shader recompiler
//!   to compile shaders to SPIR-V and render on the GPU.

use std::collections::HashMap;
use std::sync::Arc;

use parking_lot::{Mutex, RwLock};

use crate::command_processor::{CommandProcessor, GpEntry};
use crate::engines::fermi_2d::Fermi2D;
use crate::engines::inline_to_memory::InlineToMemory;
use crate::engines::kepler_compute::KeplerCompute;
use crate::engines::maxwell_3d::Maxwell3D;
use crate::engines::maxwell_dma::MaxwellDMA;
use crate::memory_manager::{GpuMemoryManager, MemoryManager};
use crate::rasterizer::SoftwareRasterizer;
use crate::rasterizer_interface::RasterizerInterface;
use crate::renderer_vulkan::RasterizerVulkan;
use crate::syncpoint::SyncpointManager;

// ── NvMap registry ──────────────────────────────────────────────────────────

/// Entry in the NvMap handle registry.
pub struct NvMapEntry {
    /// Guest CPU address of the allocation.
    pub address: u64,
    /// Size in bytes.
    pub size: u32,
}

/// Shared NvMap handle registry — maps handle → (address, size).
///
/// Written by NvMap on `Alloc`, read by NvHostAsGpu (`MapBufferEx`) and
/// ViBinderService (`SET_PREALLOCATED_BUFFER`) to resolve guest addresses.
pub struct NvMapRegistry {
    entries: RwLock<HashMap<u32, NvMapEntry>>,
}

impl NvMapRegistry {
    pub fn new() -> Self {
        Self {
            entries: RwLock::new(HashMap::new()),
        }
    }

    /// Register (or overwrite) a handle → address mapping.
    pub fn register(&self, handle: u32, address: u64, size: u32) {
        self.entries
            .write()
            .insert(handle, NvMapEntry { address, size });
    }

    /// Look up the guest address for a handle.
    pub fn get_address(&self, handle: u32) -> Option<u64> {
        self.entries.read().get(&handle).map(|e| e.address)
    }

    /// Look up the size for a handle.
    pub fn get_size(&self, handle: u32) -> Option<u32> {
        self.entries.read().get(&handle).map(|e| e.size)
    }
}

impl Default for NvMapRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ── Rendering mode ──────────────────────────────────────────────────────────

/// GPU rendering backend selection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RenderMode {
    /// Software rasterizer (CPU-based, default fallback).
    Software,
    /// Vulkan GPU renderer using the Maxwell shader recompiler.
    Vulkan,
    /// OpenGL GPU renderer.
    OpenGL,
}

// ── GPU context ─────────────────────────────────────────────────────────────

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

/// Write-back from a GPU engine to GPU VA space (blit, DMA copy, etc.).
pub struct GpuWriteBack {
    pub gpu_va: u64,
    pub data: Vec<u8>,
}

/// Output from a GPU flush — framebuffer + pending write-backs.
pub struct FlushOutput {
    pub framebuffer: Option<FramebufferOutput>,
    pub write_backs: Vec<GpuWriteBack>,
}

/// Central GPU state shared between the service layer and the main loop.
pub struct GpuContext {
    pub memory_manager: RwLock<GpuMemoryManager>,
    pub syncpoints: Arc<SyncpointManager>,
    pub nvmap_registry: NvMapRegistry,
    command_processor: Mutex<CommandProcessor>,
    gpfifo_queue: Mutex<Vec<GpEntry>>,
    /// Active rendering mode.
    render_mode: Mutex<RenderMode>,
    /// GPU rasterizer (Null, OpenGL, or Vulkan).
    /// Initialized lazily via `set_rasterizer` or `set_vulkan_renderer`.
    rasterizer: Mutex<Option<Box<dyn RasterizerInterface + Send>>>,
    /// Vulkan-specific renderer (kept for `render_draw_calls` which needs
    /// the full `RasterizerVulkan` API, not just the trait).
    vulkan_renderer: Mutex<Option<RasterizerVulkan>>,
}

impl GpuContext {
    /// Create a new GPU context with default engines and null backend.
    pub fn new() -> Self {
        let compute_memory_manager = Arc::new(Mutex::new(MemoryManager::new(0)));
        let dma_memory_manager = Arc::new(Mutex::new(MemoryManager::new(0)));
        let engines: Vec<Option<Box<dyn crate::engines::Engine>>> = vec![
            Some(Box::new(Maxwell3D::new())), // subchannel 0
            Some(Box::new(KeplerCompute::new(compute_memory_manager))), // subchannel 1
            Some(Box::new(InlineToMemory::new())), // subchannel 2
            Some(Box::new(Fermi2D::new())),   // subchannel 3
            Some(Box::new(MaxwellDMA::new(dma_memory_manager))), // subchannel 4
        ];

        Self {
            memory_manager: RwLock::new(GpuMemoryManager::new()),
            syncpoints: Arc::new(SyncpointManager::new()),
            nvmap_registry: NvMapRegistry::new(),
            command_processor: Mutex::new(CommandProcessor::new(engines)),
            gpfifo_queue: Mutex::new(Vec::new()),
            render_mode: Mutex::new(RenderMode::Software),
            rasterizer: Mutex::new(None),
            vulkan_renderer: Mutex::new(None),
        }
    }

    /// Set the Vulkan GPU renderer and switch to Vulkan rendering mode.
    ///
    /// Called from the main binary after the VulkanPresenter has been created
    /// and its Vulkan device handles are available.
    pub fn set_vulkan_renderer(&self, renderer: RasterizerVulkan) {
        log::info!("GpuContext: Vulkan renderer installed, switching to GPU rendering");
        *self.vulkan_renderer.lock() = Some(renderer);
        *self.render_mode.lock() = RenderMode::Vulkan;
    }

    /// Set a generic rasterizer (implements `RasterizerInterface`).
    ///
    /// Used for OpenGL and Null rasterizers. For Vulkan, use
    /// `set_vulkan_renderer` instead (needs the concrete type for
    /// `render_draw_calls`).
    pub fn set_rasterizer(
        &self,
        rasterizer: Box<dyn RasterizerInterface + Send>,
        mode: RenderMode,
    ) {
        log::info!("GpuContext: rasterizer installed, mode={:?}", mode);
        *self.rasterizer.lock() = Some(rasterizer);
        *self.render_mode.lock() = mode;
    }

    /// Mark the OpenGL renderer as active.
    ///
    /// Called from the main binary after the OpenGL context and RendererOpenGL
    /// have been successfully created.
    pub fn set_opengl_renderer_active(&self) {
        log::info!("GpuContext: OpenGL renderer active, switching to OpenGL mode");
        *self.render_mode.lock() = RenderMode::OpenGL;
    }

    /// Get the current rendering mode.
    pub fn render_mode(&self) -> RenderMode {
        *self.render_mode.lock()
    }

    /// Set the rendering mode (Software, Vulkan, or OpenGL).
    ///
    /// If switching to Vulkan and no renderer is installed, falls back to Software.
    pub fn set_render_mode(&self, mode: RenderMode) {
        if mode == RenderMode::Vulkan && self.vulkan_renderer.lock().is_none() {
            log::warn!("GpuContext: cannot switch to Vulkan mode — no renderer installed");
            return;
        }
        *self.render_mode.lock() = mode;
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
    /// Returns flush output containing framebuffer data and pending write-backs
    /// from engine operations (blit, DMA copy, etc.).
    pub fn flush(&self, read_mem: &dyn Fn(u64, &mut [u8])) -> Option<FlushOutput> {
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

        // Execute pending memory operations (blit, DMA copy).
        let pending = proc.execute_pending_ops(&gpu_read);
        let mut write_backs: Vec<GpuWriteBack> = pending
            .into_iter()
            .map(|pw| GpuWriteBack {
                gpu_va: pw.gpu_va,
                data: pw.data,
            })
            .collect();

        // Collect framebuffer output from engines (CLEAR_SURFACE result).
        let framebuffers = proc.take_framebuffers();
        let mut framebuffer = framebuffers.into_iter().next();

        // Take draw calls from Maxwell3D and render.
        let draw_calls = proc.take_draw_calls();
        if !draw_calls.is_empty() {
            let mode = *self.render_mode.lock();
            match mode {
                RenderMode::Vulkan => {
                    // Try Vulkan rendering, fall back to software on failure.
                    let mut vk_renderer = self.vulkan_renderer.lock();
                    if let Some(ref mut renderer) = *vk_renderer {
                        let result =
                            renderer.render_draw_calls(&draw_calls, &gpu_read, framebuffer);
                        framebuffer = result;
                    } else {
                        log::warn!(
                            "GpuContext: Vulkan mode but no renderer — falling back to software"
                        );
                        framebuffer = self.render_software(
                            &draw_calls,
                            &gpu_read,
                            framebuffer,
                            &mut write_backs,
                        );
                    }
                }
                RenderMode::Software | RenderMode::OpenGL => {
                    // OpenGL rasterizer lives in the presenter; GPU context
                    // falls back to software for draw call processing.
                    framebuffer =
                        self.render_software(&draw_calls, &gpu_read, framebuffer, &mut write_backs);
                }
            }
        }

        let framebuffer = framebuffer.map(|fb| FramebufferOutput {
            gpu_va: fb.gpu_va,
            width: fb.width,
            height: fb.height,
            pixels: fb.pixels,
        });

        Some(FlushOutput {
            framebuffer,
            write_backs,
        })
    }

    /// Software rasterizer rendering path.
    fn render_software(
        &self,
        draw_calls: &[crate::engines::maxwell_3d::DrawCall],
        gpu_read: &dyn Fn(u64, &mut [u8]),
        framebuffer: Option<crate::engines::Framebuffer>,
        write_backs: &mut Vec<GpuWriteBack>,
    ) -> Option<crate::engines::Framebuffer> {
        let raster_writes: std::sync::Mutex<Vec<GpuWriteBack>> = std::sync::Mutex::new(Vec::new());
        let gpu_write = |gpu_va: u64, data: &[u8]| {
            raster_writes.lock().unwrap().push(GpuWriteBack {
                gpu_va,
                data: data.to_vec(),
            });
        };
        let rasterized =
            SoftwareRasterizer::render_draw_calls(draw_calls, gpu_read, &gpu_write, framebuffer);
        write_backs.extend(raster_writes.into_inner().unwrap());
        rasterized
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
    fn test_registry_register_and_get() {
        let reg = NvMapRegistry::new();
        reg.register(1, 0xDEAD_0000, 0x1000);
        assert_eq!(reg.get_address(1), Some(0xDEAD_0000));
        assert_eq!(reg.get_size(1), Some(0x1000));
    }

    #[test]
    fn test_registry_missing_handle() {
        let reg = NvMapRegistry::new();
        assert_eq!(reg.get_address(42), None);
        assert_eq!(reg.get_size(42), None);
    }

    #[test]
    fn test_registry_overwrite() {
        let reg = NvMapRegistry::new();
        reg.register(1, 0x1000, 0x100);
        reg.register(1, 0x2000, 0x200);
        assert_eq!(reg.get_address(1), Some(0x2000));
        assert_eq!(reg.get_size(1), Some(0x200));
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

    #[test]
    fn test_render_mode_default() {
        let ctx = GpuContext::new();
        assert_eq!(ctx.render_mode(), RenderMode::Software);
    }

    #[test]
    fn test_render_mode_no_vulkan_fallback() {
        let ctx = GpuContext::new();
        // Cannot switch to Vulkan without a renderer installed.
        ctx.set_render_mode(RenderMode::Vulkan);
        assert_eq!(ctx.render_mode(), RenderMode::Software);
    }
}

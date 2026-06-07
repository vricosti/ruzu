// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/engine_upload.h and engine_upload.cpp
//!
//! Implements the GPU inline-to-memory upload mechanism (P2MF / I2M).
//! Engines that support inline uploads (KeplerMemory, Maxwell3D) embed
//! an `upload::State` that accumulates data words and flushes them to
//! GPU virtual memory when the transfer completes.

use crate::rasterizer_interface::{RasterizerHandle, RasterizerInterface};
use crate::textures::decoders;
use parking_lot::Mutex;
use std::sync::Arc;

/// GPU virtual address type.
pub type GPUVAddr = u64;

/// Upload destination registers, corresponding to the C++ `Upload::Registers` struct.
///
/// Layout matches upstream at offset 0x60 within the engine register file.
#[derive(Debug, Clone, Copy, Default)]
pub struct Registers {
    pub line_length_in: u32,
    pub line_count: u32,
    pub dest: DestRegisters,
}

/// Destination surface descriptor embedded in `Registers`.
#[derive(Debug, Clone, Copy, Default)]
pub struct DestRegisters {
    pub address_high: u32,
    pub address_low: u32,
    pub pitch: u32,
    /// Packed block dimensions: bits [0:3] = width, [4:7] = height, [8:11] = depth.
    pub block_dims: u32,
    pub width: u32,
    pub height: u32,
    pub depth: u32,
    pub layer: u32,
    pub x: u32,
    pub y: u32,
}

impl DestRegisters {
    /// Compute the full GPU virtual address from high and low halves.
    pub fn address(&self) -> GPUVAddr {
        ((self.address_high as u64) << 32) | (self.address_low as u64)
    }

    /// Block width exponent (bits [0:3]).
    pub fn block_width(&self) -> u32 {
        self.block_dims & 0xF
    }

    /// Block height exponent (bits [4:7]).
    pub fn block_height(&self) -> u32 {
        (self.block_dims >> 4) & 0xF
    }

    /// Block depth exponent (bits [8:11]).
    pub fn block_depth(&self) -> u32 {
        (self.block_dims >> 8) & 0xF
    }
}

/// Upload state machine, corresponding to the C++ `Upload::State` class.
///
/// Accumulates inline data words and writes them to GPU memory when
/// a transfer is completed.
///
/// Upstream holds `MemoryManager&` and a rasterizer pointer bound through
/// `BindRasterizer`. Rust stores the same owner edges through an `Arc<Mutex<_>>`
/// memory-manager handle and a non-owning `RasterizerHandle`.
pub struct State {
    /// Current write offset within `inner_buffer`.
    write_offset: u32,
    /// Total number of bytes to copy for this transfer.
    copy_size: u32,
    /// Accumulation buffer for incoming data words.
    inner_buffer: Vec<u8>,
    /// Temporary buffer for block-linear swizzle operations.
    tmp_buffer: Vec<u8>,
    /// Whether the current transfer target is pitch-linear (vs block-linear).
    is_linear: bool,
    /// Upstream `MemoryManager& memory_manager`.
    memory_manager: Option<Arc<Mutex<crate::memory_manager::MemoryManager>>>,
    /// Upstream `VideoCore::RasterizerInterface* rasterizer`.
    rasterizer: Option<RasterizerHandle>,
}

impl State {
    /// Create a reduced upload state without its upstream `MemoryManager&`.
    ///
    /// Runtime owners call `new_with_memory_manager`. This reduced constructor
    /// stays crate-local so ownerless upload state cannot be introduced through
    /// the public runtime API by accident.
    #[cfg(test)]
    pub(crate) fn new() -> Self {
        Self {
            write_offset: 0,
            copy_size: 0,
            inner_buffer: Vec::new(),
            tmp_buffer: Vec::new(),
            is_linear: false,
            memory_manager: None,
            rasterizer: None,
        }
    }

    /// Create a new upload state with the upstream-owned `MemoryManager`.
    pub fn new_with_memory_manager(
        memory_manager: Arc<Mutex<crate::memory_manager::MemoryManager>>,
    ) -> Self {
        Self {
            write_offset: 0,
            copy_size: 0,
            inner_buffer: Vec::new(),
            tmp_buffer: Vec::new(),
            is_linear: false,
            memory_manager: Some(memory_manager),
            rasterizer: None,
        }
    }

    /// Bind the upstream `MemoryManager&` owner after reduced construction.
    #[cfg(test)]
    pub(crate) fn bind_memory_manager(
        &mut self,
        memory_manager: Arc<Mutex<crate::memory_manager::MemoryManager>>,
    ) {
        self.memory_manager = Some(memory_manager);
    }

    /// Binds a rasterizer to this engine.
    ///
    /// Corresponds to upstream `State::BindRasterizer`.
    pub fn bind_rasterizer(&mut self, rasterizer: &dyn RasterizerInterface) {
        self.rasterizer = Some(RasterizerHandle::from_ref(rasterizer));
    }

    /// Begin a new transfer. Called when the engine's exec register is written.
    ///
    /// Corresponds to `State::ProcessExec`.
    pub fn process_exec(&mut self, regs: &Registers, is_linear: bool) {
        self.write_offset = 0;
        self.copy_size = regs.line_length_in * regs.line_count;
        self.inner_buffer.resize(self.copy_size as usize, 0);
        self.is_linear = is_linear;
        if std::env::var_os("RUZU_TRACE_UPLOAD_EXEC").is_some() {
            log::info!(
                "engine_upload::process_exec line_length_in={} line_count={} copy_size={} linear={} dest=0x{:X}",
                regs.line_length_in,
                regs.line_count,
                self.copy_size,
                is_linear,
                regs.dest.address()
            );
        }
    }

    /// Append a single data word to the transfer buffer.
    ///
    /// Corresponds to `State::ProcessData(u32, bool)`.
    pub fn process_data_word(&mut self, regs: &Registers, data: u32, is_last_call: bool) {
        self.accumulate_word(data);
        if is_last_call {
            let buffer: Vec<u8> = self.inner_buffer.clone();
            self.process_data_bytes(regs, &buffer);
        }
    }

    /// Append multiple data words to the transfer buffer.
    ///
    /// Corresponds to `State::ProcessData(const u32*, size_t)`.
    pub fn process_data_multi(&mut self, regs: &Registers, data: &[u32]) {
        // Safe conversion: reinterpret &[u32] as &[u8] matching C++ reinterpret_cast.
        let byte_view: &[u8] =
            unsafe { std::slice::from_raw_parts(data.as_ptr() as *const u8, data.len() * 4) };
        let buffer: Vec<u8> = byte_view.to_vec();
        self.process_data_bytes(regs, &buffer);
    }

    /// Target GPU virtual address for the current transfer.
    pub fn exec_target_address(&self, regs: &Registers) -> GPUVAddr {
        regs.dest.address()
    }

    /// Total upload size in bytes.
    pub fn get_upload_size(&self) -> u32 {
        self.copy_size
    }

    // ── Internal helpers ─────────────────────────────────────────────────

    /// Accumulate a single u32 data word into the inner buffer.
    fn accumulate_word(&mut self, data: u32) {
        let sub_copy_size = std::cmp::min(4, self.copy_size - self.write_offset) as usize;
        let bytes = data.to_le_bytes();
        let offset = self.write_offset as usize;
        self.inner_buffer[offset..offset + sub_copy_size].copy_from_slice(&bytes[..sub_copy_size]);
        self.write_offset += sub_copy_size as u32;
    }

    /// Flush data to GPU memory.
    ///
    /// Corresponds to `State::ProcessData(span<const u8>)`.
    /// Upstream logic:
    ///   - Linear: iterate lines, call rasterizer->AccelerateInlineToMemory per line.
    ///   - Block-linear: compute BPP shift, read GPU memory, swizzle subrect, write back.
    fn process_data_bytes(&mut self, regs: &Registers, read_buffer: &[u8]) {
        let address = regs.dest.address();
        if std::env::var_os("RUZU_TRACE_UPLOAD_BYTES").is_some() {
            log::info!(
                "engine_upload::process_data_bytes target=0x{:X} bytes={} linear={} line_length={} line_count={}",
                address,
                read_buffer.len(),
                self.is_linear,
                regs.line_length_in,
                regs.line_count,
            );
        }
        if std::env::var_os("RUZU_TRACE_DMA_FLOW").is_some() {
            log::info!(
                "engine_upload::State::process_data_bytes target=0x{:X} bytes={} linear={} line_length={} line_count={} pitch={} width={} height={} depth={} x={} y={}",
                address,
                read_buffer.len(),
                self.is_linear,
                regs.line_length_in,
                regs.line_count,
                regs.dest.pitch,
                regs.dest.width,
                regs.dest.height,
                regs.dest.depth,
                regs.dest.x,
                regs.dest.y
            );
        }
        let Some(memory_manager) = self.memory_manager.as_ref().map(Arc::clone) else {
            log::warn!(
                "engine_upload::State::process_data_bytes: flush skipped (no MemoryManager bound)"
            );
            return;
        };
        if self.is_linear {
            // Linear copy: iterate lines, call rasterizer->AccelerateInlineToMemory
            // for each line. Upstream:
            //   for (line = 0; line < line_count; ++line) {
            //       dest_line = address + line * dest.pitch;
            //       buffer = read_buffer[line * line_length_in .. +line_length_in];
            //       rasterizer->AccelerateInlineToMemory(dest_line, line_length_in, buffer);
            //   }
            for line in 0..regs.line_count as usize {
                let dest_line = address + (line as u64) * (regs.dest.pitch as u64);
                let start = line * regs.line_length_in as usize;
                let end = start + regs.line_length_in as usize;
                if end <= read_buffer.len() {
                    let mut rasterizer = self.rasterizer.map(|handle| unsafe { handle.as_mut() });
                    if let Some(ref mut rast) = rasterizer {
                        if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
                            let ptr = *rast as *mut dyn RasterizerInterface;
                            log::info!(
                                "engine_upload::State::process_data_bytes calling_rasterizer ptr={:p} dest=0x{:X} size={}",
                                ptr,
                                dest_line,
                                regs.line_length_in
                            );
                        }
                        rast.accelerate_inline_to_memory(
                            dest_line,
                            regs.line_length_in as usize,
                            &read_buffer[start..end],
                        );
                    } else {
                        // Reduced fixtures may not bind a rasterizer; upstream runtime does.
                        memory_manager
                            .lock()
                            .write_block(dest_line, &read_buffer[start..end]);
                    }
                }
            }
        } else {
            // Block-linear copy: calculate BPP shift, swizzle subrect.
            // Upstream uses Common::FoldRight to compute bpp_shift as:
            //   min(4, min of countr_zero(width), countr_zero(x_elements),
            //       countr_zero(x_offset), countr_zero(address))
            let mut width = regs.dest.width;
            let mut x_elements = regs.line_length_in;
            let mut x_offset = regs.dest.x;

            // Compute bpp_shift matching upstream FoldRight(4, min(x, countr_zero(y)), ...)
            let bpp_shift = [width, x_elements, x_offset, address as u32]
                .iter()
                .fold(4u32, |acc, &val| acc.min(val.trailing_zeros()));

            width >>= bpp_shift;
            x_elements >>= bpp_shift;
            x_offset >>= bpp_shift;
            let bytes_per_pixel = 1u32 << bpp_shift;

            let dst_size = decoders::calculate_size(
                true,
                bytes_per_pixel,
                width,
                regs.dest.height,
                regs.dest.depth,
                regs.dest.block_height(),
                regs.dest.block_depth(),
            );

            // Read existing GPU memory into tmp_buffer. Upstream uses
            // GpuGuestMemoryScoped<SafeReadCachedWrite>.
            self.tmp_buffer.resize(dst_size, 0);
            memory_manager
                .lock()
                .read_block(address, &mut self.tmp_buffer);

            // Swizzle the upload data into the tiled buffer.
            decoders::swizzle_subrect(
                &mut self.tmp_buffer,
                read_buffer,
                bytes_per_pixel,
                width,
                regs.dest.height,
                regs.dest.depth,
                x_offset,
                regs.dest.y,
                x_elements,
                regs.line_count,
                regs.dest.block_height(),
                regs.dest.block_depth(),
                regs.line_length_in,
            );

            // Write the swizzled buffer back to GPU memory with the upstream cached-write path.
            memory_manager
                .lock()
                .write_block_cached(address, &self.tmp_buffer);
        }
    }
}

#[cfg(test)]
impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

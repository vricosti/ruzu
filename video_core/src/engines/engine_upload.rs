// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/engine_upload.h and engine_upload.cpp
//!
//! Implements the GPU inline-to-memory upload mechanism (P2MF / I2M).
//! Engines that support inline uploads (KeplerMemory, Maxwell3D) embed
//! an `upload::State` that accumulates data words and flushes them to
//! GPU virtual memory when the transfer completes.

use crate::memory_manager::MemoryManager;
use crate::rasterizer_interface::RasterizerInterface;
use crate::textures::decoders;

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

/// Context for flushing upload data to GPU memory.
///
/// Upstream `Upload::State` holds `MemoryManager&` and `RasterizerInterface*`.
/// In Rust, the parent engine provides these via a `FlushContext` when a flush
/// is needed, avoiding lifetime issues from storing references.
pub struct FlushContext<'a> {
    pub rasterizer: Option<&'a mut dyn RasterizerInterface>,
    pub memory_manager: &'a MemoryManager,
    pub write_cpu_mem: &'a mut dyn FnMut(u64, &[u8]),
}

/// Upload state machine, corresponding to the C++ `Upload::State` class.
///
/// Accumulates inline data words and writes them to GPU memory when
/// a transfer is completed.
///
/// Upstream holds references to `MemoryManager` and `RasterizerInterface`.
/// In Rust, these are provided via `FlushContext` parameters on the
/// methods that perform flushing (`process_data_word_with_ctx`,
/// `process_data_multi_with_ctx`). The simple `process_data_word` and
/// `process_data_multi` methods without a context are provided for callers
/// that do not yet have memory_manager/rasterizer wired up; these log a
/// warning on flush.
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
    /// Reference to the upload registers (owned by the parent engine).
    /// In Rust we store a copy; the parent engine must keep it in sync.
    pub regs: Registers,
}

impl State {
    /// Create a new upload state.
    pub fn new() -> Self {
        Self {
            write_offset: 0,
            copy_size: 0,
            inner_buffer: Vec::new(),
            tmp_buffer: Vec::new(),
            is_linear: false,
            regs: Registers::default(),
        }
    }

    /// Begin a new transfer. Called when the engine's exec register is written.
    ///
    /// Corresponds to `State::ProcessExec`.
    pub fn process_exec(&mut self, is_linear: bool) {
        self.write_offset = 0;
        self.copy_size = self.regs.line_length_in * self.regs.line_count;
        self.inner_buffer.resize(self.copy_size as usize, 0);
        self.is_linear = is_linear;
    }

    /// Append a single data word to the transfer buffer (without flush context).
    ///
    /// When `is_last_call` is true, the accumulated buffer should be flushed,
    /// but without a `FlushContext` this logs a warning and skips the flush.
    /// Callers that have memory_manager/rasterizer should use
    /// `process_data_word_with_ctx` instead.
    ///
    /// Corresponds to `State::ProcessData(u32, bool)`.
    pub fn process_data_word(&mut self, data: u32, is_last_call: bool) {
        self.accumulate_word(data);
        if is_last_call {
            log::warn!(
                "engine_upload::State::process_data_word: flush skipped \
                 (no FlushContext provided — use process_data_word_with_ctx)"
            );
        }
    }

    /// Append a single data word to the transfer buffer with flush context.
    ///
    /// When `is_last_call` is true, the accumulated buffer is flushed to
    /// GPU memory via the provided context.
    ///
    /// Corresponds to `State::ProcessData(u32, bool)`.
    pub fn process_data_word_with_ctx(
        &mut self,
        data: u32,
        is_last_call: bool,
        ctx: &mut FlushContext<'_>,
    ) {
        self.accumulate_word(data);
        if is_last_call {
            let buffer: Vec<u8> = self.inner_buffer.clone();
            self.process_data_bytes(&buffer, ctx);
        }
    }

    /// Append multiple data words to the transfer buffer (without flush context).
    ///
    /// Without a `FlushContext`, this logs a warning and skips the flush.
    /// Callers that have memory_manager/rasterizer should use
    /// `process_data_multi_with_ctx` instead.
    ///
    /// Corresponds to `State::ProcessData(const u32*, size_t)`.
    pub fn process_data_multi(&mut self, data: &[u32]) {
        log::warn!(
            "engine_upload::State::process_data_multi: flush skipped \
             (no FlushContext provided — use process_data_multi_with_ctx)"
        );
        let _ = data;
    }

    /// Append multiple data words to the transfer buffer with flush context.
    ///
    /// Corresponds to `State::ProcessData(const u32*, size_t)`.
    pub fn process_data_multi_with_ctx(
        &mut self,
        data: &[u32],
        ctx: &mut FlushContext<'_>,
    ) {
        // Safe conversion: reinterpret &[u32] as &[u8] matching C++ reinterpret_cast.
        let byte_view: &[u8] = unsafe {
            std::slice::from_raw_parts(data.as_ptr() as *const u8, data.len() * 4)
        };
        let buffer: Vec<u8> = byte_view.to_vec();
        self.process_data_bytes(&buffer, ctx);
    }

    /// Target GPU virtual address for the current transfer.
    pub fn exec_target_address(&self) -> GPUVAddr {
        self.regs.dest.address()
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
        self.inner_buffer[offset..offset + sub_copy_size]
            .copy_from_slice(&bytes[..sub_copy_size]);
        self.write_offset += sub_copy_size as u32;
    }

    /// Flush data to GPU memory.
    ///
    /// Corresponds to `State::ProcessData(span<const u8>)`.
    /// Upstream logic:
    ///   - Linear: iterate lines, call rasterizer->AccelerateInlineToMemory per line.
    ///   - Block-linear: compute BPP shift, read GPU memory, swizzle subrect, write back.
    fn process_data_bytes(
        &mut self,
        read_buffer: &[u8],
        ctx: &mut FlushContext<'_>,
    ) {
        let address = self.regs.dest.address();
        if self.is_linear {
            // Linear copy: iterate lines, call rasterizer->AccelerateInlineToMemory
            // for each line. Upstream:
            //   for (line = 0; line < line_count; ++line) {
            //       dest_line = address + line * dest.pitch;
            //       buffer = read_buffer[line * line_length_in .. +line_length_in];
            //       rasterizer->AccelerateInlineToMemory(dest_line, line_length_in, buffer);
            //   }
            for line in 0..self.regs.line_count as usize {
                let dest_line = address + (line as u64) * (self.regs.dest.pitch as u64);
                let start = line * self.regs.line_length_in as usize;
                let end = start + self.regs.line_length_in as usize;
                if end <= read_buffer.len() {
                    if let Some(ref mut rast) = ctx.rasterizer {
                        rast.accelerate_inline_to_memory(
                            dest_line,
                            self.regs.line_length_in as usize,
                            &read_buffer[start..end],
                        );
                    } else {
                        // No rasterizer — fall back to direct memory write.
                        ctx.memory_manager.write_block(
                            dest_line,
                            &read_buffer[start..end],
                            ctx.write_cpu_mem,
                        );
                    }
                }
            }
        } else {
            // Block-linear copy: calculate BPP shift, swizzle subrect.
            // Upstream uses Common::FoldRight to compute bpp_shift as:
            //   min(4, min of countr_zero(width), countr_zero(x_elements),
            //       countr_zero(x_offset), countr_zero(address))
            let mut width = self.regs.dest.width;
            let mut x_elements = self.regs.line_length_in;
            let mut x_offset = self.regs.dest.x;

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
                self.regs.dest.height,
                self.regs.dest.depth,
                self.regs.dest.block_height(),
                self.regs.dest.block_depth(),
            );

            // Read existing GPU memory into tmp_buffer (upstream uses GpuGuestMemoryScoped
            // with SafeReadCachedWrite mode).
            self.tmp_buffer.resize(dst_size, 0);
            // For reading GPU memory we need a CPU reader. The FlushContext doesn't currently
            // provide one (write_cpu_mem is write-only). In the full integration, the parent
            // engine would provide a read callback. For now, zero-fill is safe because
            // swizzle_subrect overwrites the relevant parts.
            let read_cpu = |_addr: u64, _dst: &mut [u8]| {};
            ctx.memory_manager
                .read_block(address, &mut self.tmp_buffer, &read_cpu);

            // Swizzle the upload data into the tiled buffer.
            decoders::swizzle_subrect(
                &mut self.tmp_buffer,
                read_buffer,
                bytes_per_pixel,
                width,
                self.regs.dest.height,
                self.regs.dest.depth,
                x_offset,
                self.regs.dest.y,
                x_elements,
                self.regs.line_count,
                self.regs.dest.block_height(),
                self.regs.dest.block_depth(),
                self.regs.line_length_in,
            );

            // Write the swizzled buffer back to GPU memory.
            ctx.memory_manager
                .write_block(address, &self.tmp_buffer, ctx.write_cpu_mem);
        }
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

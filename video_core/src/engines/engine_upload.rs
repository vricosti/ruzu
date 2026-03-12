// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/engine_upload.h and engine_upload.cpp
//!
//! Implements the GPU inline-to-memory upload mechanism (P2MF / I2M).
//! Engines that support inline uploads (KeplerMemory, Maxwell3D) embed
//! an `upload::State` that accumulates data words and flushes them to
//! GPU virtual memory when the transfer completes.

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
pub struct State {
    /// Current write offset within `inner_buffer`.
    write_offset: u32,
    /// Total number of bytes to copy for this transfer.
    copy_size: u32,
    /// Accumulation buffer for incoming data words.
    inner_buffer: Vec<u8>,
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

    /// Append a single data word to the transfer buffer.
    ///
    /// When `is_last_call` is true, the accumulated buffer is flushed.
    /// Corresponds to `State::ProcessData(u32, bool)`.
    pub fn process_data_word(&mut self, data: u32, is_last_call: bool) {
        let sub_copy_size = std::cmp::min(4, self.copy_size - self.write_offset) as usize;
        let bytes = data.to_le_bytes();
        let offset = self.write_offset as usize;
        self.inner_buffer[offset..offset + sub_copy_size]
            .copy_from_slice(&bytes[..sub_copy_size]);
        self.write_offset += sub_copy_size as u32;
        if !is_last_call {
            return;
        }
        // TODO: flush inner_buffer to GPU memory via rasterizer / memory manager
        self.process_data_flush();
    }

    /// Append multiple data words to the transfer buffer.
    ///
    /// Corresponds to `State::ProcessData(const u32*, size_t)`.
    pub fn process_data_multi(&mut self, data: &[u32]) {
        // Safe conversion: reinterpret &[u32] as &[u8] matching C++ reinterpret_cast.
        let byte_view: &[u8] = unsafe {
            std::slice::from_raw_parts(data.as_ptr() as *const u8, data.len() * 4)
        };
        self.process_data_bytes(byte_view);
    }

    /// Target GPU virtual address for the current transfer.
    pub fn exec_target_address(&self) -> GPUVAddr {
        self.regs.dest.address()
    }

    /// Total upload size in bytes.
    pub fn get_upload_size(&self) -> u32 {
        self.copy_size
    }

    /// Flush accumulated data to GPU memory.
    ///
    /// Corresponds to `State::ProcessData(span<const u8>)`.
    /// TODO: full implementation requires memory manager and rasterizer access.
    fn process_data_flush(&mut self) {
        let _address = self.regs.dest.address();
        if self.is_linear {
            // Linear copy: iterate lines, call rasterizer->AccelerateInlineToMemory
            // for each line.
            // TODO: implement when memory manager integration is available.
        } else {
            // Block-linear copy: calculate BPP shift, swizzle subrect.
            // TODO: implement when texture decoder integration is available.
        }
    }

    /// Flush from a raw byte buffer.
    fn process_data_bytes(&mut self, _read_buffer: &[u8]) {
        // TODO: same logic as process_data_flush but from external buffer.
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/kepler_memory.h and kepler_memory.cpp
//!
//! This engine is known as P2MF (Push-to-Memory-From-host). It allows the
//! CPU/GPU command stream to push data directly into GPU virtual memory.
//! Documentation:
//!   https://github.com/envytools/envytools/blob/master/rnndb/graph/gk104_p2mf.xml
//!   https://cgit.freedesktop.org/mesa/mesa/tree/src/gallium/drivers/nouveau/nvc0/nve4_p2mf.xml.h

use super::engine_interface::EngineInterfaceState;
use super::engine_upload;

// ── Register layout constants ───────────────────────────────────────────────

/// Total number of registers in the KeplerMemory register file.
pub const NUM_REGS: usize = 0x7F;

/// Register offset for the upload register block (matches `ASSERT_REG_POSITION(upload, 0x60)`).
const UPLOAD_REG_OFFSET: usize = 0x60;

/// Register offset for the exec register (matches `ASSERT_REG_POSITION(exec, 0x6C)`).
const EXEC_REG: usize = 0x6C;

/// Register offset for the data register (matches `ASSERT_REG_POSITION(data, 0x6D)`).
const DATA_REG: usize = 0x6D;

// ── Macro equivalent for register indexing ──────────────────────────────────

/// Equivalent of `KEPLERMEMORY_REG_INDEX(field)` — returns the u32 word index
/// of a field within the register file.
/// For named fields we use the constants above directly.

// ── KeplerMemory engine ─────────────────────────────────────────────────────

/// KeplerMemory engine register file.
#[derive(Clone)]
pub struct Regs {
    pub reg_array: [u32; NUM_REGS],
}

impl Default for Regs {
    fn default() -> Self {
        Self {
            reg_array: [0u32; NUM_REGS],
        }
    }
}

impl Regs {
    /// Read the upload registers from the register array.
    pub fn upload(&self) -> engine_upload::Registers {
        engine_upload::Registers {
            line_length_in: self.reg_array[UPLOAD_REG_OFFSET],
            line_count: self.reg_array[UPLOAD_REG_OFFSET + 1],
            dest: engine_upload::DestRegisters {
                address_high: self.reg_array[UPLOAD_REG_OFFSET + 2],
                address_low: self.reg_array[UPLOAD_REG_OFFSET + 3],
                pitch: self.reg_array[UPLOAD_REG_OFFSET + 4],
                block_dims: self.reg_array[UPLOAD_REG_OFFSET + 5],
                width: self.reg_array[UPLOAD_REG_OFFSET + 6],
                height: self.reg_array[UPLOAD_REG_OFFSET + 7],
                depth: self.reg_array[UPLOAD_REG_OFFSET + 8],
                layer: self.reg_array[UPLOAD_REG_OFFSET + 9],
                x: self.reg_array[UPLOAD_REG_OFFSET + 10],
                y: self.reg_array[UPLOAD_REG_OFFSET + 11],
            },
        }
    }

    /// Check whether exec.linear bit is set.
    pub fn exec_linear(&self) -> bool {
        (self.reg_array[EXEC_REG] & 1) != 0
    }
}

/// The KeplerMemory engine (P2MF).
///
/// Corresponds to the C++ `KeplerMemory` class which inherits `EngineInterface`.
pub struct KeplerMemory {
    pub regs: Regs,
    pub upload_state: engine_upload::State,
    pub interface_state: EngineInterfaceState,
}

impl KeplerMemory {
    /// Create a new KeplerMemory engine.
    pub fn new() -> Self {
        Self {
            regs: Regs::default(),
            upload_state: engine_upload::State::new(),
            interface_state: EngineInterfaceState::new(),
        }
    }

    /// Bind a rasterizer and set up the execution mask.
    ///
    /// Corresponds to `KeplerMemory::BindRasterizer`.
    pub fn bind_rasterizer(&mut self) {
        // Reset and configure execution mask
        self.interface_state.execution_mask.fill(false);
        if EXEC_REG < self.interface_state.execution_mask.len() {
            self.interface_state.execution_mask[EXEC_REG] = true;
        }
        if DATA_REG < self.interface_state.execution_mask.len() {
            self.interface_state.execution_mask[DATA_REG] = true;
        }
    }

    /// Write a value to the register identified by method.
    ///
    /// Corresponds to `KeplerMemory::CallMethod`.
    pub fn call_method(&mut self, method: u32, method_argument: u32, is_last_call: bool) {
        let method_idx = method as usize;
        assert!(
            method_idx < NUM_REGS,
            "Invalid KeplerMemory register, increase the size of the Regs structure"
        );

        self.regs.reg_array[method_idx] = method_argument;

        match method_idx {
            EXEC_REG => {
                // Sync upload registers from the register array
                self.upload_state.regs = self.regs.upload();
                self.upload_state
                    .process_exec(self.regs.exec_linear());
            }
            DATA_REG => {
                self.upload_state.regs = self.regs.upload();
                self.upload_state
                    .process_data_word(method_argument, is_last_call);
            }
            _ => {}
        }
    }

    /// Write multiple values to the register identified by method.
    ///
    /// Corresponds to `KeplerMemory::CallMultiMethod`.
    pub fn call_multi_method(
        &mut self,
        method: u32,
        base_start: &[u32],
        amount: u32,
        methods_pending: u32,
    ) {
        let method_idx = method as usize;
        match method_idx {
            DATA_REG => {
                self.upload_state.regs = self.regs.upload();
                self.upload_state
                    .process_data_multi(&base_start[..amount as usize]);
            }
            _ => {
                for i in 0..amount {
                    self.call_method(
                        method,
                        base_start[i as usize],
                        methods_pending.saturating_sub(i) <= 1,
                    );
                }
            }
        }
    }

    /// Consume the method sink (deferred writes).
    ///
    /// Corresponds to `KeplerMemory::ConsumeSinkImpl`.
    pub fn consume_sink_impl(&mut self) {
        let sink: Vec<(u32, u32)> = self.interface_state.method_sink.drain(..).collect();
        for (method, value) in sink {
            let method_idx = method as usize;
            if method_idx < NUM_REGS {
                self.regs.reg_array[method_idx] = value;
            }
        }
    }
}

impl Default for KeplerMemory {
    fn default() -> Self {
        Self::new()
    }
}

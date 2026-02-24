// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPFIFO command processor.
//!
//! Parses 64-bit GPFIFO entries, reads pushbuffer data from guest memory via
//! the GPU memory manager, unpacks 32-bit compressed method headers, and
//! dispatches register writes to the appropriate engine subchannels.
//!
//! ## GPFIFO Entry Format (64 bits)
//!
//! | Bits    | Field             |
//! |---------|-------------------|
//! | 1:0     | Fetch mode        |
//! | 39:2    | Address >> 2      |
//! | 41:40   | Unused            |
//! | 42      | Priv / no prefetch|
//! | 62:43   | Length (in dwords) |
//! | 63      | Sync              |
//!
//! ## Compressed Method Header (32 bits)
//!
//! | Bits    | Field             |
//! |---------|-------------------|
//! | 12:0    | Method address    |
//! | 15:13   | Subchannel        |
//! | 28:16   | Method count / Immd data |
//! | 31:29   | SecOp             |

use crate::engines::Engine;

/// A 64-bit GPFIFO entry.
#[derive(Debug, Clone, Copy, Default)]
pub struct GpEntry {
    pub entry0: u32,
    pub entry1: u32,
}

impl GpEntry {
    /// GPU virtual address of the pushbuffer (byte-aligned).
    pub fn address(&self) -> u64 {
        let lo = (self.entry0 as u64 >> 2) << 2;
        let hi = (self.entry1 as u64 & 0xFF) << 32;
        hi | lo
    }

    /// Length of the pushbuffer in 32-bit words.
    pub fn length(&self) -> u32 {
        (self.entry1 >> 10) & 0x1F_FFFF
    }
}

/// Secondary operation type from compressed method header bits [31:29].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SecOp {
    Grp0UseTert = 0,
    IncMethod = 1,
    Grp2UseTert = 2,
    NonIncMethod = 3,
    ImmdDataMethod = 4,
    OneInc = 5,
    Reserved6 = 6,
    EndPbSegment = 7,
}

impl SecOp {
    fn from_raw(v: u32) -> Self {
        match v {
            0 => Self::Grp0UseTert,
            1 => Self::IncMethod,
            2 => Self::Grp2UseTert,
            3 => Self::NonIncMethod,
            4 => Self::ImmdDataMethod,
            5 => Self::OneInc,
            6 => Self::Reserved6,
            7 => Self::EndPbSegment,
            _ => unreachable!(),
        }
    }
}

/// A parsed compressed method header.
#[derive(Debug, Clone, Copy)]
pub struct CompressedMethod(u32);

impl CompressedMethod {
    pub fn new(raw: u32) -> Self {
        Self(raw)
    }

    /// Method register address (bits [12:0]).
    pub fn method_address(&self) -> u32 {
        self.0 & 0x1FFF
    }

    /// Subchannel index (bits [15:13]).
    pub fn subchannel(&self) -> u32 {
        (self.0 >> 13) & 0x7
    }

    /// Method count (bits [28:16]) — for IncMethod, NonIncMethod, OneInc.
    pub fn method_count(&self) -> u32 {
        (self.0 >> 16) & 0x1FFF
    }

    /// Immediate data (bits [28:16]) — for ImmdDataMethod.
    pub fn immd_data(&self) -> u32 {
        (self.0 >> 16) & 0x1FFF
    }

    /// Secondary operation (bits [31:29]).
    pub fn sec_op(&self) -> SecOp {
        SecOp::from_raw((self.0 >> 29) & 0x7)
    }
}

/// Number of engine subchannels.
const NUM_SUBCHANNELS: usize = 8;

/// Processes GPFIFO entries by parsing compressed method headers and
/// dispatching register writes to engine subchannels.
pub struct CommandProcessor {
    engines: Vec<Option<Box<dyn Engine>>>,
}

impl CommandProcessor {
    pub fn new(engines: Vec<Option<Box<dyn Engine>>>) -> Self {
        assert!(engines.len() <= NUM_SUBCHANNELS);
        let mut e = engines;
        e.resize_with(NUM_SUBCHANNELS, || None);
        Self { engines: e }
    }

    /// Process a list of GPFIFO entries by reading pushbuffer data from memory.
    ///
    /// `read_mem` reads bytes from a GPU virtual address into a buffer.
    pub fn process_entries(&mut self, entries: &[GpEntry], read_mem: &dyn Fn(u64, &mut [u8])) {
        for entry in entries {
            let addr = entry.address();
            let len = entry.length() as usize;

            if len == 0 {
                continue;
            }

            // Read the pushbuffer words from GPU memory.
            let mut buf = vec![0u8; len * 4];
            read_mem(addr, &mut buf);

            // Convert to u32 words.
            let words: Vec<u32> = buf
                .chunks_exact(4)
                .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]]))
                .collect();

            self.process_pushbuffer(&words);
        }
    }

    /// Process a pushbuffer (sequence of compressed method headers + data words).
    fn process_pushbuffer(&mut self, data: &[u32]) {
        let mut pos = 0;

        while pos < data.len() {
            let header = CompressedMethod::new(data[pos]);
            pos += 1;

            let subchannel = header.subchannel();
            let mut method = header.method_address();
            let count = header.method_count() as usize;

            match header.sec_op() {
                SecOp::IncMethod => {
                    // Incrementing method: each data word goes to method, method+1, etc.
                    for i in 0..count {
                        if pos >= data.len() {
                            break;
                        }
                        self.dispatch(subchannel, method + i as u32, data[pos]);
                        pos += 1;
                    }
                }
                SecOp::NonIncMethod => {
                    // Non-incrementing: all data words go to the same method.
                    for _ in 0..count {
                        if pos >= data.len() {
                            break;
                        }
                        self.dispatch(subchannel, method, data[pos]);
                        pos += 1;
                    }
                }
                SecOp::OneInc => {
                    // First word increments, rest go to the same (method+1).
                    for i in 0..count {
                        if pos >= data.len() {
                            break;
                        }
                        self.dispatch(subchannel, method, data[pos]);
                        pos += 1;
                        if i == 0 {
                            method += 1;
                        }
                    }
                }
                SecOp::ImmdDataMethod => {
                    // Immediate data is embedded in the header.
                    self.dispatch(subchannel, method, header.immd_data());
                }
                SecOp::EndPbSegment => {
                    // End of this pushbuffer segment.
                    break;
                }
                SecOp::Grp0UseTert | SecOp::Grp2UseTert | SecOp::Reserved6 => {
                    log::trace!(
                        "CommandProcessor: unhandled SecOp {:?} at pos {}",
                        header.sec_op(),
                        pos - 1
                    );
                    // Skip the data words.
                    pos += count;
                }
            }
        }
    }

    /// Dispatch a single register write to the appropriate engine.
    fn dispatch(&mut self, subchannel: u32, method: u32, value: u32) {
        let idx = subchannel as usize;
        if idx < self.engines.len() {
            if let Some(engine) = &mut self.engines[idx] {
                engine.write_reg(method, value);
            } else {
                log::trace!(
                    "CommandProcessor: no engine on subchannel {} (method=0x{:X}, value=0x{:X})",
                    subchannel,
                    method,
                    value
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engines::{ClassId, Engine};

    /// Test engine that records register writes.
    struct RecordingEngine {
        writes: Vec<(u32, u32)>,
    }

    impl RecordingEngine {
        fn new() -> Self {
            Self { writes: Vec::new() }
        }
    }

    impl Engine for RecordingEngine {
        fn class_id(&self) -> ClassId {
            ClassId::Threed
        }
        fn write_reg(&mut self, method: u32, value: u32) {
            self.writes.push((method, value));
        }
    }

    #[test]
    fn test_gp_entry_address_and_length() {
        let entry = GpEntry {
            entry0: 0x0000_1000,
            entry1: 0x0000_0400, // length = (0x400 >> 10) & 0x1FFFFF = 1
        };
        assert_eq!(entry.address(), 0x1000);
        assert_eq!(entry.length(), 1);
    }

    #[test]
    fn test_gp_entry_high_address() {
        // Test with high address bits in entry1.
        let entry = GpEntry {
            entry0: 0x0000_2000,
            entry1: 0x0000_0001, // high byte = 0x01
        };
        assert_eq!(entry.address(), 0x1_0000_2000);
    }

    #[test]
    fn test_compressed_method_fields() {
        // SecOp=1 (IncMethod), subchannel=0, method=0x100, count=3
        let raw = (1u32 << 29) | (3 << 16) | (0 << 13) | 0x100;
        let hdr = CompressedMethod::new(raw);
        assert_eq!(hdr.sec_op(), SecOp::IncMethod);
        assert_eq!(hdr.subchannel(), 0);
        assert_eq!(hdr.method_address(), 0x100);
        assert_eq!(hdr.method_count(), 3);
    }

    #[test]
    fn test_process_inc_method() {
        let engine = Box::new(RecordingEngine::new());
        let engine_ptr = &*engine as *const RecordingEngine;
        let mut proc = CommandProcessor::new(vec![Some(engine)]);

        // Build pushbuffer: IncMethod, subchannel=0, method=0x10, count=2
        let header = (1u32 << 29) | (2 << 16) | (0 << 13) | 0x10;
        let data = vec![header, 0xAAAA, 0xBBBB];

        proc.process_pushbuffer(&data);

        let engine = unsafe { &*engine_ptr };
        assert_eq!(engine.writes.len(), 2);
        assert_eq!(engine.writes[0], (0x10, 0xAAAA));
        assert_eq!(engine.writes[1], (0x11, 0xBBBB));
    }

    #[test]
    fn test_process_non_inc_method() {
        let engine = Box::new(RecordingEngine::new());
        let engine_ptr = &*engine as *const RecordingEngine;
        let mut proc = CommandProcessor::new(vec![Some(engine)]);

        // NonIncMethod (SecOp=3), subchannel=0, method=0x20, count=3
        let header = (3u32 << 29) | (3 << 16) | (0 << 13) | 0x20;
        let data = vec![header, 0x11, 0x22, 0x33];

        proc.process_pushbuffer(&data);

        let engine = unsafe { &*engine_ptr };
        assert_eq!(engine.writes.len(), 3);
        // All writes go to the same method.
        assert_eq!(engine.writes[0], (0x20, 0x11));
        assert_eq!(engine.writes[1], (0x20, 0x22));
        assert_eq!(engine.writes[2], (0x20, 0x33));
    }

    #[test]
    fn test_process_immd_method() {
        let engine = Box::new(RecordingEngine::new());
        let engine_ptr = &*engine as *const RecordingEngine;
        let mut proc = CommandProcessor::new(vec![Some(engine)]);

        // ImmdDataMethod (SecOp=4), subchannel=0, method=0x30, immd_data=0x42
        let header = (4u32 << 29) | (0x42 << 16) | (0 << 13) | 0x30;
        let data = vec![header];

        proc.process_pushbuffer(&data);

        let engine = unsafe { &*engine_ptr };
        assert_eq!(engine.writes.len(), 1);
        assert_eq!(engine.writes[0], (0x30, 0x42));
    }

    #[test]
    fn test_process_one_inc() {
        let engine = Box::new(RecordingEngine::new());
        let engine_ptr = &*engine as *const RecordingEngine;
        let mut proc = CommandProcessor::new(vec![Some(engine)]);

        // OneInc (SecOp=5), subchannel=0, method=0x50, count=3
        let header = (5u32 << 29) | (3 << 16) | (0 << 13) | 0x50;
        let data = vec![header, 0xAA, 0xBB, 0xCC];

        proc.process_pushbuffer(&data);

        let engine = unsafe { &*engine_ptr };
        assert_eq!(engine.writes.len(), 3);
        assert_eq!(engine.writes[0], (0x50, 0xAA)); // first goes to method
        assert_eq!(engine.writes[1], (0x51, 0xBB)); // rest go to method+1
        assert_eq!(engine.writes[2], (0x51, 0xCC));
    }

    #[test]
    fn test_process_entries_with_read() {
        let engine = Box::new(RecordingEngine::new());
        let engine_ptr = &*engine as *const RecordingEngine;
        let mut proc = CommandProcessor::new(vec![Some(engine)]);

        // Build pushbuffer data: IncMethod header + 1 data word.
        let header = (1u32 << 29) | (1 << 16) | (0 << 13) | 0x10;
        let value = 0xDEAD_BEEF_u32;
        let mut mem_data = Vec::new();
        mem_data.extend_from_slice(&header.to_le_bytes());
        mem_data.extend_from_slice(&value.to_le_bytes());

        // GpEntry pointing to address 0, length 2 words.
        let entry = GpEntry {
            entry0: 0,
            entry1: 2 << 10, // length = 2
        };

        let read_mem = |_addr: u64, buf: &mut [u8]| {
            let len = buf.len().min(mem_data.len());
            buf[..len].copy_from_slice(&mem_data[..len]);
        };

        proc.process_entries(&[entry], &read_mem);

        let engine = unsafe { &*engine_ptr };
        assert_eq!(engine.writes.len(), 1);
        assert_eq!(engine.writes[0], (0x10, 0xDEAD_BEEF));
    }
}

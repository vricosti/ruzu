// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! HID shared memory layout matching the Nintendo Switch's `hid` service.
//!
//! The HID shared memory is 0x40000 bytes (256 KB). This module provides a
//! `HidSharedMemory` struct that maintains an in-memory buffer and updates
//! the Npad handheld LIFO ring buffer each frame.

/// Total size of the HID shared memory region.
pub const HID_SHARED_MEMORY_SIZE: usize = 0x40000;

/// Offset of the first Npad entry within HID shared memory.
const NPAD_OFFSET: usize = 0x9A00;

/// Size of each Npad entry.
const NPAD_ENTRY_SIZE: usize = 0x5000;

/// Number of entries in each LIFO ring buffer.
const LIFO_ENTRY_COUNT: usize = 17;

// Npad internal state layout offsets (from entry base):
//   +0x00: style_tag (u32)
//   +0x04: assignment_mode (u32)
//   +0x08: fullkey_color (12 bytes)
//   +0x14: joycon_color (20 bytes)
//   +0x28: fullkey_lifo (0x350 bytes)
//   +0x378: handheld_lifo (0x350 bytes)
//   ...

/// Offset of the handheld LIFO within an Npad entry.
const HANDHELD_LIFO_OFFSET: usize = 0x378;

/// Size of one LIFO: 0x20 header + 17 * 0x30 entries = 0x350.
const LIFO_SIZE: usize = 0x350;

/// Size of one LIFO entry (NpadGenericState).
const LIFO_ENTRY_SIZE: usize = 0x30;

/// LIFO header layout:
///   +0x00: timestamp (i64)
///   +0x08: total_count (i64)
///   +0x10: tail (i64)
///   +0x18: count (i64)
const LIFO_HEADER_SIZE: usize = 0x20;

/// NpadGenericState layout (0x30 bytes total, but only 0x28 used):
///   +0x00: sampling_number (i64)
///   +0x08: buttons (u64)
///   +0x10: l_stick_x (i32)
///   +0x14: l_stick_y (i32)
///   +0x18: r_stick_x (i32)
///   +0x1C: r_stick_y (i32)
///   +0x20: connection_status (u32)
///   +0x24: reserved (u32)
///   +0x28: (8 bytes padding to 0x30)

/// Switch controller button bitmasks.
pub mod buttons {
    pub const A: u64 = 1 << 0;
    pub const B: u64 = 1 << 1;
    pub const X: u64 = 1 << 2;
    pub const Y: u64 = 1 << 3;
    pub const LSTICK: u64 = 1 << 4;
    pub const RSTICK: u64 = 1 << 5;
    pub const L: u64 = 1 << 6;
    pub const R: u64 = 1 << 7;
    pub const ZL: u64 = 1 << 8;
    pub const ZR: u64 = 1 << 9;
    pub const PLUS: u64 = 1 << 10;
    pub const MINUS: u64 = 1 << 11;
    pub const DPAD_LEFT: u64 = 1 << 12;
    pub const DPAD_UP: u64 = 1 << 13;
    pub const DPAD_RIGHT: u64 = 1 << 14;
    pub const DPAD_DOWN: u64 = 1 << 15;
}

/// Connection status flags for NpadGenericState.
const CONNECTED: u32 = 0x1;
const HANDHELD: u32 = 0x2;
const USING_BODY: u32 = 0x4;

/// HID shared memory buffer with ring buffer management.
pub struct HidSharedMemory {
    /// The raw 256 KB buffer.
    pub data: Vec<u8>,
    /// Monotonic sampling number (incremented each update).
    sampling_number: i64,
}

impl HidSharedMemory {
    /// Create a new zeroed HID shared memory buffer with initialized headers.
    pub fn new() -> Self {
        let mut shm = Self {
            data: vec![0u8; HID_SHARED_MEMORY_SIZE],
            sampling_number: 0,
        };

        // Initialize the first Npad entry (player 1 / handheld).
        let entry_base = NPAD_OFFSET;

        // Style tag: 0x20 = NpadHandheld
        shm.write_u32(entry_base, 0x20);

        // Initialize handheld LIFO header.
        let lifo_base = entry_base + HANDHELD_LIFO_OFFSET;
        shm.write_i64(lifo_base, 0); // timestamp
        shm.write_i64(lifo_base + 8, 0); // total_count
        shm.write_i64(lifo_base + 16, 0); // tail
        shm.write_i64(lifo_base + 24, LIFO_ENTRY_COUNT as i64); // count (ring capacity)

        shm
    }

    /// Update the handheld controller state in the ring buffer.
    pub fn update_input(&mut self, btns: u64, l_stick: (i32, i32), r_stick: (i32, i32)) {
        let entry_base = NPAD_OFFSET;
        let lifo_base = entry_base + HANDHELD_LIFO_OFFSET;

        // Read current total_count to compute tail.
        let total_count = self.read_i64(lifo_base + 8);
        let tail = total_count % LIFO_ENTRY_COUNT as i64;

        // Compute entry offset within the LIFO.
        let entry_offset = lifo_base + LIFO_HEADER_SIZE + (tail as usize) * LIFO_ENTRY_SIZE;

        // Write NpadGenericState.
        self.sampling_number += 1;
        self.write_i64(entry_offset, self.sampling_number); // sampling_number
        self.write_u64(entry_offset + 8, btns); // buttons
        self.write_i32(entry_offset + 0x10, l_stick.0); // l_stick_x
        self.write_i32(entry_offset + 0x14, l_stick.1); // l_stick_y
        self.write_i32(entry_offset + 0x18, r_stick.0); // r_stick_x
        self.write_i32(entry_offset + 0x1C, r_stick.1); // r_stick_y
        self.write_u32(entry_offset + 0x20, CONNECTED | HANDHELD | USING_BODY); // connection_status

        // Update LIFO header.
        self.write_i64(lifo_base, self.sampling_number); // timestamp
        self.write_i64(lifo_base + 8, total_count + 1); // total_count
        self.write_i64(lifo_base + 16, tail); // tail index
    }

    // ── Byte-level helpers ─────────────────────────────────────────────────

    fn write_u32(&mut self, offset: usize, val: u32) {
        if offset + 4 <= self.data.len() {
            self.data[offset..offset + 4].copy_from_slice(&val.to_le_bytes());
        }
    }

    fn write_u64(&mut self, offset: usize, val: u64) {
        if offset + 8 <= self.data.len() {
            self.data[offset..offset + 8].copy_from_slice(&val.to_le_bytes());
        }
    }

    fn write_i32(&mut self, offset: usize, val: i32) {
        if offset + 4 <= self.data.len() {
            self.data[offset..offset + 4].copy_from_slice(&val.to_le_bytes());
        }
    }

    fn write_i64(&mut self, offset: usize, val: i64) {
        if offset + 8 <= self.data.len() {
            self.data[offset..offset + 8].copy_from_slice(&val.to_le_bytes());
        }
    }

    fn read_i64(&self, offset: usize) -> i64 {
        if offset + 8 <= self.data.len() {
            i64::from_le_bytes(self.data[offset..offset + 8].try_into().unwrap())
        } else {
            0
        }
    }
}

impl Default for HidSharedMemory {
    fn default() -> Self {
        Self::new()
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_hid_shared_memory() {
        let shm = HidSharedMemory::new();
        assert_eq!(shm.data.len(), HID_SHARED_MEMORY_SIZE);

        // Check style tag is set to NpadHandheld (0x20).
        let style_tag = u32::from_le_bytes(
            shm.data[NPAD_OFFSET..NPAD_OFFSET + 4].try_into().unwrap(),
        );
        assert_eq!(style_tag, 0x20);
    }

    #[test]
    fn test_update_input_increments_sampling_number() {
        let mut shm = HidSharedMemory::new();
        shm.update_input(buttons::A, (0, 0), (0, 0));
        assert_eq!(shm.sampling_number, 1);

        shm.update_input(buttons::B, (100, -100), (0, 0));
        assert_eq!(shm.sampling_number, 2);
    }

    #[test]
    fn test_update_input_writes_buttons() {
        let mut shm = HidSharedMemory::new();
        shm.update_input(buttons::A | buttons::DPAD_UP, (0, 0), (0, 0));

        let lifo_base = NPAD_OFFSET + HANDHELD_LIFO_OFFSET;
        let entry_offset = lifo_base + LIFO_HEADER_SIZE;

        let btns = u64::from_le_bytes(
            shm.data[entry_offset + 8..entry_offset + 16]
                .try_into()
                .unwrap(),
        );
        assert_eq!(btns, buttons::A | buttons::DPAD_UP);
    }

    #[test]
    fn test_ring_buffer_wraps() {
        let mut shm = HidSharedMemory::new();

        // Fill 17 entries (one full ring).
        for i in 0..LIFO_ENTRY_COUNT {
            shm.update_input(i as u64, (0, 0), (0, 0));
        }

        let lifo_base = NPAD_OFFSET + HANDHELD_LIFO_OFFSET;
        let total = shm.read_i64(lifo_base + 8);
        assert_eq!(total, LIFO_ENTRY_COUNT as i64);

        // Next update wraps to slot 0.
        shm.update_input(0xFF, (0, 0), (0, 0));
        let tail = shm.read_i64(lifo_base + 16);
        assert_eq!(tail, 0);
    }

    #[test]
    fn test_connection_status() {
        let mut shm = HidSharedMemory::new();
        shm.update_input(0, (0, 0), (0, 0));

        let lifo_base = NPAD_OFFSET + HANDHELD_LIFO_OFFSET;
        let entry_offset = lifo_base + LIFO_HEADER_SIZE;
        let status = u32::from_le_bytes(
            shm.data[entry_offset + 0x20..entry_offset + 0x24]
                .try_into()
                .unwrap(),
        );
        assert_eq!(status, CONNECTED | HANDHELD | USING_BODY);
    }
}

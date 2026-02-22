// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Minimal Android Parcel serialization for Binder IPC used by the VI service.
//!
//! Wire format:
//! ```text
//! [0x00] data_size: u32
//! [0x04] data_offset: u32  (typically 0x10)
//! [0x08] objects_size: u32
//! [0x0C] objects_offset: u32
//! [data_offset..] data bytes
//! [objects_offset..] object bytes
//! ```

/// Android Parcel for Binder IPC serialization/deserialization.
pub struct Parcel {
    data: Vec<u8>,
    objects: Vec<u8>,
    /// Read cursor position for deserialization.
    read_pos: usize,
}

impl Parcel {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            objects: Vec::new(),
            read_pos: 0,
        }
    }

    // ── Writing ─────────────────────────────────────────────────────────

    pub fn write_u32(&mut self, val: u32) {
        self.data.extend_from_slice(&val.to_le_bytes());
    }

    pub fn write_i32(&mut self, val: i32) {
        self.data.extend_from_slice(&val.to_le_bytes());
    }

    pub fn write_i64(&mut self, val: i64) {
        self.data.extend_from_slice(&val.to_le_bytes());
    }

    pub fn write_u64(&mut self, val: u64) {
        self.data.extend_from_slice(&val.to_le_bytes());
    }

    /// Write a UTF-16LE string with length prefix (Android Parcel format).
    pub fn write_string16(&mut self, s: &str) {
        let utf16: Vec<u16> = s.encode_utf16().collect();
        self.write_i32(utf16.len() as i32);
        for &ch in &utf16 {
            self.data.extend_from_slice(&ch.to_le_bytes());
        }
        // Null terminator (u16).
        self.data.extend_from_slice(&0u16.to_le_bytes());
        // Align to 4 bytes.
        while self.data.len() % 4 != 0 {
            self.data.push(0);
        }
    }

    /// Write an interface token (used for Binder transaction validation).
    pub fn write_interface_token(&mut self, token: &str) {
        // Strict mode policy (Android compat): write a u32 first.
        self.write_u32(0x100); // strict mode policy
        self.write_string16(token);
    }

    /// Serialize the Parcel to its wire format.
    pub fn serialize(&self) -> Vec<u8> {
        let data_offset = 0x10u32;
        let objects_offset = data_offset + self.data.len() as u32;

        let mut buf = Vec::new();
        buf.extend_from_slice(&(self.data.len() as u32).to_le_bytes());
        buf.extend_from_slice(&data_offset.to_le_bytes());
        buf.extend_from_slice(&(self.objects.len() as u32).to_le_bytes());
        buf.extend_from_slice(&objects_offset.to_le_bytes());
        buf.extend_from_slice(&self.data);
        buf.extend_from_slice(&self.objects);

        buf
    }

    // ── Reading ─────────────────────────────────────────────────────────

    /// Deserialize a Parcel from its wire format.
    pub fn from_bytes(raw: &[u8]) -> Self {
        if raw.len() < 0x10 {
            return Self::new();
        }

        let data_size = u32::from_le_bytes(raw[0..4].try_into().unwrap_or([0; 4])) as usize;
        let data_offset = u32::from_le_bytes(raw[4..8].try_into().unwrap_or([0; 4])) as usize;
        let objects_size = u32::from_le_bytes(raw[8..12].try_into().unwrap_or([0; 4])) as usize;
        let objects_offset =
            u32::from_le_bytes(raw[12..16].try_into().unwrap_or([0; 4])) as usize;

        let data_end = data_offset.saturating_add(data_size).min(raw.len());
        let objects_end = objects_offset
            .saturating_add(objects_size)
            .min(raw.len());

        let data = if data_offset <= raw.len() {
            raw[data_offset..data_end].to_vec()
        } else {
            Vec::new()
        };

        let objects = if objects_offset <= raw.len() {
            raw[objects_offset..objects_end].to_vec()
        } else {
            Vec::new()
        };

        Self {
            data,
            objects,
            read_pos: 0,
        }
    }

    pub fn read_u32(&mut self) -> u32 {
        if self.read_pos + 4 <= self.data.len() {
            let val = u32::from_le_bytes(
                self.data[self.read_pos..self.read_pos + 4]
                    .try_into()
                    .unwrap(),
            );
            self.read_pos += 4;
            val
        } else {
            self.read_pos = self.data.len();
            0
        }
    }

    pub fn read_i32(&mut self) -> i32 {
        self.read_u32() as i32
    }

    pub fn read_i64(&mut self) -> i64 {
        let lo = self.read_u32() as u64;
        let hi = self.read_u32() as u64;
        (hi << 32 | lo) as i64
    }

    /// Remaining data bytes available for reading.
    pub fn remaining(&self) -> usize {
        self.data.len().saturating_sub(self.read_pos)
    }
}

impl Default for Parcel {
    fn default() -> Self {
        Self::new()
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serialize_deserialize_roundtrip() {
        let mut p = Parcel::new();
        p.write_u32(42);
        p.write_i32(-1);
        p.write_i64(0x1234_5678_ABCD);

        let bytes = p.serialize();
        let mut p2 = Parcel::from_bytes(&bytes);
        assert_eq!(p2.read_u32(), 42);
        assert_eq!(p2.read_i32(), -1);
        assert_eq!(p2.read_i64(), 0x1234_5678_ABCD);
    }

    #[test]
    fn test_string16() {
        let mut p = Parcel::new();
        p.write_string16("hello");

        let bytes = p.serialize();
        assert!(bytes.len() > 0x10);
    }

    #[test]
    fn test_empty_parcel() {
        let p = Parcel::new();
        let bytes = p.serialize();
        assert_eq!(bytes.len(), 0x10); // Just the header
    }

    #[test]
    fn test_from_bytes_too_small() {
        let p = Parcel::from_bytes(&[0u8; 4]);
        assert_eq!(p.data.len(), 0);
    }
}
